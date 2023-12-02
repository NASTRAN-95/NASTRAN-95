!*==polypt.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE polypt(Loctof,Stedge,Tr,Ngridf,Fledge,Fl,Locfos,Eps,Npoly,P)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) :: Loctof
   INTEGER , DIMENSION(2,3) :: Stedge
   REAL*8 , DIMENSION(3,3) :: Tr
   INTEGER :: Ngridf
   INTEGER , DIMENSION(2,4) :: Fledge
   REAL*8 , DIMENSION(3,4) :: Fl
   INTEGER , DIMENSION(4) :: Locfos
   REAL*8 , DIMENSION(2) :: Eps
   INTEGER :: Npoly
   REAL*8 , DIMENSION(2,7) :: P
!
! Local variable declarations rewritten by SPAG
!
   REAL :: ap1 , ap2
   INTEGER :: i , inter , ip , j , j1 , j2 , jj1 , jj2 , jlast , jp1 , k , k1 , k2 , kk1 , kk2 , klast
   INTEGER , DIMENSION(2,7) :: jedge
   INTEGER , DIMENSION(2,5) :: kedge
   REAL*8 , DIMENSION(2) :: p1 , ss
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     POLYPT DETERMINES PERIMETER POINTS OF AREA COMMON TO STRUCTURAL
!        TRIANGLE BOUNDED BY TR POINTS AND FLUID ELEMENT BOUNDED BY
!        (3 OR 4) FL POINTS
!
!
   ip = 0
   Npoly = 0
!
   DO i = 1 , 2
      DO j = 1 , 7
         P(i,j) = 0.D0
      ENDDO
   ENDDO
!
   DO k = 1 , 3
      IF ( Loctof(k)<0 ) GOTO 100
   ENDDO
!
!     STRUCTURAL TRIANGLE IS COMMON AREA WHEN NO STR PTS LIE OUTSIDE
!        FLUID ELEMENT BOUNDRY
   ip = 3
   DO k = 1 , 3
      DO i = 1 , 2
         P(i,k) = Tr(i,k)
      ENDDO
   ENDDO
!
!
   Npoly = ip
   GOTO 99999
!
!
 100  k = Ngridf - 1
   DO i = 1 , 2
      DO j = 1 , k
         jedge(i,j) = Fledge(i,j)
         jedge(i,j+Ngridf) = Fledge(i,j)
      ENDDO
      jedge(i,Ngridf) = Fledge(i,Ngridf)
   ENDDO
!
   DO i = 1 , 2
      DO j = 1 , 2
         kedge(i,j) = Stedge(i,j)
         kedge(i,j+3) = Stedge(i,j)
      ENDDO
      kedge(i,3) = Stedge(i,3)
   ENDDO
!
!
   DO k = 1 , 3
      k1 = kedge(1,k)
      k2 = kedge(2,k)
      DO j = 1 , Ngridf
         j1 = jedge(1,j)
         j2 = jedge(2,j)
         CALL ptintr(Tr(1,k1),Tr(1,k2),Fl(1,j1),Fl(1,j2),ss,inter,Eps)
         IF ( inter==1 ) GOTO 200
      ENDDO
   ENDDO
!
! - - AREAS ARE DISJOINT
   Npoly = ip
   GOTO 99999
!
!
 200  jlast = j
   jj1 = j
   jj2 = j + Ngridf - 1
   klast = k
   kk1 = k + 1
   kk2 = k + 2
!
   IF ( Loctof(k1)==1 ) THEN
!
      P(1,ip+1) = Tr(1,k1)
      P(2,ip+1) = Tr(2,k1)
      P(1,ip+2) = ss(1)
      P(2,ip+2) = ss(2)
      ip = ip + 2
      GOTO 600
   ELSE
!     1ST TRI POINT IS OUTSIDE FLUID BOUNDRY
      p1(2) = ss(2)
      p1(1) = ss(1)
      ap1 = (p1(1)-Tr(1,k1))**2 + (p1(2)-Tr(2,k1))**2
      jp1 = jlast
      jj1 = jlast + 1
!
      DO j = jj1 , jj2
         j1 = jedge(1,j)
         j2 = jedge(2,j)
         CALL ptintr(Tr(1,k1),Tr(1,k2),Fl(1,j1),Fl(1,j2),ss,inter,Eps)
         IF ( inter==1 ) GOTO 300
      ENDDO
!
      ip = ip + 1
      P(1,ip) = p1(1)
      P(2,ip) = p1(2)
      GOTO 400
   ENDIF
!
 300  ap2 = (ss(1)-Tr(1,k1))**2 + (ss(2)-Tr(2,k1))**2
   IF ( ap1<ap2 ) THEN
!
      P(1,ip+1) = p1(1)
      P(2,ip+1) = p1(2)
      P(1,ip+2) = ss(1)
      P(2,ip+2) = ss(2)
      ip = ip + 2
      jlast = j
   ELSE
!
      P(1,ip+1) = ss(1)
      P(2,ip+1) = ss(2)
      P(1,ip+2) = p1(1)
      P(2,ip+2) = p1(2)
      ip = ip + 2
      jlast = jp1
   ENDIF
!
   IF ( jlast>Ngridf ) jlast = jlast - Ngridf
   jj1 = jlast
   jj2 = jj1 + Ngridf - 1
   j2 = jedge(2,jlast)
   GOTO 600
!
!     SEARCH ALONG LAST STRUCTURAL TRIANGLE EDGE FOR NEXT PTINTR
!
 400  DO WHILE ( Loctof(k2)>=0 )
      IF ( Tr(1,k2)==P(1,1) .AND. Tr(2,k2)==P(2,1) ) THEN
         Npoly = ip
         GOTO 99999
      ELSE
         ip = ip + 1
         P(1,ip) = Tr(1,k2)
         P(2,ip) = Tr(2,k2)
         klast = klast + 1
         IF ( klast==kk2 ) THEN
            Npoly = ip
            GOTO 99999
         ELSE
            k2 = kedge(2,klast)
         ENDIF
      ENDIF
   ENDDO
!
   jj1 = jlast
   IF ( jj1<=jj2 ) THEN
      DO j = jj1 , jj2
         j1 = jedge(1,j)
         j2 = jedge(2,j)
         CALL ptintr(P(1,ip),Tr(1,k2),Fl(1,j1),Fl(1,j2),ss,inter,Eps)
         IF ( inter==1 ) GOTO 500
!
      ENDDO
   ENDIF
   Npoly = ip
   GOTO 99999
!
 500  IF ( ss(1)==P(1,1) .AND. ss(2)==P(2,1) ) THEN
      Npoly = ip
      GOTO 99999
   ELSE
      ip = ip + 1
      P(1,ip) = ss(1)
      P(2,ip) = ss(2)
      jlast = j
   ENDIF
!
!     SEARCH ALONG LAST FLUID EDGE FOR NEXT PTINTR
!
 600  DO WHILE ( Locfos(j2)>=0 )
      IF ( Fl(1,j2)==P(1,1) .AND. Fl(2,j2)==P(2,1) ) THEN
         Npoly = ip
         GOTO 99999
      ELSE
         ip = ip + 1
         P(1,ip) = Fl(1,j2)
         P(2,ip) = Fl(2,j2)
         jlast = jlast + 1
         IF ( jlast>jj2 ) THEN
            Npoly = ip
            GOTO 99999
         ELSE
            j2 = jedge(2,jlast)
         ENDIF
      ENDIF
   ENDDO
!
   kk1 = klast
   IF ( kk1<=kk2 ) THEN
      DO k = kk1 , kk2
         k1 = kedge(1,k)
         k2 = kedge(2,k)
         CALL ptintr(P(1,ip),Fl(1,j2),Tr(1,k1),Tr(1,k2),ss,inter,Eps)
         IF ( inter==1 ) GOTO 700
!
      ENDDO
   ENDIF
   Npoly = ip
   GOTO 99999
!
 700  IF ( ss(1)==P(1,1) .AND. ss(2)==P(2,1) ) THEN
      Npoly = ip
   ELSE
      ip = ip + 1
      P(1,ip) = ss(1)
      P(2,ip) = ss(2)
      klast = k
      GOTO 400
   ENDIF
99999 END SUBROUTINE polypt
