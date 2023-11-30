
SUBROUTINE qvol
   IMPLICIT NONE
   INTEGER Bg , Ifm , Iout , Lc , Nx(12) , Old , Slt
   REAL Consts(5) , Core(1) , Sysbuf , Twopi
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /condas/ Consts
   COMMON /loadx / Lc , Slt , Bg , Old , Nx , Ifm
   COMMON /system/ Sysbuf , Iout
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Core
   REAL card(12) , coef , d12(3) , d13(3) , d14(3) , data4(4,9) , el , fact , p(8) , r(4,8)
   INTEGER i , i1 , i2 , i3 , i4 , id , iel , im , imap , ip(3) , isil , l , map(4,14) , n , nel , npts , nsil(8) , order(8) ,      &
         & reason , type
!
!     CALCULATES THERMAL LOADS DUE TO QVOL CARDS
!
   EQUIVALENCE (Consts(2),Twopi) , (npts,card(1)) , (id,card(2)) , (nsil(1),card(3)) , (coef,card(11)) , (type,card(12)) ,          &
    & (r(1,1),data4(2,1)) , (i1,ip(1)) , (i2,ip(2)) , (i3,ip(3))
   DATA map/1 , 2 , 3 , 4 , 1 , 2 , 3 , 6 , 1 , 2 , 6 , 5 , 1 , 4 , 5 , 6 , 1 , 2 , 3 , 6 , 1 , 3 , 4 , 8 , 1 , 3 , 8 , 6 , 1 , 5 , &
      & 6 , 8 , 3 , 6 , 7 , 8 , 2 , 3 , 4 , 7 , 1 , 2 , 4 , 5 , 2 , 4 , 5 , 7 , 2 , 5 , 6 , 7 , 4 , 5 , 7 , 8/
!
!     READ AND PROCESS ONE ELEMENT OF ONE QVOL CARD PER CALL
!     THE LOAD COEFFICIENTS ARE GENERATED AND INSERTED HERE
!
!     THE INPUT DATA ON FILE SLT IS
!
!     FIELD       DATA
!       1         NO. OF POINTS
!       2         EL. ID.
!      3-10       1 TO 8 SILS
!                        *  A*Q  FOR  TYPE=1 (RODS,ETC)
!       11        COEF = *  T*Q  FOR  TYPE=2 (TRIANGLES ETC)
!                        *    Q  FOR  TYPE=3 (BELL) OR 4 (SOLID)
!       12        TYPE
!
   CALL fread(Slt,card,12,0)
   reason = 1
   IF ( npts>1 ) THEN
      CALL permut(nsil(1),order(1),npts,Old)
      reason = 2
      DO i = 1 , npts
         l = order(i)
         CALL fndpnt(data4(1,l),nsil(l))
         n = nsil(l)
         CALL fndsil(n)
         IF ( n/=nsil(l) ) GOTO 100
         p(i) = 0.0
      ENDDO
      reason = 3
      IF ( type>=1 .AND. type<=4 ) THEN
         IF ( type==2 .OR. type==3 ) THEN
!
!     MEMBRANES, PLATES, AND AXISYMMETRIC SOLIDS
!
            IF ( npts==3 ) THEN
               nel = 1
               fact = coef/6.0
            ELSEIF ( npts==4 ) THEN
               nel = 4
               fact = coef/12.0
            ELSE
               reason = 4
               GOTO 100
            ENDIF
            DO iel = 1 , nel
               DO i = 1 , 3
                  ip(i) = i + iel - 1
                  IF ( ip(i)>4 ) ip(i) = ip(i) - 4
               ENDDO
               DO i = 1 , 3
                  d12(i) = r(i,i2) - r(i,i1)
                  d13(i) = r(i,i3) - r(i,i1)
               ENDDO
               CALL saxb(d12(1),d13(1),d12(1))
               el = fact*sqrt(d12(1)**2+d12(2)**2+d12(3)**2)
!
!     SPECIAL FACTOR FOR  AXISYMMETRIC ELEMENTS
!
               IF ( type/=2 ) el = el*Twopi*(r(1,i1)+r(1,i2)+r(1,i3))/3.0
               p(i1) = p(i1) + el
               p(i2) = p(i2) + el
               p(i3) = p(i3) + el
            ENDDO
         ELSEIF ( type==4 ) THEN
!
!     SOLID ELEMENTS
!
            IF ( npts==4 ) THEN
               nel = 1
               fact = coef/24.0
               imap = 1
            ELSEIF ( npts==6 ) THEN
               nel = 3
               imap = 2
               fact = coef/24.0
            ELSEIF ( npts==8 ) THEN
               imap = 5
               nel = 10
               fact = coef/48.0
            ELSE
               reason = 5
               GOTO 100
            ENDIF
            DO iel = 1 , nel
               im = imap + iel - 1
               i1 = map(1,im)
               i2 = map(2,im)
               i3 = map(3,im)
               i4 = map(4,im)
               DO i = 1 , 3
!
                  d12(i) = r(i,i2) - r(i,i1)
                  d13(i) = r(i,i3) - r(i,i1)
                  d14(i) = r(i,i4) - r(i,i1)
               ENDDO
!
               CALL saxb(d12(1),d13(1),d12(1))
               el = fact*abs(d12(1)*d14(1)+d12(2)*d14(2)+d12(3)*d14(3))
               DO i = 1 , 4
                  l = map(i,im)
                  p(l) = p(l) + el
               ENDDO
            ENDDO
         ELSE
!
!     RODS, CONRODS, TUBES, BARS
!
            el = 0.0
            DO i = 1 , 3
               el = el + (r(i,1)-r(i,2))**2
            ENDDO
            p(1) = coef*sqrt(el)*0.5
            p(2) = p(1)
         ENDIF
!
!     INSERT THE LOADS
!
         DO i = 1 , npts
            isil = nsil(i)
            Core(isil) = Core(isil) + p(i)
         ENDDO
         RETURN
      ENDIF
   ENDIF
!
!     ERROR MESSAGE
!
 100  WRITE (Iout,99001) Sfm , id , reason
99001 FORMAT (A25,' 3093, ELEMENT =',I9,'.   REASON =',I7)
   CALL mesage(-61,0,0)
END SUBROUTINE qvol
