
SUBROUTINE flbemg
   IMPLICIT NONE
   INTEGER Af , Afdict , Afmat , Bgpdt , Conect , Cstm , Dkgg , Ect , Eqexin , Fbelm , Frelm , Geom2 , Geom3 , Ibgpdt , Ibuf1 ,     &
         & Ibuf2 , Ibuf3 , Ibuf4 , Ibuf5 , Icore , Igrav , Igrid , Isil , Kgdict , Kgmat , Lcore , Mpt , Nbgpdt , Ngrav , Ngrid ,   &
         & Nofree , Nograv , Nsil , Sil , Uset , Usetf , Usets , Z(1)
   LOGICAL Error
   REAL Tilt(2)
   COMMON /blank / Nograv , Nofree , Tilt
   COMMON /flbfil/ Geom2 , Ect , Bgpdt , Sil , Mpt , Geom3 , Cstm , Uset , Eqexin , Usetf , Usets , Af , Dkgg , Fbelm , Frelm ,     &
                 & Conect , Afmat , Afdict , Kgmat , Kgdict
   COMMON /flbptr/ Error , Icore , Lcore , Ibgpdt , Nbgpdt , Isil , Nsil , Igrav , Ngrav , Igrid , Ngrid , Ibuf1 , Ibuf2 , Ibuf3 ,  &
                 & Ibuf4 , Ibuf5
   COMMON /zzzzzz/ Z
   DOUBLE PRECISION afe(48) , kge(144)
   INTEGER dict(2) , fbrec(12) , file , frrec(7) , grav(2) , i , icstm , id , imat , j , jsil , n , nafe , name(2) , ncstm , nkge , &
         & nmat , nz , pos
   LOGICAL nocard
!
!     GENERATES ELEMENT AREA FACTOR AND GRAVITIATIONAL STIFFNESS
!     MATRICES
!
!
!
!
!     GINO FILES
!
!
!     OPEN CORE
!
!
!     CORE POINTERS
!
!
!     MODULE PARAMETERS
!
!
   DATA name/4HFLBE , 4HMG  /
   DATA grav/4401 , 44/
!
!***********************************************************************
!
!     READ MATERIAL PROPERTY DATA INTO CORE
!
   imat = Icore
   nz = Ibuf5 - imat
   CALL premat(Z(imat),Z(imat),Z(Ibuf1),nz,nmat,Mpt,0)
!
!     READ CSTM DATA INTO CORE
!
   icstm = imat + nmat
   ncstm = 0
   nz = Ibuf5 - icstm
   file = Cstm
   CALL open(*200,Cstm,Z(Ibuf1),0)
   CALL fwdrec(*800,Cstm)
   CALL read(*800,*100,Cstm,Z(icstm),nz,0,ncstm)
   n = -8
   CALL mesage(n,file,name)
   GOTO 99999
 100  CALL close(Cstm,1)
   CALL pretrd(Z(icstm),ncstm)
!
!     READ GRAV DATA INTO CORE
!
 200  Igrav = icstm + ncstm
   Ngrav = 0
   nz = Ibuf5 - Igrav
   Nograv = -1
   nocard = .TRUE.
   file = Geom3
   CALL preloc(*400,Z(Ibuf1),Geom3)
   CALL locate(*300,Z(Ibuf1),grav,id)
   nocard = .FALSE.
   CALL read(*800,*300,Geom3,Z(Igrav),nz,0,Ngrav)
   n = -8
   CALL mesage(n,file,name)
   GOTO 99999
!
 300  CALL close(Geom3,1)
!
!     OPEN MATRIX AND DICTIONARY FILES
!
 400  CALL gopen(Afmat,Z(Ibuf2),1)
   CALL gopen(Afdict,Z(Ibuf4),1)
   IF ( .NOT.(nocard) ) THEN
      CALL gopen(Kgmat,Z(Ibuf3),1)
      CALL gopen(Kgdict,Z(Ibuf5),1)
   ENDIF
!
!
!     PASS THROUGH FBELM FILE AND PROCESS EACH ENTRY ON THE BOUNDARY.
!     SUBROUTINE BOUND WILL GENERATE THE ELEMENT MATRICES FOR
!     EACH ENTRY.
!
   file = Fbelm
   CALL gopen(Fbelm,Z(Ibuf1),0)
   DO
      CALL read(*800,*500,Fbelm,fbrec,12,0,n)
!
      CALL bound(fbrec,afe,nafe,kge,nkge)
      IF ( .NOT.(Error) ) THEN
!
!     CONVERT GRID POINTS TO SILS
!
         DO i = 1 , 4
            j = fbrec(i+2) - 1
            IF ( j>=0 ) fbrec(i+2) = Z(Isil+j)
            j = fbrec(i+8) - 1
            IF ( j>=0 ) fbrec(i+8) = Z(Isil+j)
         ENDDO
!
!     WRITE AREA MATRICES AND DICTIONARY ENTRUES
!
         CALL write(Afmat,fbrec(3),4,0)
         CALL write(Afmat,fbrec(9),4,0)
         CALL write(Afmat,afe,nafe,1)
         CALL savpos(Afmat,pos)
         dict(2) = pos
         DO i = 1 , 4
            dict(1) = fbrec(i+8)
            IF ( dict(1)>=0 ) CALL write(Afdict,dict,2,0)
         ENDDO
!
!     WRITE GRAVITATIONAL STIFFNESS MATRICES IF THEY EXIST
!
         IF ( nkge/=0 ) THEN
            CALL write(Kgmat,fbrec(3),4,0)
            CALL write(Kgmat,fbrec(3),4,0)
            CALL write(Kgmat,kge,nkge,1)
            CALL savpos(Kgmat,pos)
            dict(2) = pos
            DO i = 1 , 4
               jsil = fbrec(i+2)
               IF ( jsil>=0 ) THEN
                  DO j = 1 , 3
                     dict(1) = jsil
                     CALL write(Kgdict,dict,2,0)
                     jsil = jsil + 1
                  ENDDO
               ENDIF
!
            ENDDO
         ENDIF
      ENDIF
   ENDDO
 500  CALL close(Fbelm,1)
!
!
!     PASS THROUGH FRELM FILE AND PROCESS EACH ENTRY ON THE FREE
!     SURFACE.  SUBROUTINE FLFREE WILL CALCULATE THE AREA AND
!     GRAVITATIONAL STIFFNESS MATRICES FOR EACH ENTRY
!
   IF ( Nofree<0 ) GOTO 700
   file = Frelm
   CALL gopen(Frelm,Z(Ibuf1),0)
   DO
      CALL read(*800,*600,Frelm,frrec,7,0,n)
!
      CALL flfree(frrec,afe,nafe,kge,nkge)
      IF ( .NOT.(Error) ) THEN
!
!     CONVERT GRID POINTS TO SILS
!
         DO i = 1 , 4
            j = frrec(i+2) - 1
            IF ( j>=0 ) frrec(i+2) = Z(Isil+j)
         ENDDO
!
!     WRITE AREA MATRICES AND DICTIONARY ENTRIES
!
         CALL write(Afmat,frrec(3),4,0)
         CALL write(Afmat,frrec(3),4,0)
         CALL write(Afmat,afe,nafe,1)
         CALL savpos(Afmat,pos)
         dict(2) = pos
         DO i = 1 , 4
            dict(1) = frrec(i+2)
            IF ( dict(1)>=0 ) CALL write(Afdict,dict,2,0)
         ENDDO
!
!     WRITE GRAVITATIONAL STIFFNESS MATRICES IF THEY EXIST
!
         IF ( nkge/=0 ) THEN
            CALL write(Kgmat,frrec(3),4,0)
            CALL write(Kgmat,frrec(3),4,0)
            CALL write(Kgmat,kge,nkge,1)
            CALL savpos(Kgmat,pos)
            dict(2) = pos
            DO i = 1 , 4
               dict(1) = frrec(i+2)
               IF ( dict(1)>=0 ) CALL write(Kgdict,dict,2,0)
!
            ENDDO
         ENDIF
      ENDIF
   ENDDO
 600  CALL close(Frelm,1)
!
!     CLOSE FILES AND RETURN
!
 700  CALL close(Afmat,1)
   CALL close(Afdict,1)
   IF ( .NOT.(nocard) ) THEN
      CALL close(Kgmat,1)
      CALL close(Kgdict,1)
   ENDIF
!
   RETURN
!
!     ERROR CONDITIONS
!
 800  n = -2
   CALL mesage(n,file,name)
99999 RETURN
END SUBROUTINE flbemg