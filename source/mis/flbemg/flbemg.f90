!*==flbemg.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE flbemg
USE C_BLANK
USE C_FLBFIL
USE C_FLBPTR
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(48) :: afe
   INTEGER , DIMENSION(2) :: dict
   INTEGER , DIMENSION(12) :: fbrec
   INTEGER :: file , i , icstm , id , imat , j , jsil , n , nafe , ncstm , nkge , nmat , nz , pos
   INTEGER , DIMENSION(7) :: frrec
   INTEGER , DIMENSION(2) , SAVE :: grav , name
   REAL(REAL64) , DIMENSION(144) :: kge
   LOGICAL :: nocard
   EXTERNAL bound , close , flfree , fwdrec , gopen , locate , mesage , open , preloc , premat , pretrd , read , savpos , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         CALL open(*40,Cstm,Z(Ibuf1),0)
         CALL fwdrec(*140,Cstm)
         CALL read(*140,*20,Cstm,Z(icstm),nz,0,ncstm)
         n = -8
         CALL mesage(n,file,name)
         RETURN
 20      CALL close(Cstm,1)
         CALL pretrd(Z(icstm),ncstm)
!
!     READ GRAV DATA INTO CORE
!
 40      Igrav = icstm + ncstm
         Ngrav = 0
         nz = Ibuf5 - Igrav
         Nograv = -1
         nocard = .TRUE.
         file = Geom3
         CALL preloc(*80,Z(Ibuf1),Geom3)
         CALL locate(*60,Z(Ibuf1),grav,id)
         nocard = .FALSE.
         CALL read(*140,*60,Geom3,Z(Igrav),nz,0,Ngrav)
         n = -8
         CALL mesage(n,file,name)
         RETURN
!
 60      CALL close(Geom3,1)
!
!     OPEN MATRIX AND DICTIONARY FILES
!
 80      CALL gopen(Afmat,Z(Ibuf2),1)
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
            CALL read(*140,*100,Fbelm,fbrec,12,0,n)
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
 100     CALL close(Fbelm,1)
!
!
!     PASS THROUGH FRELM FILE AND PROCESS EACH ENTRY ON THE FREE
!     SURFACE.  SUBROUTINE FLFREE WILL CALCULATE THE AREA AND
!     GRAVITATIONAL STIFFNESS MATRICES FOR EACH ENTRY
!
         IF ( Nofree<0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = Frelm
         CALL gopen(Frelm,Z(Ibuf1),0)
         DO
            CALL read(*140,*120,Frelm,frrec,7,0,n)
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
 120     CALL close(Frelm,1)
         spag_nextblock_1 = 2
      CASE (2)
!
!     CLOSE FILES AND RETURN
!
         CALL close(Afmat,1)
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
 140     n = -2
         CALL mesage(n,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE flbemg
