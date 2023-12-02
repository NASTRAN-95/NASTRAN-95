!*==flbemg.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE flbemg
   USE c_blank
   USE c_flbfil
   USE c_flbptr
   USE c_zzzzzz
   USE iso_fortran_env
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
         imat = icore
         nz = ibuf5 - imat
         CALL premat(z(imat),z(imat),z(ibuf1),nz,nmat,mpt,0)
!
!     READ CSTM DATA INTO CORE
!
         icstm = imat + nmat
         ncstm = 0
         nz = ibuf5 - icstm
         file = cstm
         CALL open(*40,cstm,z(ibuf1),0)
         CALL fwdrec(*140,cstm)
         CALL read(*140,*20,cstm,z(icstm),nz,0,ncstm)
         n = -8
         CALL mesage(n,file,name)
         RETURN
 20      CALL close(cstm,1)
         CALL pretrd(z(icstm),ncstm)
!
!     READ GRAV DATA INTO CORE
!
 40      igrav = icstm + ncstm
         ngrav = 0
         nz = ibuf5 - igrav
         nograv = -1
         nocard = .TRUE.
         file = geom3
         CALL preloc(*80,z(ibuf1),geom3)
         CALL locate(*60,z(ibuf1),grav,id)
         nocard = .FALSE.
         CALL read(*140,*60,geom3,z(igrav),nz,0,ngrav)
         n = -8
         CALL mesage(n,file,name)
         RETURN
!
 60      CALL close(geom3,1)
!
!     OPEN MATRIX AND DICTIONARY FILES
!
 80      CALL gopen(afmat,z(ibuf2),1)
         CALL gopen(afdict,z(ibuf4),1)
         IF ( .NOT.(nocard) ) THEN
            CALL gopen(kgmat,z(ibuf3),1)
            CALL gopen(kgdict,z(ibuf5),1)
         ENDIF
!
!
!     PASS THROUGH FBELM FILE AND PROCESS EACH ENTRY ON THE BOUNDARY.
!     SUBROUTINE BOUND WILL GENERATE THE ELEMENT MATRICES FOR
!     EACH ENTRY.
!
         file = fbelm
         CALL gopen(fbelm,z(ibuf1),0)
         DO
            CALL read(*140,*100,fbelm,fbrec,12,0,n)
!
            CALL bound(fbrec,afe,nafe,kge,nkge)
            IF ( .NOT.(error) ) THEN
!
!     CONVERT GRID POINTS TO SILS
!
               DO i = 1 , 4
                  j = fbrec(i+2) - 1
                  IF ( j>=0 ) fbrec(i+2) = z(isil+j)
                  j = fbrec(i+8) - 1
                  IF ( j>=0 ) fbrec(i+8) = z(isil+j)
               ENDDO
!
!     WRITE AREA MATRICES AND DICTIONARY ENTRUES
!
               CALL write(afmat,fbrec(3),4,0)
               CALL write(afmat,fbrec(9),4,0)
               CALL write(afmat,afe,nafe,1)
               CALL savpos(afmat,pos)
               dict(2) = pos
               DO i = 1 , 4
                  dict(1) = fbrec(i+8)
                  IF ( dict(1)>=0 ) CALL write(afdict,dict,2,0)
               ENDDO
!
!     WRITE GRAVITATIONAL STIFFNESS MATRICES IF THEY EXIST
!
               IF ( nkge/=0 ) THEN
                  CALL write(kgmat,fbrec(3),4,0)
                  CALL write(kgmat,fbrec(3),4,0)
                  CALL write(kgmat,kge,nkge,1)
                  CALL savpos(kgmat,pos)
                  dict(2) = pos
                  DO i = 1 , 4
                     jsil = fbrec(i+2)
                     IF ( jsil>=0 ) THEN
                        DO j = 1 , 3
                           dict(1) = jsil
                           CALL write(kgdict,dict,2,0)
                           jsil = jsil + 1
                        ENDDO
                     ENDIF
!
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
 100     CALL close(fbelm,1)
!
!
!     PASS THROUGH FRELM FILE AND PROCESS EACH ENTRY ON THE FREE
!     SURFACE.  SUBROUTINE FLFREE WILL CALCULATE THE AREA AND
!     GRAVITATIONAL STIFFNESS MATRICES FOR EACH ENTRY
!
         IF ( nofree<0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = frelm
         CALL gopen(frelm,z(ibuf1),0)
         DO
            CALL read(*140,*120,frelm,frrec,7,0,n)
!
            CALL flfree(frrec,afe,nafe,kge,nkge)
            IF ( .NOT.(error) ) THEN
!
!     CONVERT GRID POINTS TO SILS
!
               DO i = 1 , 4
                  j = frrec(i+2) - 1
                  IF ( j>=0 ) frrec(i+2) = z(isil+j)
               ENDDO
!
!     WRITE AREA MATRICES AND DICTIONARY ENTRIES
!
               CALL write(afmat,frrec(3),4,0)
               CALL write(afmat,frrec(3),4,0)
               CALL write(afmat,afe,nafe,1)
               CALL savpos(afmat,pos)
               dict(2) = pos
               DO i = 1 , 4
                  dict(1) = frrec(i+2)
                  IF ( dict(1)>=0 ) CALL write(afdict,dict,2,0)
               ENDDO
!
!     WRITE GRAVITATIONAL STIFFNESS MATRICES IF THEY EXIST
!
               IF ( nkge/=0 ) THEN
                  CALL write(kgmat,frrec(3),4,0)
                  CALL write(kgmat,frrec(3),4,0)
                  CALL write(kgmat,kge,nkge,1)
                  CALL savpos(kgmat,pos)
                  dict(2) = pos
                  DO i = 1 , 4
                     dict(1) = frrec(i+2)
                     IF ( dict(1)>=0 ) CALL write(kgdict,dict,2,0)
!
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
 120     CALL close(frelm,1)
         spag_nextblock_1 = 2
      CASE (2)
!
!     CLOSE FILES AND RETURN
!
         CALL close(afmat,1)
         CALL close(afdict,1)
         IF ( .NOT.(nocard) ) THEN
            CALL close(kgmat,1)
            CALL close(kgdict,1)
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
