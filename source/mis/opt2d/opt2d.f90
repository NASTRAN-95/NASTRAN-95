!*==opt2d.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE opt2d(Ipr,Pr)
   USE c_blank
   USE c_names
   USE c_optpw2
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ipr
   REAL , DIMENSION(1) :: Pr
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: eor , i , n
   INTEGER , DIMENSION(1) :: iz
   EXTERNAL eof , fname , fread , rdtrl , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!-----
!   COPY OPTP1 TO OPTP2 DATA FILE.
!  CHANGE RECORD 3      WORD 1 = IABS (PID).
!                       WORD 4 = PLST
!                       WORD 5 = ALPH
!-----
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
!
!  . RECORD ZERO - COPY NAME AND 6 PARAMETERS...
!
         CALL fread(optp1,z(1),8,next)
         CALL fname(optp2,z(1))
         CALL write(optp2,z(1),8,next)
!
!  . RECORD ONE (POINTERS) AND TWO (ELEMENT DATA)...
!
         DO i = 1 , 2
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  n = zcor
                  spag_nextblock_2 = 2
               CASE (2)
                  eor = next
                  CALL read(*2,*2,optp1,z,zcor,0,n)
                  eor = 0
 2                CALL write(optp2,z(1),n,eor)
                  IF ( eor==0 ) THEN
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
!
!  . RECORD THREE - PROPERTY DATA...
!
         eor = 0
         DO i = 1 , nprw , nwdsp
            Ipr(i) = iabs(Ipr(i))
            Pr(i+4) = -1.0
            CALL write(optp2,Ipr(i),nwdsp,eor)
         ENDDO
         CALL write(optp2,0,0,next)
!
!  . RECORD FOUR - PLIMIT DATA...
!
         CALL fread(optp1,0,0,next)
         n = zcor
         spag_nextblock_1 = 2
      CASE (2)
         eor = next
         CALL read(*20,*20,optp1,z,zcor,0,n)
         eor = 0
 20      CALL write(optp2,z(1),n,eor)
         IF ( eor==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         CALL eof(optp2)
         iz(1) = optp1
         CALL rdtrl(iz(1))
         iz(1) = optp2
         CALL wrttrl(iz(1))
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE opt2d
