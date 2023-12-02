!*==opt2d.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE opt2d(Ipr,Pr)
   IMPLICIT NONE
   USE C_BLANK
   USE C_NAMES
   USE C_OPTPW2
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
         CALL fread(Optp1,Z(1),8,Next)
         CALL fname(Optp2,Z(1))
         CALL write(Optp2,Z(1),8,Next)
!
!  . RECORD ONE (POINTERS) AND TWO (ELEMENT DATA)...
!
         DO i = 1 , 2
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  n = Zcor
                  spag_nextblock_2 = 2
               CASE (2)
                  eor = Next
                  CALL read(*2,*2,Optp1,Z,Zcor,0,n)
                  eor = 0
 2                CALL write(Optp2,Z(1),n,eor)
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
         DO i = 1 , Nprw , Nwdsp
            Ipr(i) = iabs(Ipr(i))
            Pr(i+4) = -1.0
            CALL write(Optp2,Ipr(i),Nwdsp,eor)
         ENDDO
         CALL write(Optp2,0,0,Next)
!
!  . RECORD FOUR - PLIMIT DATA...
!
         CALL fread(Optp1,0,0,Next)
         n = Zcor
         spag_nextblock_1 = 2
      CASE (2)
         eor = Next
         CALL read(*20,*20,Optp1,Z,Zcor,0,n)
         eor = 0
 20      CALL write(Optp2,Z(1),n,eor)
         IF ( eor==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         CALL eof(Optp2)
         iz(1) = Optp1
         CALL rdtrl(iz(1))
         iz(1) = Optp2
         CALL wrttrl(iz(1))
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE opt2d
