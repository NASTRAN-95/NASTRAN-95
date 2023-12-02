!*==trht1b.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trht1b(Iof,Delta)
USE C_BLANK
USE C_TRHTX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iof
   REAL :: Delta
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: blk , block
   INTEGER , DIMENSION(11) :: iblock
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) :: name
   EXTERNAL factor , factru , mesage , rdtrl , ssg2c
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!
!
!
   !>>>>EQUIVALENCE (iqblk(1),qblock(1))
   !>>>>EQUIVALENCE (iqblk(2),iblock(1))
   !>>>>EQUIVALENCE (qblock(2),block(1))
   !>>>>EQUIVALENCE (qblock(5),blk(1))
!
! ----------------------------------------------------------------------
!
         iblock(1) = 2
         block(1) = 1.0D0/Delta
         block(2) = 0.0D0
         iblock(7) = 2
         blk(1) = Beta
         blk(2) = 0.0D0
         CALL ssg2c(Ib,Ik,Icr6,1,iblock)
         mcb(1) = Icr6
         CALL rdtrl(mcb(1))
         IF ( mcb(4)==6 ) THEN
!
!     SYMMETRIC DECOMP
!
            CALL factor(Icr6,Icr1,Icr2,Icr3,Icr4,Icr7)
            Isym = 1
         ELSE
            CALL factru(*20,Icr6,Icr1,Icr2,Icr3,Icr4,Icr7)
            Isym = 0
         ENDIF
!
!     LLL  IS ON ICR1
!
!     FORM  A  MATRIX
!
         blk(1) = -(1.0D0-Beta)
         blk(2) = 0.0
         CALL ssg2c(Ib,Ik,Icr6,1,iblock)
         spag_nextblock_1 = 2
      CASE (2)
         RETURN
 20      CALL mesage(-5,Icr6,name)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE trht1b
