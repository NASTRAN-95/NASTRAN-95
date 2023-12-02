!*==empcor.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE empcor(Mt1x,Mt2x,Pt,Pc,Frsrow,Midrow,Lasrow,Nx,A,Z)
   IMPLICIT NONE
   USE C_PACKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Mt1x
   INTEGER :: Mt2x
   INTEGER :: Pt
   INTEGER :: Pc
   INTEGER :: Frsrow
   INTEGER :: Midrow
   INTEGER :: Lasrow
   INTEGER :: Nx
   REAL , DIMENSION(1) :: A
   REAL , DIMENSION(1) :: Z
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) , SAVE :: mcb
   INTEGER :: mt , mt1 , mt2 , n , na , row
   EXTERNAL close , gopen , pack
!
! End of declarations rewritten by SPAG
!
!
!     EMPTY CORE OF A TRIANGULAR MATRIX
!
!
!
!     MT1      FIRST PART OF THE MATRIX (UP TO ROW -MIDROW-).
!     MT2      REST OF THE MATRIX.
!     PT       PRECISION OF THE MATRIX ON TAPE.
!     PC       ......... .. ... ...... IN CORE.
!     FRSROW   FIRST ROW IN CORE.
!     LAST     LAST  ... .. CORE.
!     N        SIZE OF THE COMPLETE MATRIX.
!     A        LOCATION OF THE COMPLETE MATRIX.
!
   DATA mcb/7*0/
   mt1 = Mt1x
   mt2 = Mt2x
   n = Nx
   mt = mt1
   IF ( Frsrow>Midrow .AND. mt2/=0 ) mt = mt2
   na = 1
   Incr = 1
   It1 = Pc
   It2 = Pt
   Jj = n
   DO row = Frsrow , Lasrow
      Ii = row
      CALL pack(A(na),mt,mcb)
      IF ( row==n ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      na = na + Pc*(n-row+1)
      IF ( row==Midrow .AND. mt2/=0 ) THEN
         CALL close(mt,1)
         mt = mt2
         CALL gopen(mt,Z,1)
      ENDIF
   ENDDO
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
!     END OF CORE DUMP
!
      CALL close(mt,1)
   END SUBROUTINE spag_block_1
END SUBROUTINE empcor
