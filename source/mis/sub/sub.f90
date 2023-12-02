!*==sub.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sub(X,Y,A,B)
USE C_INVPWX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: X
   REAL(REAL64) , DIMENSION(1) :: Y
   REAL(REAL64) :: A
   REAL(REAL64) :: B
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
!
! End of declarations rewritten by SPAG
!
!*******
!     SUB WILL FORM Y = A*X - B*Y  WHERE A AND B ARE SCALAR MULTIPLIERS
!     FOR THE VECTORS X AND Y
!*******
   DO i = 1 , Ncol
      Y(i) = X(i)*A - Y(i)*B
   ENDDO
END SUBROUTINE sub
