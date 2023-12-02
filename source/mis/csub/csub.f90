!*==csub.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE csub(X,Y,Z,A,B)
USE C_CINVPX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: X
   REAL(REAL64) , DIMENSION(2) :: Y
   REAL(REAL64) , DIMENSION(1) :: Z
   REAL(REAL64) , DIMENSION(2) :: A
   REAL(REAL64) , DIMENSION(2) :: B
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: dum
   INTEGER :: i , ncol2
!
! End of declarations rewritten by SPAG
!
!*******
!     CSUB WILL FORM Z = A*X - B*Y WHERE A AND B ARE SCALAR
!     MULTIPLIERS FOR THE COMPLEX VECTORS X AND Y
!*******
   ncol2 = Ncol + Ncol
   DO i = 1 , ncol2 , 2
      dum = X(i)*A(1) - X(i+1)*A(2) - Y(i)*B(1) + Y(i+1)*B(2)
      Z(i+1) = X(i)*A(2) + X(i+1)*A(1) - Y(i+1)*B(1) - Y(i)*B(2)
      Z(i) = dum
   ENDDO
END SUBROUTINE csub
