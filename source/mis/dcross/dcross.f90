!*==dcross.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dcross(X,Y,Z)
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(3) :: X
   REAL(REAL64) , DIMENSION(3) :: Y
   REAL(REAL64) , DIMENSION(3) :: Z
!
! End of declarations rewritten by SPAG
!
!
!     DOUBLE PRECISION CROSS PRODUCT
!
!
   Z(1) = X(2)*Y(3) - X(3)*Y(2)
   Z(2) = Y(1)*X(3) - Y(3)*X(1)
   Z(3) = X(1)*Y(2) - X(2)*Y(1)
END SUBROUTINE dcross
