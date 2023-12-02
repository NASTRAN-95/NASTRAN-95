!*==dnorm.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dnorm(X,Mag)
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(3) :: X
   REAL(REAL64) :: Mag
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: a
!
! End of declarations rewritten by SPAG
!
!
!     DOUBLE PRECISION NORMALIZATION
!
!
   Mag = 0.D0
   a = X(1)*X(1) + X(2)*X(2) + X(3)*X(3)
   IF ( a>0.D0 ) Mag = dsqrt(a)
   IF ( Mag==0.0D0 ) RETURN
   X(1) = X(1)/Mag
   X(2) = X(2)/Mag
   X(3) = X(3)/Mag
END SUBROUTINE dnorm
