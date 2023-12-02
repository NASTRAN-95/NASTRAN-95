!*==csqrtx.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE csqrtx(Xx,Y)
USE iso_fortran_env
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: Xx
   REAL(REAL64) , DIMENSION(2) :: Y
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: r
   REAL(REAL64) , DIMENSION(2) :: x
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!*******
!     ROUTINE TO FIND THE COMPLEX SQUARE ROOT OF X AND STORE IT IN Y
!*******
   x(1) = Xx(1)
   x(2) = Xx(2)
   r = dsqrt(x(1)**2+x(2)**2)
   Y(1) = dsqrt(dabs(x(1)+r)/2.)
   Y(2) = dsqrt(dabs(-x(1)+r)/2.)
   IF ( x(2)==0.0D0 ) RETURN
   Y(2) = dsign(Y(2),x(2))
END SUBROUTINE csqrtx
