!*==sanorm.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sanorm(A) !HIDESTARS (*,A)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(3) :: A
!
! Local variable declarations rewritten by SPAG
!
   REAL :: xl
!
! End of declarations rewritten by SPAG
!
!
!     VECTOR NORMALIZATION AND VECTOR LENGTH
!
   xl = A(1)*A(1) + A(2)*A(2) + A(3)*A(3)
   IF ( xl<=0.0 ) RETURN 1
   xl = sqrt(xl)
   A(1) = A(1)/xl
   A(2) = A(2)/xl
   A(3) = A(3)/xl
END SUBROUTINE sanorm
