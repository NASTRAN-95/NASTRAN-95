
SUBROUTINE sanorm(*,A)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL A(3)
!
! Local variable declarations
!
   REAL xl
!
! End of declarations
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
