
FUNCTION corwds(I,J)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER I , J
   INTEGER corwds
!
! Local variable declarations
!
   INTEGER locfx
!
! End of declarations
!
!
!
   corwds = iabs(locfx(I)-locfx(J)) + 1
END FUNCTION corwds
