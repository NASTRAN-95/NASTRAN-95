
FUNCTION corwds(I,J)
   IMPLICIT NONE
   INTEGER I , J
   INTEGER corwds
   INTEGER locfx
!
!
   corwds = iabs(locfx(I)-locfx(J)) + 1
END FUNCTION corwds