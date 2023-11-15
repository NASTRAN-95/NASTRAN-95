
DOUBLE PRECISION FUNCTION dvmag(V1,Eps)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   DOUBLE PRECISION Eps
   DOUBLE PRECISION V1(3)
!
! Local variable declarations
!
   DOUBLE PRECISION a
   DOUBLE PRECISION dadotb
!
! End of declarations
!
!
!     RETURNS DOUBLE PRECISION MAGNITUDE OF VECTOR V1
!        DVMAG= 0.D0 WHEN .LE. EPS
!
!
!
   dvmag = 0.D0
   a = dadotb(V1,V1)
   IF ( a>0.D0 ) dvmag = dsqrt(a)
   IF ( dvmag<=Eps ) dvmag = 0.D0
END FUNCTION dvmag
