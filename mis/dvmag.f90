
DOUBLE PRECISION FUNCTION dvmag(V1,Eps)
   IMPLICIT NONE
   DOUBLE PRECISION Eps
   DOUBLE PRECISION V1(3)
   DOUBLE PRECISION a
   DOUBLE PRECISION dadotb
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
