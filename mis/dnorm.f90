
SUBROUTINE dnorm(X,Mag)
   IMPLICIT NONE
   DOUBLE PRECISION Mag
   DOUBLE PRECISION X(3)
   DOUBLE PRECISION a
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
