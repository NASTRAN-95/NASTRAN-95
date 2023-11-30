
SUBROUTINE csqrtx(Xx,Y)
   IMPLICIT NONE
   DOUBLE PRECISION Xx(2) , Y(2)
   DOUBLE PRECISION r , x(2)
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
