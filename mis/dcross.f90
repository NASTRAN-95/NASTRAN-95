
SUBROUTINE dcross(X,Y,Z)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   DOUBLE PRECISION X(3) , Y(3) , Z(3)
!
! End of declarations
!
!
!     DOUBLE PRECISION CROSS PRODUCT
!
!
   Z(1) = X(2)*Y(3) - X(3)*Y(2)
   Z(2) = Y(1)*X(3) - Y(3)*X(1)
   Z(3) = X(1)*Y(2) - X(2)*Y(1)
END SUBROUTINE dcross
