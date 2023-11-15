
DOUBLE PRECISION FUNCTION dadotb(A,B)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   DOUBLE PRECISION A(3) , B(3)
!
! End of declarations
!
!*****
!  DOUBLE PRECISION VERSION
!
!  DOT PRODUCT  A . B
!*****
   dadotb = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
END FUNCTION dadotb
