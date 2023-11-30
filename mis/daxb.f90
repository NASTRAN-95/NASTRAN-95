
SUBROUTINE daxb(A,B,C)
   IMPLICIT NONE
   DOUBLE PRECISION A(3) , B(3) , C(3)
   DOUBLE PRECISION d(3)
!*****
!  DOUBLE PRECISION VERSION
!
!  THIS ROUTINE PERFORMS A X B INTO C  (C MAY OVERLAP A OR B IN CORE)
!*****
   d(1) = A(2)*B(3) - A(3)*B(2)
   d(2) = A(3)*B(1) - A(1)*B(3)
   d(3) = A(1)*B(2) - A(2)*B(1)
   C(1) = d(1)
   C(2) = d(2)
   C(3) = d(3)
END SUBROUTINE daxb
