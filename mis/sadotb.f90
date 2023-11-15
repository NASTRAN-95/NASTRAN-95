
FUNCTION sadotb(A,B)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL A(3) , B(3)
   REAL sadotb
!
! End of declarations
!
!*****
!  SINGLE-PRECISION VERSION
!
!  DOT PRODUCT A . B
!*****
   sadotb = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
END FUNCTION sadotb
