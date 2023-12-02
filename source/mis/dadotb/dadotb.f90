!*==dadotb.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION dadotb(A,B)
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) :: dadotb
   REAL(REAL64) , DIMENSION(3) :: A
   REAL(REAL64) , DIMENSION(3) :: B
!
! End of declarations rewritten by SPAG
!
!*****
!  DOUBLE PRECISION VERSION
!
!  DOT PRODUCT  A . B
!*****
   dadotb = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
END FUNCTION dadotb
