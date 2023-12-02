!*==sadotb.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION sadotb(A,B)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: sadotb
   REAL , DIMENSION(3) :: A
   REAL , DIMENSION(3) :: B
!
! End of declarations rewritten by SPAG
!
!
! Function and Dummy argument declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!*****
!  SINGLE-PRECISION VERSION
!
!  DOT PRODUCT A . B
!*****
   sadotb = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
END FUNCTION sadotb
