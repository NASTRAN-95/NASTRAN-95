!*==alg7.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION alg7(H,S)
   IMPLICIT NONE
   USE C_GAS
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: alg7
   REAL :: H
   REAL :: S
!
! End of declarations rewritten by SPAG
!
!
!
   alg7 = H/Cp
END FUNCTION alg7
