!*==alg5.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION alg5(H,S)
   IMPLICIT NONE
   USE C_GAS
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: alg5
   REAL :: H
   REAL :: S
   EXTERNAL alg4
!
! End of declarations rewritten by SPAG
!
!
!
   alg5 = alg4(H,S)/(R*H)*Cp
END FUNCTION alg5
