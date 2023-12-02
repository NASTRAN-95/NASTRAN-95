!*==alg6.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION alg6(P,T)
   IMPLICIT NONE
   USE C_GAS
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: alg6
   REAL :: P
   REAL :: T
!
! End of declarations rewritten by SPAG
!
!
!
   alg6 = Cp*T
END FUNCTION alg6
