!*==alg6.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION alg6(P,T)
   USE c_gas
   IMPLICIT NONE
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
   alg6 = cp*T
END FUNCTION alg6
