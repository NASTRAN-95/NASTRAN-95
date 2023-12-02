!*==alg9.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION alg9(H,S,V2)
   USE c_gas
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: alg9
   REAL :: H
   REAL :: S
   REAL :: V2
!
! End of declarations rewritten by SPAG
!
!
!
   alg9 = cp*V2/(gamma*g*r*H)
END FUNCTION alg9
