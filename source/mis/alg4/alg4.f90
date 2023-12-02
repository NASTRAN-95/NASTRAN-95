!*==alg4.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION alg4(H,S)
   USE c_gas
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: alg4
   REAL :: H
   REAL :: S
!
! End of declarations rewritten by SPAG
!
!
!
   alg4 = exp(alog(H/cp)/rojcp-ej/r*S)
END FUNCTION alg4
