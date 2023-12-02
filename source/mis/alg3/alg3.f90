!*==alg3.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION alg3(P,H)
   USE c_gas
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: alg3
   REAL :: P
   REAL :: H
!
! End of declarations rewritten by SPAG
!
!
!
   alg3 = cp*alog(H/cp) - r/ej*alog(P)
END FUNCTION alg3
