!*==alg3.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION alg3(P,H)
   IMPLICIT NONE
   USE C_GAS
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
   alg3 = Cp*alog(H/Cp) - R/Ej*alog(P)
END FUNCTION alg3
