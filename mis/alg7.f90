
FUNCTION alg7(H,S)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
!
! Dummy argument declarations
!
   REAL H , S
   REAL alg7
!
! End of declarations
!
!
!
   alg7 = H/Cp
END FUNCTION alg7
