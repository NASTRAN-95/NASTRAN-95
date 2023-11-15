
FUNCTION alg8(H,S)
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
   REAL alg8
!
! End of declarations
!
!
!
   alg8 = Gamma
END FUNCTION alg8
