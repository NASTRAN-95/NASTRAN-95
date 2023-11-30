
FUNCTION alg8(H,S)
   IMPLICIT NONE
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
   REAL H , S
   REAL alg8
!
!
   alg8 = Gamma
END FUNCTION alg8
