
FUNCTION alg7(H,S)
   IMPLICIT NONE
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
   REAL H , S
   REAL alg7
!
!
   alg7 = H/Cp
END FUNCTION alg7
