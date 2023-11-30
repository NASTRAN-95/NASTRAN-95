
FUNCTION alg5(H,S)
   IMPLICIT NONE
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
   REAL H , S
   REAL alg5
   REAL alg4
!
!
   alg5 = alg4(H,S)/(R*H)*Cp
END FUNCTION alg5