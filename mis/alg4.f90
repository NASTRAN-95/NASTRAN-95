
FUNCTION alg4(H,S)
   IMPLICIT NONE
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
   REAL H , S
   REAL alg4
!
!
   alg4 = exp(alog(H/Cp)/Rojcp-Ej/R*S)
END FUNCTION alg4