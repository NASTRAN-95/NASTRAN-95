
FUNCTION alg2(S,P)
   IMPLICIT NONE
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
   REAL P , S
   REAL alg2
!
!
   alg2 = Cp*exp(S/Cp+Rojcp*alog(P))
END FUNCTION alg2
