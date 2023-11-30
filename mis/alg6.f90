
FUNCTION alg6(P,T)
   IMPLICIT NONE
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
   REAL P , T
   REAL alg6
!
!
   alg6 = Cp*T
END FUNCTION alg6
