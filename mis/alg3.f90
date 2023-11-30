
FUNCTION alg3(P,H)
   IMPLICIT NONE
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
   REAL H , P
   REAL alg3
!
!
   alg3 = Cp*alog(H/Cp) - R/Ej*alog(P)
END FUNCTION alg3
