
FUNCTION alg9(H,S,V2)
   IMPLICIT NONE
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
   REAL H , S , V2
   REAL alg9
!
!
   alg9 = Cp*V2/(Gamma*G*R*H)
END FUNCTION alg9