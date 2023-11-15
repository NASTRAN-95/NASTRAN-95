
FUNCTION alg9(H,S,V2)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
!
! Dummy argument declarations
!
   REAL H , S , V2
   REAL alg9
!
! End of declarations
!
!
!
   alg9 = Cp*V2/(Gamma*G*R*H)
END FUNCTION alg9
