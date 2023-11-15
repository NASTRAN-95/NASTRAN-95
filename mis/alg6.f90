
FUNCTION alg6(P,T)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
!
! Dummy argument declarations
!
   REAL P , T
   REAL alg6
!
! End of declarations
!
!
!
   alg6 = Cp*T
END FUNCTION alg6
