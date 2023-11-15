
FUNCTION alg5(H,S)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
!
! Dummy argument declarations
!
   REAL H , S
   REAL alg5
!
! Local variable declarations
!
   REAL alg4
!
! End of declarations
!
!
!
   alg5 = alg4(H,S)/(R*H)*Cp
END FUNCTION alg5
