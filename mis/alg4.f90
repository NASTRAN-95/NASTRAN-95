
FUNCTION alg4(H,S)
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
   REAL alg4
!
! End of declarations
!
!
!
   alg4 = exp(alog(H/Cp)/Rojcp-Ej/R*S)
END FUNCTION alg4
