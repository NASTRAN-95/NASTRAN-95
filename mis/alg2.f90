
FUNCTION alg2(S,P)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
!
! Dummy argument declarations
!
   REAL P , S
   REAL alg2
!
! End of declarations
!
!
!
   alg2 = Cp*exp(S/Cp+Rojcp*alog(P))
END FUNCTION alg2
