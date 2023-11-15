
FUNCTION alg3(P,H)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cp , Ej , G , Gamma , R , Rojcp
   COMMON /gas   / G , Ej , R , Cp , Gamma , Rojcp
!
! Dummy argument declarations
!
   REAL H , P
   REAL alg3
!
! End of declarations
!
!
!
   alg3 = Cp*alog(H/Cp) - R/Ej*alog(P)
END FUNCTION alg3
