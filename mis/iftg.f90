
SUBROUTINE iftg(Tha,Rp,Cp)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL Cp , Rp , Tha
!
! Local variable declarations
!
   REAL c , c1 , r , r1
!
! End of declarations
!
   CALL ifte2(Tha,r,c)
   CALL ifte4(Tha,r1,c1)
   Rp = 2.*r - r1
   Cp = 2.*c - c1
END SUBROUTINE iftg
