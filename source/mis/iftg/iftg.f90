!*==iftg.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE iftg(Tha,Rp,Cp)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Tha
   REAL :: Rp
   REAL :: Cp
!
! Local variable declarations rewritten by SPAG
!
   REAL :: c , c1 , r , r1
   EXTERNAL ifte2 , ifte4
!
! End of declarations rewritten by SPAG
!
   CALL ifte2(Tha,r,c)
   CALL ifte4(Tha,r1,c1)
   Rp = 2.*r - r1
   Cp = 2.*c - c1
END SUBROUTINE iftg
