!*==corwds.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION corwds(I,J)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: corwds
   INTEGER :: I
   INTEGER :: J
   EXTERNAL locfx
!
! End of declarations rewritten by SPAG
!
!
!
   corwds = iabs(locfx(I)-locfx(J)) + 1
END FUNCTION corwds
