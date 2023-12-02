!*==dsrlse.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsrlse
   USE I_DSIOF
   USE I_GINOX
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'GINOX.COM'
   INTEGER inext , nexdsn
   inext = ifilex
   SPAG_Loop_1_1: DO
      nexdsn = iand(mdsfcb(3,inext),maskh2)
      IF ( nexdsn==0 ) EXIT SPAG_Loop_1_1
      mdsfcb(1,inext) = iand(mdsfcb(1,inext),maskh1)
      mdsfcb(2,inext) = 0
      mdsfcb(3,inext) = 0
!
! OPEN AND CLOSE FILE TO DELETE SPACE ALLOCATION
!
      CALL dsopen(mdsnam(nexdsn),nexdsn,1)
      CALL dsclos(nexdsn)
      inext = nexdsn
   ENDDO SPAG_Loop_1_1
END SUBROUTINE dsrlse
