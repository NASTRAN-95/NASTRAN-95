!*==dsgncl.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsgncl
   USE I_DSIOF
   USE I_GINOX
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'GINOX.COM'
   idsn = mdsfcb(2,ifilex)
   CALL dsclos(idsn)
   mdsfcb(1,idsn) = iand(mdsfcb(1,idsn),maskh1)
END SUBROUTINE dsgncl
