!*==dswrnb.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
 
SUBROUTINE dswrnb
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   ibase(indbas+4) = indclr - indbas + 1
   CALL dbmmgr(4)
   nblock = fcb(4,ifilex)
   indclr = indbas + 5
   ibase(indbas+3) = nblock
   indcbp = indclr
END SUBROUTINE dswrnb
