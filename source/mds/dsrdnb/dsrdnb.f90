!*==dsrdnb.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsrdnb
   USE i_dsiof
   USE i_xnstrn
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER iblk
   CALL dbmmgr(5)
   nblock = fcb(4,ifilex)
   indclr = indbas + 5
   indcbp = indclr
   lcw = ibase(indbas+4)
   iblk = ibase(indbas+3)
   IF ( iblk/=nblock ) CALL dsmsg(102)
END SUBROUTINE dsrdnb
