!*==filpos.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE filpos(File,Ipos)
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File , Ipos
   INTEGER icblk
   name = File
   CALL dsgefl
   nblock = iand(Ipos,maskh2)
   icblk = fcb(4,ifilex)
   IF ( icblk/=nblock ) CALL dbmmgr(6)
   indclr = Ipos/mulq2 + indbas - 1
   indcbp = indclr
   CALL dssdcb
END SUBROUTINE filpos
