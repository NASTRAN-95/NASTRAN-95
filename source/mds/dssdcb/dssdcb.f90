!*==dssdcb.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dssdcb
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER iclr
   iclr = indclr - indbas + 1
   fcb(3,ifilex) = iclr
   fcb(4,ifilex) = nblock
   ibase(indbas+1) = indcbp - indbas + 1
   ibase(indbas+2) = iclr
   lasnam = name
!        WRITE(6,40646)IFILEX,NBLOCK,ICLR,INDBAS,INDCLR
99001 FORMAT (' DSSDCB,IFILEX,NBLOCK,ICLR,BAS,CLR=',I3,I5,6I7)
END SUBROUTINE dssdcb
