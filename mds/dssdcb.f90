
SUBROUTINE dssdcb
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER iclr
   iclr = Indclr - Indbas + 1
   Fcb(3,Ifilex) = iclr
   Fcb(4,Ifilex) = Nblock
   Ibase(Indbas+1) = Indcbp - Indbas + 1
   Ibase(Indbas+2) = iclr
   Lasnam = Name
!        WRITE(6,40646)IFILEX,NBLOCK,ICLR,INDBAS,INDCLR
99001 FORMAT (' DSSDCB,IFILEX,NBLOCK,ICLR,BAS,CLR=',I3,I5,6I7)
END SUBROUTINE dssdcb
