
SUBROUTINE dsrdnb
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER iblk
   CALL dbmmgr(5)
   Nblock = Fcb(4,Ifilex)
   Indclr = Indbas + 5
   Indcbp = Indclr
   Lcw = Ibase(Indbas+4)
   iblk = Ibase(Indbas+3)
   IF ( iblk/=Nblock ) CALL dsmsg(102)
END SUBROUTINE dsrdnb
