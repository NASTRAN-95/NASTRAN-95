
 
 
SUBROUTINE dswrnb
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   Ibase(Indbas+4) = Indclr - Indbas + 1
   CALL dbmmgr(4)
   Nblock = Fcb(4,Ifilex)
   Indclr = Indbas + 5
   Ibase(Indbas+3) = Nblock
   Indcbp = Indclr
END SUBROUTINE dswrnb