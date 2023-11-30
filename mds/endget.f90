
SUBROUTINE endget(Block)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Block(15)
   INTEGER nelm
   Name = Block(1)
   CALL dsgefl
   Nwords = Block(11)
   nelm = iand(Ibase(Indcbp-2),Maskh2)
   Indcbp = Indcbp + nelm*Nwords + Block(3)*2
   CALL dssdcb
END SUBROUTINE endget