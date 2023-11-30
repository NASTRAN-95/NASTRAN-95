
SUBROUTINE endgtb(Block)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Block(15)
   INTEGER id , len
   Name = Block(1)
   CALL dsgefl
   id = iand(Ibase(Indcbp),Maskq1)
   IF ( id/=Idsst ) CALL dsmsg(117)
   len = iand(Ibase(Indcbp),Maskh2)*Block(11)
   Indcbp = Indcbp - len - 2
   CALL dssdcb
END SUBROUTINE endgtb