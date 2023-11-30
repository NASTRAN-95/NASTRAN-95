
SUBROUTINE dsblpk(Block)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'PAKBLK.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Block(15)
   INTEGER iflag
   Block(1) = Name
   Block(2) = Itypo
   IF ( Itrail==-1 ) THEN
      Block(3) = 1
   ELSE
      Block(3) = 0
   ENDIF
   Block(4) = 0
   Block(7) = 0
   Block(8) = -1
   Block(10) = 0
   Block(12) = Block(12) + 1
   Block(13) = Itypi
   CALL putstr(Block)
   iflag = Fcb(8,Ifilex)
   IF ( iflag==0 ) THEN
      Block(12) = 1
      Fcb(8,Ifilex) = 1
      Ibase(Indclr+2) = 1
   ENDIF
END SUBROUTINE dsblpk
