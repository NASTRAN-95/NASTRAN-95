
SUBROUTINE bldpkn(File,Block,Mcb)
   IMPLICIT NONE
   INCLUDE 'PAKBLK.COM'
   INCLUDE 'DSIOF.COM'
   INTEGER File
   INTEGER Block(15) , Mcb(7)
   Name = File
   IF ( Block(1)==0 ) THEN
      CALL dsbpnk(Iblka,Mcb)
   ELSE
      CALL dsbpnk(Block,Mcb)
   ENDIF
END SUBROUTINE bldpkn
