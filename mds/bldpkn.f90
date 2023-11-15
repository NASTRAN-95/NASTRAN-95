
SUBROUTINE bldpkn(File,Block,Mcb)
   IMPLICIT NONE
   INCLUDE 'PAKBLK.COM'
   INCLUDE 'DSIOF.COM'
!
! Dummy argument declarations
!
   INTEGER File
   INTEGER Block(15) , Mcb(7)
!
! End of declarations
!
   Name = File
   IF ( Block(1)==0 ) THEN
      CALL dsbpnk(Iblka,Mcb)
   ELSE
      CALL dsbpnk(Block,Mcb)
   ENDIF
END SUBROUTINE bldpkn
