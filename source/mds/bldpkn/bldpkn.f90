!*==bldpkn.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bldpkn(File,Block,Mcb)
   USE i_pakblk
   USE i_dsiof
   USE I_PAKBLK
   USE I_DSIOF
   IMPLICIT NONE
   INCLUDE 'PAKBLK.COM'
   INCLUDE 'DSIOF.COM'
   INTEGER File
   INTEGER Block(15) , Mcb(7)
   name = File
   IF ( Block(1)==0 ) THEN
      CALL dsbpnk(iblka,Mcb)
   ELSE
      CALL dsbpnk(Block,Mcb)
   ENDIF
END SUBROUTINE bldpkn
