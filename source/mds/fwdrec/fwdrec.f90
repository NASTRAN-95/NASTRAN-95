!*==fwdrec.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fwdrec(File) !HIDESTARS (*,File)
   USE I_DSIOF
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER File
   name = File
   CALL dsgefl
   CALL dsfwr1
   CALL dssdcb
   IF ( iretrn==1 ) RETURN 1
END SUBROUTINE fwdrec
