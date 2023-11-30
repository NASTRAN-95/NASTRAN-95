
SUBROUTINE fwdrec(*,File)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER File
   Name = File
   CALL dsgefl
   CALL dsfwr1
   CALL dssdcb
   IF ( Iretrn==1 ) RETURN 1
END SUBROUTINE fwdrec
