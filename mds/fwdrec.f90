
SUBROUTINE fwdrec(*,File)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
!
! Dummy argument declarations
!
   INTEGER File
!
! End of declarations
!
   Name = File
   CALL dsgefl
   CALL dsfwr1
   CALL dssdcb
   IF ( Iretrn==1 ) RETURN 1
END SUBROUTINE fwdrec
