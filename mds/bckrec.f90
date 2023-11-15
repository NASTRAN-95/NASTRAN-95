
SUBROUTINE bckrec(File)
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
   CALL dsbrc1
   CALL dssdcb
END SUBROUTINE bckrec
