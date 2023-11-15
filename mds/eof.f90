
SUBROUTINE eof(File)
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
   Iretrn = 0
   CALL dsgefl
   CALL dsefwr
   CALL dssdcb
END SUBROUTINE eof
