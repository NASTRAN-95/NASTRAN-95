
SUBROUTINE bckrec(File)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER File
   Name = File
   CALL dsgefl
   CALL dsbrc1
   CALL dssdcb
END SUBROUTINE bckrec
