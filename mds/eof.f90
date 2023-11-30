
SUBROUTINE eof(File)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER File
   Name = File
   Iretrn = 0
   CALL dsgefl
   CALL dsefwr
   CALL dssdcb
END SUBROUTINE eof
