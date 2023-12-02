!*==eof.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE eof(File)
   USE I_DSIOF
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER File
   name = File
   iretrn = 0
   CALL dsgefl
   CALL dsefwr
   CALL dssdcb
END SUBROUTINE eof
