!*==bckrec.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bckrec(File)
   USE I_DSIOF
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER File
   name = File
   CALL dsgefl
   CALL dsbrc1
   CALL dssdcb
END SUBROUTINE bckrec
