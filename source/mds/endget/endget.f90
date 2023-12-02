!*==endget.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE endget(Block)
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Block(15)
   INTEGER nelm
   name = Block(1)
   CALL dsgefl
   nwords = Block(11)
   nelm = iand(ibase(indcbp-2),maskh2)
   indcbp = indcbp + nelm*nwords + Block(3)*2
   CALL dssdcb
END SUBROUTINE endget
