!*==endgtb.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE endgtb(Block)
   USE i_dsiof
   USE i_xnstrn
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Block(15)
   INTEGER id , len
   name = Block(1)
   CALL dsgefl
   id = iand(ibase(indcbp),maskq1)
   IF ( id/=idsst ) CALL dsmsg(117)
   len = iand(ibase(indcbp),maskh2)*Block(11)
   indcbp = indcbp - len - 2
   CALL dssdcb
END SUBROUTINE endgtb
