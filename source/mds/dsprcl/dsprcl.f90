!*==dsprcl.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsprcl(Block)
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Block(15)
   INTEGER idiv(4)
   DATA idiv/1 , 2 , 1 , 2/
   Block(2) = iand(ibase(indcbp),maskq4)
   Block(3) = iand(ibase(indcbp),maskq3)
   Block(3) = Block(3)/mulq3
   Block(11) = nwrdel(Block(2))
   Block(12) = ibase(indcbp+1)
   Block(14) = idiv(Block(2))
END SUBROUTINE dsprcl
