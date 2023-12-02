!*==dsblpk.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsblpk(Block)
   USE i_dsiof
   USE i_pakblk
   USE i_xnstrn
   USE I_DSIOF
   USE I_PAKBLK
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'PAKBLK.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Block(15)
   INTEGER iflag
   Block(1) = name
   Block(2) = itypo
   IF ( itrail==-1 ) THEN
      Block(3) = 1
   ELSE
      Block(3) = 0
   ENDIF
   Block(4) = 0
   Block(7) = 0
   Block(8) = -1
   Block(10) = 0
   Block(12) = Block(12) + 1
   Block(13) = itypi
   CALL putstr(Block)
   iflag = fcb(8,ifilex)
   IF ( iflag==0 ) THEN
      Block(12) = 1
      fcb(8,ifilex) = 1
      ibase(indclr+2) = 1
   ENDIF
END SUBROUTINE dsblpk
