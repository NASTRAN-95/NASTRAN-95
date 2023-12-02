!*==oldel3.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE oldel3
   IMPLICIT NONE
!
!     ANY ELEMENT (NEW OR OLD) WHICH HAS NOT BEEN CONVERTED TO USE
!     EMGPRO SHOULD HAVE AN ENTRY POINT IN OLDEL1, OLDEL2, OR OLDEL3
!     ***************************************************************
!
   ENTRY tetras
   ENTRY tetrad
   ENTRY traprs
   ENTRY traprd
   ENTRY triars
   ENTRY triard
   ENTRY tria1s
   ENTRY tria1d
   ENTRY tria2s
   ENTRY tria2d
   ENTRY trplts
   ENTRY trpltd
   ENTRY wedges
   ENTRY wedged
!
   CALL emgold
END SUBROUTINE oldel3
