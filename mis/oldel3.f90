
SUBROUTINE oldel3
   IMPLICIT NONE
!
! End of declarations
!
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
