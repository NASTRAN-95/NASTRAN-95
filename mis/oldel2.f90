
SUBROUTINE oldel2
   IMPLICIT NONE
!
!     ANY ELEMENT (NEW OR OLD) WHICH HAS NOT BEEN CONVERTED TO USE
!     EMGPRO SHOULD HAVE AN ENTRY POINT IN OLDEL1, OLDEL2, OR OLDEL3
!     ***************************************************************
!
   ENTRY hexa1s
   ENTRY hexa1d
   ENTRY hexa2s
   ENTRY hexa2d
   ENTRY plotls
   ENTRY plotld
   ENTRY qdmems
   ENTRY qdmemd
   ENTRY qdplts
   ENTRY qdpltd
   ENTRY quad1s
   ENTRY quad1d
   ENTRY quad2s
   ENTRY quad2d
   ENTRY slot3s
   ENTRY slot3d
   ENTRY slot4s
   ENTRY slot4d
!
   CALL emgold
END SUBROUTINE oldel2