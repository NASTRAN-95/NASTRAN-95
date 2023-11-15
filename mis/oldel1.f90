
SUBROUTINE oldel1
   IMPLICIT NONE
!
! End of declarations
!
!
!     ANY ELEMENT (NEW OR OLD) WHICH HAS NOT BEEN CONVERTED TO USE
!     EMGPRO SHOULD HAVE AN ENTRY POINT IN OLDEL1, OLDEL2, OR OLDEL3
!     ***************************************************************
!
   ENTRY axif2s
   ENTRY axif2d
   ENTRY axif3s
   ENTRY axif3d
   ENTRY axif4s
   ENTRY axif4d
   ENTRY cones
   ENTRY coned
   ENTRY elbows
   ENTRY elbowd
   ENTRY flmass
   ENTRY flmasd
   ENTRY flud2s
   ENTRY flud2d
   ENTRY flud3s
   ENTRY flud3d
   ENTRY flud4s
   ENTRY flud4d
!
   CALL emgold
END SUBROUTINE oldel1
