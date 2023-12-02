!*==oldel1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE oldel1
   IMPLICIT NONE
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
