!*==olplot.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE olplot
   USE c_system
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: plt2
!
! End of declarations rewritten by SPAG
!
!
!     DRIVER FOR USER SUPPLIED INTERACTIVE PLOTTER
!
!     NOTE - ALL FORTRAN STOPS MUST BE CHANGED TO RETURNS
!     OTHERWISE THE FORTRAN STOPS WILL KILL THE INTERACTIVE SESSION.
!
   DATA plt2/13/
!
   WRITE (nout,99001)
99001 FORMAT ('    USER MUST SUPPLY SITE DEPENDENT PLOTTING PACKAGE',/4X,'IN SUBROUTINE OLPLOT FOR INTERACTIVE PLOTS')
!
!
   REWIND plt2
!     CALL THE SITE DEPENDENT PLOTTING ROUTINES HERE.
!     CALL NASPLOT
END SUBROUTINE olplot
