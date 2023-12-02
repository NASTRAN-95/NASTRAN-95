!*==dsclos.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsclos(Iunit)
   USE i_dsiof
   USE I_DSIOF
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER Iunit
!      print *,' dsclos,iunit=',iunit
   CLOSE (Iunit)
   numcls = numcls + 1
END SUBROUTINE dsclos
