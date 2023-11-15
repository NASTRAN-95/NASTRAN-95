
SUBROUTINE dsclos(Iunit)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
!
! Dummy argument declarations
!
   INTEGER Iunit
!
! End of declarations
!
!      print *,' dsclos,iunit=',iunit
   CLOSE (Iunit)
   Numcls = Numcls + 1
END SUBROUTINE dsclos
