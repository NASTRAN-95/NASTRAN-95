
SUBROUTINE dsclos(Iunit)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER Iunit
!      print *,' dsclos,iunit=',iunit
   CLOSE (Iunit)
   Numcls = Numcls + 1
END SUBROUTINE dsclos