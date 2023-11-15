
SUBROUTINE klock(Icpusc)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Icpusc
!
! End of declarations
!
!
!     THIS SUBROUTINE OBTAINS THE CURRENT CPU TIME AS AN INTEGER VALUE
!
   CALL cputim(Icpusc,Icpusc,0)
END SUBROUTINE klock
