
SUBROUTINE klock(Icpusc)
   IMPLICIT NONE
   INTEGER Icpusc
!
!     THIS SUBROUTINE OBTAINS THE CURRENT CPU TIME AS AN INTEGER VALUE
!
   CALL cputim(Icpusc,Icpusc,0)
END SUBROUTINE klock