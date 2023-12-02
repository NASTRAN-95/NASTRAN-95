!*==klock.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE klock(Icpusc)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Icpusc
   EXTERNAL cputim
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE OBTAINS THE CURRENT CPU TIME AS AN INTEGER VALUE
!
   CALL cputim(Icpusc,Icpusc,0)
END SUBROUTINE klock
