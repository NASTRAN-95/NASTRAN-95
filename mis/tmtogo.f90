
SUBROUTINE tmtogo(Togo)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Tmbegn , Xsys(17)
   INTEGER Tprob
   COMMON /stime / Tprob
   COMMON /system/ Xsys , Tmbegn
!
! Dummy argument declarations
!
   INTEGER Togo
!
! Local variable declarations
!
   INTEGER tbegin , tnow
!
! End of declarations
!
!
!     TO COMPUTE THE TIME (IN SECONDS) REMAINING
!
!
!     GET PRESENT TIME
!
   CALL klock(tnow)
!
!     COMPUTE TIME TO GO
!
   tbegin = Tmbegn
   Togo = Tprob - (tnow-tbegin)
END SUBROUTINE tmtogo
