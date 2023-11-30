
SUBROUTINE tmtogo(Togo)
   IMPLICIT NONE
   REAL Tmbegn , Xsys(17)
   INTEGER Tprob
   COMMON /stime / Tprob
   COMMON /system/ Xsys , Tmbegn
   INTEGER Togo
   INTEGER tbegin , tnow
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