!*==waltim.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE waltim(Walsec)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Walsec
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) :: time
   EXTERNAL itime
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE OBTAINS THE CURRENT WALL CLOCK TIME IN SECONDS,
!     PASS MID-NIGHT
!
!
   CALL itime(time)
   Walsec = time(1)*3600 + time(2)*60 + time(3)
END SUBROUTINE waltim
