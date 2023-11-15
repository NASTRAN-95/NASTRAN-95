
SUBROUTINE waltim(Walsec)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Walsec
!
! Local variable declarations
!
   INTEGER time(3)
!
! End of declarations
!
!
!     THIS ROUTINE OBTAINS THE CURRENT WALL CLOCK TIME IN SECONDS,
!     PASS MID-NIGHT
!
!
   CALL itime(time)
   Walsec = time(1)*3600 + time(2)*60 + time(3)
END SUBROUTINE waltim
