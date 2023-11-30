
SUBROUTINE waltim(Walsec)
   IMPLICIT NONE
   INTEGER Walsec
   INTEGER time(3)
!
!     THIS ROUTINE OBTAINS THE CURRENT WALL CLOCK TIME IN SECONDS,
!     PASS MID-NIGHT
!
!
   CALL itime(time)
   Walsec = time(1)*3600 + time(2)*60 + time(3)
END SUBROUTINE waltim