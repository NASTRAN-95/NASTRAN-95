
SUBROUTINE logfil(Line)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Lout
   COMMON /logout/ Lout
!
! Dummy argument declarations
!
   INTEGER Line(18)
!
! End of declarations
!
!
!
!
   WRITE (Lout,99001) Line
!
99001 FORMAT (1X,18A4)
   RETURN
END SUBROUTINE logfil
