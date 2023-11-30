
SUBROUTINE logfil(Line)
   IMPLICIT NONE
   INTEGER Lout
   COMMON /logout/ Lout
   INTEGER Line(18)
!
!
!
   WRITE (Lout,99001) Line
!
99001 FORMAT (1X,18A4)
   RETURN
END SUBROUTINE logfil
