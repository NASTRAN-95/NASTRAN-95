!*==logfil.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE logfil(Line)
   USE c_logout
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(18) :: Line
!
! End of declarations rewritten by SPAG
!
!
!
!
   WRITE (lout,99001) Line
!
99001 FORMAT (1X,18A4)
END SUBROUTINE logfil
