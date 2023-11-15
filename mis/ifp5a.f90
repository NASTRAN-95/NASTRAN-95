
SUBROUTINE ifp5a(Num)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   LOGICAL Nogo
   INTEGER Output
   REAL Sysbuf
   CHARACTER*23 Ufm
   COMMON /system/ Sysbuf , Output , Nogo
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   INTEGER Num
!
! Local variable declarations
!
   INTEGER i
!
! End of declarations
!
!
!     IFP5A PRINTS MESSAGE NUMBER LINE ONLY.
!     CALLING SUBROUTINE PRINTS THE MESSAGE.
!
!
   CALL page2(4)
   i = Num + 4080
   WRITE (Output,99001) Ufm , i
99001 FORMAT (A23,I15,1H.)
   Nogo = .TRUE.
END SUBROUTINE ifp5a
