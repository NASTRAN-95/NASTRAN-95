
SUBROUTINE ifp5a(Num)
   IMPLICIT NONE
   LOGICAL Nogo
   INTEGER Output
   REAL Sysbuf
   CHARACTER*23 Ufm
   COMMON /system/ Sysbuf , Output , Nogo
   COMMON /xmssg / Ufm
   INTEGER Num
   INTEGER i
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