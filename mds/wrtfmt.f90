
SUBROUTINE wrtfmt(Iout,Nwds,Ifmt)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Iout , Nwds
   CHARACTER*1 Ifmt(*)
!
! End of declarations
!
   CALL forwrt(Ifmt,Iout,Nwds)
END SUBROUTINE wrtfmt
