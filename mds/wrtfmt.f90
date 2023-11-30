
SUBROUTINE wrtfmt(Iout,Nwds,Ifmt)
   IMPLICIT NONE
   INTEGER Iout , Nwds
   CHARACTER*1 Ifmt(*)
   CALL forwrt(Ifmt,Iout,Nwds)
END SUBROUTINE wrtfmt