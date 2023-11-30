
SUBROUTINE errtrc(Name,Ival)
   IMPLICIT NONE
   INTEGER Isysbf , Nout
   COMMON /system/ Isysbf , Nout
   INTEGER Ival
   CHARACTER*(*) Name
   WRITE (Nout,*) ' ERRTRC CALLED'
   WRITE (Nout,*) ' NAME=' , Name
   WRITE (Nout,*) ' IVAL=' , Ival
END SUBROUTINE errtrc