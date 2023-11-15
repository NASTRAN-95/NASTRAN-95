
SUBROUTINE errtrc(Name,Ival)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Isysbf , Nout
   COMMON /system/ Isysbf , Nout
!
! Dummy argument declarations
!
   INTEGER Ival
   CHARACTER*(*) Name
!
! End of declarations
!
   WRITE (Nout,*) ' ERRTRC CALLED'
   WRITE (Nout,*) ' NAME=' , Name
   WRITE (Nout,*) ' IVAL=' , Ival
END SUBROUTINE errtrc
