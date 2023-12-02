!*==errtrc.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE errtrc(Name,Ival)
   IMPLICIT NONE
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   CHARACTER(*) :: Name
   INTEGER :: Ival
!
! End of declarations rewritten by SPAG
!
   WRITE (Nout,*) ' ERRTRC CALLED'
   WRITE (Nout,*) ' NAME=' , Name
   WRITE (Nout,*) ' IVAL=' , Ival
END SUBROUTINE errtrc
