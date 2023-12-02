!*==errtrc.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE errtrc(Name,Ival)
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   CHARACTER(*) :: Name
   INTEGER :: Ival
!
! End of declarations rewritten by SPAG
!
   WRITE (nout,*) ' ERRTRC CALLED'
   WRITE (nout,*) ' NAME=' , Name
   WRITE (nout,*) ' IVAL=' , Ival
END SUBROUTINE errtrc
