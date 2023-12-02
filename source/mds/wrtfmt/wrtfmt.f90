!*==wrtfmt.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE wrtfmt(Iout,Nwds,Ifmt)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iout
   INTEGER :: Nwds
   CHARACTER(1) , DIMENSION(*) :: Ifmt
   EXTERNAL forwrt
!
! End of declarations rewritten by SPAG
!
   CALL forwrt(Ifmt,Iout,Nwds)
END SUBROUTINE wrtfmt
