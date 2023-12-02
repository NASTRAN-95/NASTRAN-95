!*==makmcb.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE makmcb(Mcb,Ifile,Irow,If,It)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: Mcb
   INTEGER :: Ifile
   INTEGER :: Irow
   INTEGER :: If
   INTEGER :: It
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!
   Mcb(1) = Ifile
   Mcb(2) = 0
   Mcb(3) = Irow
   Mcb(4) = If
   Mcb(5) = It
   Mcb(6) = 0
   Mcb(7) = 0
!
!
END SUBROUTINE makmcb
