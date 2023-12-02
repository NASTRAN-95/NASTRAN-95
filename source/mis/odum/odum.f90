!*==odum.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE odum(I1,Ix,Itype,Nmult,Nlines,Id)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: I1
   INTEGER :: Ix
   INTEGER :: Itype
   INTEGER :: Nmult
   INTEGER :: Nlines
   INTEGER :: Id
!
! End of declarations rewritten by SPAG
!
!     OUTPUT DUMMY ROUTINE
   IF ( Ix+Ix+Itype==Nmult ) Nlines = Id
END SUBROUTINE odum
