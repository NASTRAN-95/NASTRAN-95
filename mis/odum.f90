
SUBROUTINE odum(I1,Ix,Itype,Nmult,Nlines,Id)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER I1 , Id , Itype , Ix , Nlines , Nmult
!
! End of declarations
!
!     OUTPUT DUMMY ROUTINE
   IF ( Ix+Ix+Itype==Nmult ) Nlines = Id
END SUBROUTINE odum
