
SUBROUTINE odum(I1,Ix,Itype,Nmult,Nlines,Id)
   IMPLICIT NONE
   INTEGER I1 , Id , Itype , Ix , Nlines , Nmult
!     OUTPUT DUMMY ROUTINE
   IF ( Ix+Ix+Itype==Nmult ) Nlines = Id
END SUBROUTINE odum