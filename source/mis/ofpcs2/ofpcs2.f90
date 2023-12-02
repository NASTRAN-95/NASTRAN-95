!*==ofpcs2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ofpcs2(Ix,L1,L2,L3,L4,L5,Point)
   IMPLICIT NONE
   USE C_OFPB4
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ix
   INTEGER :: L1
   INTEGER :: L2
   INTEGER :: L3
   INTEGER :: L4
   INTEGER :: L5
   INTEGER :: Point
!
! End of declarations rewritten by SPAG
!
!*****
!  SETS HEADER LINE FORMATS FOR COMPLEX STRESSES SORT2
!*****
   Ix = C(Point)
   L1 = C(Point+1)
   L2 = C(Point+2)
   L3 = C(Point+3)
   L4 = C(Point+4)
   L5 = C(Point+5)
END SUBROUTINE ofpcs2
