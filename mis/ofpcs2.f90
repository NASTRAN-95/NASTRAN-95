
SUBROUTINE ofpcs2(Ix,L1,L2,L3,L4,L5,Point)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER C(10)
   COMMON /ofpb4 / C
!
! Dummy argument declarations
!
   INTEGER Ix , L1 , L2 , L3 , L4 , L5 , Point
!
! End of declarations
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
