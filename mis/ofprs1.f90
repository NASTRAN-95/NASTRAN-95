
SUBROUTINE ofprs1(Ix,L1,L2,L3,L4,L5,Point)
   IMPLICIT NONE
   INTEGER C(10)
   COMMON /ofpb1 / C
   INTEGER Ix , L1 , L2 , L3 , L4 , L5 , Point
!*****
!  SETS HEADER LINE FORMATS FOR REAL STREESES SORT1
!*****
   Ix = C(Point)
   L1 = C(Point+1)
   L2 = C(Point+2)
   L3 = C(Point+3)
   L4 = C(Point+4)
   L5 = C(Point+5)
END SUBROUTINE ofprs1