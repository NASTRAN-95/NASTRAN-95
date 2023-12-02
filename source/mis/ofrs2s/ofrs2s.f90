!*==ofrs2s.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ofrs2s(Ix,L1,L2,L3,L4,L5,Point)
   USE c_ofpb3s
   IMPLICIT NONE
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
!  SETS HEADER LINE FORMATS FOR REAL STRESSES SORT2 - STATICS
!*****
!*****
   Ix = c(Point)
   L1 = c(Point+1)
   L2 = c(Point+2)
   L3 = c(Point+3)
   L4 = c(Point+4)
   L5 = c(Point+5)
END SUBROUTINE ofrs2s
