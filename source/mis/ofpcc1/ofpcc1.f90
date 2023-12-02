!*==ofpcc1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ofpcc1(Ix,L1,L2,L3,L4,L5,Ipoint)
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
   INTEGER :: Ipoint
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(48) , SAVE :: idata
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!*****
!     SETS HEADER LINE FORMATS FOR COMPLEX ELEMENT STRESSES IN
!     MATERIAL COORDINATE SYSTEM  --  SORT 1 OUTPUT
!*****
!
   DATA idata/3951 , 104 , 139 , 125 , 0 , 432 , 3977 , 104 , 139 , 126 , 0 , 432 , 3951 , 104 , 140 , 125 , 0 , 432 , 3977 , 104 , &
      & 140 , 126 , 0 , 432 , 3951 , 104 , 135 , 125 , 0 , 432 , 3977 , 104 , 135 , 126 , 0 , 432 , 3951 , 104 , 134 , 125 , 0 ,    &
      & 432 , 3977 , 104 , 134 , 126 , 0 , 432/
!
   Ix = idata(Ipoint)
   L1 = idata(Ipoint+1)
   L2 = idata(Ipoint+2)
   L3 = idata(Ipoint+3)
   L4 = idata(Ipoint+4)
   L5 = idata(Ipoint+5)
!
END SUBROUTINE ofpcc1
