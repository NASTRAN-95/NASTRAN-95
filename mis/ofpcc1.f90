
SUBROUTINE ofpcc1(Ix,L1,L2,L3,L4,L5,Ipoint)
   IMPLICIT NONE
   INTEGER Ipoint , Ix , L1 , L2 , L3 , L4 , L5
   INTEGER idata(48)
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