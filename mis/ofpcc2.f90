
SUBROUTINE ofpcc2(Ix,L1,L2,L3,L4,L5,Ipoint)
   IMPLICIT NONE
   INTEGER Ipoint , Ix , L1 , L2 , L3 , L4 , L5
   INTEGER idata(48)
!*****
!     SETS HEADER LINE FORMATS FOR COMPLEX ELEMENT STRESSES IN
!     MATERIAL COORDINATE SYSTEM  --  SORT 2 OUTPUT
!*****
!
   DATA idata/4003 , 108 , 139 , 125 , 0 , 433 , 4031 , 108 , 139 , 126 , 0 , 433 , 4003 , 108 , 140 , 125 , 0 , 433 , 4031 , 108 , &
      & 140 , 126 , 0 , 433 , 4003 , 108 , 135 , 125 , 0 , 433 , 4031 , 108 , 135 , 126 , 0 , 433 , 4003 , 108 , 134 , 125 , 0 ,    &
      & 433 , 4031 , 108 , 134 , 126 , 0 , 433/
!
   Ix = idata(Ipoint)
   L1 = idata(Ipoint+1)
   L2 = idata(Ipoint+2)
   L3 = idata(Ipoint+3)
   L4 = idata(Ipoint+4)
   L5 = idata(Ipoint+5)
!
END SUBROUTINE ofpcc2
