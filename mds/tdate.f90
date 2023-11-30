
SUBROUTINE tdate(Date)
   IMPLICIT NONE
   INTEGER Date(3)
   INTEGER date1(3)
!
!     VAX VERSION
!     ===========
!     (ALSO SiliconGraphics, DEC/ultrix, and SUN.
!      CRAY AND HP DO NOT HAVE IDATE)
!
!     THIS ROUTINE OBTAINS THE MONTH, DAY AND YEAR, IN INTEGER FORMAT
!
!
   CALL idate(date1)
!                 DAY   MONTH     YEAR
!     THESE DATES HAD TO BE INTERCHANGED FOR THE SUN
   Date(1) = date1(2)
   Date(2) = date1(1)
!DME  19 JAN 2016
!DME  D. Everhart
!DME  This is basically a Y2K bugfix.
   Date(3) = mod(date1(3),100)
!DME  DATE(3)=DATE1(3)-1900
END SUBROUTINE tdate