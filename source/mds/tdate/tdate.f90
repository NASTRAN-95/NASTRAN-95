!*==tdate.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tdate(Date)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) :: Date
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) :: date1
   EXTERNAL idate
!
! End of declarations rewritten by SPAG
!
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
