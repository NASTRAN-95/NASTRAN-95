!*==nastim.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE nastim(Ihr,Imn,Isc,Cpusec)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ihr
   INTEGER :: Imn
   INTEGER :: Isc
   REAL :: Cpusec
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: array
   REAL :: secs , time
   EXTERNAL etime
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
!DME  19 JAN 2016
!DME  D. Everhart
!DME  Changed to conform to GFORTRAN implementation of ETIME subroutine.
   CALL etime(array,time)
!DME  CALL ETIME(ARRAY)
   secs = array(2)
   Ihr = secs/3600.
   Imn = (secs-3600.*Ihr)/60.
   Isc = secs - (3600.*Ihr) - (60.*Imn)
   Cpusec = secs
END SUBROUTINE nastim
