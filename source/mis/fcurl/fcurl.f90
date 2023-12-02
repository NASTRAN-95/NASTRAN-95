!*==fcurl.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fcurl(Fmeo,Fme1,Ffeo,Ffe1,Yi,S,Lam1)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(10,2) :: Fmeo
   REAL , DIMENSION(10,2) :: Fme1
   REAL , DIMENSION(10,2) :: Ffeo
   REAL , DIMENSION(10,2) :: Ffe1
   REAL , DIMENSION(6,7) :: Yi
   REAL :: S
   REAL :: Lam1
!
! Local variable declarations rewritten by SPAG
!
   REAL :: s1
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
!     ------------------------------------------------------------------
   Fmeo(1,1) = 0.0
   Fmeo(2,1) = Yi(1,1)
   Fmeo(3,1) = Yi(1,2)*2.0
   Fmeo(4,1) = Yi(1,3)*3.0
   Fmeo(5,1) = Yi(1,1)*Lam1
   Fmeo(6,1) = Yi(1,2)*Lam1
   Fmeo(7,1) = Yi(1,3)*Lam1
   Fmeo(8,1) = Yi(1,4)*Lam1
   Fmeo(9,1) = Yi(1,5)*Lam1
   Fmeo(10,1) = Yi(1,6)*Lam1
!
   Fmeo(1,2) = Yi(4,1)
   Fmeo(2,2) = Yi(4,2)
   Fmeo(3,2) = Yi(4,3)
   Fmeo(4,2) = Yi(4,4)
   Fmeo(5,2) = Yi(2,1)
   Fmeo(6,2) = Yi(2,2)
   Fmeo(7,2) = Yi(2,3)
   Fmeo(8,2) = Yi(2,4)
   Fmeo(9,2) = Yi(2,5)
   Fmeo(10,2) = Yi(2,6)
!
   s1 = 1.0/S
   Fme1(1,1) = 0.0
   Fme1(2,1) = s1*Yi(1,2)
   Fme1(3,1) = s1*Yi(1,3)*2.0
   Fme1(4,1) = s1*Yi(1,4)*3.0
   Fme1(5,1) = s1*Yi(1,2)*Lam1
   Fme1(6,1) = s1*Yi(1,3)*Lam1
   Fme1(7,1) = s1*Yi(1,4)*Lam1
   Fme1(8,1) = s1*Yi(1,5)*Lam1
   Fme1(9,1) = s1*Yi(1,6)*Lam1
   Fme1(10,1) = s1*Yi(1,7)*Lam1
!
   Fme1(1,2) = s1*Yi(4,2)
   Fme1(2,2) = s1*Yi(4,3)
   Fme1(3,2) = s1*Yi(4,4)
   Fme1(4,2) = s1*Yi(4,5)
   Fme1(5,2) = s1*Yi(2,2)
   Fme1(6,2) = s1*Yi(2,3)
   Fme1(7,2) = s1*Yi(2,4)
   Fme1(8,2) = s1*Yi(2,5)
   Fme1(9,2) = s1*Yi(2,6)
   Fme1(10,2) = s1*Yi(2,7)
!
   Ffeo(1,1) = 0.0
   Ffeo(2,1) = 0.0
   Ffeo(3,1) = 0.0
   Ffeo(4,1) = 0.0
   Ffeo(5,1) = 0.0
   Ffeo(6,1) = 0.0
   Ffeo(7,1) = -2.0*Yi(1,1)
   Ffeo(8,1) = -6.0*Yi(1,2)
   Ffeo(9,1) = -12.0*Yi(1,3)
   Ffeo(10,1) = -20.0*Yi(1,4)
!
   Ffeo(1,2) = 0.0
   Ffeo(2,2) = 0.0
   Ffeo(3,2) = 0.0
   Ffeo(4,2) = 0.0
   Ffeo(5,2) = 0.0
   Ffeo(6,2) = -Yi(4,1)
   Ffeo(7,2) = -2.0*Yi(4,2)
   Ffeo(8,2) = -3.0*Yi(4,3)
   Ffeo(9,2) = -4.0*Yi(4,4)
   Ffeo(10,2) = -5.0*Yi(4,5)
!
   Ffe1(1,1) = 0.0
   Ffe1(2,1) = 0.0
   Ffe1(3,1) = 0.0
   Ffe1(4,1) = 0.0
   Ffe1(5,1) = 0.0
   Ffe1(6,1) = 0.0
   Ffe1(7,1) = -s1*2.0*Yi(1,2)
   Ffe1(8,1) = -s1*6.0*Yi(1,3)
   Ffe1(9,1) = -s1*12.0*Yi(1,4)
   Ffe1(10,1) = -s1*20.0*Yi(1,5)
!
   Ffe1(1,2) = 0.0
   Ffe1(2,2) = 0.0
   Ffe1(3,2) = 0.0
   Ffe1(4,2) = 0.0
   Ffe1(5,2) = 0.0
   Ffe1(6,2) = -s1*Yi(4,2)
   Ffe1(7,2) = -s1*2.0*Yi(4,3)
   Ffe1(8,2) = -s1*3.0*Yi(4,4)
   Ffe1(9,2) = -s1*4.0*Yi(4,5)
   Ffe1(10,2) = -s1*5.0*Yi(4,6)
END SUBROUTINE fcurl
