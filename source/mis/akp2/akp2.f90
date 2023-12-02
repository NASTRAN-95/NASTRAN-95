!*==akp2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE akp2
   IMPLICIT NONE
   USE C_BLK1
!
! Local variable declarations rewritten by SPAG
!
   REAL :: c1 , c2 , d1 , d2 , dc1da , dc2da , dgda , gam , s1
!
! End of declarations rewritten by SPAG
!
!
!
!
   gam = sqrt(Del**2-Scrk**2)
   s1 = Sns*gam
   c1 = (Sigma-s1)/2.0
   c2 = (Sigma+s1)/2.0
   dgda = Del/gam
   d1 = Sps/2.0
   d2 = Sns/2.0*dgda
   dc1da = d1 - d2
   dc2da = d1 + d2
   Res = 1.0/gam*dgda + Sns*cos(s1)/sin(s1)*dgda - cos(c1)/sin(c1)*dc1da - cos(c2)/sin(c2)*dc2da
END SUBROUTINE akp2
