!*==akp2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE akp2
   USE c_blk1
   IMPLICIT NONE
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
   gam = sqrt(del**2-scrk**2)
   s1 = sns*gam
   c1 = (sigma-s1)/2.0
   c2 = (sigma+s1)/2.0
   dgda = del/gam
   d1 = sps/2.0
   d2 = sns/2.0*dgda
   dc1da = d1 - d2
   dc2da = d1 + d2
   res = 1.0/gam*dgda + sns*cos(s1)/sin(s1)*dgda - cos(c1)/sin(c1)*dc1da - cos(c2)/sin(c2)*dc2da
END SUBROUTINE akp2
