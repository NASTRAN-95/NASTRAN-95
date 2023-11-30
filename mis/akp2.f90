
SUBROUTINE akp2
   IMPLICIT NONE
   COMPLEX Ai
   REAL Beta , Del , Dstr , Pi , Res , Scrk , Sigma , Sns , Sps
   COMMON /blk1  / Scrk , Sps , Sns , Dstr , Ai , Pi , Del , Sigma , Beta , Res
   REAL c1 , c2 , d1 , d2 , dc1da , dc2da , dgda , gam , s1
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