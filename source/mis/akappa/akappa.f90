!*==akappa.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE akappa(Arg,Bkappa)
   IMPLICIT NONE
   USE C_BLK1
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Arg
   REAL :: Bkappa
!
! Local variable declarations rewritten by SPAG
!
   REAL :: arg1 , c1 , c2 , gam , s1 , scrk1
!
! End of declarations rewritten by SPAG
!
!
!     SUBROUTINE FOR COMPUTING KAPPA
!
!
!
   scrk1 = abs(Scrk)
   arg1 = abs(Arg)
   IF ( scrk1>arg1 ) THEN
      gam = sqrt(Scrk**2-Arg**2)
      s1 = Sns*gam
      c1 = -Beta*gam*sinh(s1)
      c2 = cosh(s1) - cos((Arg-Del)*Sps+Sigma)
      Bkappa = c1/c2
      RETURN
   ENDIF
   gam = sqrt(Arg**2-Scrk**2)
   s1 = Sns*gam
   c1 = Beta*gam*sin(s1)
   c2 = cos(s1) - cos((Arg-Del)*Sps+Sigma)
   Bkappa = c1/c2
   RETURN
END SUBROUTINE akappa
