!*==akappa.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE akappa(Arg,Bkappa)
   USE c_blk1
   IMPLICIT NONE
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
   scrk1 = abs(scrk)
   arg1 = abs(Arg)
   IF ( scrk1>arg1 ) THEN
      gam = sqrt(scrk**2-Arg**2)
      s1 = sns*gam
      c1 = -beta*gam*sinh(s1)
      c2 = cosh(s1) - cos((Arg-del)*sps+sigma)
      Bkappa = c1/c2
      RETURN
   ENDIF
   gam = sqrt(Arg**2-scrk**2)
   s1 = sns*gam
   c1 = beta*gam*sin(s1)
   c2 = cos(s1) - cos((Arg-del)*sps+sigma)
   Bkappa = c1/c2
END SUBROUTINE akappa
