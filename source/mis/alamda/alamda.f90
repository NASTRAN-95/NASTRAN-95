!*==alamda.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alamda(Arg,Y,Blamda)
   USE c_blk1
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Arg
   REAL :: Y
   COMPLEX :: Blamda
!
! Local variable declarations rewritten by SPAG
!
   REAL :: arg1 , c2 , gam , s1 , scrk1
   COMPLEX :: c1
!
! End of declarations rewritten by SPAG
!
!
!     SUBROUTINE FOR COMPUTING LAMDA
!
!
!
   scrk1 = abs(scrk)
   arg1 = abs(Arg)
   s1 = (Arg-del)*sps + sigma
   IF ( scrk1>arg1 ) THEN
      gam = sqrt(scrk**2-Arg**2)
      c1 = cosh(gam*(sns-Y)) - cexp(ai*s1)*cosh(gam*Y)
      c2 = cosh(sns*gam) - cos(s1)
      Blamda = c1/c2
      RETURN
   ENDIF
   gam = sqrt(Arg**2-scrk**2)
   c1 = cos(gam*(sns-Y)) - cexp(ai*s1)*cos(gam*Y)
   c2 = cos(sns*gam) - cos(s1)
   Blamda = c1/c2
END SUBROUTINE alamda
