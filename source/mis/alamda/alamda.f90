!*==alamda.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alamda(Arg,Y,Blamda)
   IMPLICIT NONE
   USE C_BLK1
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
   scrk1 = abs(Scrk)
   arg1 = abs(Arg)
   s1 = (Arg-Del)*Sps + Sigma
   IF ( scrk1>arg1 ) THEN
      gam = sqrt(Scrk**2-Arg**2)
      c1 = cosh(gam*(Sns-Y)) - cexp(Ai*s1)*cosh(gam*Y)
      c2 = cosh(Sns*gam) - cos(s1)
      Blamda = c1/c2
      RETURN
   ENDIF
   gam = sqrt(Arg**2-Scrk**2)
   c1 = cos(gam*(Sns-Y)) - cexp(Ai*s1)*cos(gam*Y)
   c2 = cos(Sns*gam) - cos(s1)
   Blamda = c1/c2
   RETURN
END SUBROUTINE alamda
