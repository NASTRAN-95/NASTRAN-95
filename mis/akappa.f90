
SUBROUTINE akappa(Arg,Bkappa)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   COMPLEX Ai
   REAL Beta , Del , Dstr , Pi , Res , Scrk , Sigma , Sns , Sps
   COMMON /blk1  / Scrk , Sps , Sns , Dstr , Ai , Pi , Del , Sigma , Beta , Res
!
! Dummy argument declarations
!
   REAL Arg , Bkappa
!
! Local variable declarations
!
   REAL arg1 , c1 , c2 , gam , s1 , scrk1
!
! End of declarations
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
      GOTO 99999
   ENDIF
   gam = sqrt(Arg**2-Scrk**2)
   s1 = Sns*gam
   c1 = Beta*gam*sin(s1)
   c2 = cos(s1) - cos((Arg-Del)*Sps+Sigma)
   Bkappa = c1/c2
   RETURN
99999 RETURN
END SUBROUTINE akappa
