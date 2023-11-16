
SUBROUTINE alamda(Arg,Y,Blamda)
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
   REAL Arg , Y
   COMPLEX Blamda
!
! Local variable declarations
!
   REAL arg1 , c2 , gam , s1 , scrk1
   COMPLEX c1
!
! End of declarations
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
      GOTO 99999
   ENDIF
   gam = sqrt(Arg**2-Scrk**2)
   c1 = cos(gam*(Sns-Y)) - cexp(Ai*s1)*cos(gam*Y)
   c2 = cos(Sns*gam) - cos(s1)
   Blamda = c1/c2
   RETURN
99999 RETURN
END SUBROUTINE alamda
