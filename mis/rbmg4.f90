
SUBROUTINE rbmg4
   IMPLICIT NONE
!
! Local variable declarations
!
   INTEGER dm , mll , mlr , mr , mrr , scr1 , scr2 , scr3
!
! End of declarations
!
!*****
! RBMG4 COMPUTES MR FROM THE MATRIX EQUATION
!      MR = MRR + DM(T) * MLR + MLR(T) * DM + DM(T) * MLL * DM
!*****
!*****
!     INPUT DATA FILES
!*****
   DATA dm , mll , mlr , mrr/101 , 102 , 103 , 104/
!*****
!     OUTPUT DATA FILES
!*****
   DATA mr/201/
!*****
!     SCRATCH DATA FILES
!*****
   DATA scr1 , scr2 , scr3/301 , 302 , 303/
!*****
!     COMPUTE MR
!*****
   CALL elim(mrr,mlr,mll,dm,mr,scr1,scr2,scr3)
END SUBROUTINE rbmg4
