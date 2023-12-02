!*==rbmg4.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rbmg4
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: dm , mll , mlr , mr , mrr , scr1 , scr2 , scr3
   EXTERNAL elim
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
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
