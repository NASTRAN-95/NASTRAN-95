!*==rbmg1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rbmg1
   IMPLICIT NONE
   USE C_BITPOS
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: kaa , kll , klr , krr , maa , mll , mlr , mrr , scr1 , uset
   EXTERNAL mpart , upart
!
! End of declarations rewritten by SPAG
!
!*****
! RBMG1 PARTITIONS KAA INTO KLL, KLR AND KRR AND MAA SIMILARLY.
!*****
!
!*****
!     INPUT DATA FILES
!*****
   DATA uset , kaa , maa/101 , 102 , 103/
!*****
!     OUTPUT DATA FILES
!*****
   DATA kll , klr , krr , mll , mlr , mrr/201 , 202 , 203 , 204 , 205 , 206/
!*****
!     SCRATCH DATA FILES
!*****
   DATA scr1/301/
!*****
!     PARTITION  KAA INTO KLL,KLR, AND KRR
!     PARTITION  MAA INTO MLL,MLR, AND MRR
!*****
   CALL upart(uset,scr1,Ua,Ul,Ur)
   CALL mpart(kaa,kll,0,klr,krr)
   CALL mpart(maa,mll,0,mlr,mrr)
END SUBROUTINE rbmg1
