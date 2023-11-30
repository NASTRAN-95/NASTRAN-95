
SUBROUTINE rbmg1
   IMPLICIT NONE
   INTEGER Ua , Ul , Ur
   REAL Ue , Uf , Ug , Um , Un , Uo , Up , Us , Usb , Usg
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug , Ue , Up
   INTEGER kaa , kll , klr , krr , maa , mll , mlr , mrr , scr1 , uset
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
