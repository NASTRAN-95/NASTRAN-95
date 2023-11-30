
SUBROUTINE rbmg3
   IMPLICIT NONE
   INTEGER dm , klr , krr , lll , scr1 , scr2
   REAL eps
!*****
!     SOLVE KLL * DM = -KLR  FOR DM (WHERE LLL IS THE TRI FACTOR)
!     THEN COMPUTE X = KRR + KLR(T) * DM
!     AND ESP = NORM(X) / NORM(KRR)
!*****
!
   DATA lll , klr , krr/101 , 102 , 103/ , dm/201/ , scr1 , scr2/301 , 302/
!*****
   CALL solver(lll,dm,klr,krr,scr1,eps,1,scr2)
   CALL mesage(35,0,eps)
!
END SUBROUTINE rbmg3
