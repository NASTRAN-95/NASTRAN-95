!*==rbmg3.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rbmg3
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: dm , klr , krr , lll , scr1 , scr2
   REAL :: eps
   EXTERNAL mesage , solver
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
