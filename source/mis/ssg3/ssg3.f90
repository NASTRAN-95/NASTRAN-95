!*==ssg3.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssg3
   USE c_blank
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: kll , koob , lll , loo , pl , po , rulv , ruov , sr1 , sr2 , ulv , uov
   EXTERNAL ssg3a
!
! End of declarations rewritten by SPAG
!
!
!     DMAP FOR STATIC SOLUTION GENERATOR 3
!
!     SSG3   LLL,KLL,PL,LOO,KOOB,PO /ULV,UOV,RULV,RUOV/ V,N,OMIT/
!            V,Y,IRES/V,N,SKIP/V,N,EPSI $
!
   DATA lll , kll , pl , loo , koob , po , ulv , uov , rulv , ruov/101 , 102 , 103 , 104 , 105 , 106 , 201 , 202 , 203 , 204/
   DATA sr1 , sr2/301 , 302/
!
   CALL ssg3a(kll,lll,pl,ulv,sr1,sr2,0,rulv)
   IF ( omit>=0 ) CALL ssg3a(koob,loo,po,uov,sr1,sr2,0,ruov)
END SUBROUTINE ssg3
