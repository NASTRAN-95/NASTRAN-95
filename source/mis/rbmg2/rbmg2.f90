!*==rbmg2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rbmg2
USE C_BLANK
USE C_SFACT
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: det
   INTEGER :: ipwr
   INTEGER , SAVE :: kll , lll , scr1 , scr2 , scr3 , scr4
   EXTERNAL factor
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Qq(25),Det(1)) , (Qq(29),Ipwr)
   DATA kll , lll , scr1 , scr2 , scr3 , scr4/101 , 201 , 301 , 302 , 303 , 304/
!
!     DECOMPOSE KLL INTO LLL
!
   CALL factor(kll,lll,scr1,scr2,scr3,scr4)
   Jpowr = ipwr
   Detrm = det(1)
END SUBROUTINE rbmg2
