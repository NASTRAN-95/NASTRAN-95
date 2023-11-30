
SUBROUTINE rbmg2
   IMPLICIT NONE
   DOUBLE PRECISION Det(2)
   REAL Detrm , Qq(29)
   INTEGER Ipwr , Jpowr
   COMMON /blank / Jpowr , Detrm
   COMMON /sfact / Qq
   INTEGER kll , lll , scr1 , scr2 , scr3 , scr4
!
   EQUIVALENCE (Qq(25),Det(1)) , (Qq(29),Ipwr)
   DATA kll , lll , scr1 , scr2 , scr3 , scr4/101 , 201 , 301 , 302 , 303 , 304/
!
!     DECOMPOSE KLL INTO LLL
!
   CALL factor(kll,lll,scr1,scr2,scr3,scr4)
   Jpowr = Ipwr
   Detrm = Det(1)
END SUBROUTINE rbmg2
