!*==gpwg.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gpwg
   IMPLICIT NONE
   USE C_BLANK
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bgpdt , cstm , eqexin , mgg , ogpwg , scr1 , scr2 , scr3 , scr4
   INTEGER :: ip , nogo
   EXTERNAL gpwg1a , gpwg1b , ssg2b , tranp1
!
! End of declarations rewritten by SPAG
!
!
!     GRID POINT WEIGHT GENERATOR
!
!     INPUTS  - BGPDT,CSTM,EQEXIN,MGG
!
!     OUTPUTS - OGPWG
!
!     PARAMETERS -- POINT,WTMASS
!
   DATA bgpdt , cstm , eqexin , mgg , ogpwg , scr1 , scr2 , scr3 , scr4/101 , 102 , 103 , 104 , 201 , 301 , 302 , 303 , 304/
!
!     FORM D MATRIX (TRANSPOSED)
!
   ip = Point
!
   CALL gpwg1a(Point,bgpdt,cstm,eqexin,scr3,nogo)
!
!     CHECK FOR AN ALL SCALAR PROBLEM AND A STUPID USER
!
   IF ( nogo/=0 ) THEN
!
!     COMPUTE MZERO = DT*MGG*D
!
      CALL tranp1(scr3,scr1,2,scr2,scr4,0,0,0,0,0,0)
      CALL ssg2b(mgg,scr1,0,scr2,0,1,1,scr3)
      CALL ssg2b(scr1,scr2,0,scr4,1,1,1,scr3)
!
!     M-ZERO IS ON SCR4
!
!     FORM OUTPUT  STUFF
!
      IF ( Point==0 ) ip = 0
      CALL gpwg1b(scr4,ogpwg,Wtmass,ip)
   ENDIF
END SUBROUTINE gpwg
