
SUBROUTINE gpwg
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Point
   REAL Wtmass
   COMMON /blank / Point , Wtmass
!
! Local variable declarations
!
   INTEGER bgpdt , cstm , eqexin , ip , mgg , nogo , ogpwg , scr1 , scr2 , scr3 , scr4
!
! End of declarations
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
