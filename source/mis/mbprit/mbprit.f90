!*==mbprit.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbprit(Aw,Ac,At)
   USE c_mboxa
   USE c_mboxc
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Aw
   REAL :: Ac
   REAL :: At
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
!
! End of declarations rewritten by SPAG
!
!
!     SUBROUTINE TO PRINT GEOMETRY DATA
!
!
   WRITE (n6,99001) cntrl2 , cntrl1 , crank1 , crank2 , asym
99001 FORMAT (1H1,35X,27HSUPERSONIC MACH BOX PROGRAM/1H0,43X,12HCONTROL DATA/L20,9X,6HCNTRL2/L20,9X,6HCNTRL1/L20,9X,                &
             &21HCRANK  (LEADING EDGE)/L20,9X,22HCRANK  (TRAILING EDGE)/L20,9X,14HANTI-SYMMETRIC/L20)
!
   WRITE (n6,99002) (i,x(i),y(i),tang(i),ang(i),i=1,7)
99002 FORMAT (1H-,42X,13HGEOMETRY DATA/1H0,8X,1HN,11X,1HX,17X,1HY,16X,4HTANG,14X,3HANG/(I10,4E18.6))
!
   WRITE (n6,99003) (i,x(i),y(i),tang(i),i=8,10) , (i,x(i),y(i),i=11,12)
99003 FORMAT (I10,3E18.6/I10,3E18.6/I10,3E18.6/(I10,2E18.6))
!
   WRITE (n6,99004) Aw , Ac , At
99004 FORMAT (1H0,5X,23HAREA OF MAIN (SEMISPAN),11X,15HAREA OF CNTRL1 ,18X,14HAREA OF CNTRL2/E22.6,E34.6,E29.6)
END SUBROUTINE mbprit
