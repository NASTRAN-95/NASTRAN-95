!*==mbprit.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbprit(Aw,Ac,At)
   IMPLICIT NONE
   USE C_MBOXA
   USE C_MBOXC
   USE C_SYSTEM
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
   WRITE (N6,99001) Cntrl2 , Cntrl1 , Crank1 , Crank2 , Asym
99001 FORMAT (1H1,35X,27HSUPERSONIC MACH BOX PROGRAM/1H0,43X,12HCONTROL DATA/L20,9X,6HCNTRL2/L20,9X,6HCNTRL1/L20,9X,                &
             &21HCRANK  (LEADING EDGE)/L20,9X,22HCRANK  (TRAILING EDGE)/L20,9X,14HANTI-SYMMETRIC/L20)
!
   WRITE (N6,99002) (i,X(i),Y(i),Tang(i),Ang(i),i=1,7)
99002 FORMAT (1H-,42X,13HGEOMETRY DATA/1H0,8X,1HN,11X,1HX,17X,1HY,16X,4HTANG,14X,3HANG/(I10,4E18.6))
!
   WRITE (N6,99003) (i,X(i),Y(i),Tang(i),i=8,10) , (i,X(i),Y(i),i=11,12)
99003 FORMAT (I10,3E18.6/I10,3E18.6/I10,3E18.6/(I10,2E18.6))
!
   WRITE (N6,99004) Aw , Ac , At
99004 FORMAT (1H0,5X,23HAREA OF MAIN (SEMISPAN),11X,15HAREA OF CNTRL1 ,18X,14HAREA OF CNTRL2/E22.6,E34.6,E29.6)
END SUBROUTINE mbprit
