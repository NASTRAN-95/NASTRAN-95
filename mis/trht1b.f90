
SUBROUTINE trht1b(Iof,Delta)
   IMPLICIT NONE
   REAL Beta , Radlin , Tabs
   INTEGER Ib(7) , Icr1 , Icr2 , Icr3 , Icr4 , Icr5 , Icr6 , Icr7 , Ik(7) , Isym , Norad
   COMMON /blank / Beta , Tabs , Norad , Radlin
   COMMON /trhtx / Ik , Ib , Icr1 , Icr2 , Icr3 , Icr4 , Icr5 , Isym , Icr6 , Icr7
   REAL Delta
   INTEGER Iof
   DOUBLE PRECISION blk(2) , block(2) , qblock(6)
   INTEGER iblock(11) , iqblk(12) , mcb(7) , name(2)
!
!
!
!
!
   !>>>>EQUIVALENCE (iqblk(1),qblock(1))
   !>>>>EQUIVALENCE (iqblk(2),iblock(1))
   !>>>>EQUIVALENCE (qblock(2),block(1))
   !>>>>EQUIVALENCE (qblock(5),blk(1))
!
! ----------------------------------------------------------------------
!
   iblock(1) = 2
   block(1) = 1.0D0/Delta
   block(2) = 0.0D0
   iblock(7) = 2
   blk(1) = Beta
   blk(2) = 0.0D0
   CALL ssg2c(Ib,Ik,Icr6,1,iblock)
   mcb(1) = Icr6
   CALL rdtrl(mcb(1))
   IF ( mcb(4)==6 ) THEN
!
!     SYMMETRIC DECOMP
!
      CALL factor(Icr6,Icr1,Icr2,Icr3,Icr4,Icr7)
      Isym = 1
   ELSE
      CALL factru(*200,Icr6,Icr1,Icr2,Icr3,Icr4,Icr7)
      Isym = 0
   ENDIF
!
!     LLL  IS ON ICR1
!
!     FORM  A  MATRIX
!
   blk(1) = -(1.0D0-Beta)
   blk(2) = 0.0
   CALL ssg2c(Ib,Ik,Icr6,1,iblock)
 100  RETURN
 200  CALL mesage(-5,Icr6,name)
   GOTO 100
END SUBROUTINE trht1b