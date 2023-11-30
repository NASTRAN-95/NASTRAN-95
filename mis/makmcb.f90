
SUBROUTINE makmcb(Mcb,Ifile,Irow,If,It)
   IMPLICIT NONE
   INTEGER If , Ifile , Irow , It
   INTEGER Mcb(7)
!
!
   Mcb(1) = Ifile
   Mcb(2) = 0
   Mcb(3) = Irow
   Mcb(4) = If
   Mcb(5) = It
   Mcb(6) = 0
   Mcb(7) = 0
!
!
END SUBROUTINE makmcb