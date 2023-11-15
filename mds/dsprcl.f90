
SUBROUTINE dsprcl(Block)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! Dummy argument declarations
!
   INTEGER Block(15)
!
! Local variable declarations
!
   INTEGER idiv(4)
!
! End of declarations
!
   DATA idiv/1 , 2 , 1 , 2/
   Block(2) = iand(Ibase(Indcbp),Maskq4)
   Block(3) = iand(Ibase(Indcbp),Maskq3)
   Block(3) = Block(3)/Mulq3
   Block(11) = Nwrdel(Block(2))
   Block(12) = Ibase(Indcbp+1)
   Block(14) = idiv(Block(2))
END SUBROUTINE dsprcl
