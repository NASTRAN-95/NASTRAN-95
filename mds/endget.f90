
SUBROUTINE endget(Block)
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
   INTEGER nelm
!
! End of declarations
!
   Name = Block(1)
   CALL dsgefl
   Nwords = Block(11)
   nelm = iand(Ibase(Indcbp-2),Maskh2)
   Indcbp = Indcbp + nelm*Nwords + Block(3)*2
   CALL dssdcb
END SUBROUTINE endget
