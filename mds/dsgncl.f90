
SUBROUTINE dsgncl
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'GINOX.COM'
!
! End of declarations
!
   Idsn = Mdsfcb(2,Ifilex)
   CALL dsclos(Idsn)
   Mdsfcb(1,Idsn) = iand(Mdsfcb(1,Idsn),Maskh1)
END SUBROUTINE dsgncl
