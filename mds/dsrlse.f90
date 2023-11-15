
SUBROUTINE dsrlse
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'GINOX.COM'
!
! Local variable declarations
!
   INTEGER inext , nexdsn
!
! End of declarations
!
   inext = Ifilex
   DO
      nexdsn = iand(Mdsfcb(3,inext),Maskh2)
      IF ( nexdsn==0 ) EXIT
      Mdsfcb(1,inext) = iand(Mdsfcb(1,inext),Maskh1)
      Mdsfcb(2,inext) = 0
      Mdsfcb(3,inext) = 0
!
! OPEN AND CLOSE FILE TO DELETE SPACE ALLOCATION
!
      CALL dsopen(Mdsnam(nexdsn),nexdsn,1)
      CALL dsclos(nexdsn)
      inext = nexdsn
   ENDDO
END SUBROUTINE dsrlse
