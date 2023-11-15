
SUBROUTINE dsgnop
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'GINOX.COM'
   INCLUDE 'XNSTRN.COM'
!
! Local variable declarations
!
   INTEGER inext , iop , itest
!
! End of declarations
!
   Idsn = Ifilex
   IF ( Iocode>=2 ) THEN
      DO
         inext = iand(Mdsfcb(3,Idsn),Maskh2)
         IF ( inext==0 ) EXIT
         itest = Fcb(6,Idsn)
         IF ( Nblock<=itest ) EXIT
         Idsn = inext
      ENDDO
   ELSEIF ( Iocode==1 ) THEN
      CALL dsrlse
   ENDIF
   iop = mod(Iocode,2)
   Mdsfcb(2,Ifilex) = Idsn
   Mdsfcb(1,Idsn) = ior(Mdsfcb(1,Idsn),Maskh2)
   CALL dsopen(Mdsnam(Idsn),Idsn,Iocode)
END SUBROUTINE dsgnop
