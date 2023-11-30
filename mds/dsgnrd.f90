
SUBROUTINE dsgnrd
   IMPLICIT NONE
   INCLUDE 'XNSTRN.COM'
   INCLUDE 'DSIOF.COM'
   INCLUDE 'GINOX.COM'
   CHARACTER*4 Cbuff(2)
   INTEGER idsnr , iend , iop , isave , istrb
   EQUIVALENCE (Cbuff,Ibase)
   Idsn = Mdsfcb(2,Ifilex)
   idsnr = Idsn
 100  istrb = Fcb(5,idsnr)
   IF ( Nblock>=istrb ) THEN
      iend = Fcb(6,idsnr)
      IF ( Nblock<=iend ) GOTO 200
      idsnr = iand(Mdsfcb(3,idsnr),Maskh2)
   ELSE
      idsnr = Mdsfcb(3,idsnr)/Mulq2
   ENDIF
   IF ( idsnr>=1 .AND. idsnr<=Maxdsn ) GOTO 100
   CALL dsmsg(121)
 200  IF ( Idsn/=idsnr ) THEN
      CALL dsclos(Idsn)
      Mdsfcb(1,Idsn) = iand(Mdsfcb(1,Idsn),Maskh1)
      Idsn = idsnr
      Mdsfcb(1,Idsn) = ior(Mdsfcb(1,Idsn),Maskh2)
      Mdsfcb(2,Ifilex) = Idsn
      isave = iop
      iop = 0
      CALL dsopen(Mdsnam(Idsn),Idsn,iop)
      iop = isave
      Cbuff(Indbas) = Mdsnam(Idsn)
   ENDIF
   Ioblk = Nblock - istrb + 1
   CALL dsread(Idsn,Ibase(Indbas+3),Nbuff,Ioblk)
END SUBROUTINE dsgnrd
