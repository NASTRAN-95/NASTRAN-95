!*==dsgnrd.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsgnrd
   USE i_xnstrn
   USE i_dsiof
   USE i_ginox
   USE I_XNSTRN
   USE I_DSIOF
   USE I_GINOX
   IMPLICIT NONE
   INCLUDE 'XNSTRN.COM'
   INCLUDE 'DSIOF.COM'
   INCLUDE 'GINOX.COM'
   CHARACTER*4 cbuff(2)
   INTEGER idsnr , iend , iop , isave , istrb
   !>>>>EQUIVALENCE (Cbuff,Ibase)
   idsn = mdsfcb(2,ifilex)
   idsnr = idsn
   SPAG_Loop_1_1: DO
      istrb = fcb(5,idsnr)
      IF ( nblock>=istrb ) THEN
         iend = fcb(6,idsnr)
         IF ( nblock<=iend ) EXIT SPAG_Loop_1_1
         idsnr = iand(mdsfcb(3,idsnr),maskh2)
      ELSE
         idsnr = mdsfcb(3,idsnr)/mulq2
      ENDIF
      IF ( idsnr<1 .OR. idsnr>maxdsn ) THEN
         CALL dsmsg(121)
         EXIT SPAG_Loop_1_1
      ENDIF
   ENDDO SPAG_Loop_1_1
   IF ( idsn/=idsnr ) THEN
      CALL dsclos(idsn)
      mdsfcb(1,idsn) = iand(mdsfcb(1,idsn),maskh1)
      idsn = idsnr
      mdsfcb(1,idsn) = ior(mdsfcb(1,idsn),maskh2)
      mdsfcb(2,ifilex) = idsn
      isave = iop
      iop = 0
      CALL dsopen(mdsnam(idsn),idsn,iop)
      iop = isave
      cbuff(indbas) = mdsnam(idsn)
   ENDIF
   ioblk = nblock - istrb + 1
   CALL dsread(idsn,ibase(indbas+3),nbuff,ioblk)
END SUBROUTINE dsgnrd
