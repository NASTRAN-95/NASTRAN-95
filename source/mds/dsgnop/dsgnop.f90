!*==dsgnop.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsgnop
   USE i_dsiof
   USE i_ginox
   USE i_xnstrn
   USE I_DSIOF
   USE I_GINOX
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'GINOX.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER inext , iop , itest
   idsn = ifilex
   IF ( iocode>=2 ) THEN
      SPAG_Loop_1_1: DO
         inext = iand(mdsfcb(3,idsn),maskh2)
         IF ( inext==0 ) EXIT SPAG_Loop_1_1
         itest = fcb(6,idsn)
         IF ( nblock<=itest ) EXIT SPAG_Loop_1_1
         idsn = inext
      ENDDO SPAG_Loop_1_1
   ELSEIF ( iocode==1 ) THEN
      CALL dsrlse
   ENDIF
   iop = mod(iocode,2)
   mdsfcb(2,ifilex) = idsn
   mdsfcb(1,idsn) = ior(mdsfcb(1,idsn),maskh2)
   CALL dsopen(mdsnam(idsn),idsn,iocode)
END SUBROUTINE dsgnop
