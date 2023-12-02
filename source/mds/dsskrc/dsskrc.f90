!*==dsskrc.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsskrc
   USE i_dsiof
   USE i_xnstrn
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER iclr , id , iflg , len
 100  SPAG_Loop_1_1: DO
      id = iand(ibase(indclr),maskq1)
      IF ( id==idsrh ) EXIT SPAG_Loop_1_1
      IF ( id==idssb ) EXIT SPAG_Loop_1_1
      IF ( id==idsef ) THEN
         indclr = indclr + 1
         indcbp = indclr
         RETURN
      ELSE
         IF ( id/=idseb ) CALL dsmsg(103)
         CALL dsrdnb
      ENDIF
   ENDDO SPAG_Loop_1_1
   len = iand(ibase(indclr),maskh2)
   iclr = indclr + len + 1
   id = iand(ibase(iclr),maskq1)
   IF ( id/=idsrt ) CALL dsmsg(104)
   iflg = iand(ibase(iclr),maskq2)
   IF ( iflg==idsc ) THEN
      indclr = iclr + 1
      indcbp = indclr
   ELSE
      CALL dsrdnb
      GOTO 100
   ENDIF
END SUBROUTINE dsskrc
