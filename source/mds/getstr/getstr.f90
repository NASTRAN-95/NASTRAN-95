!*==getstr.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE getstr(Block) !HIDESTARS (*,Block)
   USE i_dsiof
   USE i_xnstrn
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Block(15)
   INTEGER id
   iretrn = 0
   name = Block(1)
   CALL dsgefl
   IF ( Block(8)==-1 ) THEN
      SPAG_Loop_1_1: DO
         IF ( (indclr-indbas+1)>lcw ) CALL dsmsg(113)
         id = iand(ibase(indclr),maskq1)
         IF ( id==idssb ) THEN
            id = iand(ibase(indcbp+1),maskq1)
            IF ( id/=idsch ) THEN
               CALL dsmsg1(Block)
               CALL dsmsg(111)
            ENDIF
            indcbp = indcbp + 1
            CALL dsprcl(Block)
            indcbp = indcbp + 2
            Block(8) = 0
            EXIT SPAG_Loop_1_1
         ELSE
            IF ( id/=idseb ) CALL dsmsg(110)
            CALL dsrdnb
         ENDIF
      ENDDO SPAG_Loop_1_1
   ENDIF
   SPAG_Loop_1_2: DO
      id = iand(ibase(indcbp),maskq1)
      indcbp = indcbp + 1
      IF ( id==idssh ) THEN
         Block(4) = ibase(indcbp)
         Block(6) = iand(ibase(indcbp-1),maskh2)
         indcbp = indcbp + 1
         Block(5) = (indcbp-1)/Block(14) + 1
         EXIT SPAG_Loop_1_2
      ELSEIF ( id/=idssd ) THEN
         IF ( id==idsct ) THEN
            CALL dsskrc
            Block(6) = 0
            Block(8) = 1
            iretrn = 1
            EXIT SPAG_Loop_1_2
         ELSE
            IF ( id/=idsse ) THEN
               IF ( id/=idsrt ) CALL dsmsg(112)
            ENDIF
            CALL dsrdnb
            indcbp = indcbp + 1
         ENDIF
      ENDIF
   ENDDO SPAG_Loop_1_2
   CALL dssdcb
   IF ( iretrn==1 ) RETURN 1
END SUBROUTINE getstr
