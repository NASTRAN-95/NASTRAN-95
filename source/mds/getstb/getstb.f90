!*==getstb.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE getstb(Block) !HIDESTARS (*,Block)
   USE i_dsiof
   USE i_xnstrn
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Block(15)
   INTEGER id , idiv
   iretrn = 0
   name = Block(1)
   CALL dsgefl
   IF ( Block(8)==-1 ) THEN
      IF ( (indclr-indbas)<=5 ) CALL dsrdpb
      indcbp = indcbp - 1
      id = iand(ibase(indcbp),maskq1)
      IF ( id/=idsrt ) CALL dsmsg(114)
      indcbp = indcbp - 2
      id = iand(ibase(indcbp),maskq1)
      IF ( id/=idsct ) CALL dsmsg(115)
      CALL dsprcl(Block)
      Block(8) = 0
   ENDIF
   SPAG_Loop_1_1: DO
      indcbp = indcbp - 2
      IF ( (indcbp-indbas)>5 ) THEN
         id = iand(ibase(indcbp),maskq1)
         IF ( id==idsch ) THEN
            indcbp = indcbp - 1
            indclr = indcbp
            Block(8) = 1
            iretrn = 1
         ELSE
            IF ( id/=idsst ) THEN
               IF ( id==idssh ) CYCLE
               IF ( id==idsrt ) CYCLE
               IF ( id==idssd ) CYCLE
               IF ( id==idsse ) CYCLE
!WKBNB 1/94
               id = iand(ibase(indcbp+1),maskq1)
               IF ( id/=idssd ) THEN
!WKBNE 1/94
!WKBR 1/94  CALL DSMSG ( 116 )
                  CALL dsmsg(116)
               ELSE
                  indcbp = indcbp + 1
                  CYCLE
               ENDIF
            ENDIF
            Block(4) = ibase(indcbp+1)
            Block(6) = iand(ibase(indcbp),maskh2)
            idiv = min0(2,Block(11))
            Block(5) = indcbp - 1
            IF ( Block(2)==2 ) Block(5) = (indcbp-1)/idiv
            IF ( Block(2)==3 ) Block(5) = indcbp - 2
            IF ( Block(2)==4 ) Block(5) = (indcbp-3)/idiv
         ENDIF
         CALL dssdcb
         IF ( iretrn==1 ) RETURN 1
         EXIT SPAG_Loop_1_1
      ELSE
         CALL dsrdpb
         indcbp = indcbp + 1
      ENDIF
   ENDDO SPAG_Loop_1_1
END SUBROUTINE getstb
