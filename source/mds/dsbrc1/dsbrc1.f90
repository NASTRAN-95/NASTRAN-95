!*==dsbrc1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsbrc1
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER id , iflag
!        PRINT *,' DSBRC1-1,NBLOCK,INDCLR,INDBAS=',NBLOCK,INDCLR,INDBAS
   IF ( indclr/=indcbp ) THEN
      indcbp = indclr
   ELSEIF ( (indclr-indbas)/=5 ) THEN
      indcbp = indcbp - 1
   ELSEIF ( nblock/=1 ) THEN
      CALL dsrdpb
      indcbp = indcbp - 1
   ENDIF
   SPAG_Loop_1_1: DO
      IF ( nblock==1 ) THEN
         IF ( (indclr-indbas)==5 ) THEN
            indclr = indcbp
            EXIT SPAG_Loop_1_1
         ENDIF
      ENDIF
      id = iand(ibase(indcbp),maskq1)
      IF ( id==idsef ) THEN
         indclr = indcbp
      ELSE
         IF ( id==idsrt ) THEN
            indcbp = indbas + (iand(ibase(indcbp),maskh2)) - 1
            id = iand(ibase(indcbp),maskq1)
         ENDIF
         IF ( id/=idsrh ) THEN
            IF ( id/=idssb ) THEN
               IF ( id/=idseb ) CALL dsmsg(106)
               indcbp = indcbp - 1
               CYCLE
            ENDIF
         ENDIF
         iflag = iand(ibase(indcbp),maskq2)
         IF ( iflag==idsc ) THEN
            indclr = indcbp
         ELSEIF ( iflag==idsp ) THEN
            indclr = indcbp
         ELSEIF ( (indcbp-indbas)<=5 ) THEN
            IF ( nblock==1 ) THEN
               indclr = indcbp
            ELSE
               CALL dsrdpb
               indcbp = indcbp - 1
               CYCLE
            ENDIF
         ELSE
            indcbp = indcbp - 1
            CYCLE
         ENDIF
      ENDIF
      EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
!        PRINT *,' DSBRC1-2,NBLOCK,INDCLR,INDBAS=',NBLOCK,INDCLR,INDBAS
END SUBROUTINE dsbrc1
