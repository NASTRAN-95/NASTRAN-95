
SUBROUTINE dsbrc1
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER id , iflag
!        PRINT *,' DSBRC1-1,NBLOCK,INDCLR,INDBAS=',NBLOCK,INDCLR,INDBAS
   IF ( Indclr/=Indcbp ) THEN
      Indcbp = Indclr
   ELSEIF ( (Indclr-Indbas)/=5 ) THEN
      Indcbp = Indcbp - 1
   ELSEIF ( Nblock/=1 ) THEN
      CALL dsrdpb
      Indcbp = Indcbp - 1
   ENDIF
 100  IF ( Nblock==1 ) THEN
      IF ( (Indclr-Indbas)==5 ) THEN
         Indclr = Indcbp
         GOTO 99999
      ENDIF
   ENDIF
   id = iand(Ibase(Indcbp),Maskq1)
   IF ( id==Idsef ) THEN
      Indclr = Indcbp
   ELSE
      IF ( id==Idsrt ) THEN
         Indcbp = Indbas + (iand(Ibase(Indcbp),Maskh2)) - 1
         id = iand(Ibase(Indcbp),Maskq1)
      ENDIF
      IF ( id/=Idsrh ) THEN
         IF ( id/=Idssb ) THEN
            IF ( id/=Idseb ) CALL dsmsg(106)
            Indcbp = Indcbp - 1
            GOTO 100
         ENDIF
      ENDIF
      iflag = iand(Ibase(Indcbp),Maskq2)
      IF ( iflag==Idsc ) THEN
         Indclr = Indcbp
      ELSEIF ( iflag==Idsp ) THEN
         Indclr = Indcbp
      ELSEIF ( (Indcbp-Indbas)<=5 ) THEN
         IF ( Nblock==1 ) THEN
            Indclr = Indcbp
         ELSE
            CALL dsrdpb
            Indcbp = Indcbp - 1
            GOTO 100
         ENDIF
      ELSE
         Indcbp = Indcbp - 1
         GOTO 100
      ENDIF
   ENDIF
!        PRINT *,' DSBRC1-2,NBLOCK,INDCLR,INDBAS=',NBLOCK,INDCLR,INDBAS
99999 RETURN
END SUBROUTINE dsbrc1
