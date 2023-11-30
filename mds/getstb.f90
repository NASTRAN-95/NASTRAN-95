
SUBROUTINE getstb(*,Block)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Block(15)
   INTEGER id , idiv
   Iretrn = 0
   Name = Block(1)
   CALL dsgefl
   IF ( Block(8)==-1 ) THEN
      IF ( (Indclr-Indbas)<=5 ) CALL dsrdpb
      Indcbp = Indcbp - 1
      id = iand(Ibase(Indcbp),Maskq1)
      IF ( id/=Idsrt ) CALL dsmsg(114)
      Indcbp = Indcbp - 2
      id = iand(Ibase(Indcbp),Maskq1)
      IF ( id/=Idsct ) CALL dsmsg(115)
      CALL dsprcl(Block)
      Block(8) = 0
   ENDIF
   DO
      Indcbp = Indcbp - 2
      IF ( (Indcbp-Indbas)>5 ) THEN
         id = iand(Ibase(Indcbp),Maskq1)
         IF ( id==Idsch ) THEN
            Indcbp = Indcbp - 1
            Indclr = Indcbp
            Block(8) = 1
            Iretrn = 1
         ELSE
            IF ( id/=Idsst ) THEN
               IF ( id==Idssh ) CYCLE
               IF ( id==Idsrt ) CYCLE
               IF ( id==Idssd ) CYCLE
               IF ( id==Idsse ) CYCLE
!WKBNB 1/94
               id = iand(Ibase(Indcbp+1),Maskq1)
               IF ( id/=Idssd ) THEN
!WKBNE 1/94
!WKBR 1/94  CALL DSMSG ( 116 )
                  CALL dsmsg(116)
               ELSE
                  Indcbp = Indcbp + 1
                  CYCLE
               ENDIF
            ENDIF
            Block(4) = Ibase(Indcbp+1)
            Block(6) = iand(Ibase(Indcbp),Maskh2)
            idiv = min0(2,Block(11))
            Block(5) = Indcbp - 1
            IF ( Block(2)==2 ) Block(5) = (Indcbp-1)/idiv
            IF ( Block(2)==3 ) Block(5) = Indcbp - 2
            IF ( Block(2)==4 ) Block(5) = (Indcbp-3)/idiv
         ENDIF
         CALL dssdcb
         IF ( Iretrn==1 ) RETURN 1
         EXIT
      ELSE
         CALL dsrdpb
         Indcbp = Indcbp + 1
      ENDIF
   ENDDO
END SUBROUTINE getstb