
SUBROUTINE getstr(*,Block)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! Dummy argument declarations
!
   INTEGER Block(15)
!
! Local variable declarations
!
   INTEGER id
!
! End of declarations
!
   Iretrn = 0
   Name = Block(1)
   CALL dsgefl
   IF ( Block(8)==-1 ) THEN
      DO
         IF ( (Indclr-Indbas+1)>Lcw ) CALL dsmsg(113)
         id = iand(Ibase(Indclr),Maskq1)
         IF ( id==Idssb ) THEN
            id = iand(Ibase(Indcbp+1),Maskq1)
            IF ( id/=Idsch ) THEN
               CALL dsmsg1(Block)
               CALL dsmsg(111)
            ENDIF
            Indcbp = Indcbp + 1
            CALL dsprcl(Block)
            Indcbp = Indcbp + 2
            Block(8) = 0
            EXIT
         ELSE
            IF ( id/=Idseb ) CALL dsmsg(110)
            CALL dsrdnb
         ENDIF
      ENDDO
   ENDIF
   DO
      id = iand(Ibase(Indcbp),Maskq1)
      Indcbp = Indcbp + 1
      IF ( id==Idssh ) THEN
         Block(4) = Ibase(Indcbp)
         Block(6) = iand(Ibase(Indcbp-1),Maskh2)
         Indcbp = Indcbp + 1
         Block(5) = (Indcbp-1)/Block(14) + 1
         EXIT
      ELSEIF ( id/=Idssd ) THEN
         IF ( id==Idsct ) THEN
            CALL dsskrc
            Block(6) = 0
            Block(8) = 1
            Iretrn = 1
            EXIT
         ELSE
            IF ( id/=Idsse ) THEN
               IF ( id/=Idsrt ) CALL dsmsg(112)
            ENDIF
            CALL dsrdnb
            Indcbp = Indcbp + 1
         ENDIF
      ENDIF
   ENDDO
   CALL dssdcb
   IF ( Iretrn==1 ) RETURN 1
END SUBROUTINE getstr
