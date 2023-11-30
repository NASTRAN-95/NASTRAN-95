
SUBROUTINE bldpk(Itypin,Itypot,File,Block,Iflag)
   IMPLICIT NONE
   INCLUDE 'PAKBLK.COM'
   INCLUDE 'DSIOF.COM'
   INTEGER File , Iflag , Itypin , Itypot
   INTEGER Block(15)
   Itrail = Iflag
   Itypi = Itypin
   Itypo = Itypot
   Name = File
   IF ( Itypi>=1 .AND. Itypi<=4 ) THEN
      IF ( Itypo>=1 .AND. Itypo<=4 ) THEN
         IF ( Iflag==0 ) THEN
            Itrail = 0
            CALL dsblpk(Iblka)
         ELSE
            CALL dsblpk(Block)
         ENDIF
         GOTO 99999
      ENDIF
   ENDIF
   IF ( Iflag==0 ) CALL dsmsg1(Iblka)
   IF ( Iflag/=0 ) CALL dsmsg1(Block)
   CALL dsmsg(118)
99999 RETURN
END SUBROUTINE bldpk
