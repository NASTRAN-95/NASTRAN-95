!*==bldpk.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bldpk(Itypin,Itypot,File,Block,Iflag)
   USE i_pakblk
   USE i_dsiof
   USE I_PAKBLK
   USE I_DSIOF
   IMPLICIT NONE
   INCLUDE 'PAKBLK.COM'
   INCLUDE 'DSIOF.COM'
   INTEGER File , Iflag , Itypin , Itypot
   INTEGER Block(15)
   itrail = Iflag
   itypi = Itypin
   itypo = Itypot
   name = File
   IF ( itypi>=1 .AND. itypi<=4 ) THEN
      IF ( itypo>=1 .AND. itypo<=4 ) THEN
         IF ( Iflag==0 ) THEN
            itrail = 0
            CALL dsblpk(iblka)
         ELSE
            CALL dsblpk(Block)
         ENDIF
         RETURN
      ENDIF
   ENDIF
   IF ( Iflag==0 ) CALL dsmsg1(iblka)
   IF ( Iflag/=0 ) CALL dsmsg1(Block)
   CALL dsmsg(118)
END SUBROUTINE bldpk
