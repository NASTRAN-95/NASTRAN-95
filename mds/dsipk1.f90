
SUBROUTINE dsipk1(Block,Itypot)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
!
! Dummy argument declarations
!
   INTEGER Itypot
   INTEGER Block(15)
!
! Local variable declarations
!
   INTEGER iflag
!
! End of declarations
!
   Iretrn = 0
   Block(1) = Name
   Block(8) = -1
   IF ( Itypot>0 ) THEN
      iflag = Itypot
   ELSE
      iflag = iabs(Itypot) + 64
   ENDIF
   Block(13) = iflag
   CALL getstr(*100,Block)
   Block(7) = 0
   IF ( iflag<1 .OR. iflag>4 ) THEN
      IF ( iflag<65 .OR. iflag>68 ) THEN
         CALL dsmsg1(Block)
         CALL dsmsg(118)
      ENDIF
   ENDIF
   GOTO 99999
 100  Iretrn = 1
99999 RETURN
END SUBROUTINE dsipk1
