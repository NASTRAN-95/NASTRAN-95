!*==dsipk1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsipk1(Block,Itypot)
   USE I_DSIOF
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER Itypot
   INTEGER Block(15)
   INTEGER iflag
   iretrn = 0
   Block(1) = name
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
   RETURN
 100  iretrn = 1
END SUBROUTINE dsipk1
