!*==dsbpnk.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsbpnk(Block,Mcb)
   USE I_DSIOF
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER Block(15) , Mcb(7)
   INTEGER num
   IF ( Block(1)/=name ) THEN
      CALL dsmsg1(Block)
      CALL dsmsg(120)
   ENDIF
   IF ( Mcb(2)==0 ) Mcb(7) = mcbmas
   Mcb(2) = Mcb(2) + 1
   num = Block(10)
   IF ( Mcb(6)<=num ) Mcb(6) = num
   Mcb(7) = Mcb(7) + num
   Block(8) = 1
   CALL endput(Block)
END SUBROUTINE dsbpnk
