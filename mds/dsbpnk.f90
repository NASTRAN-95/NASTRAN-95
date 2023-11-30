
SUBROUTINE dsbpnk(Block,Mcb)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER Block(15) , Mcb(7)
   INTEGER num
   IF ( Block(1)/=Name ) THEN
      CALL dsmsg1(Block)
      CALL dsmsg(120)
   ENDIF
   IF ( Mcb(2)==0 ) Mcb(7) = Mcbmas
   Mcb(2) = Mcb(2) + 1
   num = Block(10)
   IF ( Mcb(6)<=num ) Mcb(6) = num
   Mcb(7) = Mcb(7) + num
   Block(8) = 1
   CALL endput(Block)
END SUBROUTINE dsbpnk
