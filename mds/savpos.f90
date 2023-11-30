
SUBROUTINE savpos(File,Ipos)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER Iflpos(2,80)
   COMMON /ddiosv/ Iflpos
   INTEGER File , Ipos
   Name = File
   CALL dsgefl
   Ipos = Iflpos(1,Ifilex)*Mulq2 + Iflpos(2,Ifilex)
   IF ( Iprvop==0 ) Ipos = Fcb(3,Ifilex)*Mulq2 + Fcb(4,Ifilex)
END SUBROUTINE savpos