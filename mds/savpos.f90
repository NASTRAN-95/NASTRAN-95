
SUBROUTINE savpos(File,Ipos)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
!
! COMMON variable declarations
!
   INTEGER Iflpos(2,80)
   COMMON /ddiosv/ Iflpos
!
! Dummy argument declarations
!
   INTEGER File , Ipos
!
! End of declarations
!
   Name = File
   CALL dsgefl
   Ipos = Iflpos(1,Ifilex)*Mulq2 + Iflpos(2,Ifilex)
   IF ( Iprvop==0 ) Ipos = Fcb(3,Ifilex)*Mulq2 + Fcb(4,Ifilex)
END SUBROUTINE savpos
