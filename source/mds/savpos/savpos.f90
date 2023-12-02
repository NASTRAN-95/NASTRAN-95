!*==savpos.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE savpos(File,Ipos)
   IMPLICIT NONE
   USE I_DSIOF
   USE C_DDIOSV
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   INTEGER :: Ipos
!
! End of declarations rewritten by SPAG
!
   name = File
   CALL dsgefl
   Ipos = Iflpos(1,ifilex)*mulq2 + Iflpos(2,ifilex)
   IF ( iprvop==0 ) Ipos = fcb(3,ifilex)*mulq2 + fcb(4,ifilex)
END SUBROUTINE savpos
