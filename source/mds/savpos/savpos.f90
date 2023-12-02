!*==savpos.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE savpos(File,Ipos)
   USE i_dsiof
   USE c_ddiosv
   IMPLICIT NONE
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
   Ipos = iflpos(1,ifilex)*mulq2 + iflpos(2,ifilex)
   IF ( iprvop==0 ) Ipos = fcb(3,ifilex)*mulq2 + fcb(4,ifilex)
END SUBROUTINE savpos
