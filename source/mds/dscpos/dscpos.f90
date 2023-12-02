!*==dscpos.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dscpos(File,Icblk,Iclr,Icbp)
   USE i_dsiof
   USE I_DSIOF
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER File , Icblk , Icbp , Iclr
!
! RETURNS THE CURRENT BLOCK NUMBER "ICBLK", CURRENT LOGICAL RECORD
! POINTER "ICLR" AND CURRENT BUFFER POINT "ICBP" FOR "FILE"
!
   name = File
   CALL dsgefl
   Icblk = fcb(4,ifilex)
   Iclr = indclr - indbas + 1
   Icbp = indcbp - indbas + 1
END SUBROUTINE dscpos
