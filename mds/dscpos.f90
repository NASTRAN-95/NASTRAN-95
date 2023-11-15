
SUBROUTINE dscpos(File,Icblk,Iclr,Icbp)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
!
! Dummy argument declarations
!
   INTEGER File , Icblk , Icbp , Iclr
!
! End of declarations
!
!
! RETURNS THE CURRENT BLOCK NUMBER "ICBLK", CURRENT LOGICAL RECORD
! POINTER "ICLR" AND CURRENT BUFFER POINT "ICBP" FOR "FILE"
!
   Name = File
   CALL dsgefl
   Icblk = Fcb(4,Ifilex)
   Iclr = Indclr - Indbas + 1
   Icbp = Indcbp - Indbas + 1
END SUBROUTINE dscpos
