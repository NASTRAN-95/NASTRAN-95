
SUBROUTINE dsrdpb
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! Local variable declarations
!
   INTEGER iblk
!
! End of declarations
!
   Nblock = Nblock - 1
   CALL dbmmgr(6)
   Indclr = Ibase(Indbas+4) + Indbas - 1
   Indcbp = Indclr
   iblk = Ibase(Indbas+3)
   IF ( iblk/=Nblock ) CALL dsmsg(102)
END SUBROUTINE dsrdpb
