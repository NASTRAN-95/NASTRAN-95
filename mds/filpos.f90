
SUBROUTINE filpos(File,Ipos)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! Dummy argument declarations
!
   INTEGER File , Ipos
!
! Local variable declarations
!
   INTEGER icblk
!
! End of declarations
!
   Name = File
   CALL dsgefl
   Nblock = iand(Ipos,Maskh2)
   icblk = Fcb(4,Ifilex)
   IF ( icblk/=Nblock ) CALL dbmmgr(6)
   Indclr = Ipos/Mulq2 + Indbas - 1
   Indcbp = Indclr
   CALL dssdcb
END SUBROUTINE filpos
