
SUBROUTINE dsspos(File,Kcblk,Kclr,Kcbp)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! Dummy argument declarations
!
   INTEGER File , Kcblk , Kcbp , Kclr
!
! Local variable declarations
!
   INTEGER icblk
!
! End of declarations
!
!
! DSSPOS REPOSITIONS THE "FILE" TO BLOCK "KCBLK" WITH THE CURRENT
! LOGICAL RECORD POINTER SET TO "KCLR" AND THE CURRENT BUFFER
! POINTER SET TO "KCBP"
!
   Name = File
   CALL dsgefl
   icblk = Fcb(4,Ifilex)
   IF ( icblk/=Kcblk ) THEN
      Nblock = Kcblk
      CALL dbmmgr(6)
   ENDIF
   Indclr = Kclr + Indbas - 1
   Indcbp = Kcbp + Indbas - 1
   CALL dssdcb
END SUBROUTINE dsspos
