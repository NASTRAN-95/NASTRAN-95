!*==dsspos.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsspos(File,Kcblk,Kclr,Kcbp)
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File , Kcblk , Kcbp , Kclr
   INTEGER icblk
!
! DSSPOS REPOSITIONS THE "FILE" TO BLOCK "KCBLK" WITH THE CURRENT
! LOGICAL RECORD POINTER SET TO "KCLR" AND THE CURRENT BUFFER
! POINTER SET TO "KCBP"
!
   name = File
   CALL dsgefl
   icblk = fcb(4,ifilex)
   IF ( icblk/=Kcblk ) THEN
      nblock = Kcblk
      CALL dbmmgr(6)
   ENDIF
   indclr = Kclr + indbas - 1
   indcbp = Kcbp + indbas - 1
   CALL dssdcb
END SUBROUTINE dsspos
