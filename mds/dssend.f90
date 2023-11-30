
SUBROUTINE dssend(File)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File
   INTEGER icblk
!
! DSSEND (Dataset Set to End) will position a file to the end
! to allow for closing a file for read and opening it for write
! append.  This eliminates having to read sequentially to the end
! of the file before closing for read.
!
   Name = File
   CALL dsgefl
!
! GET LAST BLOCK NUMBER IN THIS FILE FROM FCB
!
   Nblock = Fcb(6,Ifilex)
!
! GET CURRENT BLOCK NUMBER IN THIS FILE FROM FCB
!
   icblk = Fcb(4,Ifilex)
   IF ( icblk/=Nblock ) CALL dbmmgr(6)
   Indclr = Ibase(Indbas+4) + Indbas - 1
   Indcbp = Indclr
   CALL dssdcb
END SUBROUTINE dssend
