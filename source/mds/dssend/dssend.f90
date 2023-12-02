!*==dssend.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dssend(File)
   USE i_dsiof
   USE i_xnstrn
   USE I_DSIOF
   USE I_XNSTRN
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
   name = File
   CALL dsgefl
!
! GET LAST BLOCK NUMBER IN THIS FILE FROM FCB
!
   nblock = fcb(6,ifilex)
!
! GET CURRENT BLOCK NUMBER IN THIS FILE FROM FCB
!
   icblk = fcb(4,ifilex)
   IF ( icblk/=nblock ) CALL dbmmgr(6)
   indclr = ibase(indbas+4) + indbas - 1
   indcbp = indclr
   CALL dssdcb
END SUBROUTINE dssend
