!*==wrtblk.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE wrtblk(File,Iend)
   USE i_dsiof
   USE i_xnstrn
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File , Iend
   name = File
   CALL dsgefl
!        PRINT *,' WRTBLK,NAME,IFILEX,INDBAS=',NAME,IFILEX,INDBAS
!        WRITE(6,40646)(IBASE(INDBAS+K),K=0,7)
99001 FORMAT (' WRTBLK,BUFFER=',8(1X,Z8))
   CALL dbmmgr(8)
   indclr = ibase(indbas+4)
   nblock = ibase(indbas+3)
   ibase(indbas+1) = ibase(indbas+4)
   ibase(indbas+2) = ibase(indbas+4)
   fcb(3,ifilex) = indclr
   fcb(4,ifilex) = nblock
!       INNN = FCB(12, IFILEX)
!       PRINT *,' WRTBLK-2,IFILEX,INNN=',IFILEX,INNN
!       WRITE(6,40646)(IBASE(INNN+K),K=0,7)
   IF ( Iend/=1 ) CALL dbmmgr(4)
END SUBROUTINE wrtblk
