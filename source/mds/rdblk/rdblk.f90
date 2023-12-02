!*==rdblk.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rdblk(*,File,Ifirst,Left)
   USE i_dsiof
   USE i_xnstrn
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File , Ifirst , Left
   name = File
   CALL dsgefl
!        PRINT *,' RDBLK,NAME,IFILEX,INDBAS=',NAME,IFILEX,INDBAS
!        WRITE(6,40646)(IBASE(INDBAS+K),K=1,8)
99001 FORMAT (' BUFFER=',8(1X,Z8))
   IF ( iprvop/=0 ) CALL dsmsg(4)
   IF ( Ifirst==0 ) CALL dsrdnb
   CALL dbmmgr(9)
!      WRITE(6,44771)(FCB(K,IFILEX),K=1,15)
99002 FORMAT (' RDBLK,FCB=',/,2(5I8,/),2I8,4X,2A4,4X,I8)
!      INNN = FCB( 12, IFILEX )
!       PRINT *,' RDBLK-2,IFILEX,INNN=',IFILEX,INNN
!       WRITE(6,40646)(IBASE(INNN+K),K=0,7)
   ibase(indbas+1) = ibase(indbas+4)
   ibase(indbas+2) = ibase(indbas+4)
   Left = nbuff + 3 - lcw
   IF ( ibase(indbas+lcw-2)==idsef ) RETURN 1
END SUBROUTINE rdblk
