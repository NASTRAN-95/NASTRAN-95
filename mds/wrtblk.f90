
SUBROUTINE wrtblk(File,Iend)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! Dummy argument declarations
!
   INTEGER File , Iend
!
! End of declarations
!
   Name = File
   CALL dsgefl
!        PRINT *,' WRTBLK,NAME,IFILEX,INDBAS=',NAME,IFILEX,INDBAS
!        WRITE(6,40646)(IBASE(INDBAS+K),K=0,7)
99001 FORMAT (' WRTBLK,BUFFER=',8(1X,Z8))
   CALL dbmmgr(8)
   Indclr = Ibase(Indbas+4)
   Nblock = Ibase(Indbas+3)
   Ibase(Indbas+1) = Ibase(Indbas+4)
   Ibase(Indbas+2) = Ibase(Indbas+4)
   Fcb(3,Ifilex) = Indclr
   Fcb(4,Ifilex) = Nblock
!       INNN = FCB(12, IFILEX)
!       PRINT *,' WRTBLK-2,IFILEX,INNN=',IFILEX,INNN
!       WRITE(6,40646)(IBASE(INNN+K),K=0,7)
   IF ( Iend/=1 ) CALL dbmmgr(4)
END SUBROUTINE wrtblk
