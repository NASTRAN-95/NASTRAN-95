
SUBROUTINE rdblk(*,File,Ifirst,Left)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! Dummy argument declarations
!
   INTEGER File , Ifirst , Left
!
! End of declarations
!
   Name = File
   CALL dsgefl
!        PRINT *,' RDBLK,NAME,IFILEX,INDBAS=',NAME,IFILEX,INDBAS
!        WRITE(6,40646)(IBASE(INDBAS+K),K=1,8)
99001 FORMAT (' BUFFER=',8(1X,Z8))
   IF ( Iprvop/=0 ) CALL dsmsg(4)
   IF ( Ifirst==0 ) CALL dsrdnb
   CALL dbmmgr(9)
!      WRITE(6,44771)(FCB(K,IFILEX),K=1,15)
99002 FORMAT (' RDBLK,FCB=',/,2(5I8,/),2I8,4X,2A4,4X,I8)
!      INNN = FCB( 12, IFILEX )
!       PRINT *,' RDBLK-2,IFILEX,INNN=',IFILEX,INNN
!       WRITE(6,40646)(IBASE(INNN+K),K=0,7)
   Ibase(Indbas+1) = Ibase(Indbas+4)
   Ibase(Indbas+2) = Ibase(Indbas+4)
   Left = Nbuff + 3 - Lcw
   IF ( Ibase(Indbas+Lcw-2)==Idsef ) RETURN 1
END SUBROUTINE rdblk
