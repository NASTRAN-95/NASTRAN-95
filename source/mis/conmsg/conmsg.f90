!*==conmsg.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE conmsg(Mesage,Nwords,Idummy)
   IMPLICIT NONE
   USE C_CHMACH
   USE C_LOGOUT
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Mesage
   INTEGER :: Nwords
   INTEGER :: Idummy
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(41) :: ahead
   REAL :: cpustr , cputim , cputmm , inctim , oldcpu
   CHARACTER(8) :: ctime
   CHARACTER(4) , DIMENSION(8) :: cvalues
   INTEGER :: fchar , i , imodtm , loglin , logpag , mwords , ncmnam , ncmos , nllog
   INTEGER , SAVE :: iaudt , idash , idsms , impya , iwrtt
   INTEGER , DIMENSION(3) :: icrdat , idate , itime
   REAL , SAVE :: modtim
   CHARACTER(12) , DIMENSION(3) :: real_clock
   CHARACTER(12) :: time
   INTEGER , DIMENSION(8) :: values
   EXTERNAL flush , nastim
!
! End of declarations rewritten by SPAG
!
!
!
   !>>>>EQUIVALENCE (values,cvalues)
!
!
   !>>>>EQUIVALENCE (Isystm(15),Idate(1)) , (Isystm(18),Cpustr) , (Isystm(42),Icrdat) , (Isystm(75),Cputim) , (Isystm(151),Nllog) ,      &
!>>>>    & (Isystm(152),Loglin) , (Isystm(159),Logpag) , (Isystm(160),Oldcpu)
!
   DATA idsms , iwrtt , iaudt , impya/4HDSMS , 4HWRTT , 4HAUDT , 4HMPYA/
   DATA modtim/0.0/
   DATA idash/4H----/
!
!   ASSEMBLE PAGE HEADING
!
   ahead = ' '
   ncmnam = index(Mchnam,' ') - 1
   IF ( ncmnam<=-1 ) ncmnam = 11
   ncmos = index(Machos,' ') - 1
   IF ( ncmos<=-1 ) ncmos = 7
   fchar = (18-ncmnam-ncmos) + 1
   ahead(fchar:fchar+6) = 'LOG OF '
   fchar = fchar + 7
   WRITE (ahead(fchar:fchar+1),99001) icrdat(3)
99001 FORMAT (A2)
   fchar = fchar + 3
   ahead(fchar:41) = Mchnam(1:ncmnam)//' '//Machos(1:ncmos)//' NASTRAN JOB'
!
   imodtm = 0
   IF ( Idummy==111111 .OR. Idummy==222222 ) imodtm = Idummy/111111
   IF ( loglin>=nllog .OR. loglin<=0 ) THEN
      IF ( loglin==0 ) WRITE (Lout,99002) idate , ahead
!
99002 FORMAT (1H1,77(1H*)/1X,1H*,75X,1H*/1X,1H*,7X,'DATE ',2(I2,'/'),I2,7X,A41,7X,1H*/1X,1H*,75X,1H*/1X,77(1H*)/)
      IF ( loglin==0 ) WRITE (Lout,99003)
99003 FORMAT (1X,2X,'WALL',15X,'TOTAL',7X,'INCREMENTAL',6X,'MODULE',14X,'MODULE/'/1X,2X,'CLOCK',15X,'CPU',12X,'CPU',12X,'CPU',13X,  &
             &'SUBROUTINE'/1X,2X,'TIME',14X,'SECONDS',8X,'SECONDS',8X,'SECONDS',13X,'STATUS'//1X,78(1H-)/)
      IF ( Mesage(1)==idsms .AND. Nwords==1 ) RETURN
      IF ( Mesage(1)==iwrtt .AND. Nwords==1 ) RETURN
      IF ( Mesage(1)==iaudt .AND. Nwords==1 ) RETURN
      IF ( Mesage(1)==impya .AND. Nwords==1 ) RETURN
   ENDIF
   CALL nastim(itime(1),itime(2),itime(3),cputmm)
   WRITE (ctime,99004) itime
!
99004 FORMAT (2(I2,':'),I2)
   CALL date_and_time(real_clock(1),time,real_clock(3),values)
   IF ( ctime(4:4)==' ' ) ctime(4:4) = '0'
   IF ( ctime(7:7)==' ' ) ctime(7:7) = '0'
   cputmm = cputmm + oldcpu - cpustr
   inctim = cputmm - cputim
   IF ( cputim==0.0 ) inctim = 0.0
   IF ( imodtm==1 ) modtim = 0.0
   IF ( imodtm==2 ) modtim = cputmm - modtim
   mwords = min0(Nwords,15)
   IF ( imodtm/=2 ) WRITE (Lout,99005) time(1:2) , time(3:4) , time(5:10) , cputmm , inctim , (Mesage(i),i=1,mwords)
99005 FORMAT (1X,a2,':',a2,':',a6,4X,F10.3,5X,F10.3,15X,5X,A4,2X,2A4,2X,12A4)
   IF ( imodtm==2 ) WRITE (Lout,99006) time(1:2) , time(3:4) , time(5:10) , cputmm , inctim , modtim , (Mesage(i),i=1,mwords)
99006 FORMAT (1X,a2,':',a2,':',a6,4X,F10.3,5X,F10.3,5X,F10.3,5X,A4,2X,2A4,2X,12A4)
   CALL flush(4)
!
   loglin = loglin + 1
   cputim = cputmm
   IF ( imodtm==1 ) modtim = cputmm
END SUBROUTINE conmsg
