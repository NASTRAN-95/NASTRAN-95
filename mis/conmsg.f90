
SUBROUTINE conmsg(Mesage,Nwords,Idummy)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cpustr , Cputim , Oldcpu
   INTEGER Icrdat(3) , Idate(3) , Isystm(175) , Loglin , Logpag , Lout , Nllog
   CHARACTER*7 Machos
   CHARACTER*11 Mchnam
   COMMON /chmach/ Mchnam , Machos
   COMMON /logout/ Lout
   COMMON /system/ Isystm
!
! Dummy argument declarations
!
   INTEGER Idummy , Nwords
   INTEGER Mesage(1)
!
! Local variable declarations
!
   CHARACTER*41 ahead
   REAL cputmm , inctim , modtim
   CHARACTER*8 ctime
   CHARACTER*4 cvalues(8)
   INTEGER fchar , i , iaudt , idash , idsms , imodtm , impya , itime(3) , iwrtt , mwords , ncmnam , ncmos , values(8)
   CHARACTER*12 real_clock(3) , time
!
! End of declarations
!
!
!
   EQUIVALENCE (values,cvalues)
!
!
   EQUIVALENCE (Isystm(15),Idate(1)) , (Isystm(18),Cpustr) , (Isystm(42),Icrdat) , (Isystm(75),Cputim) , (Isystm(151),Nllog) ,      &
    & (Isystm(152),Loglin) , (Isystm(159),Logpag) , (Isystm(160),Oldcpu)
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
   WRITE (ahead(fchar:fchar+1),99001) Icrdat(3)
99001 FORMAT (A2)
   fchar = fchar + 3
   ahead(fchar:41) = Mchnam(1:ncmnam)//' '//Machos(1:ncmos)//' NASTRAN JOB'
!
   imodtm = 0
   IF ( Idummy==111111 .OR. Idummy==222222 ) imodtm = Idummy/111111
   IF ( Loglin>=Nllog .OR. Loglin<=0 ) THEN
      IF ( Loglin==0 ) WRITE (Lout,99002) Idate , ahead
!
99002 FORMAT (1H1,77(1H*)/1X,1H*,75X,1H*/1X,1H*,7X,'DATE ',2(I2,'/'),I2,7X,A41,7X,1H*/1X,1H*,75X,1H*/1X,77(1H*)/)
      IF ( Loglin==0 ) WRITE (Lout,99003)
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
   cputmm = cputmm + Oldcpu - Cpustr
   inctim = cputmm - Cputim
   IF ( Cputim==0.0 ) inctim = 0.0
   IF ( imodtm==1 ) modtim = 0.0
   IF ( imodtm==2 ) modtim = cputmm - modtim
   mwords = min0(Nwords,15)
   IF ( imodtm/=2 ) WRITE (Lout,99005) time(1:2) , time(3:4) , time(5:10) , cputmm , inctim , (Mesage(i),i=1,mwords)
99005 FORMAT (1X,a2,':',a2,':',a6,4X,F10.3,5X,F10.3,15X,5X,A4,2X,2A4,2X,12A4)
   IF ( imodtm==2 ) WRITE (Lout,99006) time(1:2) , time(3:4) , time(5:10) , cputmm , inctim , modtim , (Mesage(i),i=1,mwords)
99006 FORMAT (1X,a2,':',a2,':',a6,4X,F10.3,5X,F10.3,5X,F10.3,5X,A4,2X,2A4,2X,12A4)
   CALL flush(4)
!
   Loglin = Loglin + 1
   Cputim = cputmm
   IF ( imodtm==1 ) modtim = cputmm
   RETURN
END SUBROUTINE conmsg
