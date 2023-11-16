
SUBROUTINE giggks
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bagpdt , Ccard(5) , Cstm , Ecta , Gm , Go , Gsize , Gtka , Iz(1) , Ksize , Ngset , Nkset , Out , Scard(5) , Scr1 , Scr2 ,&
         & Scr3 , Scr4 , Scr5 , Sila , Spline , Sysbuf , Useta
   REAL Ch1 , Ch2 , Crard(16) , Degra , Dum(3) , Set2(8) , Sp1 , Sp2 , Z(28) , Z1 , Z2
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /condas/ Dum , Degra
   COMMON /gicom / Spline , Useta , Cstm , Bagpdt , Sila , Ecta , Gm , Go , Gtka , Ksize , Gsize , Scr1 , Scr2 , Scr3 , Scr4 , Scr5
   COMMON /system/ Sysbuf , Out
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Iz
!
! Local variable declarations
!
   INTEGER atab(2) , buff , buff1 , buff2 , caero(3) , ctyp , eqt(7) , gkset , i , i1 , i18 , idum , ieq , ifil , ifrst , ii , il , &
         & iz2 , j , j1 , jj , jjj , jl , k , kk , kkk , l , lca , ls2(3) , max , n , n1 , nbg , nco , ncord , ncstm , neq , nk ,   &
         & nmax , nmin , nogo , nr , ns(2) , ns1 , ns2 , ns3 , nsil , nw , nwds , nwr , pbgpt , pcstm , prcp , pre , ptcp , pte ,   &
         & set1(3) , spl3(3) , ss1(3) , st2(3) , trl(7) , type
   REAL b(6) , c(18) , cb(18) , px1 , px2 , px3 , px4 , py1 , py2 , py3 , py4 , sum , temp(3) , temp1(6) , x1b(3) , x1e(3) , x2e ,  &
      & x3e , x4b(3) , x4e(3) , y2e , y3e
   INTEGER korsz
!
! End of declarations
!
!
!     THIS SUBROUTINE READS THE SPLINE CARDS AND DETERMINES THE
!     POINTS IN THE G AND K S
!
!
!     CHANGE IN EQUIV FOR SIZE OF SCARD OR CCARD
!
   EQUIVALENCE (Iz(1),Z(1),Scard(1),Set2(1)) , (Z(28),Nkset)
   EQUIVALENCE (Z(11),Ccard(1),Crard(1)) , (Z(27),Ngset) , (Set2(3),Sp1) , (Set2(4),Sp2) , (Set2(5),Ch1) , (Set2(6),Ch2) ,          &
    & (Set2(7),Z1) , (Set2(8),Z2)
   DATA c/18*0.0/ , set1/3502 , 35 , 999/
   DATA ss1/3302 , 33 , 6/ , ls2/3402 , 34 , 10/ , st2/3602 , 36 , 8/ , caero/3002 , 30 , 16/
   DATA spl3/4901 , 49 , 1/ , atab/200 , 2/
   DATA ns/4HGIGG , 4HKS  / , iz2/2/
!     DATA    IECT / 3002,46     /
!
!     INITILIZE
!
   CALL sswtch(18,i18)
   nwds = korsz(Iz)
   nogo = 0
   ns1 = 0
   ns2 = 0
   ns3 = 0
!
!     BUFF  HAS SPLINE
!     BUFF1 HAS CSTM,BGPT,EQAERO,SILA,SCR1
!     BUFF2 HAS SCR2
!
   buff = nwds - Sysbuf - 1
   buff1 = buff - Sysbuf - 1
   buff2 = buff1 - Sysbuf
!
!     PROCESS SET CARDS AND WRITE G LISTS ON SCR2
!
   ifil = Scr2
   CALL open(*2900,Scr2,Iz(buff2+1),1)
   ifil = Spline
   CALL preloc(*2900,Iz(buff+1),Spline)
!
!     SET1 CARDS
!
   CALL locate(*400,Iz(buff+1),set1,idum)
   n = 1
   nco = buff2 - n
   CALL read(*3000,*100,Spline,Iz(n),nco,1,nwr)
   GOTO 3100
 100  i = n - 1
   n1 = 0
   ASSIGN 300 TO type
 200  DO
      i = i + 1
      IF ( Iz(i)==-1 ) THEN
         IF ( n1<2 ) GOTO 3300
         CALL write(Scr2,Iz(n),n1,1)
         EXIT
      ELSE
         IF ( i==nwr ) GOTO 3200
         n1 = n1 + 1
      ENDIF
   ENDDO
 300  IF ( i/=nwr ) THEN
      n = i + 1
      n1 = 0
      GOTO 200
   ENDIF
!
!     SET 2 CARDS
!
 400  CALL locate(*1400,Iz(buff+1),st2,idum)
!
!     READ IN BAGPDT AND CSTM
!
   n = ls2(3) + caero(3) + 1
   trl(1) = Cstm
   CALL rdtrl(trl)
   IF ( trl(1)<0 ) trl(3) = 0
   ncstm = (trl(3)+1)*14
   pcstm = buff2 - ncstm
   trl(1) = Bagpdt
   CALL rdtrl(trl)
   nbg = (trl(2)-trl(3))*4
   pbgpt = pcstm - nbg
   IF ( pbgpt<n+150 ) GOTO 3100
!
!     READ IN CSTM AT PCSTM + 14 ADD BASIC COORD SYSTEM
!
   Iz(pcstm) = 0
   Iz(pcstm+1) = 1
   DO i = 2 , 13
      Z(pcstm+i) = 0.0
   ENDDO
   Z(pcstm+5) = 1.0
   Z(pcstm+9) = 1.0
   Z(pcstm+13) = 1.0
   IF ( ncstm/=14 ) THEN
      ifil = Cstm
      CALL gopen(Cstm,Iz(buff1+1),0)
      CALL read(*3000,*3000,Cstm,Iz(pcstm+14),ncstm-14,1,nwr)
      CALL close(Cstm,1)
   ENDIF
!
!     READ IN BAGPDT AT PBGPT
!
   ifil = Bagpdt
   CALL gopen(Bagpdt,Iz(buff1+1),0)
   CALL read(*3000,*3000,Bagpdt,Iz(pbgpt),nbg,1,nwr)
   CALL close(Bagpdt,1)
!
!     READ IN SET2 CARDS WITH CAERO1 APPENDED
!
   ifil = Spline
   lca = 0
   ASSIGN 500 TO type
 500  CALL read(*3000,*1400,Spline,Iz(1),n-1,0,nwr)
   n1 = 1
   IF ( Ccard(1)==lca ) GOTO 1200
   lca = Ccard(1)
   k = pcstm
   j = pcstm + ncstm - 1
   IF ( Ccard(3)==0 ) GOTO 700
   DO i = k , j , 14
      IF ( Ccard(3)==Iz(i) ) GOTO 600
   ENDDO
   GOTO 3200
 600  prcp = i + 2
   ptcp = i + 5
   ctyp = Iz(i+1)
!
!     LOCATE POINTS 1 AND 4 AS INPUT
!
   IF ( ctyp==2 ) THEN
      x1b(1) = Crard(9)*cos(Crard(10)*Degra)
      x1b(2) = Crard(9)*sin(Crard(10)*Degra)
      x1b(3) = Crard(11)
      x4b(1) = Crard(13)*cos(Crard(14)*Degra)
      x4b(2) = Crard(13)*sin(Crard(14)*Degra)
      x4b(3) = Crard(15)
   ELSEIF ( ctyp==3 ) THEN
      x1b(1) = Crard(9)*sin(Crard(10)*Degra)*cos(Crard(11)*Degra)
      x1b(2) = Crard(9)*sin(Crard(10)*Degra)*sin(Crard(11)*Degra)
      x1b(3) = Crard(9)*cos(Crard(10)*Degra)
      x4b(1) = Crard(13)*sin(Crard(14)*Degra)*cos(Crard(15)*Degra)
      x4b(2) = Crard(13)*sin(Crard(14)*Degra)*sin(Crard(15)*Degra)
      x4b(3) = Crard(13)*cos(Crard(14)*Degra)
   ELSE
      GOTO 700
   ENDIF
   GOTO 800
 700  x1b(1) = Crard(9)
   x1b(2) = Crard(10)
   x1b(3) = Crard(11)
   x4b(1) = Crard(13)
   x4b(2) = Crard(14)
   x4b(3) = Crard(15)
   IF ( Ccard(3)==0 ) GOTO 900
 800  CALL gmmats(Z(ptcp),3,3,0,x1b,3,1,0,temp)
   x1b(1) = temp(1) + Z(prcp)
   x1b(2) = temp(2) + Z(prcp+1)
   x1b(3) = temp(3) + Z(prcp+2)
   CALL gmmats(Z(ptcp),3,3,0,x4b,3,1,0,temp)
   x4b(1) = temp(1) + Z(prcp)
   x4b(2) = temp(2) + Z(prcp+1)
   x4b(3) = temp(3) + Z(prcp+2)
 900  IF ( Ccard(2)==0 ) THEN
      x1e(1) = x1b(1)
      x1e(2) = x1b(2)
      x4e(1) = x4b(1)
      x4e(2) = x4b(2)
      GOTO 1100
   ELSE
!
!     FIND ELEMENT COORDINATE SYSTEM
!
      DO i = k , j , 14
         IF ( Ccard(2)==Iz(i) ) GOTO 1000
      ENDDO
      GOTO 3200
   ENDIF
 1000 pre = i + 2
   pte = i + 5
   x1b(1) = x1b(1) - Z(pre)
   x1b(2) = x1b(2) - Z(pre+1)
   x1b(3) = x1b(3) - Z(pre+2)
   x4b(1) = x4b(1) - Z(pre)
   x4b(2) = x4b(2) - Z(pre+1)
   x4b(3) = x4b(3) - Z(pre+2)
   CALL gmmats(Z(pte),3,3,1,x1b(1),3,1,0,x1e)
   CALL gmmats(Z(pte),3,3,1,x4b(1),3,1,0,x4e)
 1100 x2e = x1e(1) + Crard(12)
   y2e = x1e(2)
   x3e = x4e(1) + Crard(16)
   y3e = x4e(2)
!
!     FIND PRISM POINTS
!
 1200 px1 = (1.0-Sp1)*(1.0-Ch1)*x1e(1) + (1.0-Sp1)*Ch1*x2e + Sp1*Ch1*x3e + Sp1*(1.0-Ch1)*x4e(1)
   px2 = (1.0-Sp1)*(1.0-Ch2)*x1e(1) + (1.0-Sp1)*Ch2*x2e + Sp1*Ch2*x3e + Sp1*(1.0-Ch2)*x4e(1)
   px3 = (1.0-Sp2)*(1.0-Ch2)*x1e(1) + (1.0-Sp2)*Ch2*x2e + Sp2*Ch2*x3e + Sp2*(1.0-Ch2)*x4e(1)
   px4 = (1.0-Sp2)*(1.0-Ch1)*x1e(1) + (1.0-Sp2)*Ch1*x2e + Sp2*Ch1*x3e + Sp2*(1.0-Ch1)*x4e(1)
!
!     CHECK FOR BAD GEOMETRY
!
   IF ( px1>px2 .OR. px4>px3 ) GOTO 3400
   py1 = (1.0-Sp1)*(1.0-Ch1)*x1e(2) + (1.0-Sp1)*Ch1*y2e + Sp1*Ch1*y3e + Sp1*(1.0-Ch1)*x4e(2)
   py2 = (1.0-Sp1)*(1.0-Ch2)*x1e(2) + (1.0-Sp1)*Ch2*y2e + Sp1*Ch2*y3e + Sp1*(1.0-Ch2)*x4e(2)
   py3 = (1.0-Sp2)*(1.0-Ch2)*x1e(2) + (1.0-Sp2)*Ch2*y2e + Sp2*Ch2*y3e + Sp2*(1.0-Ch2)*x4e(2)
   py4 = (1.0-Sp2)*(1.0-Ch1)*x1e(2) + (1.0-Sp2)*Ch1*y2e + Sp2*Ch1*y3e + Sp2*(1.0-Ch1)*x4e(2)
!
!     BUILD PRISM INEQUALITY MATRICES
!
   c(1) = py1 - py2
   c(2) = px2 - px1
   c(4) = py2 - py3
   c(5) = px3 - px2
   c(7) = py3 - py4
   c(8) = px4 - px3
   c(10) = py4 - py1
   c(11) = px1 - px4
   c(15) = 0.0
   c(18) = 0.0
   b(1) = px2*py1 - px1*py2
   b(2) = px3*py2 - px2*py3
   b(3) = px4*py3 - px3*py4
   b(4) = px1*py4 - px4*py1
   nr = 4
   IF ( Z1/=0.0 ) THEN
      c(15) = -1.0
      b(5) = -Z1
      nr = 5
   ENDIF
   IF ( Z2/=0.0 ) THEN
      IF ( Z1==0.0 ) THEN
         c(15) = 1.0
         b(5) = Z2
         nr = 5
      ELSE
         c(18) = 1.0
         b(6) = Z2
         nr = 6
      ENDIF
   ENDIF
!
!     CONVERT TO BASIC
!
   IF ( Ccard(2)==0 ) THEN
      DO i = 1 , 18
         cb(i) = c(i)
      ENDDO
   ELSE
      CALL gmmats(c,nr,3,0,Z(pte),3,3,1,cb)
      CALL gmmats(Z(pte),3,3,1,Z(pre),3,1,0,temp)
      CALL gmmats(c,nr,3,0,temp,3,1,0,temp1)
      b(1) = b(1) + temp1(1)
      b(2) = b(2) + temp1(2)
      b(3) = b(3) + temp1(3)
      b(4) = b(4) + temp1(4)
      IF ( nr/=4 ) THEN
         b(5) = b(5) + temp1(5)
         IF ( nr/=5 ) b(6) = b(6) + temp1(6)
      ENDIF
   ENDIF
!
!     FINALLY TEST ALL GRID POINTS TO SEE IF THEY ARE IN PRISM
!
   kk = pbgpt
   kkk = kk + nbg - 1
   DO k = kk , kkk , 4
      IF ( Iz(k)/=-1 ) THEN
         jj = 0
         DO i = 1 , nr
            sum = 0.0
            DO j = 1 , 3
               jj = jj + 1
               sum = sum + cb(jj)*Z(k+j)
            ENDDO
            IF ( sum<b(i) ) GOTO 1300
         ENDDO
!
!     FOUND ONE
!
         n1 = n1 + 1
         Iz(n1) = (k-pbgpt)/4 + 1
      ENDIF
 1300 ENDDO
   IF ( n1<2 ) GOTO 3400
   IF ( i18/=0 ) THEN
      WRITE (Out,99001) (Iz(ii),ii=1,n1)
99001 FORMAT (5H0SET2,I8,2X,(/,10I9))
   ENDIF
   CALL write(Scr2,Iz(1),n1,1)
   GOTO 500
 1400 CALL close(Scr2,1)
   CALL open(*2900,Scr2,Iz(buff2+1),0)
   neq = Ksize*3
   eqt(1) = Sila
   CALL rdtrl(eqt)
   nsil = eqt(2)
   ieq = buff2 - neq - nsil
!
!     INITIAL CORE CHECK  PLUS FUDGE FACTOR
!
   IF ( ieq<150 ) GOTO 3100
!
!     READ SPLINE FOR K POINT POINTERS
!
!     READ SILA
!
   CALL locate(*3200,Iz(buff+1),atab,idum)
   CALL read(*3000,*1500,Spline,Iz(ieq),neq+1,0,nwr)
   GOTO 3200
 1500 neq = nwr
   ifil = Sila
   CALL gopen(Sila,Iz(buff1+1),0)
   CALL read(*3000,*3000,Sila,Iz(ieq+neq),nsil,1,nwr)
   CALL close(Sila,1)
   ifil = Spline
   trl(1) = Scr1
   max = 0
   CALL gopen(Scr1,Iz(buff1+1),1)
!
!     N = LENGTH OF LONGEST SPLINE CARD + CAERO1 CARD + 3
!     N  POINTS TO 1 ST LOCATION OF CORE AVAILABLE SEE EQIV
!
   n = ls2(3) + caero(3) + 3
   nco = ieq - n
!
!     READ SPLINE1 CARDS
!
   CALL locate(*1800,Iz(buff+1),ss1,idum)
   ASSIGN 1600 TO type
   nr = ls2(3) + caero(3)
 1600 CALL read(*3000,*1800,Spline,Iz(1),nr,0,nwr)
   ns1 = ns1 + 1
   ASSIGN 1700 TO gkset
   GOTO 2700
!
!     G AND K SET ARE IN CORE SORTED  BY INTERNAL NUMBERS
!     A SECOND SET OF G   ARE SORTED  BY SIL NUMBERS
!     A SECOND SET OF K   ARE IN CORE BY K NUMBER
!     NK POINTS TO K SET
!     N1 IS FIRST LOCATION OF OPEN CORE
!     NGSET IS THE NUMBER OF G  NKSET FOR K
!
 1700 IF ( nogo/=1 ) THEN
!
!     WRITE ALL SPLINE1 DATA ON SCR1 AS PROCESSED
!     ID OF SPLINE1 = 1
!
      Iz(iz2) = 1
      nw = n1 - 1
      max = max0(max,nw)
      CALL write(Scr1,Iz(1),nw,1)
   ENDIF
   GOTO 1600
!
!     END OF SPLINE1 CARDS
!
!     READ SPLINE2 CARDS
!
 1800 CALL locate(*2100,Iz(buff+1),ls2,idum)
   ASSIGN 1900 TO type
   nr = ls2(3) + caero(3)
 1900 CALL read(*3000,*2100,Spline,Iz(1),nr,0,nwr)
   ns2 = ns2 + 1
   ASSIGN 2000 TO gkset
   GOTO 2700
!
!     ID OF SPLINE2 = 2
!
 2000 IF ( nogo/=1 ) THEN
      Iz(iz2) = 2
      nw = n1 - 1
      max = max0(max,nw)
      CALL write(Scr1,Iz(1),nw,1)
   ENDIF
   GOTO 1900
!
!     END OF SPLINE2 CARDS
!
 2100 CALL close(Scr1,1)
   CALL close(Scr2,1)
   CALL gopen(Scr3,Iz(buff1+1),1)
!
!     SPLINE 3 CARDS TO SCR3
!
   CALL locate(*2600,Iz(buff+1),spl3,idum)
   CALL read(*3000,*2200,Spline,Iz,ieq,0,ns3)
   GOTO 3100
 2200 n = ns3 + 1
!
!     CONVERT AERO IDS TO K COLUMN NUMBERS, BUILD A LIST OF SPLINE CARD
!     POINTERS, SORT ON K COLUMNS, PROCESS CARDS IN SORTED ORDER GET
!     G POINTS TO SILS
!
   n1 = 1
   nw = ieq - 1
   ASSIGN 2500 TO type
   i = n
 2300 k = Iz(n1+3)
   DO j = 1 , neq , 3
      IF ( k==Iz(nw+j) ) GOTO 2400
   ENDDO
   WRITE (Out,99004) k , Iz(n1+2)
   nogo = 1
   GOTO 3700
 2400 Iz(n1+3) = Iz(nw+j+2)
   Iz(i) = n1
   Iz(i+1) = Iz(n1+3)
   i = i + 2
 2500 n1 = n1 + Iz(n1) + 1
   IF ( n1<ns3 ) GOTO 2300
   nw = i - n
   ns3 = nw/2
   IF ( ns3==0 ) THEN
      CALL mesage(-61,0,ns)
      nogo = 1
      GOTO 3700
   ELSE
      IF ( ns3/=1 ) CALL sort(0,0,2,2,Iz(n),nw)
!
!     PROCESS BY SORTED ORDER
!
      n = n - 1
      j = ieq + neq - 1
      jj = 5
      DO i = 1 , nw , 2
         n1 = Iz(n+i)
         jjj = Iz(n1) - caero(3)
         DO k = jj , jjj , 3
            l = Iz(n1+k)
            Iz(n1+k) = Iz(j+l)
         ENDDO
         CALL write(Scr3,Iz(n1+1),Iz(n1),1)
      ENDDO
   ENDIF
 2600 CALL close(Spline,1)
   CALL close(Scr3,1)
   CALL dmpfil(Scr1,Z,nwds)
   CALL dmpfil(Scr3,Z,nwds)
   trl(2) = max
   trl(3) = ns1 + ns2
   CALL wrttrl(trl)
   IF ( nogo==1 ) THEN
      CALL mesage(-61,0,ns)
      nogo = 1
      GOTO 3700
   ELSE
      IF ( ns1/=0 .OR. ns2/=0 .OR. ns3/=0 ) GOTO 99999
      GOTO 3200
   ENDIF
!
!     SET 1 CARDS
!     SET 2 CARDS
!
 2700 Ngset = 0
   ifil = Scr2
   DO
      CALL read(*3500,*3500,Scr2,Iz(n),1,0,nwr)
      IF ( Scard(5)==Iz(n) ) THEN
         CALL read(*3000,*2800,Scr2,Iz(n),nco,1,nwr)
         GOTO 3100
      ELSE
         CALL fwdrec(*3000,Scr2)
      ENDIF
   ENDDO
 2800 CALL rewind(Scr2)
   ifil = Spline
   Ngset = nwr
   n1 = n + Ngset
   CALL sort(0,0,1,1,Iz(n),Ngset)
!
!     GET K SET
!
   nk = n1 - 1
   Nkset = 0
   nmin = Scard(3)
   nmax = Scard(4)
   ncord = Ccard(5)
   ifrst = Ccard(1)
   IF ( nmin>nmax ) GOTO 3200
   j1 = ncord*Ccard(4) + ifrst - 1
   IF ( nmin<ifrst .OR. nmax>j1 ) GOTO 3200
   j1 = (nmin-ifrst)/ncord + 1
   i1 = (nmin-ifrst) - ncord*(j1-1) + 1
   jl = (nmax-ifrst)/ncord + 1
   il = (nmax-ifrst) - ncord*(jl-1) + 1
   DO j = j1 , jl
      DO i = i1 , il
         Iz(n1) = ifrst + (i-1) + ncord*(j-1)
         n1 = n1 + 1
         Nkset = Nkset + 1
      ENDDO
   ENDDO
!
!     MAKE A LIST OF SIL NUMBERS   FOR G SET
!
   nw = Ngset
   j = ieq + neq - 1
   DO i = 1 , nw
      k = Iz(n+i-1)
      Iz(n1) = Iz(k+j)
      n1 = n1 + 1
   ENDDO
!
!     FIND INTERNAL K POINT NUMBER  FOR BGPT PLUS K NUMBER
!
   jj = 1
   nw = ieq - 1
   DO i = 1 , Nkset
      DO j = jj , neq , 3
         IF ( Iz(nk+i)==Iz(nw+j) ) GOTO 2850
      ENDDO
      GOTO 3600
 2850 jj = j
      Iz(nk+i) = Iz(nw+j+1)
      Iz(n1) = Iz(nw+j+2)
      n1 = n1 + 1
   ENDDO
   GOTO gkset
!
!     ERROR MESSAGES
!
 2900 CALL mesage(-1,ifil,ns)
 3000 CALL mesage(-3,ifil,ns)
 3100 CALL mesage(-8,0,ns)
 3200 CALL mesage(-7,0,ns)
 3300 Scard(1) = Iz(n)
 3400 WRITE (Out,99002) Uwm , Scard(5) , Scard(1)
99002 FORMAT (A25,' 2257, SET',I9,' REFERENCED ON SPLINE CARD',I9,' IS EMPTY.')
   GOTO 3700
 3500 WRITE (Out,99003) Ufm , Scard(5) , Scard(1)
99003 FORMAT (A23,' 2258, SET',I9,' REFERENCED ON SPLINE CARD',I9,' NOT FOUND OR IT IS EMPTY.')
   CALL rewind(Scr2)
   nogo = 1
   GOTO 3700
 3600 WRITE (Out,99004) Sfm , Iz(nk+i-1) , Ccard(1)
   nogo = 1
 3700 GOTO type
99004 FORMAT (A25,' 2259, POINT ASSIGNED TO BOX',I9,' FOR CAER01',I9,' NOT IN EQAERO.')
99999 RETURN
END SUBROUTINE giggks
