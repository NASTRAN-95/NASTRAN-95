!*==giggks.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE giggks
   IMPLICIT NONE
   USE C_CONDAS
   USE C_GICOM
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: atab , ns
   REAL , DIMENSION(6) :: b , temp1
   INTEGER :: buff , buff1 , buff2 , ctyp , gkset , i , i1 , i18 , idum , ieq , ifil , ifrst , ii , il , j , j1 , jj , jjj , jl ,   &
            & k , kk , kkk , l , lca , max , n , n1 , nbg , nco , ncord , ncstm , neq , ngset , nk , nkset , nmax , nmin , nogo ,   &
            & nr , ns1 , ns2 , ns3 , nsil , nw , nwds , nwr , pbgpt , pcstm , prcp , pre , ptcp , pte , type
   REAL , DIMENSION(18) , SAVE :: c
   INTEGER , DIMENSION(3) , SAVE :: caero , ls2 , set1 , spl3 , ss1 , st2
   REAL , DIMENSION(18) :: cb
   INTEGER , DIMENSION(5) :: ccard , scard
   REAL :: ch1 , ch2 , px1 , px2 , px3 , px4 , py1 , py2 , py3 , py4 , sp1 , sp2 , sum , x2e , x3e , y2e , y3e , z1 , z2
   REAL , DIMENSION(16) :: crard
   INTEGER , DIMENSION(7) :: eqt , trl
   INTEGER , SAVE :: iz2
   REAL , DIMENSION(8) :: set2
   REAL , DIMENSION(3) :: temp , x1b , x1e , x4b , x4e
   REAL , DIMENSION(28) :: z
   EXTERNAL close , dmpfil , fwdrec , gmmats , gopen , korsz , locate , mesage , open , preloc , rdtrl , read , rewind , sort ,     &
          & sswtch , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS SUBROUTINE READS THE SPLINE CARDS AND DETERMINES THE
!     POINTS IN THE G AND K S
!
!
!     CHANGE IN EQUIV FOR SIZE OF SCARD OR CCARD
!
   !>>>>EQUIVALENCE (Iz(1),Z(1),Scard(1),Set2(1)) , (Z(28),Nkset)
   !>>>>EQUIVALENCE (Z(11),Ccard(1),Crard(1)) , (Z(27),Ngset) , (Set2(3),Sp1) , (Set2(4),Sp2) , (Set2(5),Ch1) , (Set2(6),Ch2) ,          &
!>>>>    & (Set2(7),Z1) , (Set2(8),Z2)
   DATA c/18*0.0/ , set1/3502 , 35 , 999/
   DATA ss1/3302 , 33 , 6/ , ls2/3402 , 34 , 10/ , st2/3602 , 36 , 8/ , caero/3002 , 30 , 16/
   DATA spl3/4901 , 49 , 1/ , atab/200 , 2/
   DATA ns/4HGIGG , 4HKS  / , iz2/2/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         CALL open(*340,Scr2,Iz(buff2+1),1)
         ifil = Spline
         CALL preloc(*340,Iz(buff+1),Spline)
!
!     SET1 CARDS
!
         CALL locate(*60,Iz(buff+1),set1,idum)
         n = 1
         nco = buff2 - n
         CALL read(*360,*20,Spline,Iz(n),nco,1,nwr)
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 20      i = n - 1
         n1 = 0
         ASSIGN 40 TO type
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            i = i + 1
            IF ( Iz(i)==-1 ) THEN
               IF ( n1<2 ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL write(Scr2,Iz(n),n1,1)
               EXIT SPAG_Loop_1_1
            ELSE
               IF ( i==nwr ) GOTO 380
               n1 = n1 + 1
            ENDIF
         ENDDO SPAG_Loop_1_1
 40      IF ( i/=nwr ) THEN
            n = i + 1
            n1 = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SET 2 CARDS
!
 60      CALL locate(*100,Iz(buff+1),st2,idum)
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
         IF ( pbgpt<n+150 ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ IN CSTM AT PCSTM + 14 ADD BASIC COORD SYSTEM
!
         Iz(pcstm) = 0
         Iz(pcstm+1) = 1
         DO i = 2 , 13
            z(pcstm+i) = 0.0
         ENDDO
         z(pcstm+5) = 1.0
         z(pcstm+9) = 1.0
         z(pcstm+13) = 1.0
         IF ( ncstm/=14 ) THEN
            ifil = Cstm
            CALL gopen(Cstm,Iz(buff1+1),0)
            CALL read(*360,*360,Cstm,Iz(pcstm+14),ncstm-14,1,nwr)
            CALL close(Cstm,1)
         ENDIF
!
!     READ IN BAGPDT AT PBGPT
!
         ifil = Bagpdt
         CALL gopen(Bagpdt,Iz(buff1+1),0)
         CALL read(*360,*360,Bagpdt,Iz(pbgpt),nbg,1,nwr)
         CALL close(Bagpdt,1)
!
!     READ IN SET2 CARDS WITH CAERO1 APPENDED
!
         ifil = Spline
         lca = 0
         ASSIGN 80 TO type
 80      CALL read(*360,*100,Spline,Iz(1),n-1,0,nwr)
         n1 = 1
         IF ( ccard(1)==lca ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         lca = ccard(1)
         k = pcstm
         j = pcstm + ncstm - 1
         IF ( ccard(3)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = k , j , 14
            IF ( ccard(3)==Iz(i) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         GOTO 380
      CASE (3)
         prcp = i + 2
         ptcp = i + 5
         ctyp = Iz(i+1)
!
!     LOCATE POINTS 1 AND 4 AS INPUT
!
         IF ( ctyp==2 ) THEN
            x1b(1) = crard(9)*cos(crard(10)*Degra)
            x1b(2) = crard(9)*sin(crard(10)*Degra)
            x1b(3) = crard(11)
            x4b(1) = crard(13)*cos(crard(14)*Degra)
            x4b(2) = crard(13)*sin(crard(14)*Degra)
            x4b(3) = crard(15)
         ELSEIF ( ctyp==3 ) THEN
            x1b(1) = crard(9)*sin(crard(10)*Degra)*cos(crard(11)*Degra)
            x1b(2) = crard(9)*sin(crard(10)*Degra)*sin(crard(11)*Degra)
            x1b(3) = crard(9)*cos(crard(10)*Degra)
            x4b(1) = crard(13)*sin(crard(14)*Degra)*cos(crard(15)*Degra)
            x4b(2) = crard(13)*sin(crard(14)*Degra)*sin(crard(15)*Degra)
            x4b(3) = crard(13)*cos(crard(14)*Degra)
         ELSE
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         x1b(1) = crard(9)
         x1b(2) = crard(10)
         x1b(3) = crard(11)
         x4b(1) = crard(13)
         x4b(2) = crard(14)
         x4b(3) = crard(15)
         IF ( ccard(3)==0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         CALL gmmats(z(ptcp),3,3,0,x1b,3,1,0,temp)
         x1b(1) = temp(1) + z(prcp)
         x1b(2) = temp(2) + z(prcp+1)
         x1b(3) = temp(3) + z(prcp+2)
         CALL gmmats(z(ptcp),3,3,0,x4b,3,1,0,temp)
         x4b(1) = temp(1) + z(prcp)
         x4b(2) = temp(2) + z(prcp+1)
         x4b(3) = temp(3) + z(prcp+2)
         spag_nextblock_1 = 6
      CASE (6)
         IF ( ccard(2)==0 ) THEN
            x1e(1) = x1b(1)
            x1e(2) = x1b(2)
            x4e(1) = x4b(1)
            x4e(2) = x4b(2)
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     FIND ELEMENT COORDINATE SYSTEM
!
            DO i = k , j , 14
               IF ( ccard(2)==Iz(i) ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            GOTO 380
         ENDIF
      CASE (7)
         pre = i + 2
         pte = i + 5
         x1b(1) = x1b(1) - z(pre)
         x1b(2) = x1b(2) - z(pre+1)
         x1b(3) = x1b(3) - z(pre+2)
         x4b(1) = x4b(1) - z(pre)
         x4b(2) = x4b(2) - z(pre+1)
         x4b(3) = x4b(3) - z(pre+2)
         CALL gmmats(z(pte),3,3,1,x1b(1),3,1,0,x1e)
         CALL gmmats(z(pte),3,3,1,x4b(1),3,1,0,x4e)
         spag_nextblock_1 = 8
      CASE (8)
         x2e = x1e(1) + crard(12)
         y2e = x1e(2)
         x3e = x4e(1) + crard(16)
         y3e = x4e(2)
         spag_nextblock_1 = 9
      CASE (9)
!
!     FIND PRISM POINTS
!
         px1 = (1.0-sp1)*(1.0-ch1)*x1e(1) + (1.0-sp1)*ch1*x2e + sp1*ch1*x3e + sp1*(1.0-ch1)*x4e(1)
         px2 = (1.0-sp1)*(1.0-ch2)*x1e(1) + (1.0-sp1)*ch2*x2e + sp1*ch2*x3e + sp1*(1.0-ch2)*x4e(1)
         px3 = (1.0-sp2)*(1.0-ch2)*x1e(1) + (1.0-sp2)*ch2*x2e + sp2*ch2*x3e + sp2*(1.0-ch2)*x4e(1)
         px4 = (1.0-sp2)*(1.0-ch1)*x1e(1) + (1.0-sp2)*ch1*x2e + sp2*ch1*x3e + sp2*(1.0-ch1)*x4e(1)
!
!     CHECK FOR BAD GEOMETRY
!
         IF ( px1>px2 .OR. px4>px3 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         py1 = (1.0-sp1)*(1.0-ch1)*x1e(2) + (1.0-sp1)*ch1*y2e + sp1*ch1*y3e + sp1*(1.0-ch1)*x4e(2)
         py2 = (1.0-sp1)*(1.0-ch2)*x1e(2) + (1.0-sp1)*ch2*y2e + sp1*ch2*y3e + sp1*(1.0-ch2)*x4e(2)
         py3 = (1.0-sp2)*(1.0-ch2)*x1e(2) + (1.0-sp2)*ch2*y2e + sp2*ch2*y3e + sp2*(1.0-ch2)*x4e(2)
         py4 = (1.0-sp2)*(1.0-ch1)*x1e(2) + (1.0-sp2)*ch1*y2e + sp2*ch1*y3e + sp2*(1.0-ch1)*x4e(2)
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
         IF ( z1/=0.0 ) THEN
            c(15) = -1.0
            b(5) = -z1
            nr = 5
         ENDIF
         IF ( z2/=0.0 ) THEN
            IF ( z1==0.0 ) THEN
               c(15) = 1.0
               b(5) = z2
               nr = 5
            ELSE
               c(18) = 1.0
               b(6) = z2
               nr = 6
            ENDIF
         ENDIF
!
!     CONVERT TO BASIC
!
         IF ( ccard(2)==0 ) THEN
            DO i = 1 , 18
               cb(i) = c(i)
            ENDDO
         ELSE
            CALL gmmats(c,nr,3,0,z(pte),3,3,1,cb)
            CALL gmmats(z(pte),3,3,1,z(pre),3,1,0,temp)
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
         SPAG_Loop_1_2: DO k = kk , kkk , 4
            IF ( Iz(k)/=-1 ) THEN
               jj = 0
               DO i = 1 , nr
                  sum = 0.0
                  DO j = 1 , 3
                     jj = jj + 1
                     sum = sum + cb(jj)*z(k+j)
                  ENDDO
                  IF ( sum<b(i) ) CYCLE SPAG_Loop_1_2
               ENDDO
!
!     FOUND ONE
!
               n1 = n1 + 1
               Iz(n1) = (k-pbgpt)/4 + 1
            ENDIF
         ENDDO SPAG_Loop_1_2
         IF ( n1<2 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( i18/=0 ) THEN
            WRITE (Out,99001) (Iz(ii),ii=1,n1)
99001       FORMAT (5H0SET2,I8,2X,(/,10I9))
         ENDIF
         CALL write(Scr2,Iz(1),n1,1)
         GOTO 80
 100     CALL close(Scr2,1)
         CALL open(*340,Scr2,Iz(buff2+1),0)
         neq = Ksize*3
         eqt(1) = Sila
         CALL rdtrl(eqt)
         nsil = eqt(2)
         ieq = buff2 - neq - nsil
!
!     INITIAL CORE CHECK  PLUS FUDGE FACTOR
!
         IF ( ieq<150 ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ SPLINE FOR K POINT POINTERS
!
!     READ SILA
!
         CALL locate(*380,Iz(buff+1),atab,idum)
         CALL read(*360,*120,Spline,Iz(ieq),neq+1,0,nwr)
         GOTO 380
 120     neq = nwr
         ifil = Sila
         CALL gopen(Sila,Iz(buff1+1),0)
         CALL read(*360,*360,Sila,Iz(ieq+neq),nsil,1,nwr)
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
         CALL locate(*180,Iz(buff+1),ss1,idum)
         ASSIGN 140 TO type
         nr = ls2(3) + caero(3)
 140     CALL read(*360,*180,Spline,Iz(1),nr,0,nwr)
         ns1 = ns1 + 1
         ASSIGN 160 TO gkset
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
!
!     G AND K SET ARE IN CORE SORTED  BY INTERNAL NUMBERS
!     A SECOND SET OF G   ARE SORTED  BY SIL NUMBERS
!     A SECOND SET OF K   ARE IN CORE BY K NUMBER
!     NK POINTS TO K SET
!     N1 IS FIRST LOCATION OF OPEN CORE
!     NGSET IS THE NUMBER OF G  NKSET FOR K
!
 160     IF ( nogo/=1 ) THEN
!
!     WRITE ALL SPLINE1 DATA ON SCR1 AS PROCESSED
!     ID OF SPLINE1 = 1
!
            Iz(iz2) = 1
            nw = n1 - 1
            max = max0(max,nw)
            CALL write(Scr1,Iz(1),nw,1)
         ENDIF
         GOTO 140
!
!     END OF SPLINE1 CARDS
!
!     READ SPLINE2 CARDS
!
 180     CALL locate(*240,Iz(buff+1),ls2,idum)
         ASSIGN 200 TO type
         nr = ls2(3) + caero(3)
 200     CALL read(*360,*240,Spline,Iz(1),nr,0,nwr)
         ns2 = ns2 + 1
         ASSIGN 220 TO gkset
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
!
!     ID OF SPLINE2 = 2
!
 220     IF ( nogo/=1 ) THEN
            Iz(iz2) = 2
            nw = n1 - 1
            max = max0(max,nw)
            CALL write(Scr1,Iz(1),nw,1)
         ENDIF
         GOTO 200
!
!     END OF SPLINE2 CARDS
!
 240     CALL close(Scr1,1)
         CALL close(Scr2,1)
         CALL gopen(Scr3,Iz(buff1+1),1)
!
!     SPLINE 3 CARDS TO SCR3
!
         CALL locate(*300,Iz(buff+1),spl3,idum)
         CALL read(*360,*260,Spline,Iz,ieq,0,ns3)
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 260     n = ns3 + 1
!
!     CONVERT AERO IDS TO K COLUMN NUMBERS, BUILD A LIST OF SPLINE CARD
!     POINTERS, SORT ON K COLUMNS, PROCESS CARDS IN SORTED ORDER GET
!     G POINTS TO SILS
!
         n1 = 1
         nw = ieq - 1
         ASSIGN 280 TO type
         i = n
         spag_nextblock_1 = 10
      CASE (10)
         k = Iz(n1+3)
         DO j = 1 , neq , 3
            IF ( k==Iz(nw+j) ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         WRITE (Out,99004) k , Iz(n1+2)
         nogo = 1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      CASE (11)
         Iz(n1+3) = Iz(nw+j+2)
         Iz(i) = n1
         Iz(i+1) = Iz(n1+3)
         i = i + 2
 280     n1 = n1 + Iz(n1) + 1
         IF ( n1<ns3 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nw = i - n
         ns3 = nw/2
         IF ( ns3==0 ) THEN
            CALL mesage(-61,0,ns)
            nogo = 1
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
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
 300     CALL close(Spline,1)
         CALL close(Scr3,1)
         CALL dmpfil(Scr1,z,nwds)
         CALL dmpfil(Scr3,z,nwds)
         trl(2) = max
         trl(3) = ns1 + ns2
         CALL wrttrl(trl)
         IF ( nogo==1 ) THEN
            CALL mesage(-61,0,ns)
            nogo = 1
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( ns1==0 .AND. ns2==0 .AND. ns3==0 ) GOTO 380
            RETURN
         ENDIF
      CASE (12)
!
!     SET 1 CARDS
!     SET 2 CARDS
!
         ngset = 0
         ifil = Scr2
         DO
            CALL read(*400,*400,Scr2,Iz(n),1,0,nwr)
            IF ( scard(5)==Iz(n) ) THEN
               CALL read(*360,*320,Scr2,Iz(n),nco,1,nwr)
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL fwdrec(*360,Scr2)
            ENDIF
         ENDDO
 320     CALL rewind(Scr2)
         ifil = Spline
         ngset = nwr
         n1 = n + ngset
         CALL sort(0,0,1,1,Iz(n),ngset)
!
!     GET K SET
!
         nk = n1 - 1
         nkset = 0
         nmin = scard(3)
         nmax = scard(4)
         ncord = ccard(5)
         ifrst = ccard(1)
         IF ( nmin>nmax ) GOTO 380
         j1 = ncord*ccard(4) + ifrst - 1
         IF ( nmin<ifrst .OR. nmax>j1 ) GOTO 380
         j1 = (nmin-ifrst)/ncord + 1
         i1 = (nmin-ifrst) - ncord*(j1-1) + 1
         jl = (nmax-ifrst)/ncord + 1
         il = (nmax-ifrst) - ncord*(jl-1) + 1
         DO j = j1 , jl
            DO i = i1 , il
               Iz(n1) = ifrst + (i-1) + ncord*(j-1)
               n1 = n1 + 1
               nkset = nkset + 1
            ENDDO
         ENDDO
!
!     MAKE A LIST OF SIL NUMBERS   FOR G SET
!
         nw = ngset
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
         DO i = 1 , nkset
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  DO j = jj , neq , 3
                     IF ( Iz(nk+i)==Iz(nw+j) ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  spag_nextblock_1 = 16
                  CYCLE SPAG_DispatchLoop_1
               CASE (2)
                  jj = j
                  Iz(nk+i) = Iz(nw+j+1)
                  Iz(n1) = Iz(nw+j+2)
                  n1 = n1 + 1
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         GOTO gkset
!
!     ERROR MESSAGES
!
 340     CALL mesage(-1,ifil,ns)
 360     CALL mesage(-3,ifil,ns)
         spag_nextblock_1 = 13
      CASE (13)
         CALL mesage(-8,0,ns)
 380     CALL mesage(-7,0,ns)
         spag_nextblock_1 = 14
      CASE (14)
         scard(1) = Iz(n)
         spag_nextblock_1 = 15
      CASE (15)
         WRITE (Out,99002) Uwm , scard(5) , scard(1)
99002    FORMAT (A25,' 2257, SET',I9,' REFERENCED ON SPLINE CARD',I9,' IS EMPTY.')
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 400     WRITE (Out,99003) Ufm , scard(5) , scard(1)
99003    FORMAT (A23,' 2258, SET',I9,' REFERENCED ON SPLINE CARD',I9,' NOT FOUND OR IT IS EMPTY.')
         CALL rewind(Scr2)
         nogo = 1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      CASE (16)
         WRITE (Out,99004) Sfm , Iz(nk+i-1) , ccard(1)
         nogo = 1
         spag_nextblock_1 = 17
      CASE (17)
         GOTO type
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99004 FORMAT (A25,' 2259, POINT ASSIGNED TO BOX',I9,' FOR CAER01',I9,' NOT IN EQAERO.')
END SUBROUTINE giggks
