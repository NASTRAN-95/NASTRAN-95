!*==xsfa.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xsfa(X)
!
!     ENTRY SIZE NUMBERS,  1=FIAT, 2=SOS, 3=MD, 4=DPD
!
!     REVISED  8/89,  SEE XSFABD
!
   USE c_blank
   USE c_ixsfa
   USE c_machin
   USE c_system
   USE c_xdpl
   USE c_xfiat
   USE c_xfist
   USE c_xmssg
   USE c_xsfa1
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: X
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: alcnt , almsk , apndmk , blksiz , cursno , dculg , dmxlg , dnaf , entn1 , entn2 , entn3 , entn4 , f1 , fculg , fil ,  &
            & flag , fmxlg , fn , fnos , fnx , funlg , i , iapflg , ii , inam1 , inam2 , ip , itemp , itest , itiord , itpflg ,     &
            & iunpfg , ix , iy , j , k , l , lmsk , lmt , lmt1 , lmt2 , lmt4 , lmt5 , lmt8 , lmt8p1 , lmt9 , lxmsk , m , macsft ,   &
            & mlgn , mxntu , mxntui , n , nfculg , noaval , nx , rmsk , rnos , rxmsk , s , scornt , slgn , tapmsk , thcrmk , totf , &
            & totio , trial , zap
   INTEGER , DIMENSION(1) :: ddbn , dfnu , fcum , fcus , fdbn , fequ , file , fknd , fmat , fntu , fon , ford , fpun , minp , mlsn ,&
                           & mout , mscr , sal , sdbn , sntu , sord
   INTEGER , SAVE :: ibegn , iend , ns14 , oscar1 , oscar2 , plus , pool
   INTEGER , DIMENSION(23) :: iprt
   INTEGER , DIMENSION(3) , SAVE :: nsfa
   INTEGER , DIMENSION(2,3) , SAVE :: pfil
   EXTERNAL andf , close , complf , conmsg , dbmdia , dbmstf , fwdrec , lshift , mesage , open , orf , page1 , page2 , rshift ,     &
          & skpfil , sswtch , xclean , xdph , xflszd , xpleqk , xpolck , xpunp , xsfadd , xsosgn
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   !>>>>EQUIVALENCE (Dpd(1),Dnaf) , (Dpd(2),Dmxlg) , (Dpd(3),Dculg) , (Dpd(4),Ddbn(1)) , (Dpd(6),Dfnu(1)) , (Fiat(1),Funlg) ,            &
!>>>>    & (Fiat(2),Fmxlg) , (Fiat(3),Fculg) , (Fiat(4),Fequ(1)) , (Fiat(4),File(1)) , (Fiat(4),Ford(1)) , (Fiat(5),Fdbn(1)) ,           &
!>>>>    & (Fiat(7),Fmat(1)) , (Md(1),Mlgn) , (Md(2),Mlsn(1)) , (Md(3),Minp(1)) , (Md(4),Mout(1)) , (Md(5),Mscr(1)) , (Sos(1),Slgn) ,    &
!>>>>    & (Sos(2),Sdbn(1)) , (Sos(4),Sal(1)) , (Sos(4),Sntu(1)) , (Sos(4),Sord(1)) , (Xf1at(1),Fntu(1)) , (Xf1at(1),Fon(1)) ,           &
!>>>>    & (Xf1at(2),Fpun(1)) , (Xf1at(3),Fcum(1)) , (Xf1at(4),Fcus(1)) , (Xf1at(5),Fknd(1))
   !>>>>EQUIVALENCE (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) , (Comm(4),Entn1) , (Comm(5),Entn2) , (Comm(6),Entn3) ,        &
!>>>>    & (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) , (Comm(11),Lxmsk) , (Comm(12),Macsft) , (Comm(13),Rmsk) , &
!>>>>    & (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) , (Comm(18),Thcrmk) , (Comm(19),Zap)
   DATA oscar1 , oscar2/4HXOSC , 4HAR  / , pool/4HPOOL/
   DATA nsfa/4HXSFA , 4H     , 4H    / , ns14/4HNS14/
   DATA ibegn , iend/4HBEGN , 4HEND /
   DATA plus/1H+/
   DATA pfil/4HPLTP , 4HAR   , 4HGPSE , 4HTS   , 4HELSE , 4HTS  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL xsfadd
         nsfa(3) = ibegn
         CALL conmsg(nsfa,3,0)
!
!     ALMSK  = O 377777777777     Z 7FFFFFFF
         almsk = rshift(complf(0),1)
!
!     THCRMK = O 777777000000     Z FFFFFF00
         thcrmk = lshift(almsk,nbpw-(3*nbpc))
!
!     S      = O 400000000000     Z 80000000
         s = lshift(1,nbpw-1)
!
!     MACSFT = SHIFT COUNT TO PLACE INTEGER IN 4TH FROM LEFT CHARACTER
         macsft = (ncpw-4)*nbpc
!
         entn1 = icfiat
         cursno = X
!
!     GET OSCAR FILE POSITION AND SAVE IN FNOS
!     ALSO SAVE RECORD POSITION IN RNOS
!
         CALL xpolck(oscar1,oscar2,fnos,nx)
         IF ( fnos==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         fnx = fnos
         rnos = cursno
         CALL xsosgn
         IF ( mlgn==0 ) THEN
            WRITE (outtap,99001) sfm
99001       FORMAT (A25,' 1002, OSCAR CONTAINS NO MODULES')
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL xclean
!
!     INITIALIZE PRIOR TO FIRST MODULE ALLOCATION
!
            ASSIGN 40 TO itest
!
            lmt1 = mlgn*entn3
            lmt8 = funlg*entn1
            lmt8p1 = lmt8 + 1
            DO i = 1 , lmt8 , entn1
               IF ( andf(tapmsk,file(i))/=0 ) GOTO 10
            ENDDO
            tapmsk = 0
!
!     LOOP THRU ALL MODULES IN SOS
!
 10         i = 1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         totio = minp(i) + mout(i)
         totf = totio + mscr(i)
         alcnt = 0
         lmt2 = lmt3 + 1
         lmt4 = lmt3 + minp(i)*entn2
         lmt5 = lmt4 + mout(i)*entn2
         lmt3 = lmt3 + totf*entn2
         lmt9 = fculg*entn1
         nfculg = lmt9 + 1
         itiord = lshift(mlsn(i),16)
         DO j = 1 , lmt9 , entn1
            fcum(j) = 0
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_2: DO
!
!     SEQUENCE THRU SOS (ONE MODULE) LOOK FOR NAME MATCH + LTU COMPARE
!
            flag = 0
            DO k = lmt2 , lmt3 , entn2
               IF ( sal(k)>=0 ) THEN
                  itpflg = andf(tapmsk,sntu(k))
!
!     SEQUENCE THRU FIAT (NAME MATCH)
!
                  DO f1 = 1 , lmt9 , entn1
                     IF ( sdbn(k)==fdbn(f1) .AND. sdbn(k+1)==fdbn(f1+1) ) THEN
                        IF ( fpun(f1)<0 ) THEN
                           spag_nextblock_1 = 7
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        fntu(f1) = orf(andf(s,fon(f1)),sntu(k))
                        fcum(f1) = -1
                        fcus(f1) = -1
                        IF ( fknd(f1)==0 ) fknd(f1) = 1
                        GOTO 12
                     ENDIF
                  ENDDO
                  IF ( mlsn(i)>=0 ) THEN
                     IF ( k>lmt4 ) THEN
                        IF ( andf(apndmk,sord(k))/=apndmk ) THEN
!
!     SEQUENCE THRU FIAT (LTU COMPARE)
!
                           SPAG_Loop_3_1: DO f1 = 1 , lmt9 , entn1
                              IF ( itiord>andf(lmsk,ford(f1)) ) THEN
                                 IF ( fon(f1)>=0 ) THEN
                                    IF ( fcum(f1)>=0 ) THEN
                                       IF ( fdbn(f1)/=0 ) THEN
                                         IF ( andf(rmsk,file(f1))/=rmsk ) THEN
                                         IF ( andf(lmsk,ford(f1))/=lmsk ) THEN
                                         IF ( itpflg==0 .OR. andf(tapmsk,file(f1))/=0 ) THEN
                                         IF ( fequ(f1)<0 ) THEN
                                         fil = andf(rmsk,file(f1))
                                         DO l = 1 , lmt9 , entn1
                                         IF ( fequ(l)<0 ) THEN
                                         IF ( f1/=l ) THEN
                                         IF ( fil==andf(rmsk,file(l)) ) THEN
                                         IF ( itiord<=andf(lmsk,ford(l)) ) CYCLE SPAG_Loop_3_1
                                         IF ( fon(l)<0 ) CYCLE SPAG_Loop_3_1
                                         IF ( fcum(l)<0 ) CYCLE SPAG_Loop_3_1
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDDO
                                         ENDIF
                                         IF ( fculg+pad>=fmxlg ) THEN
                                         spag_nextblock_1 = 7
                                         CYCLE SPAG_DispatchLoop_1
                                         ENDIF
                                         fon(f1) = orf(s,fon(f1))
                                         fdbn(nfculg) = sdbn(k)
                                         fdbn(nfculg+1) = sdbn(k+1)
                                         ford(nfculg) = orf(andf(lxmsk,sord(k)),andf(rxmsk,file(f1)))
                                         fntu(nfculg) = sntu(k)
                                         fcum(nfculg) = -1
                                         fcus(nfculg) = -1
                                         fknd(nfculg) = 2
                                         nfculg = nfculg + entn1
                                         fculg = fculg + 1
                                         GOTO 12
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDDO SPAG_Loop_3_1
                        ENDIF
                     ENDIF
                  ENDIF
                  CYCLE
 12               sal(k) = orf(s,sal(k))
                  alcnt = alcnt + 1
               ENDIF
            ENDDO
            IF ( alcnt/=totf ) THEN
!
!     SEQUENCE THRU SOS (ONE MODULE) LOOK FOR BLANK FILES + GREATER NTU
!
               DO k = lmt2 , lmt3 , entn2
                  IF ( sal(k)>=0 ) THEN
                     IF ( flag/=0 .AND. k>lmt4 .AND. k<=lmt5 ) CYCLE SPAG_Loop_1_2
                     iapflg = 0
                     iunpfg = 0
                     IF ( andf(apndmk,sord(k))==apndmk ) iapflg = -1
                     itpflg = andf(tapmsk,sntu(k))
!
!     SEQUENCE THRU FIAT-UNIQUE (BLANK FILES)
!
                     IF ( bff>=0 ) THEN
                        DO f1 = 1 , lmt8 , entn1
                           IF ( fdbn(f1)==0 ) THEN
                              IF ( itpflg==0 .OR. andf(tapmsk,file(f1))/=0 ) THEN
                                 IF ( k<=lmt4 .OR. iapflg/=0 ) THEN
                                    CALL xpolck(sdbn(k),sdbn(k+1),fn,nx)
                                    IF ( iapflg==0 .OR. fn/=0 ) THEN
                                       IF ( fn==0 ) THEN
                                         IF ( pltflg==0 ) THEN
                                         DO ip = 1 , 3
                                         IF ( sdbn(k)==pfil(1,ip) .AND. sdbn(k+1)==pfil(2,ip) ) GOTO 14
                                         ENDDO
                                         ENDIF
                                         IF ( thislk/=ns14 ) CALL mesage(22,0,sdbn(k))
                                         GOTO 16
                                       ENDIF
 14                                    fpun(f1) = fn
                                       iunpfg = f1
                                    ENDIF
                                 ENDIF
                                 fdbn(f1) = sdbn(k)
                                 fdbn(f1+1) = sdbn(k+1)
                                 ford(f1) = orf(andf(lxmsk,sord(k)),file(f1))
                                 fntu(f1) = sntu(k)
                                 fcum(f1) = -1
                                 fcus(f1) = -1
                                 fknd(f1) = 3
 16                              sal(k) = orf(s,sal(k))
                                 alcnt = alcnt + 1
                                 GOTO 22
                              ENDIF
                           ENDIF
                        ENDDO
                        IF ( itpflg==0 ) bff = -1
                     ENDIF
!
!     SEQUENCE THRU FIAT (GREATEST NTU) FOR POOLING
!
                     IF ( mlsn(i)<0 ) THEN
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
!     BEFORE PERMITTING POOLING CHECK IF AT LEAST ONE MODULE IS ALLOCATE
!
                     IF ( i/=1 ) EXIT SPAG_Loop_1_2
                     SPAG_Loop_3_3: DO
                        mxntu = cursno
                        mxntui = 0
                        DO f1 = 1 , lmt8 , entn1
                           IF ( fcus(f1)>=0 ) THEN
                              IF ( idefr2>=0 ) THEN
                                 IF ( fmat(f1)/=0 .OR. fmat(f1+1)/=0 .OR. fmat(f1+2)/=0 ) THEN
                                    idefr1 = -1
                                    CYCLE
                                 ELSEIF ( entn1==11 .AND. (fmat(f1+5)/=0 .OR. fmat(f1+6)/=0 .OR. fmat(f1+7)/=0) ) THEN
                                    idefr1 = -1
                                    CYCLE
                                 ENDIF
                              ENDIF
                              IF ( fknd(f1)>=0 ) THEN
                                 IF ( fpun(f1)==0 ) THEN
                                    IF ( itpflg==0 .OR. andf(tapmsk,file(f1))/=0 ) THEN
                                       trial = andf(fntu(f1),rmsk)
                                       IF ( trial>mxntu ) THEN
                                         mxntu = trial
                                         mxntui = f1
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDDO
                        IF ( mxntui/=0 ) THEN
!
!     A GREATER NTU FILE EXISTS
!
                           n = 1
!
!     SEARCH FOR EQUIV OR STACKED MATCH
!
                           fil = andf(rmsk,file(mxntui))
                           DO j = lmt8p1 , lmt9 , entn1
                              IF ( fil==andf(rmsk,file(j)) ) THEN
!
!     A MATCH IS FOUND, IS MATCHED FILE USED IN CURRENT SEG
!
                                 IF ( fcus(j)<0 ) GOTO 18
!
!     IF MATCHED FILE HAS NTU LESS - TEST AND SET DEFER FLAG
!
                                 IF ( idefr2>=0 ) THEN
                                    IF ( fmat(j)/=0 .OR. fmat(j+1)/=0 .OR. fmat(j+2)/=0 ) THEN
                                       idefr1 = -1
                                       GOTO 18
                                    ELSEIF ( entn1==11 .AND. (fmat(j+5)/=0 .OR. fmat(j+6)/=0 .OR. fmat(j+7)/=0) ) THEN
                                       idefr1 = -1
                                       GOTO 18
                                    ELSEIF ( andf(rmsk,fntu(1))<andf(rmsk,fntu(mxntui)) ) THEN
                                       idefr1 = -1
                                       GOTO 18
                                    ENDIF
                                 ENDIF
!
!     MATCHED FILE IS O.K. - IS IT EQUIV OR STACKED
!
                                 IF ( fequ(j)>=0 ) THEN
!
!     STACKED - WIPE OUT MATCH (IF EMPTY)
!
                                    IF ( fmat(j)/=0 .OR. fmat(j+1)/=0 .OR. fmat(j+2)/=0 ) GOTO 18
                                    IF ( entn1==11 .AND. (fmat(j+5)/=0 .OR. fmat(j+6)/=0 .OR. fmat(j+7)/=0) ) GOTO 18
                                    file(j) = 0
                                    fdbn(j) = 0
                                    fdbn(j+1) = 0
                                 ELSE
                                    fknd(j) = 7
                                    n = n + 1
                                 ENDIF
                              ENDIF
                           ENDDO
                           fpun(mxntui) = orf(s,n)
                           IF ( k>lmt4 .AND. iapflg==0 ) EXIT SPAG_Loop_3_3
                           CALL xpolck(sdbn(k),sdbn(k+1),fn,nx)
                           IF ( iapflg/=0 .AND. fn==0 ) EXIT SPAG_Loop_3_3
                           IF ( fn/=0 ) THEN
                              fpun(nfculg) = fn
                              iunpfg = nfculg
                              EXIT SPAG_Loop_3_3
                           ELSE
                              IF ( thislk/=14 ) CALL mesage(22,0,sdbn(k))
                              GOTO 20
                           ENDIF
 18                        IF ( fknd(mxntui)==0 ) fknd(mxntui) = 9
                           fknd(mxntui) = -iabs(fknd(mxntui))
                        ELSE
!
!     FILE NOT FOUND - HAS A PASS BEEN DEFERRED
!
                           IF ( idefr1==0 ) THEN
                              spag_nextblock_1 = 7
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
!
!     PASS HAS BEEN DEFERRED - TRY IT NOW
!
                           idefr1 = 0
                           idefr2 = -1
                           DO ix = 1 , lmt8 , entn1
                              fknd(ix) = iabs(fknd(ix))
                           ENDDO
                        ENDIF
                     ENDDO SPAG_Loop_3_3
                     IF ( fculg+pad>=fmxlg ) THEN
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     fon(mxntui) = orf(s,fon(mxntui))
                     ford(nfculg) = orf(andf(rxmsk,file(mxntui)),andf(lxmsk,sord(k)))
                     fknd(nfculg) = orf(fknd(nfculg),5)
                     fdbn(nfculg) = sdbn(k)
                     fdbn(nfculg+1) = sdbn(k+1)
                     fntu(nfculg) = sntu(k)
                     fcum(nfculg) = -1
                     fcus(nfculg) = -1
                     nfculg = nfculg + entn1
                     fculg = fculg + 1
 20                  sal(k) = orf(s,sal(k))
                     alcnt = alcnt + 1
 22                  IF ( iunpfg/=0 ) THEN
                        IF ( dfnu(nx)<0 ) THEN
                           CALL xpleqk(nx,iunpfg)
                           lmt9 = fculg*entn1
                           nfculg = lmt9 + 1
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
!
!     MODULE ALLOCATION COMPLETE
!
            cursno = andf(rmsk,mlsn(i)) + 1
!
!     END OF I MODULE PSEUDO LOOP
!
            i = i + entn3
            IF ( i>lmt1 ) EXIT SPAG_Loop_1_2
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 4
      CASE (4)
!
         CALL xpunp
         CALL xdph
!
!     REPOSITION OSCAR FOR SEM
!
         CALL xpolck(oscar1,oscar2,fnos,nx)
         IF ( fnos==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         CALL open(*80,pool,buf1,0)
         IF ( fnos/=1 ) CALL skpfil(pool,fnos-1)
         DO j = 1 , rnos
            CALL fwdrec(*100,pool)
         ENDDO
         CALL close(pool,2)
         spag_nextblock_1 = 6
      CASE (6)
!
!
!     DUMP FIAT IF SENSE SWITCH 2 IS ON
!
         CALL sswtch(2,ix)
         IF ( ix/=1 ) GOTO itest
         CALL page1
         CALL page2(-4)
         WRITE (outtap,99002) fiat(1) , fiat(2) , fiat(3) , X , cursno
99002    FORMAT (15H0FIAT AFTER SFA,3I4,12H  OSCAR STR ,I4,6H, STP ,I4,//,' EQ AP  LTU  TP  UNIT  NTU  OF SG KN TR DATA-BLK      *',&
               & 6X,'*   TRAILER   *      *      *  PRI BLKS   SEC FLS/BLKS',3X,'TER FLS/BLKS')
         ii = fiat(3)*entn1
         DO ix = 1 , ii , entn1
            iprt(1) = rshift(fequ(ix),nbpw-1)
            iprt(2) = rshift(andf(apndmk,ford(ix)),30)
            iprt(3) = rshift(andf(lmsk,ford(ix)),16)
            iprt(4) = rshift(andf(tapmsk,file(ix)),15)
            iprt(5) = andf(rmsk,file(ix))
            iprt(6) = andf(rmsk,fntu(ix))
            iprt(7) = rshift(fon(ix),nbpw-1)
            iprt(8) = fcus(ix)
            iprt(9) = fknd(ix)
            iprt(10) = rshift(andf(tapmsk,fntu(ix)),15)
            iprt(11) = fdbn(ix)
            iprt(12) = fdbn(ix+1)
            IF ( iprt(11)==0 ) THEN
               iprt(11) = nsfa(2)
               iprt(12) = nsfa(2)
            ENDIF
            IF ( entn1==11 ) THEN
               iprt(13) = fmat(ix)
               iprt(14) = fmat(ix+1)
               iprt(15) = fmat(ix+2)
               iprt(16) = fmat(ix+5)
               iprt(17) = fmat(ix+6)
               iprt(18) = fmat(ix+7)
            ELSE
               iprt(13) = rshift(fmat(ix),16)
               iprt(14) = andf(rxmsk,fmat(ix))
               iprt(15) = rshift(fmat(ix+1),16)
               iprt(16) = andf(rxmsk,fmat(ix+1))
               iprt(17) = rshift(fmat(ix+2),16)
               iprt(18) = andf(rxmsk,fmat(ix+2))
            ENDIF
            iprt(19) = rshift(fmat(ix+3),16)
            itemp = andf(fmat(ix+3),rxmsk)
            iprt(20) = rshift(itemp,8)
            iprt(21) = rshift(fmat(ix+4),16)
            iprt(22) = itemp - iprt(20)*2**8
            iprt(23) = andf(rxmsk,fmat(ix+4))
            CALL page2(-1)
            WRITE (outtap,99003) (iprt(iy),iy=1,23)
99003       FORMAT (1H ,2(I2,1X),I5,1X,I2,2(1X,I5),4(1X,I2),1X,2A4,6I7,4X,I5,1X,2(7X,I2,1H/,I5))
         ENDDO
         CALL xflszd(0,blksiz,0)
         CALL page2(-2)
         WRITE (outtap,99004) blksiz
99004    FORMAT (30X,20H EACH BLOCK CONTAINS,I5,7H WORDS.)
         WRITE (outtap,99005)
99005    FORMAT (52H POOL FILE CONTENTS   EQ    SIZE   FILE   DATA BLOCK)
         ii = dpd(3)*3
         DO ix = 1 , ii , 3
            iprt(1) = rshift(dfnu(ix),nbpw-1)
            iprt(2) = rshift(dfnu(ix),16)
            iprt(3) = andf(rxmsk,dfnu(ix))
            iprt(4) = ddbn(ix)
            iprt(5) = ddbn(ix+1)
            CALL page2(-1)
            WRITE (outtap,99006) (iprt(iy),iy=1,5)
99006       FORMAT (22X,I2,I7,I7,3X,2A4)
         ENDDO
         CALL dbmdia
         CALL dbmstf
!
         GOTO itest
!
 40      j = mch
         IF ( iabs(ibnk(entn1*5))/1000/=j .AND. j>6 ) comm(4) = j
         X = cursno
         nsfa(3) = iend
         CALL conmsg(nsfa,3,0)
         RETURN
      CASE (7)
!
!     MODULE ALLOCATION INCOMPLETE
!
         IF ( i/=1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( itpflg==0 ) THEN
            cursno = 0
            spag_nextblock_1 = 5
         ELSE
!
!     LOOKING FOR A TAPE + AT LEAST ONE TAPE EXISTS
!
            noaval = 0
            DO m = 1 , lmt8 , entn1
               IF ( andf(tapmsk,file(m))/=0 ) THEN
                  IF ( andf(tapmsk,fntu(m))==0 ) GOTO 50
                  noaval = 1
               ENDIF
            ENDDO
            IF ( noaval==0 ) THEN
               cursno = 0
               spag_nextblock_1 = 5
            ELSE
               tapmsk = 0
!
               bff = 0
               spag_nextblock_1 = 3
            ENDIF
            CYCLE
!
!     A TAPE FILE EXIST CONTAINING A D.B. NOT REQUIRING A TAPE  -
!     FREE THAT TAPE***  CHECK FOR EQUIV AND LTU D.B. ON SAME UNIT
!
 50         n = 1
!
            ASSIGN 60 TO itest
            spag_nextblock_1 = 6
         ENDIF
         CYCLE
 60      ASSIGN 40 TO itest
!
         trial = andf(rmsk,file(m))
         lmt = lmt8 + 1
         DO j = lmt , lmt9 , entn1
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  IF ( trial==andf(rmsk,file(j)) ) THEN
                     inam1 = fdbn(j)
                     inam2 = fdbn(j+1)
                     IF ( fequ(m)<0 .AND. fequ(j)<0 ) THEN
                        n = n + 1
                     ELSE
                        fdbn(j) = almsk
                     ENDIF
                     SPAG_Loop_4_1: DO l = lmt2 , lmt3 , entn2
                        IF ( inam1==sdbn(l) .AND. inam2==sdbn(l+1) ) THEN
                           spag_nextblock_2 = 2
                           EXIT SPAG_Loop_4_1
                        ENDIF
                     ENDDO SPAG_Loop_4_1
                  ENDIF
               CASE (2)
!
!     TURN OFF ALLOC FLAG
!
                  sal(l) = orf(almsk,sal(l))
                  alcnt = alcnt - 1
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         inam1 = fdbn(m)
         inam2 = fdbn(m+1)
         DO l = lmt2 , lmt3 , entn2
            IF ( inam1==sdbn(l) .AND. inam2==sdbn(l+1) ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 9
      CASE (8)
         sal(l) = orf(almsk,sal(l))
         alcnt = alcnt - 1
         spag_nextblock_1 = 9
      CASE (9)
         fpun(m) = orf(s,n)
         CALL xpunp
         fdbn(m) = sdbn(k)
         fdbn(m+1) = sdbn(k+1)
         ford(m) = orf(andf(lxmsk,sord(k)),andf(rxmsk,file(m)))
         fknd(m) = 8
!
         CALL sswtch(2,ix)
         IF ( ix==1 ) THEN
            CALL page2(-2)
            WRITE (outtap,99007)
99007       FORMAT (38H0* XSFA REPEATS TO USE FREED TAPE FILE)
         ENDIF
         bff = 0
         spag_nextblock_1 = 3
      CASE (10)
!
         WRITE (outtap,99008) sfm
99008    FORMAT (A25,' 1001, OSCAR NOT FOUND IN DPL')
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 80      WRITE (outtap,99009) sfm
99009    FORMAT (A25,' 1003, POOL COULD NOT BE OPENED')
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 100     WRITE (outtap,99010) sfm
99010    FORMAT (A25,' 1004, ILLEGAL EOF ON POOL')
         spag_nextblock_1 = 11
      CASE (11)
         CALL mesage(-37,0,nsfa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE xsfa
