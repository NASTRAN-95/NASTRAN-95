
SUBROUTINE xsfa(X)
!
!     ENTRY SIZE NUMBERS,  1=FIAT, 2=SOS, 3=MD, 4=DPD
!
!     REVISED  8/89,  SEE XSFABD
!
   IMPLICIT NONE
   INTEGER Almsk , Apndmk , Bff , Buf1 , Comm(20) , Cursno , Dculg , Ddbn(1) , Dfnu(1) , Dmm(14) , Dmxlg , Dnaf , Dpd(6) , Dum(17) ,&
         & Dum1 , Dum2 , Entn1 , Entn2 , Entn3 , Entn4 , Fculg , Fcum(1) , Fcus(1) , Fdbn(1) , Fequ(1) , Fiat(7) , File(1) , Fist , &
         & Fknd(1) , Flag , Fmat(1) , Fmxlg , Fntu(1) , Fnx , Fon(1) , Ford(1) , Fpun(1) , Funlg , Ibnk(1) , Ibufsz , Icfiat ,      &
         & Idefr1 , Idefr2 , Lmsk , Lmt3 , Lxmsk , Macsft , Mch , Md(401) , Minp(1) , Mlgn , Mlsn(1) , Mout(1) , Mscr(1) , Nbpc ,   &
         & Nbpw , Ncpw , Outtap , Pad , Pltflg , Rmsk , Rxmsk , S , Sal(1) , Scornt , Sdbn(1) , Slgn , Sntu(1) , Sord(1) , Sos(1501)&
         & , Tapmsk , Thcrmk , Thislk , Xf1at(5) , Zap
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Ibnk
   COMMON /ixsfa / Lmt3 , Bff , Pad , Idefr1 , Idefr2
   COMMON /machin/ Mch
   COMMON /system/ Ibufsz , Outtap , Dum , Pltflg , Dum1 , Thislk , Dum2 , Icfiat , Dmm , Nbpc , Nbpw , Ncpw
   COMMON /xdpl  / Dpd
   COMMON /xfiat / Fiat
   COMMON /xfist / Fist
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xsfa1 / Md , Sos , Comm , Xf1at
   COMMON /zzzzzz/ Buf1
   INTEGER X
   INTEGER alcnt , blksiz , f1 , fil , fn , fnos , i , iapflg , ibegn , iend , ii , inam1 , inam2 , ip , iprt(23) , itemp , itest , &
         & itiord , itpflg , iunpfg , ix , iy , j , k , l , lmt , lmt1 , lmt2 , lmt4 , lmt5 , lmt8 , lmt8p1 , lmt9 , m , mxntu ,    &
         & mxntui , n , nfculg , noaval , ns14 , nsfa(3) , nx , oscar1 , oscar2 , pfil(2,3) , plus , pool , rnos , totf , totio ,   &
         & trial
   INTEGER andf , complf , lshift , orf , rshift
   EXTERNAL andf , complf , lshift , orf , rshift
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
!
   CALL xsfadd
   nsfa(3) = ibegn
   CALL conmsg(nsfa,3,0)
!
!     ALMSK  = O 377777777777     Z 7FFFFFFF
   Almsk = rshift(complf(0),1)
!
!     THCRMK = O 777777000000     Z FFFFFF00
   Thcrmk = lshift(Almsk,Nbpw-(3*Nbpc))
!
!     S      = O 400000000000     Z 80000000
   S = lshift(1,Nbpw-1)
!
!     MACSFT = SHIFT COUNT TO PLACE INTEGER IN 4TH FROM LEFT CHARACTER
   Macsft = (Ncpw-4)*Nbpc
!
   Entn1 = Icfiat
   Cursno = X
!
!     GET OSCAR FILE POSITION AND SAVE IN FNOS
!     ALSO SAVE RECORD POSITION IN RNOS
!
   CALL xpolck(oscar1,oscar2,fnos,nx)
   IF ( fnos==0 ) GOTO 1200
   Fnx = fnos
   rnos = Cursno
   CALL xsosgn
   IF ( Mlgn==0 ) THEN
      WRITE (Outtap,99001) Sfm
99001 FORMAT (A25,' 1002, OSCAR CONTAINS NO MODULES')
      GOTO 1500
   ELSE
      CALL xclean
!
!     INITIALIZE PRIOR TO FIRST MODULE ALLOCATION
!
      ASSIGN 700 TO itest
!
      lmt1 = Mlgn*Entn3
      lmt8 = Funlg*Entn1
      lmt8p1 = lmt8 + 1
      DO i = 1 , lmt8 , Entn1
         IF ( andf(Tapmsk,File(i))/=0 ) GOTO 50
      ENDDO
      Tapmsk = 0
!
!     LOOP THRU ALL MODULES IN SOS
!
 50   i = 1
   ENDIF
 100  totio = Minp(i) + Mout(i)
   totf = totio + Mscr(i)
   alcnt = 0
   lmt2 = Lmt3 + 1
   lmt4 = Lmt3 + Minp(i)*Entn2
   lmt5 = lmt4 + Mout(i)*Entn2
   Lmt3 = Lmt3 + totf*Entn2
   lmt9 = Fculg*Entn1
   nfculg = lmt9 + 1
   itiord = lshift(Mlsn(i),16)
   DO j = 1 , lmt9 , Entn1
      Fcum(j) = 0
   ENDDO
 200  DO
!
!     SEQUENCE THRU SOS (ONE MODULE) LOOK FOR NAME MATCH + LTU COMPARE
!
      Flag = 0
      DO k = lmt2 , Lmt3 , Entn2
         IF ( Sal(k)>=0 ) THEN
            itpflg = andf(Tapmsk,Sntu(k))
!
!     SEQUENCE THRU FIAT (NAME MATCH)
!
            DO f1 = 1 , lmt9 , Entn1
               IF ( Sdbn(k)==Fdbn(f1) .AND. Sdbn(k+1)==Fdbn(f1+1) ) THEN
                  IF ( Fpun(f1)<0 ) GOTO 800
                  Fntu(f1) = orf(andf(S,Fon(f1)),Sntu(k))
                  Fcum(f1) = -1
                  Fcus(f1) = -1
                  IF ( Fknd(f1)==0 ) Fknd(f1) = 1
                  GOTO 210
               ENDIF
            ENDDO
            IF ( Mlsn(i)>=0 ) THEN
               IF ( k>lmt4 ) THEN
                  IF ( andf(Apndmk,Sord(k))/=Apndmk ) THEN
!
!     SEQUENCE THRU FIAT (LTU COMPARE)
!
                     DO f1 = 1 , lmt9 , Entn1
                        IF ( itiord>andf(Lmsk,Ford(f1)) ) THEN
                           IF ( Fon(f1)>=0 ) THEN
                              IF ( Fcum(f1)>=0 ) THEN
                                 IF ( Fdbn(f1)/=0 ) THEN
                                    IF ( andf(Rmsk,File(f1))/=Rmsk ) THEN
                                       IF ( andf(Lmsk,Ford(f1))/=Lmsk ) THEN
                                         IF ( itpflg==0 .OR. andf(Tapmsk,File(f1))/=0 ) THEN
                                         IF ( Fequ(f1)<0 ) THEN
                                         fil = andf(Rmsk,File(f1))
                                         DO l = 1 , lmt9 , Entn1
                                         IF ( Fequ(l)<0 ) THEN
                                         IF ( f1/=l ) THEN
                                         IF ( fil==andf(Rmsk,File(l)) ) THEN
                                         IF ( itiord<=andf(Lmsk,Ford(l)) ) GOTO 202
                                         IF ( Fon(l)<0 ) GOTO 202
                                         IF ( Fcum(l)<0 ) GOTO 202
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDDO
                                         ENDIF
                                         IF ( Fculg+Pad>=Fmxlg ) GOTO 800
                                         Fon(f1) = orf(S,Fon(f1))
                                         Fdbn(nfculg) = Sdbn(k)
                                         Fdbn(nfculg+1) = Sdbn(k+1)
                                         Ford(nfculg) = orf(andf(Lxmsk,Sord(k)),andf(Rxmsk,File(f1)))
                                         Fntu(nfculg) = Sntu(k)
                                         Fcum(nfculg) = -1
                                         Fcus(nfculg) = -1
                                         Fknd(nfculg) = 2
                                         nfculg = nfculg + Entn1
                                         Fculg = Fculg + 1
                                         GOTO 210
                                         ENDIF
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
 202                 ENDDO
                  ENDIF
               ENDIF
            ENDIF
            CYCLE
 210        Sal(k) = orf(S,Sal(k))
            alcnt = alcnt + 1
         ENDIF
      ENDDO
      IF ( alcnt/=totf ) THEN
!
!     SEQUENCE THRU SOS (ONE MODULE) LOOK FOR BLANK FILES + GREATER NTU
!
         DO k = lmt2 , Lmt3 , Entn2
            IF ( Sal(k)>=0 ) THEN
               IF ( Flag/=0 .AND. k>lmt4 .AND. k<=lmt5 ) GOTO 300
               iapflg = 0
               iunpfg = 0
               IF ( andf(Apndmk,Sord(k))==Apndmk ) iapflg = -1
               itpflg = andf(Tapmsk,Sntu(k))
!
!     SEQUENCE THRU FIAT-UNIQUE (BLANK FILES)
!
               IF ( Bff>=0 ) THEN
                  DO f1 = 1 , lmt8 , Entn1
                     IF ( Fdbn(f1)==0 ) THEN
                        IF ( itpflg==0 .OR. andf(Tapmsk,File(f1))/=0 ) THEN
                           IF ( k<=lmt4 .OR. iapflg/=0 ) THEN
                              CALL xpolck(Sdbn(k),Sdbn(k+1),fn,nx)
                              IF ( iapflg==0 .OR. fn/=0 ) THEN
                                 IF ( fn==0 ) THEN
                                    IF ( Pltflg==0 ) THEN
                                       DO ip = 1 , 3
                                         IF ( Sdbn(k)==pfil(1,ip) .AND. Sdbn(k+1)==pfil(2,ip) ) GOTO 212
                                       ENDDO
                                    ENDIF
                                    IF ( Thislk/=ns14 ) CALL mesage(22,0,Sdbn(k))
                                    GOTO 214
                                 ENDIF
 212                             Fpun(f1) = fn
                                 iunpfg = f1
                              ENDIF
                           ENDIF
                           Fdbn(f1) = Sdbn(k)
                           Fdbn(f1+1) = Sdbn(k+1)
                           Ford(f1) = orf(andf(Lxmsk,Sord(k)),File(f1))
                           Fntu(f1) = Sntu(k)
                           Fcum(f1) = -1
                           Fcus(f1) = -1
                           Fknd(f1) = 3
 214                       Sal(k) = orf(S,Sal(k))
                           alcnt = alcnt + 1
                           GOTO 225
                        ENDIF
                     ENDIF
                  ENDDO
                  IF ( itpflg==0 ) Bff = -1
               ENDIF
!
!     SEQUENCE THRU FIAT (GREATEST NTU) FOR POOLING
!
               IF ( Mlsn(i)<0 ) GOTO 800
!
!     BEFORE PERMITTING POOLING CHECK IF AT LEAST ONE MODULE IS ALLOCATE
!
               IF ( i/=1 ) GOTO 400
               DO
                  mxntu = Cursno
                  mxntui = 0
                  DO f1 = 1 , lmt8 , Entn1
                     IF ( Fcus(f1)>=0 ) THEN
                        IF ( Idefr2>=0 ) THEN
                           IF ( Fmat(f1)/=0 .OR. Fmat(f1+1)/=0 .OR. Fmat(f1+2)/=0 ) THEN
                              Idefr1 = -1
                              CYCLE
                           ELSEIF ( Entn1==11 .AND. (Fmat(f1+5)/=0 .OR. Fmat(f1+6)/=0 .OR. Fmat(f1+7)/=0) ) THEN
                              Idefr1 = -1
                              CYCLE
                           ENDIF
                        ENDIF
                        IF ( Fknd(f1)>=0 ) THEN
                           IF ( Fpun(f1)==0 ) THEN
                              IF ( itpflg==0 .OR. andf(Tapmsk,File(f1))/=0 ) THEN
                                 trial = andf(Fntu(f1),Rmsk)
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
                     fil = andf(Rmsk,File(mxntui))
                     DO j = lmt8p1 , lmt9 , Entn1
                        IF ( fil==andf(Rmsk,File(j)) ) THEN
!
!     A MATCH IS FOUND, IS MATCHED FILE USED IN CURRENT SEG
!
                           IF ( Fcus(j)<0 ) GOTO 216
!
!     IF MATCHED FILE HAS NTU LESS - TEST AND SET DEFER FLAG
!
                           IF ( Idefr2>=0 ) THEN
                              IF ( Fmat(j)/=0 .OR. Fmat(j+1)/=0 .OR. Fmat(j+2)/=0 ) THEN
                                 Idefr1 = -1
                                 GOTO 216
                              ELSEIF ( Entn1==11 .AND. (Fmat(j+5)/=0 .OR. Fmat(j+6)/=0 .OR. Fmat(j+7)/=0) ) THEN
                                 Idefr1 = -1
                                 GOTO 216
                              ELSEIF ( andf(Rmsk,Fntu(1))<andf(Rmsk,Fntu(mxntui)) ) THEN
                                 Idefr1 = -1
                                 GOTO 216
                              ENDIF
                           ENDIF
!
!     MATCHED FILE IS O.K. - IS IT EQUIV OR STACKED
!
                           IF ( Fequ(j)>=0 ) THEN
!
!     STACKED - WIPE OUT MATCH (IF EMPTY)
!
                              IF ( Fmat(j)/=0 .OR. Fmat(j+1)/=0 .OR. Fmat(j+2)/=0 ) GOTO 216
                              IF ( Entn1==11 .AND. (Fmat(j+5)/=0 .OR. Fmat(j+6)/=0 .OR. Fmat(j+7)/=0) ) GOTO 216
                              File(j) = 0
                              Fdbn(j) = 0
                              Fdbn(j+1) = 0
                           ELSE
                              Fknd(j) = 7
                              n = n + 1
                           ENDIF
                        ENDIF
                     ENDDO
                     Fpun(mxntui) = orf(S,n)
                     IF ( k>lmt4 .AND. iapflg==0 ) EXIT
                     CALL xpolck(Sdbn(k),Sdbn(k+1),fn,nx)
                     IF ( iapflg/=0 .AND. fn==0 ) EXIT
                     IF ( fn/=0 ) THEN
                        Fpun(nfculg) = fn
                        iunpfg = nfculg
                        EXIT
                     ELSE
                        IF ( Thislk/=14 ) CALL mesage(22,0,Sdbn(k))
                        GOTO 220
                     ENDIF
 216                 IF ( Fknd(mxntui)==0 ) Fknd(mxntui) = 9
                     Fknd(mxntui) = -iabs(Fknd(mxntui))
                  ELSE
!
!     FILE NOT FOUND - HAS A PASS BEEN DEFERRED
!
                     IF ( Idefr1==0 ) GOTO 800
!
!     PASS HAS BEEN DEFERRED - TRY IT NOW
!
                     Idefr1 = 0
                     Idefr2 = -1
                     DO ix = 1 , lmt8 , Entn1
                        Fknd(ix) = iabs(Fknd(ix))
                     ENDDO
                  ENDIF
               ENDDO
               IF ( Fculg+Pad>=Fmxlg ) GOTO 800
               Fon(mxntui) = orf(S,Fon(mxntui))
               Ford(nfculg) = orf(andf(Rxmsk,File(mxntui)),andf(Lxmsk,Sord(k)))
               Fknd(nfculg) = orf(Fknd(nfculg),5)
               Fdbn(nfculg) = Sdbn(k)
               Fdbn(nfculg+1) = Sdbn(k+1)
               Fntu(nfculg) = Sntu(k)
               Fcum(nfculg) = -1
               Fcus(nfculg) = -1
               nfculg = nfculg + Entn1
               Fculg = Fculg + 1
 220           Sal(k) = orf(S,Sal(k))
               alcnt = alcnt + 1
 225           IF ( iunpfg/=0 ) THEN
                  IF ( Dfnu(nx)<0 ) THEN
                     CALL xpleqk(nx,iunpfg)
                     lmt9 = Fculg*Entn1
                     nfculg = lmt9 + 1
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDIF
!
!     MODULE ALLOCATION COMPLETE
!
      Cursno = andf(Rmsk,Mlsn(i)) + 1
!
!     END OF I MODULE PSEUDO LOOP
!
      i = i + Entn3
      IF ( i>lmt1 ) EXIT
      GOTO 100
 300  ENDDO
!
 400  CALL xpunp
   CALL xdph
!
!     REPOSITION OSCAR FOR SEM
!
   CALL xpolck(oscar1,oscar2,fnos,nx)
   IF ( fnos==0 ) GOTO 1200
 500  CALL open(*1300,pool,Buf1,0)
   IF ( fnos/=1 ) CALL skpfil(pool,fnos-1)
   DO j = 1 , rnos
      CALL fwdrec(*1400,pool)
   ENDDO
   CALL close(pool,2)
!
!
!     DUMP FIAT IF SENSE SWITCH 2 IS ON
!
 600  CALL sswtch(2,ix)
   IF ( ix/=1 ) GOTO itest
   CALL page1
   CALL page2(-4)
   WRITE (Outtap,99002) Fiat(1) , Fiat(2) , Fiat(3) , X , Cursno
99002 FORMAT (15H0FIAT AFTER SFA,3I4,12H  OSCAR STR ,I4,6H, STP ,I4,//,' EQ AP  LTU  TP  UNIT  NTU  OF SG KN TR DATA-BLK      *',6X,&
             &'*   TRAILER   *      *      *  PRI BLKS   SEC FLS/BLKS',3X,'TER FLS/BLKS')
   ii = Fiat(3)*Entn1
   DO ix = 1 , ii , Entn1
      iprt(1) = rshift(Fequ(ix),Nbpw-1)
      iprt(2) = rshift(andf(Apndmk,Ford(ix)),30)
      iprt(3) = rshift(andf(Lmsk,Ford(ix)),16)
      iprt(4) = rshift(andf(Tapmsk,File(ix)),15)
      iprt(5) = andf(Rmsk,File(ix))
      iprt(6) = andf(Rmsk,Fntu(ix))
      iprt(7) = rshift(Fon(ix),Nbpw-1)
      iprt(8) = Fcus(ix)
      iprt(9) = Fknd(ix)
      iprt(10) = rshift(andf(Tapmsk,Fntu(ix)),15)
      iprt(11) = Fdbn(ix)
      iprt(12) = Fdbn(ix+1)
      IF ( iprt(11)==0 ) THEN
         iprt(11) = nsfa(2)
         iprt(12) = nsfa(2)
      ENDIF
      IF ( Entn1==11 ) THEN
         iprt(13) = Fmat(ix)
         iprt(14) = Fmat(ix+1)
         iprt(15) = Fmat(ix+2)
         iprt(16) = Fmat(ix+5)
         iprt(17) = Fmat(ix+6)
         iprt(18) = Fmat(ix+7)
      ELSE
         iprt(13) = rshift(Fmat(ix),16)
         iprt(14) = andf(Rxmsk,Fmat(ix))
         iprt(15) = rshift(Fmat(ix+1),16)
         iprt(16) = andf(Rxmsk,Fmat(ix+1))
         iprt(17) = rshift(Fmat(ix+2),16)
         iprt(18) = andf(Rxmsk,Fmat(ix+2))
      ENDIF
      iprt(19) = rshift(Fmat(ix+3),16)
      itemp = andf(Fmat(ix+3),Rxmsk)
      iprt(20) = rshift(itemp,8)
      iprt(21) = rshift(Fmat(ix+4),16)
      iprt(22) = itemp - iprt(20)*2**8
      iprt(23) = andf(Rxmsk,Fmat(ix+4))
      CALL page2(-1)
      WRITE (Outtap,99003) (iprt(iy),iy=1,23)
99003 FORMAT (1H ,2(I2,1X),I5,1X,I2,2(1X,I5),4(1X,I2),1X,2A4,6I7,4X,I5,1X,2(7X,I2,1H/,I5))
   ENDDO
   CALL xflszd(0,blksiz,0)
   CALL page2(-2)
   WRITE (Outtap,99004) blksiz
99004 FORMAT (30X,20H EACH BLOCK CONTAINS,I5,7H WORDS.)
   WRITE (Outtap,99005)
99005 FORMAT (52H POOL FILE CONTENTS   EQ    SIZE   FILE   DATA BLOCK)
   ii = Dpd(3)*3
   DO ix = 1 , ii , 3
      iprt(1) = rshift(Dfnu(ix),Nbpw-1)
      iprt(2) = rshift(Dfnu(ix),16)
      iprt(3) = andf(Rxmsk,Dfnu(ix))
      iprt(4) = Ddbn(ix)
      iprt(5) = Ddbn(ix+1)
      CALL page2(-1)
      WRITE (Outtap,99006) (iprt(iy),iy=1,5)
99006 FORMAT (22X,I2,I7,I7,3X,2A4)
   ENDDO
   CALL dbmdia
   CALL dbmstf
!
   GOTO itest
!
 700  j = Mch
   IF ( iabs(Ibnk(Entn1*5))/1000/=j .AND. j>6 ) Comm(4) = j
   X = Cursno
   nsfa(3) = iend
   CALL conmsg(nsfa,3,0)
   RETURN
!
!     MODULE ALLOCATION INCOMPLETE
!
 800  IF ( i/=1 ) GOTO 400
   IF ( itpflg==0 ) THEN
      Cursno = 0
      GOTO 500
   ELSE
!
!     LOOKING FOR A TAPE + AT LEAST ONE TAPE EXISTS
!
      noaval = 0
      DO m = 1 , lmt8 , Entn1
         IF ( andf(Tapmsk,File(m))/=0 ) THEN
            IF ( andf(Tapmsk,Fntu(m))==0 ) GOTO 850
            noaval = 1
         ENDIF
      ENDDO
      IF ( noaval==0 ) THEN
         Cursno = 0
         GOTO 500
      ELSE
         Tapmsk = 0
!
         Bff = 0
         GOTO 200
      ENDIF
!
!     A TAPE FILE EXIST CONTAINING A D.B. NOT REQUIRING A TAPE  -
!     FREE THAT TAPE***  CHECK FOR EQUIV AND LTU D.B. ON SAME UNIT
!
 850  n = 1
!
      ASSIGN 900 TO itest
      GOTO 600
   ENDIF
 900  ASSIGN 700 TO itest
!
   trial = andf(Rmsk,File(m))
   lmt = lmt8 + 1
   DO j = lmt , lmt9 , Entn1
      IF ( trial==andf(Rmsk,File(j)) ) THEN
         inam1 = Fdbn(j)
         inam2 = Fdbn(j+1)
         IF ( Fequ(m)<0 .AND. Fequ(j)<0 ) THEN
            n = n + 1
         ELSE
            Fdbn(j) = Almsk
         ENDIF
         DO l = lmt2 , Lmt3 , Entn2
            IF ( inam1==Sdbn(l) .AND. inam2==Sdbn(l+1) ) GOTO 950
         ENDDO
      ENDIF
      CYCLE
!
!     TURN OFF ALLOC FLAG
!
 950  Sal(l) = orf(Almsk,Sal(l))
      alcnt = alcnt - 1
   ENDDO
   inam1 = Fdbn(m)
   inam2 = Fdbn(m+1)
   DO l = lmt2 , Lmt3 , Entn2
      IF ( inam1==Sdbn(l) .AND. inam2==Sdbn(l+1) ) GOTO 1000
   ENDDO
   GOTO 1100
 1000 Sal(l) = orf(Almsk,Sal(l))
   alcnt = alcnt - 1
 1100 Fpun(m) = orf(S,n)
   CALL xpunp
   Fdbn(m) = Sdbn(k)
   Fdbn(m+1) = Sdbn(k+1)
   Ford(m) = orf(andf(Lxmsk,Sord(k)),andf(Rxmsk,File(m)))
   Fknd(m) = 8
!
   CALL sswtch(2,ix)
   IF ( ix==1 ) THEN
      CALL page2(-2)
      WRITE (Outtap,99007)
99007 FORMAT (38H0* XSFA REPEATS TO USE FREED TAPE FILE)
   ENDIF
   Bff = 0
   GOTO 200
!
 1200 WRITE (Outtap,99008) Sfm
99008 FORMAT (A25,' 1001, OSCAR NOT FOUND IN DPL')
   GOTO 1500
 1300 WRITE (Outtap,99009) Sfm
99009 FORMAT (A25,' 1003, POOL COULD NOT BE OPENED')
   GOTO 1500
 1400 WRITE (Outtap,99010) Sfm
99010 FORMAT (A25,' 1004, ILLEGAL EOF ON POOL')
 1500 CALL mesage(-37,0,nsfa)
END SUBROUTINE xsfa