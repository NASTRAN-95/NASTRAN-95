!*==ifp.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ifp
!
   IMPLICIT NONE
   USE C_BLANK
   USE C_IFPDTA
   USE C_IFPX0
   USE C_IFPX1
   USE C_IFPX2
   USE C_IFPX3
   USE C_IFPX4
   USE C_IFPX5
   USE C_IFPX6
   USE C_IFPX7
   USE C_SYSTEM
   USE C_TWO
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(12) , SAVE :: ap
   INTEGER , SAVE :: blank , eofz , icount , iparm , it1k , it2k , ivary , jcount , jt1k , jt2k , kcount , kt1k , kt2k , ncdsmx ,   &
                   & nfls
   LOGICAL :: cf , cl , eofflg
   INTEGER :: curfil , eid , eidm1 , iap , iaxic , iaxif , ifail , ifile , iflag , ileft , index , indx , ipm , ipn , ipvs , irept ,&
            & istrt , iumfed , j , jap , jf , jj , jj1 , jmax , jmin , jpm , jpn , k1 , k2 , kb , kdum , kerror , kfil , kick , kk ,&
            & kkk , knt1 , kt721 , l , l1 , l27 , l42 , lf , line , lm , mn , nidsm1 , nnn , nw , nwds
   INTEGER , DIMENSION(2,16) , SAVE :: fnm
   INTEGER , DIMENSION(2) , SAVE :: iblkda , ifpna1 , ifpna2 , itype1 , itype2 , name
   INTEGER , DIMENSION(3) , SAVE :: iend
   INTEGER , DIMENSION(16) , SAVE :: ifle
   INTEGER , DIMENSION(16) :: ii , status
   INTEGER , DIMENSION(2) :: inam , itype , nm
   INTEGER , DIMENSION(7) :: itrl
   INTEGER , DIMENSION(20) :: jr
   INTEGER , DIMENSION(4) , SAVE :: kap
   INTEGER , DIMENSION(40) , SAVE :: kkl , mentry
   INTEGER , DIMENSION(80) , SAVE :: nentry
   INTEGER , DIMENSION(5) , SAVE :: ooo
!
! End of declarations rewritten by SPAG
!
!
!     NCDS    = LENGTH OF T1
!     NCDSMX  = NO. OF CARD NAMES IN NASTRAN
!     T3(1,K) = THE GINO OUTPUT FILE NUMBER
!     T3(2,K) = THE APPROACH ACCEPTANCE FLAG
!     T4(1,K) = THE CONICAL SHELL PROBLEM FLAG
!     T4(2,K) = USED AS INTERNAL STORAGE WITHIN IFP
!     T5(1,K) = THE MIN NO. OF WORDS ALLOWED PER CARD
!               (MINUS MEANS OPEN-ENDED CARD)
!     T5(2,K) = THE MAX NO. OF WORDS ALLOWED PER CARD
!     T6(1,K) = THE FORMAT CHECK POINTER INTO F( )
!     T6(2,K) = FIELD 2 UNIQUENESS CHECK FLAG
!     T7(1,K) = LOCATE CODE
!     T7(2,K) = TRAILER BIT POSITION
!     F(T6(1,K)) = THE START OF THE FORMAT ACCEPTANCE STRING
!
!     T1(1,K),T1(2,K) = THE BCD CARD NAMES
!
   !>>>>EQUIVALENCE (N3(3),Iumfed) , (N2(9),Line)
   DATA ncdsmx/359/
   DATA nfls/16/
   DATA fnm(1,1) , fnm(2,1)/4HGEOM , 4H1   /
   DATA fnm(1,2) , fnm(2,2)/4HEPT  , 4H    /
   DATA fnm(1,3) , fnm(2,3)/4HMPT  , 4H    /
   DATA fnm(1,4) , fnm(2,4)/4HEDT  , 4H    /
   DATA fnm(1,5) , fnm(2,5)/4HDIT  , 4H    /
   DATA fnm(1,6) , fnm(2,6)/4HPVT  , 4H    /
   DATA fnm(1,7) , fnm(2,7)/4HDYNA , 4HMICS/
   DATA fnm(1,8) , fnm(2,8)/4HGEOM , 4H2   /
   DATA fnm(1,9) , fnm(2,9)/4HGEOM , 4H3   /
   DATA fnm(1,10) , fnm(2,10)/4HGEOM , 4H4   /
   DATA fnm(1,11) , fnm(2,11)/4HGEOM , 4H5   /
   DATA fnm(1,12) , fnm(2,12)/4HPOOL , 4H    /
   DATA fnm(1,13) , fnm(2,13)/4HFORC , 4HE   /
   DATA fnm(1,14) , fnm(2,14)/4HMATP , 4HOOL /
   DATA fnm(1,15) , fnm(2,15)/4HAXIC , 4H    /
   DATA fnm(1,16) , fnm(2,16)/4HIFPF , 4HILE /
   DATA ifle/201 , 202 , 203 , 204 , 205 , 4HNPTP , 207 , 208 , 209 , 210 , 211 , 4HPOOL , 213 , 214 , 215 , 216/
   DATA iend , eofz/3*2147483647 , 4HZZZZ/
   DATA kkl/48 , 49 , 50 , 67 , 71 , 75 , 68 , 72 , 76 , 11 , 10*0 , 45 , 46 , 44 , 41 , 250 , 260 , 39 , 42 , 121 , 34 , 37 , 43 , &
      & 31 , 7*0/
   DATA iblkda/4HBULK , 4HDATA/ , ooo/1HA , 1HB , 1HC , 1HD , 1HE/
   DATA blank/1H / , kap/0 , -1 , 1 , -1/
   DATA ifpna1/4HIFP  , 4HBEGN/ , ifpna2/4HIFP  , 4HEND /
   DATA iparm , ivary/4H1PAR , 4H1VAR/
   DATA icount , jcount , kcount/3*0/
   DATA it1k , it2k , jt1k , jt2k , kt1k , kt2k/1H  , 1H  , 1H  , 1H  , 1H  , 1H /
   DATA ap/4HDMAP , 4H     , 4H     , 4HDISP , 4HLACE , 4HMENT , 4HHEAT , 4H     , 4H     , 4HAERO , 4H     , 4H    /
   DATA mentry/3001 , 3701 , 3901 , 1201 , 401 , 801 , 1301 , 501 , 901 , 5201 , 10*0 , 202 , 302 , 402 , 502 , 2202 , 5302 , 802 , &
      & 1002 , 2102 , 1302 , 1402 , 1702 , 1802 , 7*0/
   DATA name/4HIFP  , 4H    /
   DATA nentry/4HCROD , 4H     , 4HCTUB , 4HE    , 4HCVIS , 4HC    , 4HCMAS , 4HS3   , 4HCDAM , 4HP3   , 4HCELA , 4HS3   , 4HCMAS , &
       &4HS4   , 4HCDAM , 4HP4   , 4HCELA , 4HS4   , 4HPLOT , 4HEL   , 20*0 , 4HPDAM , 4HP    , 4HPELA , 4HS    , 4HPMAS , 4HS    , &
       &4HPQDM , 4HEM   , 4HPQDM , 4HEM1  , 4HPQDM , 4HEM2  , 4HPQUA , 4HD2   , 4HPSHE , 4HAR   , 4HPTOR , 4HDRG  , 4HPTRI ,        &
      & 4HA2   , 4HPTRM , 4HEM   , 4HPTWI , 4HST   , 4HPVIS , 4HC    , 14*0/
   DATA itype1/4HELEM , 4HENT /
   DATA itype2/4HPROP , 4HERTY/
!
!     ============================================================
!     REMEMBER TO CHECK FOR THE LONGEST LINK IN OVERLAY STRUCTURE.
!     ============================================================
!
!     INITIALIZE COMMON BLOCKS CIFS1P, 2P, 3P, 4P, AND CIFS5P
!
   CALL cifsdd
!
   DO j = 1 , 16
      status(j) = 1
   ENDDO
   status(6) = 3
   status(12) = 3
   lm = 100
   curfil = 0
   kick = 0
   ipvs = 0
   eofflg = .FALSE.
   Baddat = .FALSE.
   Badfor = .FALSE.
   Nparam = 0
   Kn = 0
   Iax = .FALSE.
   Nax = -1
   Iaxf = 0
   Naxf = -1
   Lharm = .TRUE.
   Kslot1 = 0
   Kslot2 = 0
   Kslot3 = 0
   Kslot4 = 0
   Kslot5 = 0
   CALL conmsg(ifpna1,2,0)
   iap = iabs(Iapp)
   jap = kap(iap)
   Knt = -1
   iaxic = Axiccc
   iaxif = Axifcc
   Axiccc = 0
   Axifcc = 0
   DO j = 1 , nfls
      ii(j) = 0
   ENDDO
   DO j = 1 , 40
      Endara(j) = 0
   ENDDO
   Nopen = korsz(Ibuff) - 3*N1
   CALL sswtch(42,l42)
   IF ( Nopen>=0 ) THEN
!
!     OPEN NPTP AND LOCATE BULK DATA
!
      kfil = ifle(6)
      CALL open(*100,kfil,Ibuff(N1+1),0)
      DO
         CALL skpfil(kfil,1)
         CALL read(*4400,*300,kfil,jr,2,1,kdum)
         IF ( jr(1)==iblkda(1) .AND. jr(2)==iblkda(2) ) THEN
            CALL read(*4300,*300,ifle(6),jr,20,1,kdum)
            Knt = Knt + 1
!
!     CHECK FOR 1PARM OR 1VARY CARDS
!
            IF ( jr(1)==iparm .OR. jr(1)==ivary ) CALL ifppvc(*400,ipvs,jr)
            IF ( l42==0 ) CALL rcard2(M1,M1f,nw,jr)
            IF ( l42/=0 ) CALL rcard(M1,M1f,nw,jr)
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) GOTO 700
            kerror = 1430
            GOTO 4500
         ELSE
            kick = kick + 1
            IF ( kick>=5 ) THEN
               CALL page2(2)
               WRITE (Nout,99001) Sfm , jr(1) , jr(2)
99001          FORMAT (A25,' 304, IFP NOT READING NPTP. FILE BEING READ = ',2A4)
               GOTO 200
            ENDIF
         ENDIF
      ENDDO
   ELSE
      CALL page2(2)
      WRITE (Nout,99002) Sfm
99002 FORMAT (A25,' 303, NO OPEN CORE FOR IFP.')
      Abort = .TRUE.
      RETURN
   ENDIF
 100  CALL page2(2)
   WRITE (Nout,99003) Sfm , kfil
99003 FORMAT (A25,' 305, IFP CANNOT OPEN GINO FILE',I10)
 200  Abort = .TRUE.
   GOTO 5300
 300  CALL page2(2)
   WRITE (Nout,99004) Sfm
99004 FORMAT (A25,' 306, READ LOGICAL RECORD ERROR')
   GOTO 200
 400  CALL close(ifle(6),1)
   GOTO 5400
!
!     READ AND DECODE ONE PHYSICAL CARD
!
 500  DO WHILE ( .NOT.(eofflg) )
      CALL read(*4600,*300,ifle(6),jr,20,1,kdum)
      Knt = Knt + 1
      IF ( l42==0 ) CALL rcard2(M1,M1f,nw,jr)
      IF ( l42/=0 ) CALL rcard(M1,M1f,nw,jr)
      IF ( M1(1)/=0 .OR. M1(2)/=0 ) GOTO 600
   ENDDO
   kerror = 1410
   GOTO 4500
 600  IF ( eofflg ) GOTO 4600
!
!     IDENTIFY CARD NAME
!
 700  DO j = 1 , ncdsmx
      K = j
      IF ( M1(1)==T1(1,K) .AND. M1(2)==T1(2,K) ) GOTO 800
   ENDDO
   IF ( kt1k/=T1(1,K) .OR. kt2k/=T1(2,K) ) THEN
      kt1k = T1(1,K)
      kt2k = T1(2,K)
   ELSE
      kcount = kcount + 1
      IF ( kcount>=7 ) THEN
         IF ( kcount==7 ) THEN
            CALL page2(3)
            WRITE (Nout,99024)
         ENDIF
         GOTO 500
      ENDIF
   ENDIF
   CALL page2(2)
   WRITE (Nout,99005) Ufm , M1(1) , M1(2)
99005 FORMAT (A23,' 307, ILLEGAL NAME FOR BULK DATA CARD ',2A4)
   Abort = .TRUE.
   GOTO 500
 800  kcount = 0
   cl = .FALSE.
   cf = .TRUE.
   Kx = K - 100
   Ky = Kx - 100
!
!     CHECK APPROACH ACCEPTABILITY
!
   IF ( T3(2,K)*jap+1<0 ) THEN
      WRITE (Nout,99006) Ufm , T1(1,K) , T1(2,K) , ap(3*iap-2) , ap(3*iap-1) , ap(3*iap)
99006 FORMAT (A23,' 308, CARD ',2A4,' NOT ALLOWED IN ',3A4,' APPROACH.')
      CALL page2(2)
      Abort = .TRUE.
   ELSEIF ( T3(2,K)*jap+1==0 ) THEN
      WRITE (Nout,99007) Uwm , T1(1,K) , T1(2,K) , ap(3*iap-2) , ap(3*iap-1) , ap(3*iap)
99007 FORMAT (A25,' 309, CARD ',2A4,' IMPROPER IN ',3A4,' APPROACH.')
      CALL page2(2)
   ENDIF
   IF ( .NOT.(.NOT.Iax .OR. T4(1,K)>=0) ) THEN
      CALL page2(2)
      WRITE (Nout,99008) Ufm , T1(1,K) , T1(2,K)
99008 FORMAT (A23,' 310, CARD ',2A4,' NOT ALLOWED IN SAME DECK WITH ','AXIC CARD.')
      Abort = .TRUE.
   ENDIF
!
!     ESTABLISH PROPER OUTPUT FILES FOR THIS CARD
!
   indx = T3(1,K)
   IF ( indx/=curfil .AND. indx/=6 ) THEN
      IF ( curfil/=0 .AND. status(curfil)/=1 ) THEN
         CALL close(ifle(curfil),2)
         status(curfil) = 3
      ENDIF
      kfil = ifle(indx)
      CALL open(*100,kfil,Ibuff,status(indx))
      curfil = indx
      status(curfil) = -status(curfil)
      IF ( status(curfil)==-1 ) THEN
         CALL write(ifle(curfil),fnm(1,curfil),2,1)
         ii(curfil) = 1
         status(curfil) = -3
      ENDIF
   ENDIF
   Id = M1(3)
 900  jf = nw - 2
   DO l = jf , lm
      M(l) = 0
   ENDDO
   DO l = 1 , jf
      M(l) = M1(l+2)
   ENDDO
!
!     TEST UNIQUENESS OF FIELD 2 IF APPLICABLE
!
   IF ( .NOT.(M1(1)==0 .AND. M1(2)==0 .OR. cf .OR. T6(2,K)/=1) ) THEN
      IF ( Id==M(1) ) THEN
         knt1 = Knt + 1
         CALL page2(2)
         WRITE (Nout,99009) Ufm , T1(1,K) , T1(2,K) , M(1) , knt1
99009    FORMAT (A23,' 311, NON-UNIQUE FIELD 2 ON BULK DATA CARD ',2A4,I8,10X,'H SORTED CARD COUNT =',I7)
         Abort = .TRUE.
      ELSE
         Id = M(1)
      ENDIF
   ENDIF
   DO l = 1 , lm
      Mf(l) = 0
   ENDDO
   lf = 0
   DO l = 1 , jf
!
!     =========================================
!     THIS SHOULD BE CHANGED WHEN RCARD CHANGES
!
      IF ( M1f(l+1)<0 ) EXIT
!     ========================================
      lf = lf + 1
      Mf(l) = M1f(l+1)
   ENDDO
   DO
      Mf(lf+1) = -32767
      DO WHILE ( .NOT.(eofflg) )
!
!     READ ANOTHER CARD (TO BE PROCESSED NEXT)
!
         Knt = Knt + 1
         CALL read(*1100,*300,ifle(6),jr,20,1,kdum)
         IF ( l42==0 ) CALL rcard2(M1,M1f,nw,jr)
         IF ( l42/=0 ) CALL rcard(M1,M1f,nw,jr)
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
            IF ( M1(1)/=T1(1,K) .OR. M1(2)/=T1(2,K) ) cl = .TRUE.
            GOTO 1200
         ELSE
!
!     CHECK FOR TOO MANY CONTINUATIONS
!
            IF ( T6(1,K)<0 .AND. lf>4 ) GOTO 1200
            IF ( jf+nw-2>lm ) THEN
               WRITE (Nout,99010) Ufm , T1(1,K) , T1(2,K) , M(1) , Knt
99010          FORMAT (A23,' 312, TOO MANY CONTINUATIONS FOR BULK DATA CARD ',2A4,I8,6X,'SORTED CARD COUNT =',I7)
               CALL page2(2)
               Abort = .TRUE.
            ELSE
               k1 = nw - 2
               DO l = 1 , k1
                  k2 = jf + l
                  M(k2) = M1(l+2)
               ENDDO
               jf = jf + nw - 2
               DO l = 1 , k1
!
!     =========================================
!     THIS SHOULD BE CHANGED WHEN RCARD CHANGES
!
                  IF ( M1f(l+1)<0 ) EXIT
!     =========================================
                  lf = lf + 1
                  Mf(lf) = M1f(l+1)
               ENDDO
               GOTO 1000
            ENDIF
         ENDIF
      ENDDO
      kerror = 1420
      GOTO 4500
 1000 ENDDO
 1100 eofflg = .TRUE.
   M1(1) = eofz
   M1(2) = eofz
   cl = .TRUE.
 1200 IF ( .NOT.(.NOT.cf .OR. T6(2,K)==2) ) THEN
      kkk = T3(1,K)
      ii(kkk) = ii(kkk) + 1
      cf = .FALSE.
      IF ( kkk==6 .OR. kkk==12 ) GOTO 1300
      itrl(1) = T7(1,K)
      itrl(2) = T7(2,K)
      itrl(3) = K
      CALL write(ifle(curfil),itrl,3,0)
   ENDIF
!
!     CHECK FOR MIN-MAX NO. OF WORDS
!
   IF ( T5(1,K)>=0 ) THEN
      l = jf
      IF ( T5(1,K)<l ) THEN
         DO
            IF ( T5(2,K)<l ) GOTO 1400
            IF ( T5(2,K)==l ) GOTO 1600
            l = l + 4
         ENDDO
      ELSEIF ( T5(1,K)==l ) THEN
         GOTO 1600
      ELSE
         GOTO 1400
      ENDIF
   ENDIF
 1300 l = -T5(1,K)
   IF ( jf>=l .AND. jf<=T5(2,K) ) GOTO 1600
 1400 WRITE (Nout,99011) Ufm , T1(1,K) , T1(2,K) , M(1) , Knt
99011 FORMAT (A23,' 313, ILLEGAL NUMBER OF WORDS ON BULK DATA CARD ',2A4,I8,6X,'SORTED CARD COUNT =',I7)
   WRITE (Nout,99012) T5(1,K) , T5(2,K) , K , l , jf
99012 FORMAT ('   T5(1&2,K),K,L,JF =',5I4)
   CALL page2(2)
   Abort = .TRUE.
   IF ( T6(1,K)<0 ) GOTO 1700
 1500 IF ( .NOT.cl ) GOTO 900
   IF ( T6(2,K)/=2 ) THEN
      CALL write(ifle(curfil),M,0,1)
      IF ( T4(2,K)<=0 ) THEN
         ii(kkk) = ii(kkk) - 1
         CALL bckrec(ifle(curfil))
      ENDIF
   ENDIF
   GOTO 600
!
!     CHECK FOR PROPER FORMAT
!
 1600 IF ( T6(1,K)>=0 ) THEN
      l = T6(1,K)
      l1 = 0
      DO k1 = 1 , lf
         l1 = l1 + 1
         IF ( Mf(k1)==3 ) l1 = l1 + 1
         k2 = l + k1 - 1
         IF ( F(k2)/=Mf(k1) .AND. F(k2)/=5 ) THEN
            IF ( Mf(k1)/=1 .OR. M(l1)/=0 ) THEN
               IF ( Mf(k1)/=0 .OR. F(k2)/=1 .AND. F(k2)/=2 ) GOTO 4100
            ENDIF
         ENDIF
      ENDDO
   ENDIF
 1700 N = 0
   Baddat = .FALSE.
   Badfor = .FALSE.
   IF ( ipvs/=0 ) CALL ifpmdc
!
!     CALL SECONDARY ROUTINE TO EXAMINE EACH TYPE OF CARD
!
   kb = (K-1)/20 + 1
   IF ( kb>18 ) GOTO 3500
   IF ( kb==2 ) THEN
   ELSEIF ( kb==3 ) THEN
      GOTO 1900
   ELSEIF ( kb==4 ) THEN
      GOTO 2000
   ELSEIF ( kb==5 ) THEN
      GOTO 2100
   ELSEIF ( kb==6 ) THEN
      GOTO 2200
   ELSEIF ( kb==7 ) THEN
      GOTO 2300
   ELSEIF ( kb==8 ) THEN
      GOTO 2400
   ELSEIF ( kb==9 ) THEN
      GOTO 2500
   ELSEIF ( kb==10 ) THEN
      GOTO 2600
   ELSEIF ( kb==11 ) THEN
      GOTO 2700
   ELSEIF ( kb==12 ) THEN
      GOTO 2800
   ELSEIF ( kb==13 ) THEN
      GOTO 2900
   ELSEIF ( kb==14 ) THEN
      GOTO 3000
   ELSEIF ( kb==15 ) THEN
      GOTO 3100
   ELSEIF ( kb==16 ) THEN
      GOTO 3200
   ELSEIF ( kb==17 ) THEN
      GOTO 3300
   ELSEIF ( kb==18 ) THEN
      GOTO 3400
   ELSE
      kb = K
      IF ( kb==1 .OR. kb==2 .OR. kb==12 .OR. kb==13 .OR. kb==17 ) THEN
         CALL ifs3p(*4200,*1500,*3600)
      ELSEIF ( kb==3 ) THEN
         CALL ifs5p(*4200,*1500,*3600)
      ELSEIF ( kb==4 .OR. kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==8 .OR. kb==9 .OR. kb==10 .OR. kb==11 .OR. kb==14 .OR. kb==15 .OR.    &
             & kb==16 .OR. kb==18 .OR. kb==19 .OR. kb==20 ) THEN
         CALL ifs1p(*4200,*1500,*3600)
      ELSE
         GOTO 1800
      ENDIF
      GOTO 3800
   ENDIF
 1800 kb = K - 20
   IF ( kb==1 .OR. kb==2 .OR. kb==3 .OR. kb==4 .OR. kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==9 .OR. kb==10 .OR. kb==11 .OR. kb==13 .OR. &
      & kb==14 .OR. kb==15 .OR. kb==16 .OR. kb==17 .OR. kb==18 .OR. kb==19 .OR. kb==20 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSEIF ( kb==8 ) THEN
      CALL ifs3p(*4200,*1500,*3600)
   ELSEIF ( kb==12 ) THEN
      CALL ifs5p(*4200,*1500,*3600)
   ELSE
      GOTO 1900
   ENDIF
   GOTO 3800
 1900 kb = K - 40
   IF ( kb==1 .OR. kb==2 .OR. kb==3 .OR. kb==4 .OR. kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==8 .OR. kb==9 .OR. kb==10 .OR. kb==12 .OR.  &
      & kb==13 .OR. kb==14 .OR. kb==15 .OR. kb==16 .OR. kb==17 .OR. kb==18 .OR. kb==19 .OR. kb==20 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSEIF ( kb==11 ) THEN
      CALL ifs5p(*4200,*1500,*3600)
   ELSE
      GOTO 2000
   ENDIF
   GOTO 3800
 2000 kb = K - 60
   IF ( kb==1 .OR. kb==2 .OR. kb==3 .OR. kb==4 .OR. kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==8 .OR. kb==9 .OR. kb==10 .OR. kb==11 .OR.  &
      & kb==12 .OR. kb==13 .OR. kb==14 .OR. kb==15 .OR. kb==16 .OR. kb==17 .OR. kb==18 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSEIF ( kb==19 .OR. kb==20 ) THEN
      CALL ifs4p(*4200,*1500,*3600)
   ELSE
      GOTO 2100
   ENDIF
   GOTO 3800
 2100 kb = K - 80
   IF ( kb==1 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSEIF ( kb==2 .OR. kb==3 .OR. kb==4 .OR. kb==12 ) THEN
      CALL ifs3p(*4200,*1500,*3600)
   ELSEIF ( kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==9 .OR. kb==13 .OR. kb==14 .OR. kb==15 .OR. kb==16 .OR. kb==17 ) THEN
      CALL ifs2p(*4200,*1500,*3600)
   ELSEIF ( kb==8 .OR. kb==19 .OR. kb==20 ) THEN
      CALL ifs5p(*4200,*1500,*3600)
   ELSEIF ( kb==10 .OR. kb==11 .OR. kb==18 ) THEN
      CALL ifs4p(*4200,*1500,*3600)
   ELSE
      GOTO 2200
   ENDIF
   GOTO 3800
 2200 kb = K - 100
   IF ( kb==1 .OR. kb==3 .OR. kb==6 .OR. kb==7 .OR. kb==8 .OR. kb==9 .OR. kb==10 .OR. kb==11 .OR. kb==12 .OR. kb==13 .OR.           &
      & kb==14 .OR. kb==15 .OR. kb==16 .OR. kb==17 .OR. kb==18 ) THEN
      CALL ifs5p(*4200,*1500,*3600)
   ELSEIF ( kb==2 .OR. kb==4 .OR. kb==5 ) THEN
      CALL ifs4p(*4200,*1500,*3600)
   ELSEIF ( kb==19 .OR. kb==20 ) THEN
      CALL ifs2p(*4200,*1500,*3600)
   ELSE
      GOTO 2300
   ENDIF
   GOTO 3800
 2300 kb = K - 120
   IF ( kb==1 .OR. kb==5 .OR. kb==7 .OR. kb==8 .OR. kb==9 .OR. kb==10 .OR. kb==15 .OR. kb==16 .OR. kb==17 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSEIF ( kb==2 .OR. kb==4 ) THEN
      CALL ifs4p(*4200,*1500,*3600)
   ELSEIF ( kb==3 .OR. kb==6 .OR. kb==11 .OR. kb==12 .OR. kb==18 .OR. kb==19 ) THEN
      CALL ifs3p(*4200,*1500,*3600)
   ELSEIF ( kb==13 .OR. kb==14 .OR. kb==20 ) THEN
      CALL ifs2p(*4200,*1500,*3600)
   ELSE
      GOTO 2400
   ENDIF
   GOTO 3800
 2400 kb = K - 140
   IF ( kb==1 ) THEN
      CALL ifs2p(*4200,*1500,*3600)
   ELSEIF ( kb==2 .OR. kb==18 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSEIF ( kb==3 .OR. kb==4 .OR. kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==8 .OR. kb==9 .OR. kb==10 .OR. kb==11 .OR. kb==12 .OR.        &
          & kb==13 .OR. kb==14 .OR. kb==15 .OR. kb==16 .OR. kb==17 ) THEN
      CALL ifs3p(*4200,*1500,*3600)
   ELSEIF ( kb==19 .OR. kb==20 ) THEN
      CALL ifs5p(*4200,*1500,*3600)
   ELSE
      GOTO 2500
   ENDIF
   GOTO 3800
 2500 kb = K - 160
   IF ( kb==1 .OR. kb==3 .OR. kb==4 .OR. kb==5 .OR. kb==7 .OR. kb==8 .OR. kb==9 .OR. kb==10 .OR. kb==11 .OR. kb==12 .OR. kb==13 .OR.&
      & kb==14 .OR. kb==15 .OR. kb==16 .OR. kb==17 .OR. kb==18 ) THEN
      CALL ifs5p(*4200,*1500,*3600)
   ELSEIF ( kb==2 ) THEN
      CALL ifs2p(*4200,*1500,*3600)
   ELSEIF ( kb==6 .OR. kb==19 .OR. kb==20 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSE
      GOTO 2600
   ENDIF
   GOTO 3800
 2600 kb = K - 180
   IF ( kb==1 .OR. kb==10 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSEIF ( kb==2 .OR. kb==3 .OR. kb==4 .OR. kb==5 ) THEN
      CALL ifs3p(*4200,*1500,*3600)
   ELSEIF ( kb==6 .OR. kb==7 .OR. kb==13 .OR. kb==14 .OR. kb==18 ) THEN
      CALL ifs5p(*4200,*1500,*3600)
   ELSEIF ( kb==8 .OR. kb==11 .OR. kb==12 .OR. kb==20 ) THEN
      CALL ifs2p(*4200,*1500,*3600)
   ELSEIF ( kb==9 .OR. kb==15 .OR. kb==16 .OR. kb==19 ) THEN
      CALL ifs4p(*4200,*1500,*3600)
   ELSEIF ( kb==17 ) THEN
      GOTO 3500
   ELSE
      GOTO 2700
   ENDIF
   GOTO 3800
 2700 kb = K - 200
   IF ( kb==1 .OR. kb==2 .OR. kb==3 .OR. kb==4 .OR. kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==8 .OR. kb==9 .OR. kb==10 .OR. kb==11 .OR.  &
      & kb==12 .OR. kb==13 .OR. kb==14 .OR. kb==17 .OR. kb==18 .OR. kb==19 .OR. kb==20 ) THEN
      CALL ifs4p(*4200,*1500,*3600)
   ELSEIF ( kb==15 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSEIF ( kb==16 ) THEN
      CALL ifs3p(*4200,*1500,*3600)
   ELSE
      GOTO 2800
   ENDIF
   GOTO 3800
 2800 kb = K - 220
   IF ( kb==1 .OR. kb==2 .OR. kb==19 ) THEN
      CALL ifs4p(*4200,*1500,*3600)
   ELSEIF ( kb==3 .OR. kb==4 .OR. kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==8 .OR. kb==9 .OR. kb==10 .OR. kb==11 .OR. kb==12 .OR.        &
          & kb==13 .OR. kb==14 .OR. kb==15 .OR. kb==16 .OR. kb==17 .OR. kb==18 .OR. kb==20 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSE
      GOTO 2900
   ENDIF
   GOTO 3800
 2900 kb = K - 240
   IF ( kb==1 .OR. kb==3 .OR. kb==9 .OR. kb==10 .OR. kb==16 .OR. kb==17 .OR. kb==18 .OR. kb==19 .OR. kb==20 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSEIF ( kb==2 ) THEN
      CALL ifs4p(*4200,*1500,*3600)
   ELSEIF ( kb==4 ) THEN
      CALL ifs3p(*4200,*1500,*3600)
   ELSEIF ( kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==8 .OR. kb==11 .OR. kb==12 .OR. kb==13 .OR. kb==14 .OR. kb==15 ) THEN
      CALL ifs5p(*4200,*1500,*3600)
   ELSE
      GOTO 3000
   ENDIF
   GOTO 3800
 3000 kb = K - 260
   IF ( kb==1 .OR. kb==2 .OR. kb==20 ) THEN
      CALL ifs2p(*4200,*1500,*3600)
   ELSEIF ( kb==3 .OR. kb==4 .OR. kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==9 .OR. kb==10 .OR. kb==11 .OR. kb==12 .OR. kb==15 .OR.       &
          & kb==16 .OR. kb==17 .OR. kb==18 ) THEN
      CALL ifs5p(*4200,*1500,*3600)
   ELSEIF ( kb==8 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSEIF ( kb==13 .OR. kb==14 .OR. kb==19 ) THEN
      CALL ifs3p(*4200,*1500,*3600)
   ELSE
      GOTO 3100
   ENDIF
   GOTO 3800
 3100 kb = K - 280
   IF ( kb==1 .OR. kb==2 .OR. kb==3 ) THEN
      CALL ifs2p(*4200,*1500,*3600)
   ELSEIF ( kb==4 .OR. kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==8 .OR. kb==10 .OR. kb==17 .OR. kb==18 ) THEN
      CALL ifs3p(*4200,*1500,*3600)
   ELSEIF ( kb==9 .OR. kb==11 .OR. kb==12 .OR. kb==13 .OR. kb==14 .OR. kb==19 .OR. kb==20 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSEIF ( kb==15 .OR. kb==16 ) THEN
      CALL ifs4p(*4200,*1500,*3600)
   ELSE
      GOTO 3200
   ENDIF
   GOTO 3800
 3200 kb = K - 300
   IF ( kb==1 .OR. kb==2 .OR. kb==3 .OR. kb==4 .OR. kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==8 .OR. kb==9 .OR. kb==10 .OR. kb==11 .OR.  &
      & kb==12 .OR. kb==13 .OR. kb==14 ) THEN
      CALL ifs5p(*4200,*1500,*3600)
   ELSEIF ( kb==15 .OR. kb==16 .OR. kb==17 .OR. kb==18 .OR. kb==19 .OR. kb==20 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSE
      GOTO 3300
   ENDIF
   GOTO 3800
 3300 kb = K - 320
   IF ( kb==1 .OR. kb==2 .OR. kb==3 .OR. kb==4 .OR. kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==8 .OR. kb==13 .OR. kb==14 .OR. kb==15 .OR. &
      & kb==16 ) THEN
      CALL ifs4p(*4200,*1500,*3600)
   ELSEIF ( kb==9 .OR. kb==10 .OR. kb==12 ) THEN
      CALL ifs3p(*4200,*1500,*3600)
   ELSEIF ( kb==11 .OR. kb==17 .OR. kb==20 ) THEN
      CALL ifs1p(*4200,*1500,*3600)
   ELSEIF ( kb==18 .OR. kb==19 ) THEN
      CALL ifs5p(*4200,*1500,*3600)
   ELSE
      GOTO 3400
   ENDIF
   GOTO 3800
 3400 kb = K - 340
   IF ( kb==5 .OR. kb==6 .OR. kb==7 .OR. kb==8 .OR. kb==9 .OR. kb==10 .OR. kb==11 .OR. kb==12 .OR. kb==13 ) THEN
      CALL ifs3p(*4200,*1500,*3600)
   ELSEIF ( kb==14 ) THEN
      CALL ifs2p(*4200,*1500,*3600)
   ELSEIF ( kb==15 .OR. kb==20 ) THEN
      GOTO 3500
   ELSE
      CALL ifs1p(*4200,*1500,*3600)
   ENDIF
   GOTO 3800
 3500 CALL page2(2)
   WRITE (Nout,99013) Sfm , K
99013 FORMAT (A25,' 314, INVALID CALL FROM IFP.  K =',I10)
   Abort = .TRUE.
   GOTO 5300
!
 3600 IF ( .NOT.Badfor ) THEN
      IF ( .NOT.Baddat ) icount = 0
   ELSE
      IF ( it1k/=T1(1,K) .OR. it2k/=T1(2,K) ) THEN
         it1k = T1(1,K)
         it2k = T1(2,K)
      ELSE
         icount = icount + 1
         IF ( icount>=7 ) THEN
            IF ( icount==7 ) THEN
               CALL page2(3)
               WRITE (Nout,99024)
            ENDIF
            GOTO 3700
         ENDIF
      ENDIF
      CALL page2(2)
      IF ( Id==0 ) Id = M(1)
      WRITE (Nout,99025) Ufm , T1(1,K) , T1(2,K) , Id , Knt
   ENDIF
 3700 IF ( .NOT.Baddat ) THEN
      IF ( .NOT.Badfor ) jcount = 0
   ELSE
      IF ( jt1k/=T1(1,K) .OR. jt2k/=T1(2,K) ) THEN
         jt1k = T1(1,K)
         jt2k = T1(2,K)
      ELSE
         jcount = jcount + 1
         IF ( jcount>=7 ) THEN
            IF ( jcount==7 ) THEN
               CALL page2(3)
               WRITE (Nout,99024)
            ENDIF
            GOTO 3800
         ENDIF
      ENDIF
      CALL page2(2)
      IF ( Id==0 ) Id = M(1)
      WRITE (Nout,99026) Ufm , T1(1,K) , T1(2,K) , Id , Knt
   ENDIF
 3800 IF ( .NOT.(.NOT.Badfor .AND. .NOT.Baddat) ) THEN
      N = 0
      Abort = .TRUE.
!
!     WRITE OUT CARD DATA ON APPROPRIATE IFP OUTPUT FILE
!
   ELSEIF ( N/=0 ) THEN
      T4(2,K) = T4(2,K) + N
      DO l = 1 , 40
         IF ( K==kkl(l) ) GOTO 3900
      ENDDO
      IF ( indx/=6 .AND. .NOT.Abort .OR. indx==15 ) CALL write(ifle(curfil),I,N,0)
   ENDIF
   GOTO 4000
 3900 CALL write(ifle(curfil),I,N,0)
 4000 IF ( Kn==0 ) GOTO 1500
   Kn = 0
   GOTO 900
 4100 Badfor = .TRUE.
 4200 IF ( Badfor ) THEN
      CALL page2(2)
      WRITE (Nout,99025) Ufm , T1(1,K) , T1(2,K) , M(1) , Knt
      Abort = .TRUE.
   ENDIF
   IF ( Baddat ) THEN
      CALL page2(2)
      WRITE (Nout,99026) Ufm , T1(1,K) , T1(2,K) , M(1) , Knt
      Abort = .TRUE.
      GOTO 1500
   ENDIF
 4300 IF ( Iapp==1 ) GOTO 5300
   IF ( Isubs/=0 ) GOTO 5300
 4400 WRITE (Nout,99014) Sfm
99014 FORMAT (A25,' 319, IFP READING EOF ON NPTP.')
   CALL page2(2)
   Abort = .TRUE.
   GOTO 5300
 4500 CALL page2(6)
   WRITE (Nout,99015) Sfm , kerror , (jr(l),l=1,20) , Knt
99015 FORMAT (A25,' 320, IFP ERROR',I5,/5X,'LAST CARD PROCESSED IS -',20A4,1H-,/5X,'SORTED CARD COUNT =',I7)
   Abort = .TRUE.
   GOTO 5300
 4600 IF ( curfil/=0 ) CALL close(ifle(curfil),2)
   DO l = 1 , nfls
      IF ( l/=6 .AND. l/=12 .AND. status(l)/=1 ) THEN
         kfil = ifle(l)
         CALL open(*100,kfil,Ibuff,3)
         CALL write(ifle(l),iend,3,1)
         ii(l) = ii(l) + 1
         CALL close(ifle(l),1)
      ENDIF
   ENDDO
!
!     CHECK TO SEE IF ALL MULTI-ENTRY CARD DATA (CROD, CTUBE, ETC.)
!     ARE SORTED ON THEIR ELEMENT/PROPERTY IDS
!
   DO l = 1 , 40
      IF ( Endara(l)<0 ) GOTO 4700
   ENDDO
!
!     EITHER NO MULTI-ENTRY CARD DATA EXIST OR, IF THEY DO,
!     THEY ARE ALL SORTED ON THEIR ELEMENT/PROPERTY IDS
!
   GOTO 5200
!
!     NOT ALL MULTI-ENTRY CARD DATA ARE SORTED ON THEIR
!     ELEMENT/PROPERTY IDS.
!
!     CLOSE SCRATCH FILE (FILE 6) AT CURRENT POSITION WITHOUT REWIND
!     AND WITHOUT END-OF-FILE.
!
 4700 CALL close(ifle(6),2)
!
!     READ DATA FROM GEOM2/EPT FILE, SORT ALL MULTI-ENTRY CARD DATA ON
!     THEIR ELEMENT/PROPERTY IDS AND WRITE THE RESULTING DATA ON
!     SCRATCH FILE (FILE 16)
!
!     NOTE.  GEOM2 IS IFLE(8) AND EPT IS IFLE(2)
!
   DO nnn = 1 , 2
      IF ( nnn==2 ) THEN
         ifile = ifle(2)
         inam(1) = fnm(1,2)
         inam(2) = fnm(2,2)
         itype(1) = itype2(1)
         itype(2) = itype2(2)
         jmin = 21
         jmax = 40
      ELSE
         ifile = ifle(8)
         inam(1) = fnm(1,8)
         inam(2) = fnm(2,8)
         itype(1) = itype1(1)
         itype(2) = itype1(2)
         jmin = 1
         jmax = 20
      ENDIF
      DO l = jmin , jmax
         IF ( Endara(l)<0 ) GOTO 4750
      ENDDO
      CYCLE
 4750 ileft = Nopen - Nparam - 2
      istrt = 2*N1 + Nparam + 2
      CALL gopen(ifile,Ibuff,0)
      kfil = ifle(16)
      CALL open(*100,ifle(16),Ibuff(N1+1),1)
      CALL write(ifle(16),inam,2,1)
      index = jmin
 4800 CALL read(*5100,*5050,ifile,Ibuff(istrt),3,0,iflag)
      CALL write(ifle(16),Ibuff(istrt),3,0)
      IF ( index<=jmax ) THEN
         DO l = jmin , jmax
            IF ( Ibuff(istrt)==mentry(l) .AND. Endara(l)<0 ) GOTO 4900
         ENDDO
      ENDIF
      DO
         CALL read(*5000,*4850,ifile,Ibuff(istrt),ileft,0,iflag)
         CALL write(ifle(16),Ibuff(istrt),ileft,0)
      ENDDO
 4850 CALL write(ifle(16),Ibuff(istrt),iflag,1)
      GOTO 4800
 4900 index = index + 1
      CALL page2(3)
      WRITE (Nout,99016) Uim , nentry(2*l-1) , nentry(2*l) , itype
99016 FORMAT (A29,' 334, ',2A4,' MULTI-ENTRY CARD DATA ARE NOT SORTED ','ON THEIR ',2A4,' IDS.',/5X,                                &
             &'SUBROUTINE IFP WILL SORT THE DATA.')
      ifail = 0
      DO
         CALL read(*5000,*4950,ifile,Ibuff(istrt),ileft,0,iflag)
         ifail = ifail + 1
      ENDDO
 4950 IF ( ifail/=0 ) THEN
         nwds = (ifail-1)*ileft + iflag
         CALL page2(4)
         WRITE (Nout,99017) Ufm , nentry(2*l-1) , nentry(2*l) , nwds
99017    FORMAT (A23,' 333, UNABLE TO SORT ',2A4,' MULTI-ENTRY CARD DATA ','IN SUBROUTINE IFP DUE TO INSUFFICIENT CORE.',/5X,       &
                &'ADDITIONAL CORE REQUIRED =',I10,7H  WORDS)
         CALL mesage(-61,0,0)
      ENDIF
      nwds = 4
      IF ( l==10 .OR. l==33 ) nwds = 3
      IF ( l==21 .OR. l==23 ) nwds = 2
      CALL sort(0,0,nwds,1,Ibuff(istrt),iflag)
      CALL write(ifle(16),Ibuff(istrt),iflag,1)
!
!     CHECK SORTED MULTI-ENTRY CARD DATA FOR NON-UNIQUE
!     ELEMENT/PROPERTY IDS
!
      irept = -10000000
      nidsm1 = iflag/nwds - 1
      DO kk = 1 , nidsm1
         eid = Ibuff(istrt+kk*nwds)
         eidm1 = Ibuff(istrt+kk*nwds-nwds)
         IF ( eid==eidm1 ) THEN
            IF ( eid/=irept ) THEN
               irept = eid
               Abort = .TRUE.
               CALL page2(2)
               WRITE (Nout,99018) Ufm , itype , eid , nentry(2*l-1) , nentry(2*l)
99018          FORMAT (A23,' 335, NON-UNIQUE ',2A4,' ID',I9,' ENCOUNTERED IN ',2A4,' MULTI-ENTRY CARD DATA.')
            ENDIF
         ENDIF
      ENDDO
      GOTO 4800
 5000 CALL mesage(-2,ifile,name)
 5050 CALL mesage(-3,ifile,name)
 5100 CALL close(ifile,1)
      CALL close(ifle(16),1)
!
!     COPY DATA BACK FROM SCRATCH FILE (FILE 16) TO GEOM2/EPT FILE
!
      kfil = ifle(16)
      CALL open(*100,ifle(16),Ibuff,0)
      kfil = ifile
      CALL open(*100,ifile,Ibuff(N1+1),1)
      CALL cpyfil(ifle(16),ifile,Ibuff(istrt),ileft,iflag)
      CALL close(ifle(16),1)
      CALL close(ifile,1)
   ENDDO
!
!     RE-OPEN SCRATCH FILE (FILE 6) TO WRITE WITHOUT REWIND
!
   kfil = ifle(6)
   CALL open(*100,ifle(6),Ibuff(N1+1),3)
!
!     WRITE TRAILERS
!
 5200 DO j = 1 , nfls
      IF ( j/=6 .AND. j/=12 ) THEN
         DO l = 2 , 7
            itrl(l) = 0
         ENDDO
         itrl(1) = ifle(j)
         IF ( .NOT.(ii(j)<=2 .OR. Abort) ) THEN
            DO l = 1 , ncdsmx
               IF ( T3(1,l)==j .AND. T4(2,l)>0 ) THEN
                  kt721 = andf(T7(2,l),511)
                  k1 = (kt721-1)/16 + 2
                  k2 = kt721 - (k1-2)*16 + 16
                  itrl(k1) = orf(itrl(k1),Two(k2))
               ENDIF
            ENDDO
         ENDIF
         CALL wrttrl(itrl)
      ENDIF
   ENDDO
!
!     WRITE PARAM CARDS ON NPTP
!
   kfil = ifle(16)
   CALL ifppar
   IF ( .NOT.(Nparam<=0 .OR. Abort) ) THEN
      CALL open(*100,kfil,Ibuff,1)
      itrl(1) = kfil
      itrl(2) = Nparam
      CALL wrttrl(itrl(1))
      CALL write(kfil,fnm(1,6),2,1)
      CALL write(kfil,Ibuff(2*N1+1),Nparam,1)
      ipm = 1
      ipn = 2*N1 + ipm
      DO
         ipm = ipm + 4
         IF ( Ibuff(ipn+2)>2 ) ipm = ipm + 1
         IF ( Ibuff(ipn+2)>5 ) ipm = ipm + 2
         IF ( ipm<Nparam ) THEN
            ipn = 2*N1 + ipm
            nm(1) = Ibuff(ipn)
            nm(2) = Ibuff(ipn+1)
            jpm = 1
            DO
               jpn = 2*N1 + jpm
               IF ( nm(1)==Ibuff(jpn) .AND. nm(2)==Ibuff(jpn+1) ) THEN
                  CALL page2(2)
                  WRITE (Nout,99019) Ufm , nm(1) , nm(2)
99019             FORMAT (A23,' 321, NON-UNIQUE PARAM NAME - ',2A4,1H-)
                  Abort = .TRUE.
               ENDIF
               jpm = jpm + 4
               IF ( Ibuff(jpn+2)>2 ) jpm = jpm + 1
               IF ( Ibuff(jpn+2)>5 ) jpm = jpm + 2
               IF ( jpm>=ipm ) EXIT
            ENDDO
            CYCLE
         ELSE
            CALL eof(kfil)
            CALL close(kfil,1)
         ENDIF
         EXIT
      ENDDO
   ENDIF
 5300 CALL close(ifle(6),1)
!
!     CHECK FOR PROPERTY ID UNIQUENESS IN EPT FILE AND PROPERTY ID
!     SPECIFIED IN GEOM2 ELEMENTS
!
   CALL sswtch(34,jj1)
   IF ( jj1/=1 ) THEN
      kfil = ifle(2)
      itrl(1) = kfil
      CALL rdtrl(itrl)
      j = itrl(2) + itrl(3) + itrl(4) + itrl(5) + itrl(6) + itrl(7)
      jj1 = 1
      IF ( itrl(1)>=0 .AND. j/=0 ) THEN
         jj1 = 0
         CALL open(*100,kfil,Ibuff,0)
      ENDIF
      kfil = ifle(8)
      itrl(1) = kfil
      CALL rdtrl(itrl)
      j = itrl(2) + itrl(3) + itrl(4) + itrl(5) + itrl(6) + itrl(7)
      IF ( itrl(1)>=0 .AND. j/=0 ) THEN
         CALL open(*100,kfil,Ibuff(N1+1),0)
         jj = N1*2 + 1
         CALL pidck(ifle(2),kfil,jj1,Ibuff(jj))
         CALL close(kfil,1)
         IF ( jj1<0 ) THEN
         ELSEIF ( jj1==0 ) THEN
            IF ( Ibuff(jj)/=0 ) THEN
               jj1 = jj + Ibuff(jj) + 1
!
!     CHECK FOR MATERIAL ID UNIQUENESS IN MPT FILE
!     AND MATERIAL ID SPECIFIED IN PROPERTY CARDS
!
               kfil = ifle(3)
               itrl(1) = kfil
               CALL rdtrl(itrl)
               j = itrl(2) + itrl(3) + itrl(4) + itrl(5) + itrl(6) + itrl(7)
               Ibuff(jj1) = 1
               IF ( itrl(1)<0 .OR. j==0 ) Ibuff(jj1) = 0
               IF ( Ibuff(jj1)==1 ) CALL open(*100,kfil,Ibuff(N1+1),0)
               CALL matck(kfil,ifle(2),Ibuff(jj),Ibuff(jj1))
               IF ( Ibuff(jj1)/=0 ) CALL close(kfil,1)
            ENDIF
         ELSE
            GOTO 5350
         ENDIF
      ENDIF
      CALL close(ifle(2),1)
!
!     CHECK COORDINATE ID'S AND THEIR REFERENCES FROM
!     OTHER BULK DATA CARDS
!
 5350 jj = Nopen + N1 - 2
!                + N1 - 2 = 2*N1 - (N1+2)
      CALL cidck(Ibuff(N1+2),Ibuff,jj)
   ENDIF
!
!     CHECK FOR ERRORS IN AXISYMMETRIC DATA
!
 5400 IF ( Iax ) Axiccc = 1
   Axifcc = Iaxf
   IF ( Axiccc<=0 .OR. Axifcc<=0 ) THEN
      IF ( Axiccc>0 ) THEN
         IF ( iaxic>0 ) GOTO 5500
         Axiccc = 0
      ELSEIF ( Axifcc<=0 ) THEN
         IF ( iaxic>0 .OR. iaxif>0 ) THEN
            Axiccc = 0
            Axifcc = 0
!
!     SUPPRESS ABORT IF IT IS A UMFEDIT RUN
!
            IF ( iumfed==0 ) THEN
               Abort = .TRUE.
               CALL page2(2)
               WRITE (Nout,99020) Ufm
99020          FORMAT (A23,' 339, ILLEGAL USE OF AXISYMMETRIC CARD IN CASE ','CONTROL DECK.')
            ENDIF
         ENDIF
         GOTO 5500
      ELSE
         IF ( iaxif>0 .OR. Axifcc==2 ) GOTO 5500
         Axifcc = 0
      ENDIF
!
!     SUPPRESS ABORT IF IT IS A UMFEDIT RUN
!
      IF ( iumfed==0 ) THEN
         Abort = .TRUE.
         CALL page2(2)
         WRITE (Nout,99021) Ufm
99021    FORMAT (A23,' 338, AXISYMMETRIC CARD REQUIRED IN CASE CONTROL')
      ENDIF
   ELSE
      Axiccc = 0
      Axifcc = 0
      Abort = .TRUE.
      CALL page2(2)
      WRITE (Nout,99022) Ufm
99022 FORMAT (A23,' 337, BOTH AXIC AND AXIF CARDS USED IN BULK DATA.')
   ENDIF
!
 5500 IF ( Iapp<0 ) THEN
!
!     CHECK CERTAIN RESTART FLAGS BASED ON BULK DATA
!
      mn = Lbd + 1
!
!     TURN ON TEMPMX$ IF MATERIALS USE TEMPS
!
      IF ( T4(2,91)+T4(2,102)+T4(2,189)/=0 ) THEN
         IF ( andf(Ib(1),Two(28))/=0 .OR. andf(Ib(5),Two(32))/=0 .OR. andf(Ib(4),Two(6))/=0 .OR. andf(Ib(3),Two(32))/=0 .OR.        &
            & andf(Ib(4),Two(2))/=0 .OR. andf(Ib(4),Two(3))/=0 .OR. andf(Ib(4),Two(4))/=0 ) Ib(mn) = orf(Ib(mn),Two(19))
      ENDIF
   ENDIF
   CALL conmsg(ifpna2,2,0)
!
   CALL sswtch(27,l27)
   IF ( l27/=0 ) THEN
      CALL page1
      line = line + 8
      WRITE (Nout,99027)
      DO j = 1 , ncdsmx
         Id = T3(1,j)
         IF ( Id<=0 ) THEN
            lf = blank
            lm = blank
         ELSE
            lf = fnm(1,Id)
            lm = fnm(2,Id)
         ENDIF
         N = j
         K = N/90 + min0(1,mod(N,90))
         N = N - 90*(K-1)
         Kx = N/30 + min0(1,mod(N,30))
         N = N - 30*(Kx-1)
         Ky = N/6 + min0(1,mod(N,6))
         l = N - 6*(Ky-1)
         iflag = 0
         IF ( eject(1)/=0 ) THEN
            WRITE (Nout,99027)
            line = line + 8
         ENDIF
         line = line + 1
         WRITE (Nout,99023) j , T1(1,j) , T1(2,j) , T3(1,j) , lf , lm , T3(2,j) , T4(1,j) , T4(2,j) , T5(1,j) , T5(2,j) , T6(1,j) , &
                          & T6(2,j) , T7(1,j) , T7(2,j) , K , Kx , ooo(Ky) , l , iflag
99023    FORMAT (1H ,I4,1X,2A4,I4,1X,1H(,2A4,1H),I3,I5,I4,I4,I4,I6,I3,I7,I8,16X,I1,I1,A1,I1,4X,I2)
      ENDDO
   ENDIF
99024 FORMAT (31X,'.',/29X,'MORE',/31X,'.')
99025 FORMAT (A23,' 315, FORMAT ERROR ON BULK DATA CARD ',2A4,I8,17X,'SORTED CARD COUNT =',I7)
99026 FORMAT (A23,' 316, ILLEGAL DATA ON BULK DATA CARD ',2A4,I8,17X,'SORTED CARD COUNT =',I7)
99027 FORMAT ('0DIAG 27 DUMP OF IFP TABLES AFTER IFP PROCESSING',/,1H0,6X,6HIFX1BD,9X,6HIFX2BD,7X,6HIFX3BD,2X,6HIFX4BD,3X,6HIFX5BD, &
            & 6X,6HIFX6BD,/,1H ,5X,8(1H-),2X,17(1H-),2X,6(1H-),2X,6(1H-),2X,8(1H-),2X,12(1H-),/,1H ,1X,3H(A),3X,3H(B),5X,3H(C),3X,  &
             &3H(D),5X,3H(E),2X,3H(N),5X,3H(F),3H(G),3X,3H(H),1X,3H(I),3X,3H(J),5X,3H(K),4X,3H(L),3X,3H(M),4X,3H(O),3X,4HFLAG,/1H0)
!
END SUBROUTINE ifp
