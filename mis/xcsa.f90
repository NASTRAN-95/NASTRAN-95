
SUBROUTINE xcsa
!
!     XCSA READS AND PROCESSES THE NASTRAN EXECUTIVE CONTROL DECK.
!
   IMPLICIT NONE
   INTEGER Alnogo , Altfil , Apprch , Card(20) , Dmapbf(1) , Dum9(9) , Gbuff(1) , Ibot , Ibufsz , Icfiat , Icpflg , Idate(3) ,      &
         & Iecho , Ifiat(1) , Ifist(1) , Ijhalf(3) , Intape , Intra , Iptdic(1) , Irdict , Irestr , Iropen , Iseqno , Isubs , Itop ,&
         & Ixpfst , Ixxfat(1) , L13 , L15 , L8 , Ldic , Ldict , Links(15) , Logfl , Lu , Lxlink , Mach , Maxlnk , Mchnam , Mskdum(3)&
         & , Nbpc , Nbpw , Ncpw , Newalt , Nlines , Nlpp , Noecho , Nrlfl , Nsubst , Otapid(6) , Outtap , Pghdg1(32) , Pghdg2(32) , &
         & Pghdg3(32) , Pghdg4(32) , Pghdg5(32) , Pghdg6(32) , Prec , Rfflag , Switch(3) , Sy10 , Sy11 , Sy13 , Sy14 , Sy18 , Sy20 ,&
         & Sy22 , Sy23 , Sy26(11) , Sy38 , Sy42(13) , Sy5 , Sy56(13) , Sy6 , Sy70(9) , Sy8 , Sy83(2) , Sy85 , Sy87(5) , Tapid(6)
   CHARACTER*25 Sfm , Uwm
   INTEGER Time , Xnogo , Xsys(100) , Zcom
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /altrxx/ Altfil , Newalt , Alnogo
   COMMON /blank / Zcom , Card
   COMMON /l15l8 / L15 , L8 , L13
   COMMON /machin/ Mach , Ijhalf , Mchnam
   COMMON /output/ Pghdg1 , Pghdg2 , Pghdg3 , Pghdg4 , Pghdg5 , Pghdg6
   COMMON /resdic/ Irdict , Iropen
   COMMON /sem   / Mskdum , Links
   COMMON /stapid/ Tapid , Otapid
   COMMON /stime / Time
   COMMON /system/ Ibufsz , Outtap , Xnogo , Intape , Sy5 , Sy6 , Logfl , Sy8 , Nlpp , Sy10 , Sy11 , Nlines , Sy13 , Sy14 , Idate , &
                 & Sy18 , Iecho , Sy20 , Apprch , Sy22 , Sy23 , Icfiat , Rfflag , Sy26 , Lu , Sy38 , Nbpc , Nbpw , Ncpw , Sy42 ,    &
                 & Prec , Sy56 , Isubs , Sy70 , Switch , Icpflg , Sy83 , Sy85 , Intra , Sy87 , Ldict
   COMMON /xechox/ Dum9 , Noecho
   COMMON /xfiat / Ifiat
   COMMON /xfist / Ifist
   COMMON /xlink / Lxlink , Maxlnk
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xoldpt/ Itop , Ibot , Ldic , Nrlfl , Iseqno
   COMMON /xpfist/ Ixpfst
   COMMON /xrgdxx/ Irestr , Nsubst
   COMMON /xxfiat/ Ixxfat
   COMMON /zzzzzz/ Gbuff
   INTEGER allon , alter(2) , altopn , altrbs , appaer , appdmp , apphea , apprec , apptyp(4) , bgnal(2) , blank , bnk , both ,     &
         & cend(2) , delete(9) , diag09 , diagx(11) , dmapbs , dmend , dolsin , drecsz , ectt(51) , endal(2) , endcd , erralt ,     &
         & flags , flgwrd , hdg(19) , i , i7 , icold , icrdct , id , iday , idfist , idisk , ignore , ileft , imhere , imnth ,      &
         & inp9 , insert(4) , intgr , irtn1 , irtn2 , irtn3 , isign , isize , itopx , iufile(2) , ixdmap , iyear , iyes , iz(2) ,   &
         & j , jj , jk , jn , jnx , k , l , lectt , m7 , mask , mask5 , maskhi , mhibyt , msgnum , n7 , ngino , no , nogo , nostup ,&
         & notalt , nscr , nsolnm , nsubs , numapp , nwpc , nxcsa(2)
   INTEGER andf , complf , khrfn1 , korsz , lshift , orf , rshift
   INTEGER nxptdc(2) , oldalt , optape , osolu(2) , outcrd(200) , plot , prnt , ptape , reel , renter , rstrt , s7 , solnm1(7,10) , &
         & solnm2(7,10) , solnm3(7,11) , solnms(7,31) , solnmx(6) , solrec(6) , solu(12) , soluf , test , timew , timex , xalt(2)
   LOGICAL tapbit
   EXTERNAL andf , complf , lshift , orf , rshift
   EQUIVALENCE (Ibufsz,Xsys(1)) , (mask,maskhi) , (ectt(16),bgnal(1)) , (ectt(25),endal(1)) , (ectt(13),cend(1)) , (ectt(34),id) ,  &
    & (solrec(1),apprec) , (solrec(2),rstrt) , (solrec(3),alter(1)) , (solrec(5),solu(1)) , (Gbuff(1),Dmapbf(1),Iptdic(1)) ,        &
    & (solnms(1,1),solnm1(1,1)) , (solnms(1,11),solnm2(1,1)) , (solnms(1,21),solnm3(1,1))
   DATA apptyp/4HDMAP , 4HDISP , 4HHEAT , 4HAERO/
   DATA blank , ixdmap , nsubs , renter , dolsin/1H  , 4HXDMA , 4HSUBS , 4HREEN , 4H$   /
   DATA iyes , no , idisk , ptape , optape , dmend/4HYES  , 4HNO   , 4HDISK , 4HNPTP , 4HOPTP , 4HEND /
   DATA iufile , xalt , nxptdc , intgr/2*0 , 4HXALT , 4HER   , 4HXPTD , 4HC    , -1/
   DATA nxcsa , diagx/4HXCSA , 4H     , 4 , 9 , 14 , 17 , 23 , 24 , 25 , 28 , 29 , 30 , 31/
   DATA appdmp , apphea , appaer , numapp , solrec/1 , 3 , 4 , 4 , 0 , 1 , 0 , 0 , 0 , 0/
   DATA soluf , osolu , icold , ignore , outcrd/0 , 2*0 , 1 , 0 , 3 , 199*4H    /
   DATA plot , prnt , both , inp9 , notalt/4HPLOT , 4HPRIN , 4HBOTH , 4HINP9 , 0/
   DATA mask/32767/
!                     32767 = O77777 = 2**15-1 = MASK HI
   DATA lectt , ectt/51 , 4HTIME , 4H     , 0 , 4HAPP  , 4H     , 0 , 4HCHKP , 4HNT   , 0 , 4HREST , 4HART  , 0 , 4HCEND , 4H     , &
      & 0 , 4HALTE , 4HR    , 0 , 4HSOL  , 4H     , 0 , 4HBEGI , 4HN    , 0 , 4HENDA , 4HLTER , 0 , 4HDIAG , 4H     , 0 , 4HUMF  ,  &
       &4H     , 0 , 4HID   , 4H     , 1 , 4HUMFE , 4HDIT  , 0 , 4HPREC , 4H     , 0 , 4HINTE , 4HRACT , 0 , 4HINSE , 4HRT   , 0 ,  &
       &4HDELE , 4HTE   , 0/
   DATA ileft/4H(   /
   DATA altopn/0/
   DATA hdg/4HN A  , 4HS T  , 4HR A  , 4HN    , 4H E X , 4H E C , 4H U T , 4H I V , 4H E   , 4H  C  , 4HO N  , 4HT R  , 4HO L  ,    &
       &4H   D , 4H E C , 4H K   , 4H  E  , 4HC H  , 4HO   /
   DATA nsolnm/26/
   DATA solnm1/4HSTAT , 4HICS  , 4H     , 4H     , 4H     , 4H     , 1 , 4HINER , 4HTIA  , 4HRELI , 4HEF   , 4H     , 4H     , 2 ,  &
       &4HNORM , 4HAL   , 4HMODE , 4HS    , 4H     , 4H     , 3 , 4HDIFF , 4HEREN , 4HSTIF , 4HFNES , 4H     , 4H     , 4 , 4HBUCK ,&
       &4HLING , 4H     , 4H     , 4H     , 4H     , 5 , 4HPIEC , 4HEWIS , 4HLINE , 4HAR   , 4H     , 4H     , 6 , 4HDIRE , 4HCT   ,&
       &4HCOMP , 4HLEX  , 4HEIGE , 4HNVAL , 7 , 4HDIRE , 4HCT   , 4HFREQ , 4HUENC , 4HRESP , 4HONSE , 8 , 4HDIRE , 4HCT   , 4HTRAN ,&
       &4HSIEN , 4HRESP , 4HONSE , 9 , 4HMODA , 4HL    , 4HCOMP , 4HLEX  , 4HEIGE , 4HNVAL , 10/
   DATA solnm2/4HMODA , 4HL    , 4HFREQ , 4HUENC , 4HRESP , 4HONSE , 11 , 4HMODA , 4HL    , 4HTRAN , 4HSIEN , 4HRESP , 4HONSE , 12 ,&
       &4HSTEA , 4HDY   , 4HSTAT , 4HE    , 4H     , 4H     , 3 , 4HTRAN , 4HSIEN , 4H     , 4H     , 4H     , 4H     , 9 , 4HMODE ,&
       &4HS    , 4H     , 4H     , 4H     , 4H     , 3 , 4HREAL , 4H     , 4HEIGE , 4HNVAL , 4H     , 4H     , 3 , 4HMODA , 4HL    ,&
       &4HFLUT , 4HTER  , 4HANAL , 4HYSIS , 10 , 4HMODA , 4HL    , 4HAERO , 4HELAS , 4HRESP , 4HONSE , 11 , 4HNORM , 4HAL   ,       &
      & 4HMODE , 4HS    , 4HANAL , 4HYSIS , 13 , 4HSTAT , 4HICS  , 4HCYCL , 4HIC   , 4HSYMM , 4HETRY , 14/
   DATA solnm3/4HMODE , 4HS    , 4HCYCL , 4HIC   , 4HSYMM , 4HETRY , 15 , 4HSTAT , 4HIC   , 4HAERO , 4HTHER , 4HMOEL , 4HASTI , 16 ,&
       &4HBLAD , 4HE    , 4HCYCL , 4HIC   , 4HMODA , 4HL    , 9 , 4HDYNA , 4HMIC  , 4HDESI , 4HGN A , 4HNALY , 4HSIS  , 17 ,        &
      & 4HDIRE , 4HCT   , 4HFORC , 4HED V , 4HIBRA , 4HTION , 18 , 4HMODA , 4HAL   , 4HFORC , 4HED V , 4HIBRA , 4HTION , 19 ,       &
      & 4H**** , 4H**** , 4H**** , 4H**** , 4H**** , 4H**** , 0 , 4H**** , 4H**** , 4H**** , 4H**** , 4H**** , 4H**** , 0 , 4H**** ,&
       &4H**** , 4H**** , 4H**** , 4H**** , 4H**** , 0 , 4H**** , 4H**** , 4H**** , 4H**** , 4H**** , 4H**** , 0 , 4H**** , 4H**** ,&
       &4H**** , 4H**** , 4H**** , 4H**** , 0/
!
!     SET UP DATA IN COMMON
!
   Itop = 0
   Ibot = 0
   Ldic = 0
   Nrlfl = 0
   Iseqno = 0
   Altfil = 301
   Newalt = 0
   Alnogo = 0
   erralt = 0
   nscr = 315
   Irestr = 0
   Nsubst = 0
   nwpc = 18
   drecsz = 0
!
!
!     INITIALIZE MACHINE DEPENDENT CONSTANTS
!
!     ALLON  = O777777777777  ALL BITS ON
!     ISIGN  = O400000000000  SIGN ON ONLY
!     MASK5  = O500000000000  SIGN AND NEXT BIT ON
!     ENDCD  = O377777777777  ALL BITS ON EXCEPT SIGN
!     MHIBYT = O770000000000  MASK IN HIGH ORDER BYTE
!
   isign = lshift(1,Nbpw-1)
   mask5 = orf(isign,rshift(isign,1))
   allon = complf(0)
   mhibyt = lshift(allon,(Ncpw-1)*Nbpc)
   endcd = rshift(allon,1)
   j = diagx(2)*5 - 1
   Card(j) = Xsys(j)
   Card(j+1) = khrfn1(bnk,1,Xsys(j),2)
   CALL na12if(*6400,Card(j),2,s7,1)
   IF ( s7/=0 ) i7 = Mach*100
!
!     DETERMINE OPEN CORE SIZE AND ALLOCATE BUFFER AREA
!
   dmapbs = korsz(Gbuff) - 2*Ibufsz
   altrbs = dmapbs + Ibufsz
   CALL waltim(timew)
   timew = mod(timew,10000000)
!
!     LOAD PAGE HEADING IN /OUTPUT/
!
   j = 32
   DO i = 1 , j
      Pghdg1(i) = blank
      Pghdg2(i) = blank
      Pghdg3(i) = blank
      Pghdg4(i) = blank
      Pghdg5(i) = blank
      Pghdg6(i) = blank
   ENDDO
   DO i = 1 , 19
      Pghdg3(i+1) = hdg(i)
   ENDDO
   CALL page
!
!     CARD PREPARATION
!
   n7 = i7 + s7
   i7 = i7/100
   n7 = n7 - 2*i7
   m7 = Card(lectt+9)
   j = iabs(m7)
   i = 3
   IF ( m7<0 .AND. mod(j,10)==7 ) i = 4
   IF ( j/10==n7 .AND. Xsys(17)-i<=s7 ) Card(lectt+2) = icold
   Card(lectt+11) = khrfn1(Card(lectt+11),2,xalt(1),3)
   Card(lectt+13) = khrfn1(Card(lectt+13),1,nxcsa(1),1)
   Card(lectt+14) = khrfn1(Card(lectt+14),2,idisk,1)
!
!     WRITE DUMMY ID FILE ON PROBLEM TAPE IN CASE OF ID CONTROL CARD
!     ERROR.
!
   nogo = Xnogo
   Xnogo = 0
   oldalt = 0
!
!     READ CONTROL CARD AND PROCESS
!
 100  IF ( altopn<=0 ) ASSIGN 300 TO irtn1
 200  DO
      Nlines = Nlines + 1
      IF ( Nlines>=Nlpp ) CALL page
      IF ( Zcom==0 ) CALL xread(*5300,Card)
!
!     ECHO CARD
!     (NOECHO IS SET BY SEMDBD AND READFILE OF FFREAD)
!
      Zcom = 0
      IF ( Noecho/=0 ) THEN
         Noecho = Noecho + 1
         Nlines = Nlines - 1
      ELSE
         WRITE (Outtap,99028) Card
      ENDIF
!
!     CHECK FOR COMMENT CARD
!
      IF ( khrfn1(blank,1,Card(1),1)/=dolsin ) THEN
!
!     CALL RMVEQ TO REPLACE ONE EQUAL SIGN BY ONE BLANK
!     IF CARD IS NOT WITHIN ALTER RANGE
!
!CCCC   NEXT LINE CAUSE ERROR IN READING RESTART DICTIONARY. POSITION
!CCCC   PROBLEM
!CCCC
!CCCC      IF (NOTALT .EQ. 0) CALL RMVEQ (CARD)
         CALL xrcard(outcrd,200,Card)
!
!     CHECK FOR ERROR DETECTED BY XRCARD
!
         IF ( Xnogo/=0 ) THEN
            IF ( nogo==0 ) nogo = 1
            Xnogo = 0
!
!     CHECK FOR BLANK CARD
!
         ELSEIF ( outcrd(1)/=0 ) THEN
            GOTO irtn1
         ENDIF
      ENDIF
   ENDDO
 300  j = 0
   DO i = 1 , lectt , 3
      j = j + 1
      IF ( outcrd(2)==ectt(i) .AND. outcrd(3)==ectt(i+1) ) GOTO 400
   ENDDO
   IF ( outcrd(2)==ixdmap ) GOTO 2100
   IF ( ignore/=0 ) GOTO 100
!
   ASSIGN 3300 TO irtn2
   msgnum = 505
   GOTO 3200
!
!     HAS THIS TYPE CARD ALREADY BEEN PROCESSED
!
 400  ignore = 0
   IF ( ectt(i+2)<0 .AND. outcrd(2)==ectt(28) ) ectt(i+2) = 0
!                                               DIAG
   IF ( ectt(i+2)<0 ) GOTO 3400
   ectt(i+2) = orf(ectt(i+2),mask5)
   IF ( j==2 ) THEN
!
!
!     NOW PROCESS APPROACH CARD
!
      DO jj = 1 , numapp
         Apprch = jj
         apprec = jj
         IF ( outcrd(4)==apptyp(jj) ) GOTO 500
      ENDDO
      imhere = 130
      GOTO 3600
   ELSEIF ( j==3 ) THEN
!
!
!     NOW PROCESS CHKPNT CARD
!
      IF ( outcrd(4)==no .OR. outcrd(6)==no ) GOTO 100
!
!     CHECK FOR ILLEGAL FORMAT
!
      imhere = 140
      IF ( outcrd(4)/=iyes .AND. outcrd(6)/=iyes ) THEN
!
         erralt = 1
         GOTO 3600
      ELSE
         Icpflg = 1
         IF ( outcrd(6)==idisk ) GOTO 100
         ASSIGN 600 TO l
!
!     CHECKPOINT FLAG IS ON,MAKE SURE NEW PROBLEM TAPE IS ON
!     PHYSICAL TAPE DRIVE
!
         idfist = ptape
!
!     CHECK TAPE SETUP
!
         IF ( tapbit(idfist) ) THEN
!
!     TAPE SETUP
!
            nostup = 0
         ELSE
!
!     TAPE NOT SETUP
!
            nostup = 1
         ENDIF
!     GO TO L, (150,470)
         GOTO l
      ENDIF
   ELSEIF ( j==4 ) THEN
!
!
!     NOW PROCESS RESTART CARD
!
      ngino = optape
      Irestr = 1
!
!     SET UNSORTED AND SORTED BULK DATA OUTPUT (ECHO = BOTH)
!     AS THE DEFAULT FOR RESTART RUNS
!
      Iecho = 3
      CALL open(*4100,optape,Gbuff(dmapbs+1),0)
      CALL read(*6100,*6100,optape,Otapid,6,0,flgwrd)
      CALL read(*6100,*700,optape,timex,1,1,flgwrd)
      GOTO 800
   ELSEIF ( j==5 ) THEN
      GOTO 2500
   ELSEIF ( j==6 .OR. j==16 .OR. j==17 ) THEN
!
!
!     PROCESS ALTER CONTROL CARDS
!
      ASSIGN 1900 TO irtn1
      IF ( ectt(27)<0 ) GOTO 200
      notalt = 1
      imhere = 330
      ngino = Altfil
      CALL open(*5900,Altfil,Gbuff(altrbs+1),1)
      altopn = 1
      IF ( j==16 ) GOTO 1300
      IF ( j/=17 ) GOTO 1200
      GOTO 1500
   ELSEIF ( j==7 ) THEN
!
!
!     NOW PROCESS SOL CONTROL CARD
!
      soluf = 1
!
!     =====================================
!     ECTT(I+2) = 0
!     DO 2000 JJ = 1,12
!2000 SOLU(JJ) = 0
!     WRITE  (6,2001)
!2001 FORMAT (16H0+++ OUTCARD +++)
!     JJ = 1
!2002 WRITE  (6,2003) JJ,OUTCRD(JJ)
!2003 FORMAT (20X,I5,5X,O20)
!     IF (OUTCRD(JJ) .EQ. ENDCD) GO TO 2004
!     JJ = JJ + 1
!     GO TO 2002
!2004 CONTINUE
!     =====================================
!
      IF ( outcrd(1)==1 ) THEN
!
         imhere = 395
         IF ( outcrd(4)/=-1 ) THEN
            erralt = 1
            GOTO 3600
         ELSE
            jk = 7
            solu(1) = outcrd(5)
            IF ( outcrd(6)==1 ) jk = jk + 3
            IF ( outcrd(6)==2 ) jk = jk + 5
            GOTO 2000
         ENDIF
      ELSE
!
         DO jj = 1 , 6
            solnmx(jj) = blank
         ENDDO
         jk = 2*outcrd(1) + 3
         solnmx(1) = outcrd(4)
         solnmx(2) = outcrd(5)
         IF ( outcrd(1)/=2 .AND. outcrd(7)/=blank ) THEN
            solnmx(3) = outcrd(6)
            solnmx(4) = outcrd(7)
            IF ( outcrd(1)/=3 .AND. outcrd(9)/=blank ) THEN
               solnmx(5) = outcrd(8)
               solnmx(6) = outcrd(9)
            ENDIF
         ENDIF
         DO jj = 1 , nsolnm
            DO k = 1 , 6
               IF ( solnmx(k)/=solnms(k,jj) ) GOTO 420
            ENDDO
            solu(1) = solnms(7,jj)
            GOTO 2000
 420     ENDDO
         iufile(1) = outcrd(4)
         iufile(2) = outcrd(5)
         solu(1) = 0
         GOTO 2000
      ENDIF
   ELSEIF ( j==8 ) THEN
      GOTO 2100
   ELSEIF ( j==9 ) THEN
!
      ASSIGN 5100 TO irtn2
      notalt = 0
      msgnum = 523
      GOTO 3200
   ELSEIF ( j==10 ) THEN
!
! 460 IMHERE = 460
!     IF (OUTCRD(4).NE.INTGR .OR. OUTCRD(6).NE.INTGR .OR.
!    1    OUTCRD(5).LE.    0 .OR. OUTCRD(7).LT.   0) GO TO 750
!
!     SET UNSORTED AND SORTED BULK DATA OUTPUT (ECHO = BOTH)
!     AS THE DEFAULT FOR RUNS USING THE UMF
!
!     IECHO = 3
!
!     MAKE SURE UMF TAPE IS SETUP
!
!     ASSIGN 470 TO L
!     IDFIST = NUMF
!     GO TO 160
! 470 IF (NOSTUP .NE. 0) GO TO 970
!
!     MAKE SURE CORRECT UMF TAPE IS MOUNTED
!
!     NGINO = NUMF
!     IMHERE= 470
!     CALL OPEN  (*1320,NUMF,GBUFF(DMAPBS+1),0)
!     CALL READ  (*1350,*1350,NUMF,UMFID,1,0,FLGWRD)
!     CALL SKPFIL (NUMF,1)
!     CALL CLOSE (NUMF,2)
!     IF (UMFID .NE. OUTCRD(5)) GO TO 1000
!     UMFID = OUTCRD(7)
!     GO TO 20
!
!
!     PROCESS DIAG CARD
!     ALLOW MULTIPLE DIAG CARDS TO BE PROCESSED.
!
      i = 2
      GOTO 2200
   ELSEIF ( j==11 ) THEN
!
!
!     NOW PROCESS UMF CARD
!     CHECK FORMAT
!
      WRITE (Outtap,99029) Uwm , ectt(i) , ectt(i+1)
      GOTO 100
   ELSEIF ( j==12 ) THEN
!
!
!     NOW PROCESS ID CARD
!     CHECK FORMAT - MUST BE AT LEAST 3 BCD FIELDS
!
      imhere = 530
      IF ( outcrd(1)>=3 ) GOTO 2400
      erralt = 1
      GOTO 3600
   ELSEIF ( j==13 ) THEN
!
!
!     UMFEDIT CARD FOUND - SET EDTUMF FLAG
!
      WRITE (Outtap,99029) Uwm , ectt(i) , ectt(i+1)
!     EDTUMF = 1
      GOTO 100
   ELSEIF ( j==14 ) THEN
!
!
!     PROCESS PREC CARD
!
      imhere = 565
      IF ( outcrd(5)/=1 .AND. outcrd(5)/=2 ) THEN
         erralt = 1
         GOTO 3600
      ELSE
         Prec = outcrd(5)
         GOTO 100
      ENDIF
   ELSEIF ( j==15 ) THEN
!
!
!     PROCESS INTERACTIVE CARD
!     SET INTRA TO NEGATIVE IN BATCH RUN (I.E. PRE-INTERACTIVE RUN)
!     INTRA WILL BE RESET TO POSITIVE IN AN ON-LINE INTERACTIVE RUN
!
!     CHECK FORMAT AND FILE ASSIGNMENT
!
      Intra = 0
      DO jj = 4 , 9
         IF ( outcrd(jj)==plot ) Intra = orf(Intra,1)
         IF ( outcrd(jj)==prnt ) Intra = orf(Intra,2)
         IF ( outcrd(jj)==both ) Intra = orf(Intra,3)
      ENDDO
      IF ( Intra==0 ) GOTO 3300
      Intra = -Intra
      jj = 1
      IF ( Mach==3 ) CALL facil(inp9,jj)
      IF ( jj/=2 ) GOTO 100
!
      ASSIGN 5600 TO irtn3
      msgnum = 530
      GOTO 5500
   ELSE
!
!
!     NOW PROCESS TIME CARD
!
      imhere = 110
      IF ( outcrd(4)/=-1 .OR. outcrd(5)<=0 ) GOTO 3600
      Time = outcrd(5)*60
      GOTO 100
   ENDIF
!
!     CHECK FOR SUBSTRUCTURE ANALYSIS
!
 500  IF ( outcrd(6)==nsubs ) THEN
      Isubs = Apprch
      IF ( outcrd(8)==-1 ) Isubs = Isubs + 10*outcrd(9)
   ENDIF
   GOTO 100
 600  IF ( nostup==0 ) GOTO 100
!
   ASSIGN 3800 TO irtn2
   msgnum = 508
   GOTO 3200
 700  outcrd(21) = 0
   timex = 0
!
!     COMPARE ID OF OLD PTAPE WITH THAT ON RSTART CARD
!
 800  rstrt = 2
!
!     UNPACK DATE
!
   i = lshift(Otapid(5),7)
   iyear = rshift(andf(i,maskhi),7)
   i = rshift(i,6)
   iday = rshift(andf(i,maskhi),9)
   i = rshift(i,5)
   imnth = rshift(andf(i,maskhi),10)
   jj = outcrd(1)*2 - 2
   DO jk = 1 , jj
      IF ( Otapid(jk)/=outcrd(jk+3) ) GOTO 3900
   ENDDO
   IF ( outcrd(9)/=0 .OR. outcrd(14)/=0 .OR. outcrd(19)/=0 ) THEN
      IF ( imnth/=outcrd(9) .OR. iday/=outcrd(14) .OR. iyear/=outcrd(19) ) GOTO 3900
   ENDIF
   IF ( outcrd(21)==0 ) timex = 0
   IF ( timex/=outcrd(21) ) GOTO 3900
!
!     MAKE SURE CORRCET REEL IS MOUNTED
!
   IF ( Otapid(6)/=1 ) GOTO 3900
   DO
!
!     GET OLD SOLUTION NUMBER
!
      CALL skpfil(optape,1)
      CALL read(*6100,*6100,optape,osolu,1,0,flgwrd)
      IF ( osolu(1)==xalt(1) ) oldalt = oldalt + 1
      IF ( osolu(1)==nxptdc(1) ) oldalt = oldalt + 1
      IF ( osolu(1)==nxcsa(1) ) THEN
         CALL fwdrec(*6100,optape)
         CALL read(*6100,*6100,optape,0,-4,0,flgwrd)
         CALL read(*6100,*6100,optape,osolu,2,1,flgwrd)
         CALL skpfil(optape,1)
         CALL close(optape,2)
!
!     LOAD PROBLEM TAPE DICTIONARY
!
         icrdct = 0
         Iseqno = 0
         Itop = drecsz + 1
         Ldic = korsz(Iptdic(Itop)) - Ibufsz
         Ibot = Itop - 3
!
!     ZERO FIRST PTDIC ENTRY IN CASE THERE ARE NO ENTRIES
!
         Iptdic(Itop) = 0
         Iptdic(Itop+1) = 0
         Iptdic(Itop+2) = 0
!
!     SET ITOPX SO THAT FIRST XVPS ENTRY IN PTDIC WILL BE PRESERVED
!
         itopx = Itop + 3
         EXIT
      ENDIF
   ENDDO
 900  icrdct = 1 + icrdct
!
!     READ IN NEXT CONTROL CARD
!
   ASSIGN 1000 TO irtn1
   GOTO 200
 1000 IF ( outcrd(1)==-1 ) THEN
      IF ( outcrd(2)/=icrdct ) THEN
!
         ASSIGN 5200 TO irtn2
         msgnum = 526
         GOTO 3200
      ELSE
         IF ( outcrd(3)/=5 ) THEN
            IF ( outcrd(3)==endcd ) GOTO 1100
            IF ( outcrd(3)<=3 ) THEN
!
!     CHECK FORMAT
!
               imhere = 275
               IF ( outcrd(3)/=3 .OR. outcrd(10)/=-1 .OR. outcrd(12)/=2 .OR. outcrd(17)/=-1 .OR. outcrd(19)/=2 .OR. outcrd(24)/=-1 )&
                  & GOTO 3600
!
!     PACK FLAGS/REEL/FILE
!
               flags = 0
               IF ( outcrd(11)>=4 ) flags = isign
               reel = orf(lshift(outcrd(18),16),outcrd(25))
!
!     SEE IF FILE IS ALREADY IN PTDIC - IF IT IS, PUT LATEST REEL/FILE
!     NO. IN EXISTING ENTRY
!
               IF ( Ibot>=itopx ) THEN
                  DO k = itopx , Ibot , 3
                     IF ( Iptdic(k)==outcrd(4) .AND. Iptdic(k+1)==outcrd(5) ) GOTO 1005
                  ENDDO
               ENDIF
!
!     FILE NOT IN PTDIC - MAKE NEW ENTRY
!
               Ibot = Ibot + 3
!
!     CHECK FOR OVERFLOW
!
               IF ( Ibot+3-Itop>Ldic ) THEN
                  ASSIGN 5700 TO irtn3
                  msgnum = 510
                  GOTO 5500
               ELSE
                  k = Ibot
                  Iptdic(k) = outcrd(4)
                  Iptdic(k+1) = outcrd(5)
               ENDIF
 1005          Iptdic(k+2) = orf(flags,reel)
               GOTO 900
            ENDIF
         ENDIF
!
!     THIS IS A REENTRY CARD - LOAD DMAP INSTRUCTION NO. IN ISEQNO
!
         imhere = 310
         IF ( outcrd(4)/=renter .OR. outcrd(14)/=-1 ) GOTO 3600
         Iseqno = lshift(outcrd(15),16)
         GOTO 900
      ENDIF
   ENDIF
!
!     DICTIONARY PROCESSED - COPY ONTO NEW PROBLEM TAPE.
!     THERE MUST ALWAYS BE AT LEAST ONE ENTRY IN PTDIC
!
 1100 IF ( Ibot<Itop ) Ibot = Itop
   ngino = ptape
   imhere = 320
   CALL open(*5900,ptape,Gbuff(dmapbs+1),3)
!
!     RECORD 1 = ID
!
   CALL write(ptape,nxptdc,2,1)
!
!     RECORD 2 = CONTENTS OF IPTDIC
!
   CALL write(ptape,Iptdic(Itop),Ibot+3-Itop,1)
   CALL eof(ptape)
   CALL close(ptape,2)
   IF ( outcrd(3)/=endcd ) GOTO 300
   GOTO 100
 1200 IF ( outcrd(6)==endcd ) THEN
      outcrd(6) = intgr
      outcrd(7) = 0
   ENDIF
   imhere = 350
   IF ( outcrd(4)/=intgr .OR. outcrd(6)/=intgr .OR. outcrd(5)<=0 .OR. outcrd(7)<0 ) THEN
      erralt = 1
      GOTO 3600
   ELSEIF ( outcrd(7)>0 .AND. outcrd(8)/=endcd ) THEN
      erralt = 1
      GOTO 3600
   ELSE
!
!
      alter(1) = outcrd(5)
      alter(2) = outcrd(7)
!
!
!     WRITE ALTER PARAMETERS ONTO THE ALTER SCRATCH FILE
!     AND FOLLOW IT BY THE CARD IMAGE
!
      CALL write(Altfil,alter,2,1)
      CALL write(Altfil,Card,18,1)
!
!     READ NEXT CARD INTO CORE
!
      GOTO 200
   ENDIF
!
!     PROCESS INSERT CONTROL CARDS HERE
!
 1300 insert(1) = outcrd(4)
   insert(2) = outcrd(5)
   insert(3) = 1
   insert(4) = 0
   IF ( outcrd(6)/=allon .OR. outcrd(7)/=ileft .OR. outcrd(8)/=intgr ) THEN
      jn = 7
      IF ( outcrd(6)/=intgr ) THEN
         IF ( outcrd(6)==endcd ) GOTO 1400
         erralt = 1
         GOTO 3600
      ENDIF
   ELSEIF ( outcrd(9)<=0 ) THEN
      erralt = 1
      GOTO 3600
   ELSE
      insert(3) = outcrd(9)
      jn = 11
      IF ( outcrd(10)/=intgr ) THEN
         IF ( outcrd(10)==endcd ) GOTO 1400
         erralt = 1
         GOTO 3600
      ENDIF
   ENDIF
   insert(4) = outcrd(jn)
   IF ( outcrd(jn+1)/=endcd ) THEN
      erralt = 1
      GOTO 3600
   ENDIF
!
!     WRITE INSERT PARAMETERS ONTO THE ALTER SCRATCH FILE
!     AND FOLLOW IT BY THE CARD IMAGE
!
 1400 CALL write(Altfil,insert,4,1)
   CALL write(Altfil,Card,18,1)
   Newalt = 1
   GOTO 200
!
!     PROCESS DELETE CONTROL CARDS HERE
!
 1500 delete(1) = outcrd(4)
   delete(2) = outcrd(5)
   delete(3) = 1
   delete(4) = 0
   delete(5) = 0
   IF ( outcrd(6)/=allon .OR. outcrd(7)/=ileft .OR. outcrd(8)/=intgr ) THEN
      jn = 7
      jnx = 7
      IF ( outcrd(6)/=intgr ) THEN
         IF ( outcrd(6)==endcd ) GOTO 1600
         jnx = 6
         GOTO 1700
      ENDIF
   ELSEIF ( outcrd(9)<=0 ) THEN
      erralt = 1
      GOTO 3600
   ELSE
      delete(3) = outcrd(9)
      jn = 11
      jnx = 11
      IF ( outcrd(10)/=intgr ) THEN
         IF ( outcrd(10)==endcd ) GOTO 1600
         IF ( outcrd(10)>0 ) GOTO 1700
         erralt = 1
         GOTO 3600
      ENDIF
   ENDIF
   delete(4) = outcrd(jn)
   jn = jn + 1
   jnx = jn + 1
   IF ( outcrd(jn)/=endcd ) THEN
      IF ( outcrd(jn)>0 ) GOTO 1700
      erralt = 1
      GOTO 3600
   ENDIF
!
!     WRITE DELETE PARAMETERS ONTO THE ALTER SCRATCH FILE
!     AND FOLLOW IT BY THE CARD IMAGE
!
 1600 CALL write(Altfil,delete,5,1)
   CALL write(Altfil,Card,18,1)
   Newalt = 1
   GOTO 200
!
 1700 jn = jnx
   delete(5) = 1
   delete(6) = outcrd(jn)
   delete(7) = outcrd(jn+1)
   delete(8) = 1
   delete(9) = 0
   jn = jn + 2
   jnx = jn + 3
   IF ( outcrd(jn)==allon .AND. outcrd(jn+1)==ileft .AND. outcrd(jn+2)==intgr ) THEN
      jn = jnx
      IF ( outcrd(jn)<=0 ) THEN
         erralt = 1
         GOTO 3600
      ELSE
         delete(8) = outcrd(jn)
         jn = jn + 1
         jnx = jn + 1
         IF ( outcrd(jn)/=intgr ) THEN
            IF ( outcrd(jn)==endcd ) GOTO 1800
            erralt = 1
            GOTO 3600
         ENDIF
      ENDIF
   ELSE
      jnx = jn + 1
      IF ( outcrd(jn)/=intgr ) THEN
         IF ( outcrd(jn)==endcd ) GOTO 1800
         erralt = 1
         GOTO 3600
      ENDIF
   ENDIF
   delete(9) = outcrd(jnx)
   IF ( outcrd(jnx+1)/=endcd ) THEN
      erralt = 1
      GOTO 3600
   ENDIF
!
!     WRITE DELETE PARAMETERS ONTO THE ALTER SCRATCH FILE
!     AND FOLLOW IT BY THE CARD IMAGE
!
 1800 CALL write(Altfil,delete,9,1)
   CALL write(Altfil,Card,18,1)
   Newalt = 1
   GOTO 200
!
!     CHECK FOR CEND CARD TO PREVENT STREAMING THRU BULK DATA
!
 1900 IF ( outcrd(2)==cend(1) .AND. outcrd(3)==cend(2) ) THEN
!
!
      ASSIGN 4300 TO irtn2
      msgnum = 514
      GOTO 3200
   ELSE
!
!     CHECK FOR ANOTHER ALTER CARD
!
      IF ( outcrd(2)==bgnal(1) .AND. outcrd(3)==bgnal(2) ) GOTO 1200
!
!     CHECK FOR ANOTHER INSERT CARD
!
      IF ( outcrd(2)==ectt(46) .AND. outcrd(3)==ectt(47) ) GOTO 1300
!
!     CHECK FOR ANOTHER DELETE CARD
!
      IF ( outcrd(2)==ectt(49) .AND. outcrd(3)==ectt(50) ) GOTO 1500
!
!     CHECK FOR ENDALTER CARD
!
      IF ( outcrd(2)/=endal(1) .OR. outcrd(3)/=endal(2) ) THEN
!
!
!
!     WRITE DMAP INSTRUCTION ON THE ALTER SCRATCH FILE
!
         IF ( ectt(27)>=0 ) CALL write(Altfil,Card,18,1)
         GOTO 200
      ELSE
!
!     ENDALTER ENCOUNTERED
!
         IF ( ectt(27)<0 ) GOTO 3400
         ectt(27) = orf(ectt(27),mask5)
         CALL eof(Altfil)
         CALL close(Altfil,2)
         altopn = -1
         notalt = 0
         GOTO 100
      ENDIF
   ENDIF
!
 2000 Rfflag = solu(1)
   IF ( outcrd(jk-1)==endcd ) GOTO 100
   imhere = 397
   jj = 1
   DO
      jj = jj + 1
      IF ( jj>12 ) THEN
         erralt = 1
         GOTO 3600
      ELSEIF ( outcrd(jk-1)/=-1 ) THEN
         erralt = 1
         GOTO 3600
      ELSE
         Nsubst = jj
         solu(jj) = outcrd(jk)
         IF ( outcrd(jk+1)==endcd ) GOTO 100
!
!     ===========================================
!2005 FORMAT (1H0,100(1H+)/1H0/1H0)
!     WRITE  (6,2006)
!2006 FORMAT (13H0+++ SOLU +++)
!     JJ = 1
!2007 IF (SOLU(JJ).EQ.0 .AND. JJ.GT.2) GO TO 2009
!     WRITE  (6,2008) JJ,SOLU(JJ)
!2008 FORMAT (20X,I5,5X,I10)
!     JJ = JJ + 1
!     GO TO 2007
!2009 CONTINUE
!     WRITE (6,2005)
!     ===========================================
!
         jk = jk + 2
      ENDIF
   ENDDO
!
!
!     B E G I N  CONTROL CARD
!     PROCESS DMAP SEQUENCE
!
 2100 jj = 0
   WRITE (Outtap,99001)
99001 FORMAT (5X,'(SEE NASTRAN SOURCE PROGRAM COMPILATION FOR LISTING ','OF DMAP SEQUENCE)')
   DO jk = 1 , nwpc
      jj = jj + 1
      Dmapbf(jj) = Card(jk)
   ENDDO
   DO
      CALL xread(*5300,Card)
      DO jk = 1 , nwpc
         jj = jj + 1
         Dmapbf(jj) = Card(jk)
      ENDDO
      IF ( jj>dmapbs ) THEN
!
         ASSIGN 5800 TO irtn3
         msgnum = 511
         GOTO 5500
      ELSE
!
!     CHECK FOR END OR CEND CARD
!
         CALL xrcard(outcrd,200,Card)
!
!     CHECK FOR ERROR DETECTED BY XRCARD
!
         IF ( Xnogo/=0 ) THEN
            WRITE (Outtap,99028) Card
            IF ( nogo==0 ) nogo = 1
            Xnogo = 0
         ELSEIF ( outcrd(2)==cend(1) .AND. outcrd(3)==cend(2) ) THEN
!
            ASSIGN 4400 TO irtn2
            msgnum = 515
            GOTO 3200
         ELSEIF ( outcrd(2)==dmend ) THEN
            WRITE (Outtap,99028) Card
            drecsz = jj
            GOTO 100
         ENDIF
      ENDIF
   ENDDO
 2200 DO
      i = i + 2
      IF ( outcrd(i)==0 ) THEN
!
!     DIAG CONTINUED ON NEXT CARD - READ IN NEXT CARD
!
         ASSIGN 2300 TO irtn1
         GOTO 200
      ELSEIF ( outcrd(i)/=intgr ) THEN
!
!     SHOULD BE END OF LOGICAL DIAG CARD
!
         imhere = 520
         IF ( outcrd(i)/=endcd ) THEN
            erralt = 1
            GOTO 3600
         ELSE
!IBMDB 5/95
!      SWITCH(3) = ORF(SWITCH(3),SWITCH(1))
!      SWITCH(1) = 0
!      CALL PRESSW (LINKS(1),I)
!
!     RE-ACTIVATE THOSE LINK1 SPECIAL DIAGS IN DIAGX LIST IF NECESSARY
!
!      IF (SWITCH(1) .EQ. SWITCH(3)) GO TO 527
!      DO 525 I = 1,11
!      JJ = DIAGX(I) - 1
!      SWITCH(1) = ORF(ANDF(LSHIFT(1,JJ),SWITCH(3)),SWITCH(1))
!  525 CONTINUE
!      IF (SWITCH(1) .NE. SWITCH(3)) CALL PRESSW (RENTER,I)
!IBMDE 5/95
            CALL sswtch(15,L15)
            CALL sswtch(8,L8)
            CALL sswtch(13,L13)
            GOTO 100
         ENDIF
      ELSE
!
!     SET SENSE SWITCH BITS. (DIAG 1 THRU 48, BIT COUNTS 0 THRU 47)
!     BITS 49 THRU 63 ARE RESERVED FOR LINK NO.  (-1 THRU -15)
!
         jj = outcrd(i+1)
!WKBD IF (JJ .GT. 63-MAXLNK) GO TO 503
!WKBD IF (JJ.GE.-MAXLNK .AND. JJ.LE.-1) JJ = 63 - MAXLNK - JJ
         IF ( jj>31 ) THEN
            IF ( jj==42 .AND. Mach>5 ) WRITE (Outtap,99002) Uwm , Mchnam
99002       FORMAT (A25,', DIAG 42 IS UNSUPPORTED IN ALL UNIX MACHINES, ','INCLUDING ',A6,' ***')
            jj = jj - 31
            Switch(2) = orf(lshift(1,jj-1),Switch(2))
         ELSE
            Switch(1) = orf(lshift(1,jj-1),Switch(1))
!
!     TURN ON DIAG 14 IF DIAG 25 HAS BEEN REQUESTED
!
            IF ( jj==25 ) Switch(1) = orf(lshift(1,13),Switch(1))
         ENDIF
      ENDIF
   ENDDO
 2300 IF ( outcrd(2)==cend(1) .AND. outcrd(3)==cend(2) ) GOTO 2500
   i = -1
   GOTO 2200
!
!     MAKE SURE ID CARD IS FIRST CONTROL CARD
!     IF ID CARD WAS IN ERROR CONTROL WILL STILL RETURN TO HERE
!
 2400 DO i = 1 , lectt , 3
      IF ( ectt(i+2)<0 .AND. ectt(i)/=id ) GOTO 4500
   ENDDO
   IF ( Logfl<=0 ) CALL logfil(Card)
   DO jj = 1 , 4
      Tapid(jj) = outcrd(jj+3)
   ENDDO
!
!      PACK DATE -
!
   imnth = lshift(Idate(1),14)
   iday = lshift(Idate(2),8)
   iyear = Idate(3)
   Tapid(5) = orf(imnth,orf(iday,iyear))
!
!     REEL NO. TO TAPID
!
   Tapid(6) = 1
!
!     OUTPUT IF ON NEW PROBLEM TAPE
!
   ngino = ptape
   CALL open(*5900,ptape,Gbuff(dmapbs+1),1)
   CALL write(ptape,Tapid,6,0)
   CALL write(ptape,timew,1,1)
   CALL eof(ptape)
   CALL close(ptape,2)
   GOTO 100
!
!     CEND CARD FOUND - NO MORE CONTROL CARDS TO PROCESS
!
!
!     SET APP DEFAULT TO 'DISPLACEMENT' AND TIME TO 10 MINUTES
!
 2500 IF ( Apprch==0 ) THEN
      Apprch = 2
      apprec = 2
      WRITE (Outtap,99003)
99003 FORMAT ('0*** APP  DECLARATION CARD MISSING.  DISPLACEMENT IS ','SELECTED BY DEFAULT')
   ENDIF
   IF ( Time<=0 ) THEN
      Time = 300
      WRITE (Outtap,99004)
99004 FORMAT ('0*** TIME  CARD MISSING. MAXIMUM EXECUTION TIME IS SET ','TO 5 MINUTES BY DEFAULT')
   ENDIF
!
!     CALL NSINFO TO PRINT DIAG48, OR
!     PRINT THE FOLLOWING MESSAGE OUT ONLY IF THE JOB IS RUN ON THE SAME
!     YEAR OF THE RELEASE DATE, AND USER DOES NOT MAKE A DIAG48 REQUEST
!
!     DIAG48 TEXT IS STORED IN 4TH SECTION OF THE NASINFO FILE
!
!
   CALL sswtch(48,jj)
   IF ( jj/=1 ) THEN
      jj = Idate(3)
      jj = mod(jj,100)
      CALL int2a8(*2600,jj,iz(1))
   ELSE
      CALL nsinfo(4)
      GOTO 2700
   ENDIF
 2600 IF ( iz(1)==Sy42(3) ) WRITE (Outtap,99005) Uim
99005 FORMAT (//,A29,', TURN DIAG 48 ON FOR NASTRAN RELEASE NEWS, ','DIAG DEFINITION, NEW DMAP',/9X,                                &
             &'MODULES AND NEW BULKDATA CARDS INFORMATION')
!
!     CLOSE NASINFO FILE IF IT EXISTS
!     AND RESET THE 37TH WORD OF /SYSTEM/ BACK TO ZERO
!
 2700 IF ( Lu/=0 ) CLOSE (UNIT=Lu)
   Lu = 0
!
!     NOW MAKE SURE ALL NECESSARY CARDS HAVE BEEN FOUND
!
   DO i = 1 , lectt , 3
      test = andf(ectt(i+2),mask)
      IF ( test>0 ) THEN
         IF ( ectt(i+2)>=0 ) GOTO 4700
      ENDIF
   ENDDO
!
!     SET APPRCH NEGATIVE FOR RESTART
!
   IF ( rstrt/=icold ) Apprch = -Apprch
   IF ( soluf==1 .AND. drecsz/=0 ) THEN
!
      ASSIGN 4900 TO irtn2
      msgnum = 521
      GOTO 3200
   ELSEIF ( soluf==0 .AND. drecsz==0 ) THEN
!
      ASSIGN 5000 TO irtn2
      msgnum = 522
      GOTO 3200
   ENDIF
!     IF (RSTRT.NE.ICOLD .AND. UMFID.NE.0) GO TO 1030
!
!
 2800 IF ( nogo>1 ) GOTO 6300
!
!     WRITE XCSA CONTROL FILE ONTO PROBLEM TAPE
!     FIRST RECORD IS HEADER RECORD CONTAINING A SINGLE WORD (XCSA)
!
   IF ( apprec/=appdmp ) THEN
!
!     IF APPROACH IS HEAT ADD TWENTY THREE TO SOLUTION
!
      IF ( apprec==apphea ) solu(1) = solu(1) + 23
!
!     IF APPROACH IS AEROELASTIC ADD THIRTY TO SOLUTION
!
      IF ( apprec==appaer ) solu(1) = solu(1) + 30
      GOTO 3000
   ENDIF
 2900 ngino = ptape
   imhere = 610
   CALL open(*5900,ptape,Gbuff(dmapbs+1),3)
   CALL write(ptape,nxcsa,2,1)
!
!     DIS OLD PT HAVE AN ALTER FILE AND/OR CKPT DIST
!
   solrec(4) = oldalt
!
!     WRITE SIX-WORD CONTROL FILE RECORD
!
   CALL write(ptape,solrec,6,1)
   CALL eof(ptape)
   CALL close(ptape,3)
   IF ( apprec/=appdmp ) GOTO 3100
 3000 ngino = nscr
   imhere = 612
   CALL open(*5900,nscr,Gbuff(dmapbs+1),1)
   IF ( apprec==appdmp ) THEN
!
!     APPROACH IS DMAP
!     WRITE DMAP SEQUENCE ONTO SCRATCH FILE FROM OPEN CORE
!
      CALL write(nscr,Dmapbf,drecsz,1)
      CALL close(nscr,1)
   ELSE
!
!     APPROACH IS RIGID FORMAT
!     WRITE RIGID FORMAT AND MED TABLES ONTO SCRATCH FILE
!
      isize = korsz(Dmapbf(1)) - Ibufsz
      IF ( altopn/=0 ) THEN
         IF ( erralt/=0 ) Newalt = 0
         IF ( Newalt/=0 ) THEN
            isize = isize - Ibufsz
            ngino = Altfil
            CALL open(*5900,Altfil,Gbuff(altrbs+1),3)
         ENDIF
      ENDIF
      CALL xrgdfm(solu,osolu,apprec,iufile,Dmapbf,isize,nscr,nogo)
      IF ( Xnogo/=0 ) THEN
         IF ( nogo==0 ) nogo = 1
         Xnogo = 0
      ENDIF
      IF ( nogo>1 ) GOTO 6300
      CALL close(nscr,1)
      solrec(3) = 0
      IF ( altopn/=0 ) THEN
         IF ( erralt/=1 ) THEN
            solrec(3) = 1
            ngino = ptape
            CALL open(*5900,ptape,Gbuff(dmapbs+1),3)
            ngino = Altfil
            CALL open(*5900,Altfil,Gbuff(altrbs+1),0)
            CALL dmpalt(isize,Dmapbf,ptape)
            CALL eof(ptape)
            CALL close(ptape,2)
            CALL close(Altfil,1)
            IF ( Alnogo/=0 ) THEN
               IF ( nogo<2 ) nogo = 2
            ENDIF
         ENDIF
      ENDIF
      GOTO 2900
   ENDIF
!
!     PUNCH RESTART CARD IF CHECKPOINT FLAG IS SET.
!
 3100 IF ( Icpflg/=0 ) THEN
!      IF (IROPEN .EQ. 1) GO TO 6405
!      OPEN (UNIT=4, FILE=DSNAMES(4), STATUS='UNKNOWN')
!      IROPEN = 1
      WRITE (Irdict,99006) (Tapid(i),i=1,4) , (Idate(j),j=1,3) , timew
99006 FORMAT (9HRESTART  ,2A4,1H,,2A4,1H,,I2,1H/,I2,1H/,I2,1H,,I8,1H,)
      CALL sswtch(9,diag09)
      IF ( diag09/=1 ) THEN
         CALL page
         WRITE (Outtap,99007) (Tapid(i),i=1,4) , (Idate(j),j=1,3) , timew
99007    FORMAT ('0ECHO OF FIRST CARD IN CHECKPOINT DICTIONARY TO BE ','PUNCHED OUT FOR THIS PROBLEM',/14H0   RESTART   ,2A4,1H,,   &
               & 2A4,1H,,I2,1H/,I2,1H/,I2,1H,,I8,1H,)
      ENDIF
   ENDIF
   Xnogo = nogo
   RETURN
!
!     ERROR MESSAGES
!
!     USER  FATAL MESSAGES
!
 3200 Nlines = Nlines + 2
   IF ( Nlines>=Nlpp ) CALL page
   IF ( nogo<1 ) nogo = 1
   ignore = 1
   GOTO irtn2
 3300 WRITE (Outtap,99008) Ufm , msgnum , outcrd(2) , outcrd(3)
99008 FORMAT (A23,I5,', CONTROL CARD ',2A4,11H IS ILLEGAL)
   GOTO 100
!
 3400 ASSIGN 3500 TO irtn2
   msgnum = 506
   GOTO 3200
 3500 WRITE (Outtap,99009) Ufm , msgnum , outcrd(2) , outcrd(3)
99009 FORMAT (A23,I5,', CONTROL CARD ',2A4,11H DUPLICATED)
   GOTO 100
 3600 ASSIGN 3700 TO irtn2
   msgnum = 507
   GOTO 3200
 3700 WRITE (Outtap,99010) Ufm , msgnum , imhere
99010 FORMAT (A23,I5,', ILLEGAL SPECIFICATION OR FORMAT ON PRECEDING ','CARD.',/5X,'IMHERE =',I5)
   IF ( outcrd(2)/=ectt(34) .OR. outcrd(3)/=ectt(35) ) GOTO 100
   GOTO 2400
 3800 WRITE (Outtap,99011) Ufm , msgnum
99011 FORMAT (A23,I5,', PROBLEM TAPE MUST BE ON PHYSICAL TAPE FOR ','CHECK POINTING')
   ignore = 0
   Icpflg = 0
   GOTO 100
!
 3900 ASSIGN 4000 TO irtn2
   msgnum = 509
   GOTO 3200
 4000 WRITE (Outtap,99012) Ufm , msgnum , (Otapid(i),i=1,4) , imnth , iday , iyear , timex , Otapid(6)
99012 FORMAT (A23,I5,', WRONG OLD TAPE MOUNTED.',/30X,23H OLD PROBLEM TAPE ID = ,2A4,1H,,2A4,1H,,I2,1H/,I2,1H/,I2,1H,,2X,I8,1H,,5X, &
             &10HREEL NO. =,I4)
!
!
!     XCSA HAS BEEN DISASTERED - GET DUMP AND QUIT.
!
   Icpflg = 0
   GOTO 6400
!
 4100 ASSIGN 4200 TO irtn2
   msgnum = 512
   GOTO 3200
 4200 WRITE (Outtap,99013) Ufm , msgnum
99013 FORMAT (A23,I5,', OLD PROBLEM TAPE IS MISSING AND IS NEEDED FOR ','RESTART')
   nogo = 3
   GOTO 100
 4300 WRITE (Outtap,99014) Ufm , msgnum
99014 FORMAT (A23,I5,', ENDALTER CARD IS MISSING')
   IF ( nogo<2 ) nogo = 2
   GOTO 2500
 4400 WRITE (Outtap,99015) Ufm , msgnum
99015 FORMAT (A23,I5,', END INSTRUCTION MISSING IN DMAP SEQUENCE')
   IF ( nogo<2 ) nogo = 2
   GOTO 2500
!
! 970 ASSIGN 980 TO IRTN2
!     MSGNUM = 516
!     GO TO 670
! 980 WRITE  (OUTTAP,990) UFM,MSGNUM
! 990 FORMAT (A23,I5,', UMF TAPE MUST BE MOUNTED ON PHYSICAL TAPE ',
!    1       'DRIVE')
!     NOGO = 3
!     GO TO 20
!
!1000 ASSIGN 1010 TO IRTN2
!     MSGNUM = 517
!     GO TO 670
!1010 WRITE  (OUTTAP,1020) UFM,MSGNUM,UMFID
!1020 FORMAT (A23,I5,', WRONG UMF TAPE MOUNTED - TAPE ID =',I10)
!     NOGO = 3
!     GO TO 20
!
!1030 ASSIGN 1040 TO IRTN2
!     MSGNUM = 518
!     GO TO 670
!1040 WRITE  (OUTTAP,1050) UFM,MSGNUM
!1050 FORMAT (A23,I5,', CANNOT USE UMF TAPE FOR RESTART')
!     NOGO = 3
!     GO TO 1380
!
 4500 ASSIGN 4600 TO irtn2
   msgnum = 519
   GOTO 3200
 4600 WRITE (Outtap,99016) Ufm , msgnum
99016 FORMAT (A23,I5,', ID CARD MUST PRECEDE ALL OTHER CONTROL CARDS')
   nogo = 3
   GOTO 100
!
 4700 ASSIGN 4800 TO irtn2
   msgnum = 520
   GOTO 3200
 4800 WRITE (Outtap,99017) Ufm , msgnum , ectt(i) , ectt(i+1)
99017 FORMAT (A23,I5,', CONTROL CARD ',2A4,' IS MISSING')
   ectt(i+2) = orf(ectt(i+2),mask5)
   IF ( ectt(i)==ectt(4) ) THEN
!
!     MISSING CARD IS APP
!
      IF ( nogo<2 ) nogo = 2
   ENDIF
   GOTO 2500
 4900 WRITE (Outtap,99018) Ufm , msgnum
99018 FORMAT (A23,I5,', SPECIFY A SOLUTION OR A DMAP SEQUENCE BUT NOT ','BOTH')
   IF ( nogo<2 ) nogo = 2
   GOTO 6300
 5000 WRITE (Outtap,99019) Ufm , msgnum
99019 FORMAT (A23,I5,', NEITHER A SOL CARD NOR A DMAP SEQUENCE WAS ','INCLUDED')
   IF ( nogo<2 ) nogo = 2
   GOTO 6300
 5100 WRITE (Outtap,99020) Ufm , msgnum
99020 FORMAT (A23,I5,', ENDALTER CARD OUT OF ORDER')
   GOTO 100
 5200 WRITE (Outtap,99021) Ufm , msgnum
99021 FORMAT (A23,I5,', CHECKPOINT DICTIONARY OUT OF SEQUENCE - ','REMAINING RESTART CARDS IGNORED')
   GOTO 100
 5300 ASSIGN 5400 TO irtn2
   msgnum = 529
   GOTO 3200
 5400 WRITE (Outtap,99022) Ufm , msgnum
99022 FORMAT (A23,I5,', MISSING CEND CARD.')
   nogo = 3
   GOTO 6300
!
!     SYSTEM FATAL MESSAGES
!
 5500 Nlines = Nlines + 2
   IF ( Nlines>=Nlpp ) CALL page
   IF ( nogo<2 ) nogo = 2
   ignore = 1
   GOTO irtn3
 5600 WRITE (Outtap,99023) Sfm , msgnum
99023 FORMAT (A25,I5,2H, ,/5X,'INP9 FILE WAS NOT ASSIGNED FOR ','NASTRAN INTERACTIVE POST-PROCESSOR',/)
   GOTO 100
 5700 WRITE (Outtap,99024) Sfm , msgnum
99024 FORMAT (A25,I5,', CHECKPOINT DICTIONARY EXCEEDS CORE SIZE - ','REMAINING RESTART CARDS IGNORED')
   GOTO 100
 5800 WRITE (Outtap,99025) Sfm , msgnum
99025 FORMAT (A25,I5,', DMAP SEQUENCE EXCEEDS CORE SIZE - ','REMAINING DMAP INSTRUCTIONS IGNORED')
   IF ( nogo<2 ) nogo = 2
   GOTO 100
!
 5900 ASSIGN 6000 TO irtn3
   msgnum = 524
   GOTO 5500
 6000 WRITE (Outtap,99026) Sfm , msgnum , ngino , imhere
99026 FORMAT (A25,I5,', ALTERNATE RETURN TAKEN WHEN OPENING FILE ',A4,3X,1H-,I3)
   nogo = 3
   Icpflg = 0
   GOTO 6400
!
 6100 ASSIGN 6200 TO irtn3
   msgnum = 525
   GOTO 5500
 6200 WRITE (Outtap,99027) Sfm , msgnum , ngino
99027 FORMAT (A25,I5,', ILLEGAL FORMAT ENCOUNTERED WHILE READING FILE ',A4)
   nogo = 3
   Icpflg = 0
   GOTO 6400
!
 6300 IF ( nogo==1 ) GOTO 2800
   IF ( nogo/=2 ) THEN
!
!     NOGO = 3 - TERMINATE JOB HERE
!
      Icpflg = 0
      CALL mesage(-61,0,0)
   ENDIF
!
!     NOGO = 2 - PUT IN DUMMY CONTROL FILE ON PROBLEM TAPE
!
   ngino = ptape
   CALL close(ptape,1)
   CALL open(*5900,ptape,Gbuff(dmapbs+1),0)
   CALL skpfil(ptape,1)
   CALL close(ptape,2)
   CALL open(*5900,ptape,Gbuff(dmapbs+1),3)
   CALL write(ptape,nxcsa,2,1)
   solu(1) = 0
   solu(2) = 0
   Apprch = appdmp
   IF ( rstrt/=icold ) Apprch = -Apprch
   CALL write(ptape,solrec,6,1)
   CALL eof(ptape)
   CALL close(ptape,3)
   GOTO 3100
 6400 CALL mesage(-37,0,nxcsa)
99028 FORMAT (5X,20A4)
99029 FORMAT (A25,', ',2A4,' CARD IS NO LONGER AVAILABLE')
END SUBROUTINE xcsa
