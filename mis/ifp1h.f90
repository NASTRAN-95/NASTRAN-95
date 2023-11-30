
SUBROUTINE ifp1h(I81,Nz,J400)
   IMPLICIT NONE
   LOGICAL Bit64
   INTEGER Blank , Corey(401) , E(1) , Iblnk , Ibuf , Icc , Icse(400) , Ieor , Iequal , Incr , Is , Isub , Last , Lencc , Line ,    &
         & Mach , Misset(1) , More(2) , Msst , Ncpw , Nelem , Nlpp , Nmodes , Nogo , Nout , Nset , Nwpc , Scr1
   REAL Casecc , Corex(1) , Dummy(3) , Skip(5) , Xblank
   CHARACTER*23 Ufm
   COMMON /gpta1 / Nelem , Last , Incr , E
   COMMON /ifp1a / Scr1 , Casecc , Is , Nwpc , Ncpw , Nmodes , Icc , Nset , Dummy , Isub , Lencc , Iblnk , Iequal , Ieor
   COMMON /ifp1hx/ Msst , Misset
   COMMON /machin/ Mach
   COMMON /system/ Ibuf , Nout , Nogo , Skip , Nlpp , More , Line
   COMMON /xifp1 / Blank , Bit64
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Corex
   INTEGER I81 , J400 , Nz
   REAL bcd(2,3) , cc(4) , comma , flag , rcore(1) , shea
   INTEGER comp(2,60) , comp1(2,19) , comp2(2,19) , comp3(2,19) , comp4(2,3) , core(1) , equal , err , etab(90) , force , i ,       &
         & icomp , idupl , ie , ielem , iend , ieq , ii , iisub , inc , irept , iscan , iset , isp(10) , iwds , iword , j , jc ,    &
         & jcomp , jj , jx , k , kc , keywds(3) , kk , ll(4) , llc , lline , lll , max , min , mome , mzero , nam(2) , ncomp ,      &
         & norm , nrp , nscan , nsv , nwdsf , nwdss , save(5) , seti , sp(30) , stress , tab(10,17) , tab1(10,9) , tab2(10,8)
   LOGICAL debug
   INTEGER khrfn1 , khrfn2 , khrfn3 , korsz
!
!     THIS ROUTINE PROCESSES THE SCAN CARD IN CASE CONTROL SECTION
!
!     WRITTEN BY G.CHAN/SPERRY,  OCTOBER 1984
!
!     PROGRAM METHOD
!
!     A 'SCAN(HELP)' INPUT CARD WILL SET J400 TO 2, AND ANY ERROR IN
!     A SCAN INPUT CARD WILL SET J400 TO 1. NON-ZERO J400 WILL CAUSE
!     SCAN COMPONENT KEY-WORDS TO BE PRINTED.
!
!     THE SCAN INPUT CARDS, AND THEIR DATA, ARE DECODED AND SAVED IN
!     CASECC FILE AS SETS OF PSEUDO SET COMMANDS (SET ID OF 10000000 FOR
!     STRESS, AND 20000000 FOR FORCE). IN THIS WAY, THE SCAN CARDS CAN
!     BE USED IN ALL SUBCASE LEVELS, OR ABOVE-SUBCASE LEVEL, SIMILAR TO
!     THE ELEM. STRESS AND ELEM. FORCE CARDS IN THE CASE CONTROL SECTION
!     HOWEVER, MULTIPLE SCAN CARDS CAN BE USED IN ALL SUBCASE LEVELS,
!     AND WITHIN EACH SUBCASE
!
!     ELEM. NAME CAN BE SPECIFIED WITH OR WITHOUT THE LEADING LETTER C
!     E.G.  BAR, CBAR, QUAD2, CQUAD2
!
!     SCAN COMPONENTS CAN BE REQUESTED BY ACTUAL OUTPUT COLUMN NUMBER(S)
!     OR BY COMPONENT KEYWORD(S)
!     IF THE ACTUAL OUTPUT COLUMN IS NOT IN THE SAME WORD ORDER AS IN
!     THE OUTPUT PRINT FILE (E.G. OES1L FOR THE QUAD4 LAYER), THE ACTUAL
!     COLUMN COUNT AS IT APPEARS IN THE PRINTOUT, IS USED HERE. ANY
!     DISCREPANCY SHOULD BE HANDLED BY SCAN OR STRSCN ROUTINES.
!
!     A LIST OF KEYWORDS WILL BE PRINTED AUTOMATICALLY IF ELEM. NAME OR
!     COMPONENT KEYWORD ARE MISSPELLED OR MISSSING
!
!     THIS LIST IS ALSO PRINTED IF A  SCAN (HELP) CARD IS IN INPUT DECK
!
!     THIS ROUTINE MAY ISSUE THE FOLLOWING ERROR MESSAGES -
!
!        604 - NON-INTEGER IN INTEGER FIELD
!        608 - SET NOT DEFINED
!        617 - IMPROPER FORMAT
!        634 - KEYWORD INSIDE BRACKET IS ILLEGAL OR MISSPELLED
!        635 - ONLY ONE SET-ID ALLOWED IN A SCAN CARD
!        636 - EXTRA VALUE ENCOUNTERED
!        637 - ILLEGAL COMPONENT SPECIFIED
!        638 - COMPONENT LIMIT OF 31 IS EXCEEDED
!        639 - SET ID ERROR (REQUESTED BEFORE EQUAL SIGN OR SPLITTED ID)
!        640 - TOO MANY COMPONENTS BY NAME
!        641 - -MAX EXCEEDS +MAX
!        642 - COMPONENT NAME NOT AVAILABLE FOR ELEMENT SELECTED
!        643 - SCAN BY STRESS OR FORCE ONLY
!        644 - WARNING MESSAGE FOR POSSIBLE INSUFFICIENT CORE
!        909 - CORE ARRAY NOT INITIALIZED CORRECTLY, OR MZERO IS NOT SET
!              IN AGREEMENT WITH XRCARD
!
!     EXAMPLE - TO ADD A NEW ELEMENT TO THE SCAN MODULE   BY G.C. 7/89
!           1.  INCREASE COMP DIMENSION TO ALLOW NEW COMPONENT WORDS
!               IF THEY DO NOT ALREADY EXIST.
!           2.  EXPAND THE SP-ARRAY IF NECESSARY. INCREASE NCOMP BY
!               THE NUMBER OF NEW WORDS ADDED
!           3.  REACTIVATE THE CORRESPONDING WORD IN ETAB THAT POINTS
!               TO THE NEW ARRAY IN TAB
!           4.  IF SP-ARRAY IS USED, MAKE SURE THAT THE COMPONENT WORDS
!               ARE PROPERLY PROCESSED, IN STATEMENT NOS. 110-120
!           5.  SET THE CODED WORDS IN TAB. SEE COMMENTS FUTHER DOWN
!           6.  PREPARE FORMAT FOR COMPONENT WORDS PRINT OUT (FMT 690)
!               UPDATE ISP-ARRAY IN CASE SP-ARRAY WAS USED PREVIOUSLY
!
   EQUIVALENCE (core(1),rcore(1),Corey(401)) , (Corex(1),Corey(1),Icse(1)) , (comp(1,1),comp1(1,1)) , (comp4(1,1),comp(1,58)) ,     &
    & (tab1(1,1),tab(1,1)) , (comp2(1,1),comp(1,20)) , (tab2(1,1),tab(1,10)) , (comp3(1,1),comp(1,39)) , (Blank,Xblank) ,           &
    & (stress,bcd(1,1)) , (force,bcd(1,2)) , (shea,comp(1,9)) , (norm,comp(1,9)) , (mome,comp(1,22))
   DATA ncomp , equal , lll , seti , debug/58 , 4H=    , 4HL    , 4HSET  , .FALSE./
   DATA llc , comma , mzero , bcd/4HC    , 4H,    , -0 , 4HSTRE , 2HSS , 4HFORC , 1HE , 2*1H /
   DATA nam/4HIFP1 , 4HH   /
!    9                or          -U  , AL-1, AL-X
!    O                or          -V  , AL-2, AL-Y
!    2                or          R-1Z, R-41
!    3                or          R-ZR, R-X , R-U , R-12
!    4                or          R-RT, R-Y , R-V , R-23
!    5                or          R-ZT, R-UV, R-2Z, R-34
   DATA comp1/4HAXIA , 4HL    , 4HTORS , 4HIONA , 4HRADI , 4HAL   , 4HNORM , 4HAL   , 4HPRIN , 4HCIPA , 4HMAJO , 4HR    , 4HMINO ,  &
       &4HR    , 4HBEND , 4HING  , 4HNORM , 4H-X   , 4HNORM , 4H-Y   , 4HNORM , 4H-Z   , 4HSHEA , 4HR    , 4HSHEA , 4HR-XY ,        &
      & 4HSHEA , 4HR-YZ , 4HSHEA , 4HR-ZX , 4HMAX- , 4HSHR  , 4HSHR- , 4HFORC , 4HOCT- , 4HSHR  , 4HSA-M , 4HAX  /
!    2                or          NT-X, NT-U , NT-1
!    3                or          NT-Y, NT-V , NT-2
   DATA comp2/4HSB-M , 4HAX   , 4HMOME , 4HNT   , 4HMOME , 4HNT-A , 4HMOME , 4HNT-B , 4HCURV , 4H     , 4HTORQ , 4HUE   , 4HCIRC ,  &
       &4HUM   , 4HTWIS , 4HT    , 4HMARG , 4HIN   , 4HMAX  , 4H     , 4HMEAN , 4H     , 4HAVG  , 4H     , 4HMEM- , 4HT    ,        &
      & 4HMEM- , 4HC    , 4HFLEX , 4H-T   , 4HFLEX , 4H-C   , 4HPRIN , 4HC-A  , 4HPRIN , 4HC-B  , 4HPRIN , 4HC-C /
!                     or          E-12,
!                     or          E-23,
!                     or          E-34,
!                     or          E-41,
   DATA comp3/4HEFOR , 4HCE   , 4HFORC , 4HE-1  , 4HFORC , 4HE-2  , 4HFORC , 4HE-3  , 4HFORC , 4HE-4  , 4HKICK , 4H-FOR , 4HSIG- ,  &
       &4HX    , 4HSIG- , 4HY    , 4HTAU- , 4HXY   , 4HHELP , 4H     , 4HON-L , 4HINE  , 4HFX+F , 4HY    , 4HFXY  , 4H     ,        &
      & 4HMX+M , 4HY    , 4HMXY  , 4H     , 4HVX+V , 4HY    , 4HKICK , 4H ON1 , 4HKICK , 4H ON2 , 4HKICK , 4H ON3/
   DATA comp4/4HKICK , 4H ON4 , 4H ..  , 4H ..  , 4H ..  , 4H .. /
   DATA sp/4HR-ZR , 4HR-U  , 4HR-RT , 4HR-V  , 4HR-ZT , 4HR-UV , 4HNT-X , 4HNT-U , 4HNT-Y , 4HNT-V , 4H-U   , 4H-V   , 4HR-X  ,     &
       &4HR-Y  , 4HR-41 , 4HR-12 , 4HR-23 , 4HR-34 , 4HNT-1 , 4HNT-2 , 4HR-1Z , 4HR-2Z , 4HAL-1 , 4HAL-2 , 4HAL-X , 4HAL-Y ,        &
      & 4HE-12 , 4HE-23 , 4HE-34 , 4HE-41/
   DATA etab/1 , -02 , 1 , 2 , 2 , 3 , 3 , 3 , 4 , 1 , 6 , 6 , 6 , -14 , 3 , 4 , 3 , 3 , 3 , -20 , -21 , -22 , -23 , -24 , -25 ,    &
      & -26 , -27 , -28 , -29 , -30 , -31 , -32 , -33 , 7 , 8 , 9 , 10 , 11 , -39 , -40 , -41 , -42 , -43 , -44 , -45 , -46 , -47 , &
      & -48 , -49 , -50 , -51 , -52 , -53 , -54 , -55 , -56 , -57 , -58 , -59 , -60 , -61 , 4 , 4 , 15 , 12 , 12 , 13 , 14 , 14 ,   &
      & -70 , -71 , -72 , -73 , -74 , -75 , -76 , -77 , -78 , -79 , 5 , 7 , -82 , 15 , -84 , -85 , -86 , -87 , -88 , -89 , -90/
!    1. ROD, TUB, CONROD
!    2. SHEAR, TWIST
!    3. TRIA1, TRIA2, QUAD1, QUAD2, TRBSC, TRPLT, QDPLT
!    4. TRMEM, QDMEM, QDMEM1, DQMEM2
!   **  CONTINUE...
!    6. ELAS1, ELAS2, ELAS3, IS2D8
!    7. BAR, ELBOW
!    8. CONEAX
!    9. TRIARG
   DATA tab1/01000002 , 02000004 , 28000503 , 0 , 0 , -01000002 , -25000003 , 0 , 0 , 0 , 16000002 , 28000004 , 31000003 ,          &
      & 29000002 , 0 , -40000002 , -41000003 , -22000002 , -23000003 , 0 , 09001103 , 10001204 , 13001305 , 06001507 , 07001608 ,   &
      & 16001709 , -22000002 , -23000003 , -13000005 , -14000006 , 09000002 , 10000003 , 13000004 , 06000006 , 07000007 , 16000008 ,&
      & -40000403 , -41000605 , -42000807 , -43000902 , -55000010 , -56000012 , -57000014 , -58000016 , -13000011 , -14000013 ,     &
      & -15000015 , -12000017 , 0 , 0 , 18000002 , -26000002 , 0 , 0 , 0 , -40000904 , -41000603 , -42000805 , -43000702 , 0 ,      &
      & 19000807 , 20001514 , 28001609 , 01000006 , 0 , -01000008 , -25000009 , -12000605 , -22000302 , -23000504 , 09041852 ,      &
      & 10051852 , 15061852 , 06081852 , 07091852 , 16101852 , -22000003 , -23000004 , -13000006 , -14000007 , 03000002 , 26000003 ,&
      & 01000004 , 12000005 , 0 , -03020353 , -26030353 , -01040353 , 0 , 0/
!    O. TRAPRG
!    1. TORDRG
!   12. IHEX1, IHEX2
!   13. IHEX3
!   14. TRIAAX, TRAPAX
!   15. QUAD4, TRIA3 (GENERAL)
!   **. QUAD4, TRIA3 (LAYER), 9 DIGIT CODE
!   17.
   DATA tab2/03020455 , 26030455 , 01040455 , 12050455 , 17060455 , -03020354 , -26030354 , -01040354 , 0 , 0 , 32020553 ,          &
      & 33030553 , 34040553 , 35050553 , 17060553 , -03000802 , -26000903 , -01001004 , -21001105 , -24001307 , 09032258 ,          &
      & 13042258 , 36052258 , 30092258 , 10112258 , 14122258 , 37132258 , 11172258 , 15182258 , 38192258 , 09032382 , 13042382 ,    &
      & 36052382 , 30092382 , 10122382 , 14132382 , 37142382 , 11182382 , 15192382 , 38202382 , 03030853 , 01040853 , 26050853 ,    &
      & 33060853 , 34070853 , 35080853 , -03030453 , -26040453 , -01050453 , 0 , 09000003 , 10000004 , 13000005 , 06000007 ,        &
      & 07000008 , 16000009 , -50000302 , -51000004 , -52000605 , -53000007 , -54000908 , 81030899 , 82040899 , 84050899 ,          &
      & 83070899 , 85080899 , 0 , 0 , 0 , 0 , 10*0/
!
!     FIRST 2 DIGITS IN A TAB ITEM ARE COMPONENT POINTER, POINTING TO
!     THE BCD WORDS IN COMP ARRAY. POSITIVE FOR STRESS, AND NEGATIVE FOR
!     FORCE DATA (WITH SOME EXCEPTIONS). THIS POINTER IS USED ONLY
!     LOCALLY WITHIN THIS SUBROUTINE.
!     NEXT 3 NUMBERS (2 DIGITS EACH) ARE POINTERS TO THE FIELD NOS.
!
!     SPECIAL CASE -
!     IF LAST FIELD IS GREATER THAN 50 THEN, THIS LAST FIELD MINUS 50 IS
!     THE REPEAT FLAG. IF LAST FIELD IS 99, WE HAVE AN OPEN-END REPEAT.
!     IF LAST FIELD IS GREATER THAN 50, NEXT TO LAST FIELD IS FIELD
!     INCREMENT, AND THE FIELD IN FRONT IS THE FIRST STARTING COLUMN TO
!     BE SCANNED.
!     THE QUAD4/TRIA3 LAYER HAS COMPONENT INDICES 81 THRU 85
!     (THUS - IN FUTURE EXPANSION, ARRAY COMP SHOULD NOT EXCEED 80)
!
!     E.G.  TAB(3,1) =  09 00 11 03
!                       09          = NORMAL-X (STRESS)
!                          00       = SKIP
!                             11 03 = SCAN BY 3RD AND 11TH FIELDS
!
!     E.G.  TAB(9,8) = -01 04 03 54
!                      -01          = AXIAL (FORCE)
!                                54 = REPEAT 4 TIMES
!                             03    = INCREASE BY 3 ON EACH REPEAT
!                          04       = SCAN BY 4, 7, 10, AND 13TH FIELDS
!
   IF ( J400==2 ) THEN
!
!     PRINT OUT SCAN COMPONENT KEYWORDS
!
      CALL page1
      ii = 20
      GOTO 500
   ELSE
      IF ( Mach==2 .OR. Mach>=5 ) mzero = -1
      CALL sswtch(20,j)
      IF ( j==1 ) debug = .TRUE.
      err = -909
      nscan = Lencc - 1
      IF ( core(I81+3)/=mzero ) GOTO 300
      iscan = I81
      iwds = I81 + 1
      iisub = I81 + 2
      ielem = I81 + 3
      iset = I81 + 4
      icomp = I81 + 5
!     +MAX  = I81 + 6 = TOP N
!     -MAX  = I81 + 7
      irept = I81 + 8
!
!     NOTE - THE IISUB WORD WILL BE DROPPED WHEN THESE WORDS ARE
!            TRANSFERRED TO CASECC
!
      jcomp = iwds
      iend = I81 + core(I81)*2 - 1
      core(iscan) = 0
      core(iisub) = Isub
      core(ielem) = 0
      core(iset) = 0
      core(jcomp) = 0
      nwdss = 0
      nwdsf = 0
      ieq = 0
      max = 0
      min = 0
      nsv = 0
      nrp = 0
      ii = I81 + 3
   ENDIF
   DO
!
      ii = ii + 2
      jj = core(ii)
      kk = core(ii+1)
      jx = jj
      IF ( .NOT.Bit64 ) THEN
      ENDIF
!WKBD 3/94      CALL MVBITS (BLANK,0,32,JX,0)
!WKBD 3/94      CALL MVBITS (BLANK,0,32,KK,0)
      IF ( jj==Ieor ) THEN
!
!     SCAN CARD COMPLETED
!
         IF ( Nogo/=0 ) GOTO 400
         err = -643
         IF ( core(iscan)/=10000000 .AND. core(iscan)/=20000000 ) GOTO 300
         core(icomp) = core(jcomp)
         core(iwds) = 6
         IF ( core(iset)==0 ) core(iset) = -1
         IF ( core(ielem)==0 .AND. core(icomp+2)==0 ) core(ielem) = -1
         IF ( max==0 ) core(icomp+1) = 20
         IF ( max<=1 .AND. min==0 ) core(icomp+2) = -1
         IF ( core(icomp+2)==-1 ) THEN
!
!     COMPUTE HOW HIGH TOPN CAN GO ASSUMING LINK14 HAS AN OPEN CORE SIZE
!     SAME AS THAT OF LINK1
!
            IF ( core(iscan)>=20000000 ) nwdss = nwdsf
            IF ( 2*nwdss*core(icomp+1)>korsz(Icse(1)) ) CALL ifp1d(644)
         ENDIF
!
!     CONVERT NAMED COMPONENT TO FIELD NO.
!
         IF ( nsv/=0 .AND. core(ielem)/=-1 ) THEN
            i = core(ielem)
            i = etab(i)
            err = -642
            IF ( i<0 ) GOTO 300
            ie = 10
            IF ( i==14 ) ie = 16
            DO k = 1 , nsv
               IF ( core(iscan)==20000000 ) save(k) = -save(k)
               DO j = 1 , ie
                  ii = tab(j,i)/1000000
                  IF ( ii/=0 ) THEN
                     IF ( save(k)==ii ) GOTO 5
                  ENDIF
               ENDDO
!
!     5 SPECIAL CASES WHERE TAB ARRAY OF 10 IS NOT LONG ENOUGH
!     SET THE 11TH ITEM OF ECAH OF THESE 3 CASES
!
               ii = 0
               IF ( i==3 .AND. save(k)==27 ) ii = -27000004
!         TRIA1         TWIST (MOMENT)
               IF ( i==12 .AND. save(k)==18 ) ii = +18102258
!         IHEX1         OCT-SHR (STRESS)    +18102270 (IHEX2)
               IF ( i==13 .AND. save(k)==18 ) ii = +18102382
!         IHEX3         OCT-SHR (STRESS)
               IF ( i==12 .AND. save(k)==16 ) ii = +16102270
!         IHEX2         MAX-SHR (STRESS)
               IF ( i==13 .AND. save(k)==16 ) ii = +16102382
!         IHEX2         MAX-SHR (STRESS)
               err = -637
               IF ( ii>0 ) GOTO 10
               GOTO 300
 5             ii = iabs(tab(j,i))
 10            ii = mod(ii,1000000)
               DO jj = 1 , 3
                  kk = mod(ii,100)
                  IF ( core(icomp)==1 ) THEN
                     core(icomp) = 0
                     IF ( ielem==66 ) kk = 70
!                    IHEX2
                     IF ( ielem==69 ) kk = 54
!                    TRIATS
                     nrp = nrp + kk
                     kk = 0
                  ENDIF
                  IF ( kk>50 ) THEN
                     nrp = (kk-50)*100
!     NRP = 4900 FOR OPEN-END REPEAT FLAG
                     kk = 1
                  ENDIF
                  IF ( kk>0 ) core(icomp) = core(icomp) + 2**(kk-1)
                  ii = ii/100
                  IF ( ii==0 ) EXIT
               ENDDO
            ENDDO
         ENDIF
!
!     NRP/100 IS REPEAT FLAG, AND MOD(NRP,100) IS INCREMENT
!
         IF ( Nogo==0 ) THEN
            core(irept) = nrp
!
!     FINAL ERROR CHECK
!
            IF ( core(ielem)==0 .OR. core(icomp)==0 ) J400 = 1
            err = -617
            j = 0
            DO i = 1 , 8
               IF ( core(I81+j)==0 ) GOTO 300
               j = j + 1
            ENDDO
!
!     ALL GO WELL, RE-SET PARAMETERS
!     NOTE - THE (LENCC-1) WORD OF CASECC RECORDS THE NO. OF SCAN CARDS
!
            Nset = Nset + 1
            ii = (Isub-1)*Lencc
            Icse(nscan+ii) = Icse(nscan+ii) + 1
!WKBD IF (DEBUG) CALL BUG1 ('IFP1H',270,CORE(I81),9)
            I81 = I81 + 9
!
!     TURN ON STRESS OR FORCE OUTPUT FLAGS IF THEY ARE NOT ALREADY DONE
!     BY THE USER.   SET OUTPUT OPTIONS TO - ALL, NOPRINT, AND REAL
!     (WORD 23 ON CASECC IS STRESS OUTPUT FLAG, AND
!      WORD 26 ON CASECC IS FORCE  OUTPUT FLAG)
!
            IF ( core(iscan)/=20000000 .AND. Icse(23+ii+1)==0 ) THEN
               Icse(23+ii) = -1
               Icse(23+ii+1) = 2
               Icse(23+ii+2) = 1
            ENDIF
            IF ( core(iscan)==20000000 .AND. Icse(26+ii+1)==0 ) THEN
               Icse(26+ii) = -1
               Icse(26+ii+1) = 2
               Icse(26+ii+2) = 1
            ENDIF
         ENDIF
         GOTO 400
      ELSEIF ( ii>iend ) THEN
         IF ( jj<0 ) THEN
!
!     NUMERIC DATA
!
            IF ( jj==-2 ) THEN
!
!     F.P. DATA = +MAX OR -MAX
!
               err = -608
               IF ( ieq/=1 ) GOTO 300
               min = 1
               err = -636
               IF ( max>=2 ) GOTO 300
            ELSEIF ( ieq==1 ) THEN
!
!     INTEGER AFTER EQUAL SIGN = TOP N
!
               err = -608
               IF ( ieq/=1 ) GOTO 300
               err = -636
               IF ( max>=1 ) GOTO 300
            ELSE
!
!     INTEGER BEFORE EQUAL SIGN = COMPONENT(S)
!
               err = -637
               IF ( jj/=-1 .OR. kk<=1 ) GOTO 300
               err = -638
               IF ( kk>31 ) GOTO 300
               core(jcomp) = core(jcomp) + 2**(kk-1)
               CYCLE
            ENDIF
            max = max + 1
            core(icomp+max) = kk
            err = -641
            IF ( max==2 .AND. rcore(icomp+2)>rcore(icomp+1) ) GOTO 300
         ELSEIF ( jj==0 ) THEN
!
!     READ CONTINUATION CARD
!
            CALL read(*200,*200,Scr1,core(1),Nwpc,0,flag)
            WRITE (Nout,99001) Icc , (core(i),i=1,Nwpc)
99001       FORMAT (11X,I8,6X,20A4)
            Icc = Icc + 1
            Line = Line + 1
            IF ( Line>Nlpp ) CALL page
            ii = I81 + 8
            Nz = Nz - ii
            CALL xrcard(core(ii),Nz,core(1))
            ii = ii - 2
            iend = ii
         ELSE
!
!     DECODE BCD WORD
!
            iend = ii + jj*2 - 1
            ii = ii - 1
         ENDIF
      ELSE
!
!     LOOK FOR EQUAL SIGN OR SET
!
         err = -617
         IF ( jj==mzero ) THEN
            IF ( kk/=equal ) GOTO 300
            ieq = ieq + 1
            IF ( ieq>1 ) GOTO 300
            core(icomp+1) = 0
            core(icomp+2) = 0
         ELSEIF ( jx==seti ) THEN
!
!     PROCESS SET
!
            err = -635
            IF ( core(iset)/=0 ) GOTO 300
            err = -639
            IF ( ieq/=1 .OR. core(ii+2)/=-1 ) GOTO 300
            core(iset) = core(ii+3)
            ii = ii + 2
            j = Nwpc + 1 + Icse(Lencc)
            DO i = 1 , Nset
               IF ( core(iset)==core(j) ) GOTO 100
               j = j + core(j+1) + 3
            ENDDO
            err = -608
            Msst = Msst + 1
            IF ( Msst>0 ) GOTO 300
            Misset(Msst) = core(iset)
            err = 0
         ELSE
!
!     LOOK FOR STRESS OR FORCE
!
            IF ( ieq==1 ) GOTO 300
            IF ( jx/=stress ) THEN
               IF ( jx/=force ) THEN
!
!     LOOK FOR ELEMENT, DROP THE FIRST LETTER C IF NECESSARY
!
                  IF ( core(ielem)==0 ) THEN
                     jc = nam(1)
                     kc = jc
                     IF ( khrfn2(jx,1,1)==llc ) THEN
                        jc = khrfn3(Blank,jx,1,1)
                        kc = khrfn3(Blank,kk,1,1)
                        jc = khrfn1(jc,4,kk,1)
                     ENDIF
                     j = 1
                     DO i = 1 , Nelem
                        IF ( jx==E(j) .AND. kk==E(j+1) ) GOTO 12
                        IF ( jc==E(j) .AND. kc==E(j+1) ) GOTO 12
                        j = j + Incr
                     ENDDO
                  ENDIF
!
!     LOOK FOR COMPONENT
!
                  DO i = 1 , ncomp
                     IF ( jx==comp(1,i) .AND. kk==comp(2,i) ) GOTO 20
                  ENDDO
                  err = -634
                  i = 0
!
!     SP ARRAYS
!        1     2     3     4     5     6     7     8     9     10
!        R-ZR  R-U   R-RT  R-V   R-ZT  R-UV  NT-X  NT-U  NT-Y  NT-V
!        11    12    13    14    15    16    17    18    19    20
!        -U    -V    R-X   R-Y   R-41  R-12  R-23  R-34  NT-1  NT-2
!        21    22    23    24    25    26    27    28    29    30
!        R-1Z  R-2Z  AL-1  AL-2  AL-X  AL-Y  E-12  E-23  E-34  E-41
!
                  IF ( jx/=force ) GOTO 15
                  IF ( kk==sp(27) ) i = 40
!                 FORCE-12 (USED IN QDMEM2)
                  IF ( kk==sp(28) ) i = 41
!                 FORCE-23
                  IF ( kk==sp(29) ) i = 42
!                 FORCE-34
                  IF ( kk==sp(30) ) i = 43
!                 FORCE-41
                  IF ( i/=0 ) GOTO 15
                  GOTO 300
 12               core(ielem) = i
                  nwdss = E(j+17)
                  nwdsf = E(j+18)
               ELSE
                  core(iscan) = core(iscan) + 20000000
               ENDIF
               CYCLE
 15            IF ( jx/=norm .AND. jx/=shea .AND. jx/=mome ) GOTO 300
               IF ( kk==sp(1) .OR. kk==sp(2) .OR. kk==sp(13) ) i = 13
!         SHEAR-ZR          SHEAR-U           SHEAR-X
               IF ( kk==sp(3) .OR. kk==sp(4) .OR. kk==sp(14) .OR. kk==sp(17) ) i = 14
!         SHEAR-RT          SHEAR-V           SHEAR-6
!         SHEAR-23
               IF ( kk==sp(5) .OR. kk==sp(6) .OR. kk==sp(18) ) i = 15
!         SHEAR-ZT          SHEAR-UV          SHEAR-34
               IF ( i==0 ) THEN
                  IF ( kk==sp(7) .OR. kk==sp(8) ) i = 22
!         MOMENT-X          MOMENT-U
                  IF ( kk==sp(9) .OR. kk==sp(10) ) i = 23
!         MOMENT-Y          MOMENT-V
                  IF ( kk==sp(15) .OR. kk==sp(19) ) i = 12
!         SHEAR-41          MOMENT-1
                  IF ( kk==sp(11) .OR. kk==sp(25) ) i = 9
!         NORM-U            NORNAL-X
                  IF ( kk==sp(12) .OR. kk==sp(26) ) i = 10
!         NORM-V            NORNAL-Y
                  IF ( i==0 ) THEN
                     IF ( kk==sp(16) .OR. kk==sp(20) ) i = 13
!         SHEAR-12          MOMENT-2
!
!     SECOND SET KEYWORDS FOR QUAD4/TRIA3 LAYER, 81 AND HIGHER
!     (THE GENERAL QUAD4/TRIA3 KEYWORDS ARE BELOW 80)
!
                     IF ( i==0 ) THEN
                        IF ( kk==sp(23) ) i = 81
!         NORAML-1
                        IF ( kk==sp(24) ) i = 82
!         NORMAL-2
                        IF ( kk==sp(16) ) i = 84
!         SHEAR-12
                        IF ( kk==sp(21) ) i = 83
!         SHEAR-1Z
                        IF ( kk==sp(22) ) i = 85
!         SHEAR-2Z
                        IF ( i==0 ) GOTO 300
                     ENDIF
                  ENDIF
               ENDIF
            ELSE
               core(iscan) = core(iscan) + 10000000
               CYCLE
            ENDIF
!
 20         IF ( i==48 ) THEN
               J400 = 1
               GOTO 400
            ELSEIF ( i==49 ) THEN
!
!     ON-LINE
!
               WRITE (Nout,99002) Ufm
99002          FORMAT (A23,', SCAN ON-LINE OPTION IS NOT AVAILABLE IN THIS ','NASTRAN RELEASE')
               Nogo = 1
               GOTO 99999
            ELSE
               err = -640
               IF ( nsv>5 ) GOTO 300
               IF ( nsv>0 ) THEN
                  DO j = 1 , nsv
                     IF ( save(j)==i ) GOTO 25
                  ENDDO
               ENDIF
               nsv = nsv + 1
               save(nsv) = i
!
!     TWO WORDS, PRINCIPAL AND TORSIONAL, HAVE A LETTER L TOO LONG
!
!     LLL IS 4HL   , BLANK FILL
!     IBLNK IS 1H  , ZERO  FILL
!
 25            iword = core(ii+2)
!WKBD 3/94      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)
               IF ( ii<iend .AND. iword==lll .AND. core(ii+3)==Iblnk ) ii = ii + 2
            ENDIF
         ENDIF
      ENDIF
 100  ENDDO
!
 200  CALL mesage(-1,Scr1,nam)
 300  CALL ifp1d(err)
   IF ( Icse(nscan)>=0 ) THEN
      IF ( err==-634 .OR. err==-637 .OR. err==-642 ) THEN
         Icse(nscan) = -10000
         J400 = 1
      ENDIF
   ENDIF
 400  RETURN
 500  DO j = 1 , 10
      isp(j) = 0
   ENDDO
   IF ( ii==1 ) THEN
      isp(8) = 19
      isp(9) = 20
      lline = 11
   ELSEIF ( ii==2 ) THEN
      isp(7) = 7
      isp(8) = 9
      isp(9) = 13
      isp(10) = 14
      lline = 14
   ELSEIF ( ii==3 ) THEN
      WRITE (Nout,99003)
99003 FORMAT (10X,'FORCE      TWIST',15X,'4')
      Line = Line + 1
      isp(7) = 27
      isp(8) = 28
      isp(9) = 29
      isp(10) = 30
      lline = 13
   ELSEIF ( ii==4 ) THEN
      lline = 8
   ELSEIF ( ii==5 ) THEN
      lline = 9
   ELSEIF ( ii==6 ) THEN
      lline = 12
   ELSEIF ( ii==7 ) THEN
      isp(1) = 11
      isp(2) = 12
      isp(3) = 6
      isp(7) = 8
      isp(8) = 10
      lline = 13
   ELSEIF ( ii==8 ) THEN
      lline = 10
   ELSEIF ( ii==9 ) THEN
      lline = 11
   ELSEIF ( ii==10 ) THEN
      lline = 13
   ELSEIF ( ii==11 ) THEN
      lline = 14
   ELSEIF ( ii==12 ) THEN
      WRITE (Nout,99004)
99004 FORMAT (10X,'STRESS     MAX-SHR',12X,'10, 32, 54, 76 ... ETC',/10X,'STRESS     OCT-SHR',12X,'10, 32, 54, 76 ... ETC')
      Line = Line + 1
      lline = 14
   ELSEIF ( ii==13 ) THEN
      WRITE (Nout,99005)
99005 FORMAT (10X,'STRESS     MAX-SHR',12X,'10, 33, 56, 79 ... 746',/10X,'STRESS     OCT-SHR',12X,'10, 33, 56, 79 ... 746')
      Line = Line + 1
      lline = 12
   ELSEIF ( ii==14 ) THEN
      isp(1) = 25
      isp(2) = 26
      lline = 19
   ELSEIF ( ii==15 ) THEN
!
!   . QUAD4/TRIA3 LAYER
!
      isp(2) = 23
      isp(3) = 24
      isp(4) = 16
      isp(5) = 21
      isp(6) = 22
      lline = 5
   ELSEIF ( ii==16 ) THEN
!
      WRITE (Nout,99006)
99006 FORMAT (1X)
      GOTO 600
   ELSEIF ( ii==20 ) THEN
      ii = 0
      WRITE (Nout,99007)
99007 FORMAT (46H0*** COMPONENT KEYWORDS FOR THE SCAN OPERATION,//5X,59HFORCE/STRESS    KEYWORD        COMPONENT (OUTPUT FIELD NO.),&
            & /5X,15(4H----),/)
      lline = 15
   ELSE
      GOTO 600
   ENDIF
   DO
!
      ii = ii + 1
      CALL page2(lline)
      IF ( ii==2 ) THEN
         WRITE (Nout,99008)
99008    FORMAT (/5X,12HSHEAR, TWIST,/)
      ELSEIF ( ii==3 ) THEN
         WRITE (Nout,99009)
99009    FORMAT (/5X,47HTRIA1, TRIA2, QUAD1, QUAD2, TRBSC, TRPLT, QDPLT,/)
      ELSEIF ( ii==4 ) THEN
         WRITE (Nout,99010)
99010    FORMAT (/5X,28HTRMEM, QDMEM, QDMEM1, QDMEM2,/)
      ELSEIF ( ii==5 .OR. ii==16 .OR. ii==20 ) THEN
      ELSEIF ( ii==6 ) THEN
         WRITE (Nout,99011)
99011    FORMAT (/5X,26HELAS1, ELAS2, ELAS3, IS2D8,/)
      ELSEIF ( ii==7 ) THEN
         WRITE (Nout,99012)
99012    FORMAT (/5X,10HBAR, ELBOW,/)
      ELSEIF ( ii==8 ) THEN
         WRITE (Nout,99013)
99013    FORMAT (/5X,6HCONEAX,/)
      ELSEIF ( ii==9 ) THEN
         WRITE (Nout,99014)
99014    FORMAT (/5X,6HTRIARG,/)
      ELSEIF ( ii==10 ) THEN
         WRITE (Nout,99015)
99015    FORMAT (/5X,6HTRAPRG,/)
      ELSEIF ( ii==11 ) THEN
         WRITE (Nout,99016)
99016    FORMAT (/5X,6HTORDRG,/)
      ELSEIF ( ii==12 ) THEN
         WRITE (Nout,99017)
99017    FORMAT (/5X,12HIHEX1, IHEX2,/)
      ELSEIF ( ii==13 ) THEN
         WRITE (Nout,99018)
99018    FORMAT (/5X,6HIHEX3 ,/)
      ELSEIF ( ii==14 ) THEN
         WRITE (Nout,99019)
99019    FORMAT (/5X,14HTRIAAX, TRAPAX,/)
      ELSEIF ( ii==15 ) THEN
         WRITE (Nout,99020)
99020    FORMAT (/5X,12HQUAD4, TRIA3,/)
      ELSEIF ( ii==17 ) THEN
         lline = 0
         CYCLE
      ELSEIF ( ii==18 .OR. ii==19 ) THEN
         CYCLE
      ELSE
         WRITE (Nout,99021)
99021    FORMAT (/5X,17HROD, TUBE, CONROD,/)
      ENDIF
!
      DO i = 1 , 10
         jj = tab(i,ii)
         IF ( jj==0 ) CYCLE
         bcd(1,3) = bcd(1,1)
         bcd(2,3) = bcd(2,1)
         IF ( jj<0 ) THEN
            bcd(1,3) = bcd(1,2)
            bcd(2,3) = bcd(2,2)
         ENDIF
         jj = iabs(jj)
!
!                     +-------------------+    LL(1) = XX
!     JJ = TAB(I,J)=  ! CC ! ZZ ! YY ! XX !    LL(2) = YY
!                     +-------------------+    LL(3) = ZZ
!                                              LL(4) = CC
         DO j = 1 , 4
            ll(j) = mod(jj,100)
            jj = jj/100
         ENDDO
         jj = ll(4)
!
!     QUAD4/TRIA3 LAYER IF JJ IS 81 THRU 85
!
         IF ( jj==81 .OR. jj==82 ) jj = 9
         IF ( jj>=83 .AND. jj<=85 ) jj = 12
!
         keywds(1) = comp(1,jj)
         keywds(2) = comp(2,jj)
         keywds(3) = Blank
         IF ( ii/=4 .AND. ii/=16 ) THEN
            IF ( jj==2 .OR. jj==5 ) keywds(3) = lll
            IF ( jj<9 .OR. jj>30 ) GOTO 520
            IF ( jj==11 .OR. (jj>=16 .AND. jj<=21) ) GOTO 520
         ENDIF
         j = isp(i)
         IF ( j>0 ) keywds(2) = sp(j)
 520     IF ( ll(1)>50 ) THEN
            idupl = ll(1) - 50
            inc = ll(2)
            jj = min0(idupl,4)
            kk = ll(3)
            DO j = 1 , jj
               ll(j) = kk
               kk = kk + inc
            ENDDO
            kk = inc*idupl + ll(1)
         ELSE
            ll(4) = 0
            idupl = 0
            jj = 3
         ENDIF
         DO j = 1 , jj
            IF ( ll(j)==0 ) GOTO 540
            cc(j) = comma
         ENDDO
         j = jj + 1
 540     jj = j - 1
         cc(jj) = Xblank
         WRITE (Nout,99022) bcd(1,3) , bcd(2,3) , keywds , (ll(j),cc(j),j=1,jj)
99022    FORMAT (10X,A4,A2,5X,2A4,A1,9X,4(I3,A1))
         IF ( idupl>4 ) THEN
            IF ( ii/=12 .AND. ii/=14 .AND. ii/=16 ) WRITE (Nout,99023) kk
99023       FORMAT (1H+,54X,3H...,I4)
            IF ( ii==12 .OR. ii==14 .OR. ii==16 ) WRITE (Nout,99024)
99024       FORMAT (1H+,54X,3H...,5H ETC.)
         ENDIF
      ENDDO
      GOTO 500
   ENDDO
 600  WRITE (Nout,99025)
99025 FORMAT (//5X,'USE OUTPUT FIELD NUMBER(S) TO SPECIFY COMPONENT(S)','FOR ELEMENTS OR KEYWORDS',/5X,'NOT LISTED ABOVE',/)
   RETURN
99999 RETURN
END SUBROUTINE ifp1h
