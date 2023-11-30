
SUBROUTINE ffread(Card) !HIDESTARS (*,Card)
   IMPLICIT NONE
   REAL Dum(2) , F(9) , Rc(9)
   INTEGER Ffflag , Ibmcdc , Ibuf , Iechop , Iechos , Iechou , Ierr , In , Inflag , Insave , Jc(9) , Kount , L(9) , Loop , Loop4 ,  &
         & Mchn , Ncard , Noecho , Nogo , Nout , Prom , Screen , Wasff , Xsort
   LOGICAL Notyet , Pct , Star
   CHARACTER*1 Qmark , Tmp(8)
   CHARACTER*8 Save(10) , Spill , Temp
   CHARACTER*4 Temp4
   COMMON /machin/ Mchn
   COMMON /qmarkq/ Qmark , Tmp , Spill , Save
   COMMON /system/ Ibuf , Nout , Nogo , In
   COMMON /xechox/ Ffflag , Iechou , Iechos , Iechop , Xsort , Wasff , Ncard , Dum , Noecho
   COMMON /xreadx/ Screen , Loop , Kount , Prom , Notyet , Star , Pct , Jc , L , Rc , F
   COMMON /xxread/ Inflag , Insave , Loop4 , Ibmcdc , Ierr
   CHARACTER*8 Card(10)
   INTEGER a(94) , a1 , aii , at , dot , i , ia , ib , ic , id , ie , ig , ih , ii , iisave , il , im , indx , int , ip , ir , is , &
         & iwo , j , j1 , je , jj , k , ke , kk , kkf , l12 , l94 , lash , lout , mach , noec , none , pt , sp , univc(11)
   CHARACTER*48 a48
   CHARACTER*5 a5 , seqep , seqgp
   CHARACTER*8 a8(10) , a81 , a8x(12) , blank , cancel , dbgn , dend , from , help , list , noprt , rdfl , scale1 , scale8 , skfl , &
             & slash , stop
   CHARACTER*1 c(80) , c1 , ca , cb , cc , cd , ce , cg , ch , cl , cm , cp , cr , cs , ct , cx(94) , dotc
   CHARACTER*4 echo , off , on , prompt , yes
   REAL facsf
   REAL fkk , xxxx
   LOGICAL fp , twodot
!
!     THIS ROUTINE READS INPUT CARDS IN FREE FIELD OR FIXED FIELD
!     FORMATS.
!
!     IF READFILE COMMAND IS ENCOUNTERED, IT SWITCHE THE INPUT FILE TO
!     THE ONE SPECIFIED BY READFILE UNTIL EOF IS REACHED. THEN IT
!     SWITCHES BACK TO THE NORMAL CARD READER. NESTED READFILE IS
!     ALLOWED.
!
!     IT ALSO PRINTS THE INPUT CARDS IF UNSORTED ECHO FLAG IS ONE
!
!     ALL INTEGERS, BCD, AND REAL NUMBERS ARE LEFT ADJUSTED BEFORE
!     RETURNING TO THE CALLER, XSORT2
!
!     IN BULK DATA SECTION -
!     ALL INTEGERS ARE LIMITED TO 8 DIGITS. REAL NUMBERS CAN BE UP TO 12
!     DIGITS IF INPUT VIA FREE-FIELD, OR UP TO 8 DIGITS IF FIXED-FIELD.
!     ALL REAL NUMBER MUST HAVE A DECIMAL POINT.  10E-6 OR 1+7 ARE
!     NOT ACCEPTABLE
!
!     THREE WORDS ARE ATTACHED TO THE END OF AN INPUT CARD TO BE USED
!     FOR ALPHA-NUMERIC SORTING
!
   !>>>>EQUIVALENCE (c(1),cx(1),a8(1),a8x(1),a5,a48,a81) , (kkf,fkk) , (Temp4,Temp,Tmp(1)) , (a1,a(1))
!
   DATA none , prompt , on , off , yes/4HNONE , 'PROM' , 'ON, ' , 'OFF,' , 'YES,'/
   DATA blank , dend , dbgn , from , slash/'      ' , '$   END ' , '$   ...' , ' FROM-' , '/    '/
   DATA ct , xxxx , cancel , list , lout/'.' , 4HXXXX , 'CANCEL' , 'LIST' , 3/
   DATA rdfl , skfl , dotc , echo/'READFILE' , 'SKIPFILE' , '.' , 'ECHO'/
   DATA help , iwo , scale8 , scale1 , stop/'HELP' , 60 , 'SCALE/8' , 'SCALE/10' , 'STOP'/
   DATA a , ib , noprt , seqgp , seqep/94*1H  , 0 , 'NOPRINT,' , 'SEQGP' , 'SEQEP'/
   DATA cb , cc , ce , cs , cl , cr , cd , cp , cm , cg/' ' , ',' , '=' , '*' , '(' , ')' , '$' , '+' , '-' , '%'/
   DATA l12 , l94/10 , 80/ , ca , ch , at/'/' , '!' , 2H@ /
   DATA univc/4H*ADD , 4H,E   , 8*4H     , 4H .  /
!
!     THIS ROUTINE IS A PREPROCESSOR FOR THE XREAD ROUTINE IN NASTRAN
!     WRITTEN BY G. CHAN/SPERRY,  APRIL 1985
!
!     FFFLAG IN /XECHOX/ MUST BE SET TO 1234 FOR FREE-FIELD INPUT.
!     IECHOS IS SET TO -2 IN STAND-ALONE VERSION.
!     MUST RESERVE 43 WORDS IN SEMINT FOR /XREADX/ IN ALL MACHINES.
!
!     FREE FIELD INPUT IS TRIGGERED BY THE PRESENCE OF COMMA (,) OR
!     EQUAL SIGN (=) IN COLS. 1 THRU 10, AND AFTER BEGIN BULK CARD WAS
!     READ.
!
!     FFREAD IS DESIGNED TO BE USER FRIENDLY -
!     UNDER NO CIRCUMSTANCES SHOULD THE USER BE KICKED OUT OF THE
!     COMPUTER DUE TO HIS OR HER STUPID INPUT ERROR(S).
!
!     DURING FREE-FIELD INPUT SESSION, FOUR CONTROL CARDS ARE ALLOWED -
!
!        ECHO  = SORT, UNSORT, BOTH, NONE, PUNCH, LINK1
!        PROMPT= ON, OFF, YES    (YES = ON + GENERATED CARD ECHO)
!        CANCEL= N, TO CANCEL N PREVIOUSLY GENERATED LINES
!        LIST  =-N, TO   LIST N PREVIOUSLY GENERATED LINES
!        (CANCEL AND LIST ARE AVAILABLE ONLY IN STAND-ALONE VERSION AND
!         A SAVE FILE HAS BEEN REQUESTED)
!
!     WRITTEN BY G.CHAN/UNISYS ON A COLD DECEMBER MORNING, 1983
!     REFERENCE - CHAN, G.C.: 'COSMIC/NASTRAN FREE-FIELD INPUT',
!                 12TH NASTRAN USERS' COLLOQUIUM, MAY 1984
!
!     THIS ROUTINE WILL HANDLE COMPUTER WORD OF ANY SIZE, 32,36,60,64
!     BITS, UPPER CASE AND LOWER CASE ASCII AND EBCDIT CHARACTER SETS.
!
!     VAX AND UNIX ONLY -
!     (UNIVAC TOO, ONLY IF OPEN STATEMENT IS USED FOR LOGICAL UNIT 5)
!     DURING FREE-FIELD SESSION, 94 COLUMNS, INSTEAD OF REGULARLY 80,
!     ARE ALLOWED FOR AN INPUT CARD COMING FROM CARD READER OR READFILE
!     (A MAXINUM OF 94 COLUMNS IS ALLOWED IN PRINT FORMAT 310)
!
!     THIS ROUTINE CALLS THE FOLLOWING SUPPORTING SUBROUTINES FOR BCD
!     (LEFT ADJUSTED), INTEGER, AND F.P. NUMBER CONVERSION -
!
!        INT 2 K8  - DECODES INTEGER TO A8 CHAR.
!        FP  2 K8  - DECODES F.P. NUMBER TO A8 CHAR.
!        NK1 2 IF  - ENCODES N(A1) CHAR. TO INTEGER OR F.P. NUMBER
!        NK1 2 K8  - ENCODES N(A1) CHARS. TO A A8 CHAR. WORD
!        K8  2 INT - DECODES A8 CHAR. TO INTEGER
!        K8  2 FP  - DECODES A8 CHAR. TO F.P. NUMBER
!        UPCASE    - REPLACES ANY LOWER-CASE LETTER BY ITS UPPER CASE
!
!     THIS ROUTINE WILL ALSO HANDLE 'READFILE' AND 'SKIPFILE' CARDS.
!     FILE NAME IS LIMITED UP TO 48 CHARACTERS,  8/91
!
!     THIS ROUTINE TRIES NOT TO USE SYSTEM ENCODE/DECODE FUNCTIONS,
!     SHIFT, AND ANY NON-STANDARD CHARACTER FUNCTIONS.
!
!
!     INPUT FILE LOGIC:
!
!     IN UNIVAC, INPUT CARDS ARE READ FROM CARD READER INFLAG, UNIT 5.
!     ALL OTHER INPUT FILES, NESTED OR NOT, ARE DYNAMICALLY INSERTED IN-
!     TO INPUT STREAM (WITH THE E-O-F MARK STRIPPED OFF), AND READ INTO
!     COMPUTER SYSTEM FROM UNIT 5 ALSO. IF AN E-O-F MARK ENCOUNTERED
!     BEFORE ENDDATA CARD, IT IS FATAL. INFLAG=TWO=IN=5
!
!     IN ALL OTHER MACHINES, INPUT CARDS ARE READ FROM CARD READER
!     INFLAG, UNIT 5. WHEN A READFILE CARD IS ENCOUNTERED, DATA ARE READ
!     INTO COMPUTER SYSTEM FROM UNIT INFLAG, WHICH BEGINS AT 60;
!          INFLAG = IWO = 60 FOR THE FIRST FILE
!          INFLAG = 61 FOR THE SECOND FILE
!          INFLAG = 62 FOR THE THIRD  FILE, ETC.
!     (NOTE, SINCE NASTRAN USES READFILE INTERNALLY TO READ RIGID FORMAT
!     FILE, NESTED READFILE IS NOT UNCOMMON)
!     WHEN E-O-F IS ENCOUNTERED, CURRENT FILE IS CLOSED AND INFLAG IS
!     DECREASE BY 1. INFLAG IS SET TO ZERO WHEN INFLAG .LE. IWO (END
!     OF CURRENT NESTED FILE OPERATION). NEXT READFILE, NESTED OR NOT,
!     IS ALLOWED.
!
!     ADD READFILE,NOPRINT OPTION.  2/2/1989
!     LAST REVISED, 8/1989, IMPROVED EFFICIENCY BY REDUCING CHARACTER
!     OPERATIONS (VERY IMPORTANT FOR CDC MACHINE)
!     8/93, LIBERAL READFILE NOPRINT FORMATS:
!           READFILE,NOPRINT  FILENAME
!           READFILE,NOPRINT, FILENAME
!           READFILE NOPRINT  FILENAME
!           READFILE(NOPRINT) FILENAME
!           (EMBEDDED BLANK, COMMA, BRACKETS, AND EQUAL-SIGN ALLOWED)
!           READFILE = FILENAME
!
!     INITIALIZE THE FOLLOWING ITEMS SO THAT COMPILER WILL NOT COMPLAIN
!
   DATA c1 , i , ii , jj , kk/' ' , 4*0/
!
   mach = Mchn
   IF ( mach==12 ) mach = 4
   IF ( mach>=5 ) THEN
      l12 = 12
      l94 = 94
   ENDIF
   IF ( ib==0 ) THEN
      CALL k2b(cb,ib,1)
      CALL k2b(cc,ic,1)
      CALL k2b(ce,ie,1)
      CALL k2b(cs,is,1)
      CALL k2b(cl,il,1)
      CALL k2b(cr,ir,1)
      CALL k2b(cd,id,1)
      CALL k2b(cp,ip,1)
      CALL k2b(cm,im,1)
      CALL k2b(cg,ig,1)
      CALL k2b(ca,ia,1)
      CALL k2b(ch,ih,1)
      CALL k2b(ct,pt,1)
      CALL k2b(dotc,dot,1)
      CALL khrfn1(univc(1),1,at,1)
   ENDIF
!
   IF ( Kount/=0 ) GOTO 1000
 100  IF ( Inflag==0 ) THEN
      IF ( Ffflag==1234 ) GOTO 800
!
!     10A8 INPUT
!
      READ (In,99040,END=300) Card
      Ncard = Ncard + 1
      IF ( Iechos==-2 ) WRITE (lout,99040) Card
!
      IF ( Card(1)/=skfl .OR. Card(2)/=blank ) THEN
         IF ( Card(1)/=rdfl ) GOTO 2800
         DO i = 1 , 10
            a8(i) = Card(i)
         ENDDO
         CALL k2b(a8,a,80)
         GOTO 1100
      ENDIF
   ELSE
      READ (Inflag,99041,END=300) (a8x(j),j=1,l12)
!     NCARD = NCARD + 1
      IF ( Iechos==-2 ) WRITE (lout,99041) a8x
      IF ( a81==rdfl ) THEN
         WRITE (Screen,99001)
99001    FORMAT (39H *** WARNING- NESTED READFILE OPERATION)
         GOTO 1100
      ELSEIF ( a81/=skfl .OR. a8(2)/=blank ) THEN
         IF ( Ffflag==1234 ) GOTO 900
         DO i = 1 , 10
            Card(i) = a8(i)
            Save(i) = a8(i)
         ENDDO
         GOTO 3200
      ENDIF
   ENDIF
!
!     IT IS A SKIPFILE CARD - TO SKIP TO THE END OF INPUT FILE
!
 200  IF ( Inflag==0 ) THEN
      WRITE (Nout,99002)
99002 FORMAT (/,48H *** SKIPFILE IGNORED.  FILE HAS NOT BEEN OPENED)
      GOTO 100
   ELSE
      DO
         READ (Inflag,99040,END=4900) Card
      ENDDO
   ENDIF
!
!     CLOSE FILE, AND SET INFLAG BACK TO ZERO, OR PREVIOUS FILE OPENED
!
 300  IF ( mach<5 ) THEN
      IF ( mach==1 .OR. mach==2 ) THEN
      ELSEIF ( mach==3 ) THEN
         GOTO 500
      ELSE
         IF ( Inflag==0 ) RETURN 1
         IF ( Inflag>=iwo ) REWIND Inflag
         Ierr = Ierr + 1
         IF ( Ierr>15 ) GOTO 3600
         GOTO 400
      ENDIF
   ENDIF
   IF ( Inflag==0 ) RETURN 1
 400  CLOSE (UNIT=Inflag)
 500  IF ( Inflag==0 ) RETURN 1
   Inflag = Inflag - 1
   IF ( Inflag<=iwo ) Inflag = 0
   Card(1) = dend
   Card(2) = rdfl
   DO j = 3 , 10
      Card(j) = blank
   ENDDO
   IF ( Iechos/=-2 ) THEN
      CALL page2(-2)
      Noecho = Noecho - 1
      IF ( Noecho>=0 ) WRITE (Nout,99003) Noecho
99003 FORMAT (12X,1H(,I4,' CARDS READ)')
      WRITE (Nout,99043) Card
      Noecho = 0
   ENDIF
   GOTO 100
!
 600  Loop = 0
   Loop4 = Loop - 4
   Kount = 0
   Star = .FALSE.
   Pct = .FALSE.
   Notyet = .FALSE.
   DO j = 1 , 9
      L(j) = 0
      F(j) = 0.0
   ENDDO
   IF ( Inflag>=iwo ) GOTO 100
   GOTO 800
!
!     FREE FIELD INPUT
!
 700  WRITE (Nout,99045)
   Ierr = Ierr + 1
   IF ( Ierr>3 ) GOTO 3600
   WRITE (Screen,99046) a8
   IF ( mach==4 .AND. In==5 ) REWIND In
 800  IF ( Prom/=0 ) WRITE (Screen,99004)
99004 FORMAT (7H ENTER )
   READ (In,99042,END=700) (cx(j),j=1,l94)
   Ncard = Ncard + 1
   lash = 0
 900  IF ( Iechos==-2 ) WRITE (lout,99042) cx
   CALL k2b(a8,a,l94)
   IF ( a1/=id ) THEN
!
      IF ( a81==rdfl ) GOTO 1100
      IF ( a81==skfl .AND. a8(2)==blank ) GOTO 200
      IF ( Ffflag==1234 ) THEN
         Wasff = +1
         DO i = 1 , 10
            IF ( a(i)==ic .OR. a(i)==ie ) GOTO 1000
         ENDDO
      ELSE
         DO i = 1 , 10
            Card(i) = a8(i)
         ENDDO
         GOTO 3200
      ENDIF
   ENDIF
   Wasff = -1
   IF ( Iechou/=0 .AND. Xsort/=0 ) THEN
      CALL page2(-1)
      WRITE (Nout,99005) a
99005 FORMAT (30X,94A1)
   ENDIF
   IF ( a1==id ) GOTO 100
   j = 0
   DO i = 1 , 10
      IF ( a8(i)/=blank ) j = 1
      Card(i) = a8(i)
   ENDDO
   Loop = -1
   Loop4 = Loop - 4
   IF ( j/=0 .OR. Iechos/=-2 ) GOTO 2800
   GOTO 4600
!
 1000 IF ( Iechos/=-2 ) THEN
      IF ( Iechou/=0 .AND. Kount<1 ) THEN
         CALL page2(-1)
         WRITE (Nout,99006) a
99006    FORMAT (30X,4H-FF-,4X,94A1)
      ENDIF
      IF ( Loop/=-1 ) THEN
         DO j = 1 , 10
            Card(j) = Save(j)
         ENDDO
      ENDIF
   ENDIF
   IF ( Kount/=0 ) GOTO 2200
 1100 ke = 0
   k = 0
   DO j = 1 , l94
      aii = a(j)
      IF ( aii==ib ) THEN
         IF ( ke==0 ) CYCLE
         IF ( a(ke)==ic .OR. a(ke)==il ) CYCLE
         IF ( a(j+1)==ic .OR. a(j+1)==ib ) CYCLE
         IF ( a(j+1)==ir .AND. k==1 ) THEN
            k = 0
            CYCLE
         ELSE
            aii = ic
         ENDIF
      ENDIF
      IF ( aii==id ) GOTO 1200
      ke = ke + 1
      a(ke) = aii
      c(ke) = c(j)
      IF ( aii==ic ) c(ke) = cc
      IF ( aii==il ) k = k + 1
      IF ( aii==ir ) k = k - 1
      IF ( k>1 ) GOTO 4800
   ENDDO
   IF ( k>0 ) GOTO 4800
   IF ( ke==0 ) GOTO 4600
 1200 IF ( a(ke)/=ic ) THEN
      ke = ke + 1
      a(ke) = ic
      c(ke) = cc
   ENDIF
   IF ( a81/=rdfl ) THEN
!
!     HERE WE GO
!
      kk = 0
      ii = 0
      jj = 0
      twodot = .FALSE.
      iisave = jj - 2
      jj = ii + 1
      GOTO 1800
   ELSE
!
!     IT IS A READFILE CARD -
!     CHECK NOPRINT OPTION, SET NOECHO = 1, IF FOUND.
!     LOOK FOR FILE NAME. SET INFLAG TO UNIT IWO (OR IWO+ IF NESTED
!     READFFILE), AND OPEN USERS FILE (NOT MEMBER OF A FILE AS IN IBM)
!
!     READFILE FORMAT - '(', ')', ',', AND '=' ARE IGNORED.
!
      Noecho = 0
      noec = 0
      i = 9
   ENDIF
 1300 a(1) = ib
   c(1) = cb
   c(8) = cc
   j = 0
   DO
      i = i + 1
      IF ( i>l94 ) GOTO 1600
      aii = a(i)
      IF ( aii/=ib ) THEN
         IF ( aii/=il .AND. aii/=ir .AND. aii/=ic .AND. aii/=ie ) THEN
            j = j + 1
            IF ( j>48 ) GOTO 1600
            a(j) = aii
            c(j) = c(i)
            IF ( j/=7 .OR. a81/=noprt ) CYCLE
            Noecho = 1
            noec = 1
            GOTO 1300
         ELSEIF ( noec<=0 ) THEN
            CYCLE
         ENDIF
      ENDIF
      IF ( j==0 ) CYCLE
      IF ( j<60 ) THEN
         j1 = j + 1
         DO i = j1 , 60
            c(i) = cb
            a(i) = ib
         ENDDO
      ENDIF
      IF ( mach==3 ) THEN
!
!     UNIVAC - USE SYSTEM FACSF ROUTINE, SO THAT IT CAN READ A FILE OR
!              AN ELEMENT OF A FILE. INPUT UNIT IWO IS NOT USED
!              MAKE SURE FILE NAME CONTAINS A DOT
!
         k = 0
         DO i = 1 , 48
            IF ( a(i)==dot ) GOTO 1350
            IF ( a(i)/=ib ) k = 1
            IF ( k==1 .AND. a(i)==ib ) GOTO 1320
         ENDDO
         i = 49
 1320    a(i) = dot
      ELSE
         IF ( Inflag<iwo ) Inflag = iwo - 1
         Inflag = Inflag + 1
!WKBI 8/94 ALPHA-VMS
         IF ( mach==21 ) THEN
            indx = index(a48,' ')
            a48(indx:indx) = '.'
            OPEN (UNIT=Inflag,FILE=a48,STATUS='OLD',ERR=1500)
         ELSE
            IF ( Ibmcdc==0 ) OPEN (UNIT=Inflag,FILE=a8(1),STATUS='OLD',ERR=1500)
!WKBNB 8/94 ALPHA-VMS
            IF ( Ibmcdc/=0 ) OPEN (UNIT=Inflag,FILE=a48,STATUS='OLD',ERR=1500)
         ENDIF
!WKBNE 8/94 ALPHA-VMS
!
         IF ( mach==4 ) REWIND Inflag
         GOTO 1400
      ENDIF
 1350 Inflag = In
      iwo = In
      READ (a48,99007) (univc(i),i=3,14)
99007 FORMAT (12A4)
      i = facsf(univc)
      IF ( i/=0 ) EXIT
!
 1400 Card(1) = dbgn
      Card(2) = rdfl
      Card(3) = from
      DO j = 4 , 10
         Card(j) = a8(j-3)
      ENDDO
      IF ( Iechos==-2 ) THEN
         Prom = +1
      ELSE
         CALL page2(-1)
         WRITE (Nout,99043) Card
      ENDIF
      GOTO 100
   ENDDO
!
 1500 WRITE (Nout,99008) Inflag , (a(i),i=1,j)
99008 FORMAT (//,29H *** CAN NOT OPEN FILE (UNIT=,I3,4H) - ,94A1)
   GOTO 1700
 1600 j = j - 1
   WRITE (Nout,99009) (a(i),i=1,j)
99009 FORMAT (//,23H *** FILE NAME ERROR - ,48A1)
   IF ( j>=48 ) WRITE (Nout,99010)
99010 FORMAT (5X,31HFILE NAME EXCEEDS 48 CHARACTERS)
 1700 Nogo = 1
   IF ( mach==3 .OR. mach>=5 ) WRITE (Nout,99011)
99011 FORMAT (5X,38HSUGGESTION- CHECK USER ID OR QUALIFIER)
   Inflag = Inflag - 1
   IF ( Inflag<=iwo ) Inflag = 0
   Card(1) = blank
   Card(2) = blank
   RETURN
 1800 DO
      ii = ii + 1
      IF ( ii>ke ) GOTO 2700
      aii = a(ii)
      IF ( aii/=ih ) THEN
         IF ( aii/=ie ) THEN
            IF ( jj<=1 ) THEN
               IF ( (Star .OR. Pct) .AND. Loop/=-1 ) WRITE (Nout,99012)
99012          FORMAT (' *** PREVIOUS CARD SETTING UP FOR DUPLICATION IS NOW ','ABANDONNED')
               Kount = 0
               Loop = 0
               Star = .FALSE.
               Pct = .FALSE.
               Notyet = .FALSE.
               DO j = 1 , 9
                  L(j) = 0
                  F(j) = 0.0
               ENDDO
            ENDIF
            IF ( aii==ic ) THEN
!
! ... COMMA (,):
!
               kk = kk + 1
               IF ( kk/=1 .AND. kk/=10 ) THEN
                  je = ii - 1
                  IF ( je>jj ) THEN
                     i = 0
                     DO j = jj , je
                        IF ( a(j)==pt ) i = i + 1
                     ENDDO
                     IF ( i>1 ) THEN
                        IF ( a5/=seqgp .AND. a5/=seqep ) THEN
                           WRITE (Screen,99013) (a(j),j=jj,je)
99013                      FORMAT (5X,27HMORE THAN ONE DEC. PT.,  - ,16A1)
                           GOTO 3400
                        ELSE
                           twodot = .TRUE.
                           Loop = -1
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
               CALL nk12k8(*3800,c(jj),ii-jj,Card(kk),1)
               iisave = jj - 2
               jj = ii + 1
               CYCLE
            ELSEIF ( aii==ia ) THEN
!
! ... SLASH (/):
!
               IF ( iisave>0 ) THEN
                  a(ii) = ih
                  c(ii) = ch
                  ii = ii + 1
                  IF ( a(ii)==ic ) THEN
                     a(ii) = ih
                     c(ii) = ch
                  ENDIF
                  ii = iisave - 1
                  jj = ii + 1
               ELSEIF ( lash==0 .AND. kk==0 ) THEN
!
!     A DELETE CARD (/) READ
!
                  lash = +1
                  iisave = jj - 2
                  jj = ii + 1
               ELSE
                  j = kk + 1
                  WRITE (Nout,99014) j
99014             FORMAT (34H *** ILLEGAL USE OF SLASH IN FIELD,I3)
                  jj = ii + 1
               ENDIF
               CYCLE
            ELSEIF ( aii==ir ) THEN
!
! ... RIGHT BRACKET ):
!
               IF ( kk==0 ) THEN
                  kk = 1
                  Card(kk) = Save(10)
                  ii = ii + 1
                  IF ( ii>ke .OR. a(ii)/=ic ) GOTO 3400
                  iisave = jj - 2
                  jj = ii + 1
                  CYCLE
               ELSE
                  IF ( ii+1>=ke ) GOTO 3900
                  aii = a(ii+1)
                  IF ( aii==is .OR. aii==ie ) GOTO 3900
                  j = 10
                  int = 1
                  IF ( aii/=ip ) CALL nk12if(*4200,c(jj),ii-jj,j,int)
                  IF ( j<=0 .OR. j>10 ) THEN
                     WRITE (Screen,99015)
99015                FORMAT (5X,'INDEX ERROR BEFORE RIGHT BRACKET )')
                     GOTO 3500
                  ELSE
                     IF ( j>kk ) THEN
                        kk = kk + 1
                        DO k = kk , j
                           Card(k) = blank
                        ENDDO
                        kk = j
                     ENDIF
                     IF ( a(ii+1)==ic ) ii = ii + 1
                     jj = ii + 1
                     DO
                        ii = ii + 1
                        IF ( ii>ke ) GOTO 2600
                        IF ( a(ii)==ic ) GOTO 2600
                     ENDDO
                  ENDIF
               ENDIF
            ELSEIF ( aii==is .OR. aii==ig ) THEN
!
! ... STAR (*), OR PERCENTAGE (%):
!
               sp = aii
               ii = ii + 1
               IF ( a(ii)/=il ) GOTO 4300
               jj = ii + 1
               fp = .FALSE.
               IF ( .NOT.(Star .OR. Pct) ) THEN
                  DO k = 1 , 9
                     L(k) = 0
                     F(k) = 0.0
                  ENDDO
               ENDIF
               IF ( sp==is ) Star = .TRUE.
               IF ( sp==ig ) Pct = .TRUE.
               DO
                  ii = ii + 1
                  aii = a(ii)
                  IF ( ii>ke .OR. aii==ic ) GOTO 4300
                  IF ( aii==pt ) fp = .TRUE.
                  IF ( ii>jj .AND. (aii==ip .OR. aii==im) ) fp = .TRUE.
                  IF ( aii==ir ) THEN
                     IF ( ii<=jj ) GOTO 4300
                     kk = kk + 1
                     IF ( fp ) THEN
                        int = -1
                        CALL nk12if(*4200,c(jj),ii-jj,kkf,int)
                        F(kk) = fkk
                        CALL k82fp(*3700,Save(kk),8,Rc(kk),int)
                        GOTO 2400
                     ELSE
                        int = 1
                        CALL nk12if(*4100,c(jj),ii-jj,L(kk),int)
                        CALL k82int(*3700,Save(kk),8,Jc(kk),int)
                        GOTO 2300
                     ENDIF
                  ENDIF
               ENDDO
            ELSE
               IF ( aii/=il ) CYCLE
               WRITE (Nout,99016)
99016          FORMAT (/,73H *** LEFT BRACKET ENCOUNTERED WITHOUT FIRST PRECEEDED BY '=', '*', OR '%')
               GOTO 3400
            ENDIF
!
! ... EQUAL (=):
!
         ELSEIF ( jj/=ii ) THEN
!
! ... ECHO OR PROMPT:
!
            CALL nk12k8(*3800,c(jj),ii-jj,Temp,1)
            IF ( Temp==cancel .OR. Temp==list ) THEN
!
! ... CANCEL = N, LIST = +N
!
               IF ( Iechos/=-2 ) THEN
                  WRITE (Nout,99017)
99017             FORMAT (/,26H *** FEATURE NOT AVAILABLE)
                  IF ( Iechos/=-2 ) WRITE (Screen,99046) a8
                  GOTO 100
               ELSE
                  Card(1) = Temp
                  jj = ii + 1
                  DO
                     ii = ii + 1
                     IF ( a(ii)==ic ) THEN
                        int = 1
                        CALL nk12if(*4100,c(jj),ii-jj,Jc(1),int)
                        IF ( Temp==cancel .AND. Jc(1)<=0 ) GOTO 4100
                        IF ( Temp==list .AND. Jc(1)<=0 ) GOTO 4100
                        Card(3) = Temp
                        GOTO 3200
                     ENDIF
                  ENDDO
               ENDIF
            ELSEIF ( Temp4==echo ) THEN
               WRITE (Screen,99018)
99018          FORMAT (45H *** SO BE IT.  TO RUN NASTRAN LINK1 ONLY ***,/)
               GOTO 100
            ELSE
               IF ( Temp4/=prompt ) GOTO 3400
               CALL nk12k8(*3800,c(ii+1),4,Temp,-1)
               IF ( Temp4/=on .AND. Temp4/=off .AND. Temp4/=yes ) GOTO 3400
               IF ( Temp4==on ) Prom = -1
               IF ( Temp4==off ) Prom = 0
               IF ( Temp4==yes ) Prom = +1
               GOTO 100
            ENDIF
         ELSE
            kk = kk + 1
            ii = ii + 1
            IF ( ii>ke ) GOTO 4000
            aii = a(ii)
            IF ( aii==il ) THEN
!
! ... DUPLICATE WITH INCREMENT, =(N):
!
               IF ( kk/=1 ) GOTO 4000
               jj = ii + 1
               DO
                  ii = ii + 1
                  IF ( ii>ke ) GOTO 4000
                  aii = a(ii)
                  IF ( aii==ir ) THEN
                     int = 1
                     CALL nk12if(*4100,c(jj),ii-jj,Loop,int)
                     IF ( Loop<=0 ) THEN
                        WRITE (Screen,99019)
99019                   FORMAT (41H *** ZERO LOOP COUNT.  NO CARDS GENERATED)
                        GOTO 3500
                     ELSE
                        Loop4 = Loop - 4
                        ii = ii + 1
                        IF ( ii+1<ke ) THEN
                           iisave = jj - 2
                           jj = ii + 1
                           GOTO 1900
                        ELSE
                           IF ( .NOT.(.NOT.Star .AND. .NOT.Pct) ) GOTO 2100
                           WRITE (Screen,99020)
99020                      FORMAT (5X,44HPREVIOUS CARD WAS NOT SET UP FOR DUPLICATION)
                           GOTO 3400
                        ENDIF
                     ENDIF
                  ELSEIF ( aii==ic .OR. aii==is .OR. aii==ie ) THEN
                     GOTO 3400
                  ENDIF
               ENDDO
            ELSE
               IF ( aii==ie ) EXIT
               IF ( aii/=ic ) GOTO 4000
               iisave = jj - 2
               jj = ii + 1
               CYCLE
            ENDIF
         ENDIF
      ENDIF
      jj = ii + 1
 1900 ENDDO
!
 2000 kk = 10
   IF ( twodot ) GOTO 3100
   IF ( Loop<=0 ) GOTO 2800
 2100 Kount = 0
   IF ( Notyet ) THEN
      Notyet = .FALSE.
      DO kk = 2 , 9
         IF ( L(kk)==none ) THEN
            L(kk) = 0
            F(kk) = (F(kk)-Rc(kk))/float(Loop)
         ELSEIF ( F(kk)/=xxxx ) THEN
            IF ( L(kk)/=0 ) Jc(kk) = Jc(kk) - L(kk)
            IF ( F(kk)/=0.0 ) Rc(kk) = Rc(kk) - F(kk)
         ELSE
            F(kk) = 0.0
            i = (L(kk)-Jc(kk))/Loop
            IF ( i*Loop+Jc(kk)/=L(kk) ) GOTO 4400
            L(kk) = i
         ENDIF
      ENDDO
   ENDIF
 2200 Kount = Kount + 1
   IF ( Kount>Loop ) GOTO 600
   DO kk = 2 , 9
      IF ( L(kk)/=0 ) THEN
         Jc(kk) = Jc(kk) + L(kk)
         CALL int2k8(*3800,Jc(kk),Card(kk))
      ELSEIF ( F(kk)/=0.0 ) THEN
         Rc(kk) = Rc(kk) + F(kk)
         CALL fp2k8(*3400,Rc(kk),Card(kk))
      ENDIF
   ENDDO
   IF ( Prom<0 .AND. Kount==Loop ) WRITE (Screen,99021) Loop , Card
99021 FORMAT (/,I5,' ADDITIONAL CARDS WERE GENERATED.  LAST CARD WAS-',/1X,10A8)
   GOTO 2800
 2300 IF ( sp/=ig ) THEN
      IF ( Loop<=0 ) THEN
         Jc(kk) = Jc(kk) + L(kk)
         CALL int2k8(*3800,Jc(kk),Card(kk))
      ENDIF
      GOTO 2500
!
   ELSEIF ( Loop>0 ) THEN
      i = (L(kk)-Jc(kk))/Loop
      IF ( i*Loop+Jc(kk)/=L(kk) ) GOTO 4400
      L(kk) = i
      GOTO 2500
   ELSE
      F(kk) = xxxx
      Notyet = .TRUE.
      GOTO 2500
   ENDIF
 2400 IF ( sp==ig ) THEN
      IF ( Loop>0 ) THEN
         F(kk) = (F(kk)-Rc(kk))/float(Loop)
      ELSE
         L(kk) = none
         Notyet = .TRUE.
      ENDIF
   ELSEIF ( Loop<=0 ) THEN
      Rc(kk) = Rc(kk) + F(kk)
      CALL fp2k8(*3400,Rc(kk),Card(kk))
   ENDIF
 2500 ii = ii + 1
   iisave = jj - 2
   jj = ii + 1
   GOTO 1800
 2600 CALL nk12k8(*3400,c(jj),ii-jj,Card(j),1)
   IF ( kk>=10 ) GOTO 2000
   IF ( ii<ke ) THEN
      iisave = jj - 2
      jj = ii + 1
      GOTO 1800
   ENDIF
!
! ... END OF CARD READ
!
 2700 IF ( kk<10 ) THEN
      DO
         kk = kk + 1
         Card(kk) = blank
         IF ( kk>=10 ) GOTO 2000
      ENDDO
   ELSEIF ( kk==10 ) THEN
      GOTO 2000
   ELSE
      WRITE (Screen,99022)
99022 FORMAT (49H *** INPUT ERROR - TOO MANY FIELDS.  REPEAT INPUT)
      GOTO 3500
   ENDIF
!
!     PREPARE TO RETURN
!
 2800 IF ( Notyet ) GOTO 100
!
! ... UPDATE CONTINUATION FIELDS IF WE ARE IN A DUPLICATION LOOP
!
   IF ( Loop==-1 ) GOTO 3100
   IF ( Kount==0 .AND. .NOT.Star ) GOTO 3100
   kk = 10
   IF ( Save(kk)==blank ) GOTO 3000
 2900 Temp = Save(kk)
   IF ( Tmp(1)==cp ) THEN
      jj = 0
      DO i = 3 , 8
         IF ( Tmp(i)==cm ) jj = i
         IF ( Tmp(i)==cb ) GOTO 2950
      ENDDO
      i = 9
 2950 IF ( jj/=0 ) THEN
         int = 1
         CALL nk12if(*4700,Tmp(jj+1),i-jj-1,j,int)
         IF ( mach==3 ) THEN
!
! ... UNIVAC USES NEXT 5 CARDS INSTEAD OF THE 3 ABOVE
!
            CALL int2k8(*4100,j,Spill)
            j = 9 - jj
            DO i = 1 , j
               Tmp(jj+i) = Tmp(8+i)
            ENDDO
         ELSE
            j = j + 1
            CALL int2k8(*4100,j,Tmp(jj+1))
         ENDIF
         j = 9
         IF ( Tmp(j)/=cb ) THEN
            WRITE (Screen,99023) (Tmp(j),j=1,9)
99023       FORMAT (35H *** CONTINUATION FIELD TOO LONG - ,9A1,/5X,25HLAST GENERATED CARD WAS -,/)
            WRITE (Screen,99044) Save
            GOTO 4500
         ELSE
            Card(kk) = Temp
         ENDIF
      ENDIF
   ENDIF
 3000 IF ( kk/=1 ) THEN
      kk = 1
      GOTO 2900
   ENDIF
!
 3100 IF ( Ffflag==1234 ) THEN
      IF ( lash==+1 ) Card(1) = slash
      IF ( Prom==+1 ) THEN
         IF ( Kount<7 .OR. Kount>Loop4 ) WRITE (Screen,99044) Card
         IF ( Kount==7 .AND. Kount<=Loop4 ) WRITE (Screen,99024)
99024    FORMAT (9X,1H.,2(/,9X,1H.))
      ENDIF
      IF ( Loop/=-1 ) THEN
         DO kk = 1 , 10
            Save(kk) = Card(kk)
         ENDDO
      ENDIF
   ENDIF
   IF ( Card(1)==help .AND. Card(2)==blank .AND. Iechos==-2 ) CALL ffhelp(*100,*3300,2)
   IF ( Card(1)==stop .AND. Card(2)==blank .AND. Iechos/=-2 ) GOTO 3300
   IF ( Card(1)==scale8 .OR. Card(1)==scale1 ) THEN
      IF ( Card(1)==scale8 ) WRITE (Nout,99025) (i,i=1,10)
99025 FORMAT (/1X,10(I5,3X),/1X,5('--------++++++++'))
      IF ( Card(1)==scale1 ) WRITE (Nout,99026) (i,i=1,8)
99026 FORMAT (/1X,8I10,/1X,8('1234567890'))
      GOTO 100
   ENDIF
!
 3200 RETURN
 3300 STOP
!
!     ERRORS
!
 3400 WRITE (Screen,99045)
 3500 IF ( Iechos==-2 ) GOTO 600
   IF ( Ierr<=15 ) WRITE (Screen,99046) a8
   Nogo = 1
   Ierr = Ierr + 1
   IF ( Ierr<30 ) GOTO 600
 3600 WRITE (Screen,99027)
99027 FORMAT (48H0*** JOB TERMINATED DUE TO TOO MANY INPUT ERRORS)
   STOP
 3700 je = ii - 1
   WRITE (Screen,99028) kk , Card(kk) , (a(j),j=jj,je)
99028 FORMAT (5X,5HFIELD,I3,2H (,A8,') OF PREVIOUS CARD SHOULD NOT BE ','USED FOR',/5X,'INCREMENTATION (BY ',8A1,                   &
             &').  ZERO IS ASSUMED')
   IF ( int>0 ) Jc(kk) = 0
   IF ( int<0 ) Rc(kk) = 0.0
   IF ( int<0 ) GOTO 2400
   IF ( int==0 ) GOTO 3400
   GOTO 2300
 3800 je = ii - 1
   WRITE (Screen,99029) kk , (a(j),j=jj,je)
99029 FORMAT (5X,'FIELD',I3,' IS TOO LONG. ONLY 8 DIGITS ALLOWED - ',16A1)
   GOTO 3400
 3900 WRITE (Screen,99030) a8
99030 FORMAT (35H *** INDEX ERROR.  NO VALUE AFTER ))
   GOTO 3500
 4000 WRITE (Screen,99031)
99031 FORMAT (37H *** INPUT ERROR AFTER EQUAL SIGN (=))
   IF ( Iechos/=-2 ) THEN
      WRITE (Screen,99046) a8
      Nogo = 1
   ENDIF
   GOTO 100
 4100 je = ii - 1
   WRITE (Screen,99032) (a(j),j=jj,je)
99032 FORMAT (5X,18HINVALID INTEGER - ,16A1)
   GOTO 3400
 4200 je = ii - 1
   WRITE (Screen,99033) (a(j),j=jj,je)
99033 FORMAT (5X,22HINVALID F.P. NUMBER - ,16A1)
   GOTO 3400
 4300 WRITE (Screen,99034)
99034 FORMAT (47H *** INPUT ERROR AFTER STAR (*), OR PERCENT (%))
   GOTO 3500
 4400 WRITE (Screen,99035) kk , L(kk) , Jc(kk) , Loop
99035 FORMAT (5X,5HFIELD,I3,2H (,I8,1H-,I8,21H) IS NOT DIVIDABLE BY,I4,/5X,12HRESUME INPUT,/)
 4500 IF ( Iechos/=-2 ) Nogo = 1
   DO j = 1 , 10
      Card(j) = Save(j)
   ENDDO
   GOTO 100
 4600 WRITE (Screen,99036)
99036 FORMAT (23H *** BLANK LINE IGNORED)
   GOTO 100
 4700 WRITE (Screen,99037) Temp
99037 FORMAT (40H *** INTEGER ERROR IN CONTINUATION ID - ,A8)
   IF ( Iechos/=-2 ) WRITE (Screen,99046) a8
   GOTO 4500
 4800 WRITE (Screen,99038)
99038 FORMAT (27H *** TOO MANY LEFT BRACKETS)
   GOTO 3500
 4900 WRITE (Nout,99039)
99039 FORMAT (/,20H *** EOF ENCOUNTERED)
   IF ( mach==4 .AND. Inflag==5 ) REWIND Inflag
   GOTO 100
99040 FORMAT (10A8)
99041 FORMAT (11A8,A6)
99042 FORMAT (94A1)
99043 FORMAT (5H0*** ,10A8)
99044 FORMAT (1X,10A8)
99045 FORMAT (31H *** CARD ERROR - INPUT IGNORED)
99046 FORMAT (5X,1H',10A8,1H',/)
!
END SUBROUTINE ffread
