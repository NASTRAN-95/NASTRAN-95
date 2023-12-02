!*==ffread.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ffread(Card) !HIDESTARS (*,Card)
   IMPLICIT NONE
   USE c_machin
   USE c_qmarkq
   USE c_system
   USE c_xechox
   USE c_xreadx
   USE c_xxread
!
! Dummy argument declarations rewritten by SPAG
!
   CHARACTER(8) , DIMENSION(10) :: Card
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(94) , SAVE :: a
   INTEGER :: a1 , aii , dot , ia , ic , id , ie , ig , ih , iisave , il , im , indx , int , ip , ir , is , j , j1 , je , k , ke ,  &
            & kkf , lash , mach , noec , pt , sp
   CHARACTER(48) :: a48
   CHARACTER(5) :: a5
   CHARACTER(8) , DIMENSION(10) :: a8
   CHARACTER(8) :: a81 , temp
   CHARACTER(8) , DIMENSION(12) :: a8x
   INTEGER , SAVE :: at , i , ib , ii , iwo , jj , kk , l12 , l94 , lout , none
   CHARACTER(8) , SAVE :: blank , cancel , dbgn , dend , from , help , list , noprt , rdfl , scale1 , scale8 , skfl , slash , stop
   CHARACTER(1) , DIMENSION(80) :: c
   CHARACTER(1) , SAVE :: c1 , ca , cb , cc , cd , ce , cg , ch , cl , cm , cp , cr , cs , ct , dotc
   CHARACTER(1) , DIMENSION(94) :: cx
   CHARACTER(4) , SAVE :: echo , off , on , prompt , yes
   REAL :: fkk
   LOGICAL :: fp , twodot
   CHARACTER(5) , SAVE :: seqep , seqgp
   CHARACTER(4) :: temp4
   INTEGER , DIMENSION(11) , SAVE :: univc
   REAL , SAVE :: xxxx
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
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
   mach = mchn
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
   IF ( kount/=0 ) GOTO 1000
 100  IF ( inflag==0 ) THEN
      IF ( ffflag==1234 ) GOTO 800
!
!     10A8 INPUT
!
      READ (in,99040,END=300) Card
      ncard = ncard + 1
      IF ( iechos==-2 ) WRITE (lout,99040) Card
!
      IF ( Card(1)/=skfl .OR. Card(2)/=blank ) THEN
         IF ( Card(1)/=rdfl ) GOTO 2700
         DO i = 1 , 10
            a8(i) = Card(i)
         ENDDO
         CALL k2b(a8,a,80)
         GOTO 1100
      ENDIF
   ELSE
      READ (inflag,99041,END=300) (a8x(j),j=1,l12)
!     NCARD = NCARD + 1
      IF ( iechos==-2 ) WRITE (lout,99041) a8x
      IF ( a81==rdfl ) THEN
         WRITE (screen,99001)
99001    FORMAT (39H *** WARNING- NESTED READFILE OPERATION)
         GOTO 1100
      ELSEIF ( a81/=skfl .OR. a8(2)/=blank ) THEN
         IF ( ffflag==1234 ) GOTO 900
         DO i = 1 , 10
            Card(i) = a8(i)
            save(i) = a8(i)
         ENDDO
         GOTO 3100
      ENDIF
   ENDIF
!
!     IT IS A SKIPFILE CARD - TO SKIP TO THE END OF INPUT FILE
!
 200  IF ( inflag==0 ) THEN
      WRITE (nout,99002)
99002 FORMAT (/,48H *** SKIPFILE IGNORED.  FILE HAS NOT BEEN OPENED)
      GOTO 100
   ELSE
      DO
         READ (inflag,99040,END=4800) Card
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
         IF ( inflag==0 ) RETURN 1
         IF ( inflag>=iwo ) REWIND inflag
         ierr = ierr + 1
         IF ( ierr>15 ) GOTO 3500
         GOTO 400
      ENDIF
   ENDIF
   IF ( inflag==0 ) RETURN 1
 400  CLOSE (UNIT=inflag)
 500  IF ( inflag==0 ) RETURN 1
   inflag = inflag - 1
   IF ( inflag<=iwo ) inflag = 0
   Card(1) = dend
   Card(2) = rdfl
   DO j = 3 , 10
      Card(j) = blank
   ENDDO
   IF ( iechos/=-2 ) THEN
      CALL page2(-2)
      noecho = noecho - 1
      IF ( noecho>=0 ) WRITE (nout,99003) noecho
99003 FORMAT (12X,1H(,I4,' CARDS READ)')
      WRITE (nout,99043) Card
      noecho = 0
   ENDIF
   GOTO 100
!
 600  loop = 0
   loop4 = loop - 4
   kount = 0
   star = .FALSE.
   pct = .FALSE.
   notyet = .FALSE.
   DO j = 1 , 9
      l(j) = 0
      f(j) = 0.0
   ENDDO
   IF ( inflag>=iwo ) GOTO 100
   GOTO 800
!
!     FREE FIELD INPUT
!
 700  WRITE (nout,99045)
   ierr = ierr + 1
   IF ( ierr>3 ) GOTO 3500
   WRITE (screen,99046) a8
   IF ( mach==4 .AND. in==5 ) REWIND in
 800  IF ( prom/=0 ) WRITE (screen,99004)
99004 FORMAT (7H ENTER )
   READ (in,99042,END=700) (cx(j),j=1,l94)
   ncard = ncard + 1
   lash = 0
 900  IF ( iechos==-2 ) WRITE (lout,99042) cx
   CALL k2b(a8,a,l94)
   IF ( a1/=id ) THEN
!
      IF ( a81==rdfl ) GOTO 1100
      IF ( a81==skfl .AND. a8(2)==blank ) GOTO 200
      IF ( ffflag==1234 ) THEN
         wasff = +1
         DO i = 1 , 10
            IF ( a(i)==ic .OR. a(i)==ie ) GOTO 1000
         ENDDO
      ELSE
         DO i = 1 , 10
            Card(i) = a8(i)
         ENDDO
         GOTO 3100
      ENDIF
   ENDIF
   wasff = -1
   IF ( iechou/=0 .AND. xsort/=0 ) THEN
      CALL page2(-1)
      WRITE (nout,99005) a
99005 FORMAT (30X,94A1)
   ENDIF
   IF ( a1==id ) GOTO 100
   j = 0
   DO i = 1 , 10
      IF ( a8(i)/=blank ) j = 1
      Card(i) = a8(i)
   ENDDO
   loop = -1
   loop4 = loop - 4
   IF ( j/=0 .OR. iechos/=-2 ) GOTO 2700
   GOTO 4500
!
 1000 IF ( iechos/=-2 ) THEN
      IF ( iechou/=0 .AND. kount<1 ) THEN
         CALL page2(-1)
         WRITE (nout,99006) a
99006    FORMAT (30X,4H-FF-,4X,94A1)
      ENDIF
      IF ( loop/=-1 ) THEN
         DO j = 1 , 10
            Card(j) = save(j)
         ENDDO
      ENDIF
   ENDIF
   IF ( kount/=0 ) GOTO 2100
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
      IF ( k>1 ) GOTO 4700
   ENDDO
   IF ( k>0 ) GOTO 4700
   IF ( ke==0 ) GOTO 4500
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
      GOTO 1700
   ELSE
!
!     IT IS A READFILE CARD -
!     CHECK NOPRINT OPTION, SET NOECHO = 1, IF FOUND.
!     LOOK FOR FILE NAME. SET INFLAG TO UNIT IWO (OR IWO+ IF NESTED
!     READFFILE), AND OPEN USERS FILE (NOT MEMBER OF A FILE AS IN IBM)
!
!     READFILE FORMAT - '(', ')', ',', AND '=' ARE IGNORED.
!
      noecho = 0
      noec = 0
      i = 9
   ENDIF
   DO
      a(1) = ib
      c(1) = cb
      c(8) = cc
      j = 0
      DO
         i = i + 1
         IF ( i>l94 ) GOTO 1500
         aii = a(i)
         IF ( aii/=ib ) THEN
            IF ( aii/=il .AND. aii/=ir .AND. aii/=ic .AND. aii/=ie ) THEN
               j = j + 1
               IF ( j>48 ) GOTO 1500
               a(j) = aii
               c(j) = c(i)
               IF ( j/=7 .OR. a81/=noprt ) CYCLE
               noecho = 1
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
               IF ( a(i)==dot ) GOTO 1220
               IF ( a(i)/=ib ) k = 1
               IF ( k==1 .AND. a(i)==ib ) GOTO 1210
            ENDDO
            i = 49
 1210       a(i) = dot
         ELSE
            IF ( inflag<iwo ) inflag = iwo - 1
            inflag = inflag + 1
!WKBI 8/94 ALPHA-VMS
            IF ( mach==21 ) THEN
               indx = index(a48,' ')
               a48(indx:indx) = '.'
               OPEN (UNIT=inflag,FILE=a48,STATUS='OLD',ERR=1400)
            ELSE
               IF ( ibmcdc==0 ) OPEN (UNIT=inflag,FILE=a8(1),STATUS='OLD',ERR=1400)
!WKBNB 8/94 ALPHA-VMS
               IF ( ibmcdc/=0 ) OPEN (UNIT=inflag,FILE=a48,STATUS='OLD',ERR=1400)
            ENDIF
!WKBNE 8/94 ALPHA-VMS
!
            IF ( mach==4 ) REWIND inflag
            GOTO 1240
         ENDIF
 1220    inflag = in
         iwo = in
         READ (a48,99007) (univc(i),i=3,14)
99007    FORMAT (12A4)
         i = facsf(univc)
         IF ( i/=0 ) EXIT
!
 1240    Card(1) = dbgn
         Card(2) = rdfl
         Card(3) = from
         DO j = 4 , 10
            Card(j) = a8(j-3)
         ENDDO
         IF ( iechos==-2 ) THEN
            prom = +1
         ELSE
            CALL page2(-1)
            WRITE (nout,99043) Card
         ENDIF
         GOTO 100
      ENDDO
      EXIT
 1300 ENDDO
!
 1400 WRITE (nout,99008) inflag , (a(i),i=1,j)
99008 FORMAT (//,29H *** CAN NOT OPEN FILE (UNIT=,I3,4H) - ,94A1)
   GOTO 1600
 1500 j = j - 1
   WRITE (nout,99009) (a(i),i=1,j)
99009 FORMAT (//,23H *** FILE NAME ERROR - ,48A1)
   IF ( j>=48 ) WRITE (nout,99010)
99010 FORMAT (5X,31HFILE NAME EXCEEDS 48 CHARACTERS)
 1600 nogo = 1
   IF ( mach==3 .OR. mach>=5 ) WRITE (nout,99011)
99011 FORMAT (5X,38HSUGGESTION- CHECK USER ID OR QUALIFIER)
   inflag = inflag - 1
   IF ( inflag<=iwo ) inflag = 0
   Card(1) = blank
   Card(2) = blank
   RETURN
 1700 DO
      ii = ii + 1
      IF ( ii>ke ) GOTO 2600
      aii = a(ii)
      IF ( aii/=ih ) THEN
         IF ( aii/=ie ) THEN
            IF ( jj<=1 ) THEN
               IF ( (star .OR. pct) .AND. loop/=-1 ) WRITE (nout,99012)
99012          FORMAT (' *** PREVIOUS CARD SETTING UP FOR DUPLICATION IS NOW ','ABANDONNED')
               kount = 0
               loop = 0
               star = .FALSE.
               pct = .FALSE.
               notyet = .FALSE.
               DO j = 1 , 9
                  l(j) = 0
                  f(j) = 0.0
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
                           WRITE (screen,99013) (a(j),j=jj,je)
99013                      FORMAT (5X,27HMORE THAN ONE DEC. PT.,  - ,16A1)
                           GOTO 3300
                        ELSE
                           twodot = .TRUE.
                           loop = -1
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
               CALL nk12k8(*3700,c(jj),ii-jj,Card(kk),1)
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
                  WRITE (nout,99014) j
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
                  Card(kk) = save(10)
                  ii = ii + 1
                  IF ( ii>ke .OR. a(ii)/=ic ) GOTO 3300
                  iisave = jj - 2
                  jj = ii + 1
                  CYCLE
               ELSE
                  IF ( ii+1>=ke ) GOTO 3800
                  aii = a(ii+1)
                  IF ( aii==is .OR. aii==ie ) GOTO 3800
                  j = 10
                  int = 1
                  IF ( aii/=ip ) CALL nk12if(*4100,c(jj),ii-jj,j,int)
                  IF ( j<=0 .OR. j>10 ) THEN
                     WRITE (screen,99015)
99015                FORMAT (5X,'INDEX ERROR BEFORE RIGHT BRACKET )')
                     GOTO 3400
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
                        IF ( ii>ke ) GOTO 2500
                        IF ( a(ii)==ic ) GOTO 2500
                     ENDDO
                  ENDIF
               ENDIF
            ELSEIF ( aii==is .OR. aii==ig ) THEN
!
! ... STAR (*), OR PERCENTAGE (%):
!
               sp = aii
               ii = ii + 1
               IF ( a(ii)/=il ) GOTO 4200
               jj = ii + 1
               fp = .FALSE.
               IF ( .NOT.(star .OR. pct) ) THEN
                  DO k = 1 , 9
                     l(k) = 0
                     f(k) = 0.0
                  ENDDO
               ENDIF
               IF ( sp==is ) star = .TRUE.
               IF ( sp==ig ) pct = .TRUE.
               DO
                  ii = ii + 1
                  aii = a(ii)
                  IF ( ii>ke .OR. aii==ic ) GOTO 4200
                  IF ( aii==pt ) fp = .TRUE.
                  IF ( ii>jj .AND. (aii==ip .OR. aii==im) ) fp = .TRUE.
                  IF ( aii==ir ) THEN
                     IF ( ii<=jj ) GOTO 4200
                     kk = kk + 1
                     IF ( fp ) THEN
                        int = -1
                        CALL nk12if(*4100,c(jj),ii-jj,kkf,int)
                        f(kk) = fkk
                        CALL k82fp(*3600,save(kk),8,rc(kk),int)
                        GOTO 2300
                     ELSE
                        int = 1
                        CALL nk12if(*4000,c(jj),ii-jj,l(kk),int)
                        CALL k82int(*3600,save(kk),8,jc(kk),int)
                        GOTO 2200
                     ENDIF
                  ENDIF
               ENDDO
            ELSE
               IF ( aii/=il ) CYCLE
               WRITE (nout,99016)
99016          FORMAT (/,73H *** LEFT BRACKET ENCOUNTERED WITHOUT FIRST PRECEEDED BY '=', '*', OR '%')
               GOTO 3300
            ENDIF
!
! ... EQUAL (=):
!
         ELSEIF ( jj/=ii ) THEN
!
! ... ECHO OR PROMPT:
!
            CALL nk12k8(*3700,c(jj),ii-jj,temp,1)
            IF ( temp==cancel .OR. temp==list ) THEN
!
! ... CANCEL = N, LIST = +N
!
               IF ( iechos/=-2 ) THEN
                  WRITE (nout,99017)
99017             FORMAT (/,26H *** FEATURE NOT AVAILABLE)
                  IF ( iechos/=-2 ) WRITE (screen,99046) a8
                  GOTO 100
               ELSE
                  Card(1) = temp
                  jj = ii + 1
                  DO
                     ii = ii + 1
                     IF ( a(ii)==ic ) THEN
                        int = 1
                        CALL nk12if(*4000,c(jj),ii-jj,jc(1),int)
                        IF ( temp==cancel .AND. jc(1)<=0 ) GOTO 4000
                        IF ( temp==list .AND. jc(1)<=0 ) GOTO 4000
                        Card(3) = temp
                        GOTO 3100
                     ENDIF
                  ENDDO
               ENDIF
            ELSEIF ( temp4==echo ) THEN
               WRITE (screen,99018)
99018          FORMAT (45H *** SO BE IT.  TO RUN NASTRAN LINK1 ONLY ***,/)
               GOTO 100
            ELSE
               IF ( temp4/=prompt ) GOTO 3300
               CALL nk12k8(*3700,c(ii+1),4,temp,-1)
               IF ( temp4/=on .AND. temp4/=off .AND. temp4/=yes ) GOTO 3300
               IF ( temp4==on ) prom = -1
               IF ( temp4==off ) prom = 0
               IF ( temp4==yes ) prom = +1
               GOTO 100
            ENDIF
         ELSE
            kk = kk + 1
            ii = ii + 1
            IF ( ii>ke ) GOTO 3900
            aii = a(ii)
            IF ( aii==il ) THEN
!
! ... DUPLICATE WITH INCREMENT, =(N):
!
               IF ( kk/=1 ) GOTO 3900
               jj = ii + 1
               DO
                  ii = ii + 1
                  IF ( ii>ke ) GOTO 3900
                  aii = a(ii)
                  IF ( aii==ir ) THEN
                     int = 1
                     CALL nk12if(*4000,c(jj),ii-jj,loop,int)
                     IF ( loop<=0 ) THEN
                        WRITE (screen,99019)
99019                   FORMAT (41H *** ZERO LOOP COUNT.  NO CARDS GENERATED)
                        GOTO 3400
                     ELSE
                        loop4 = loop - 4
                        ii = ii + 1
                        IF ( ii+1<ke ) THEN
                           iisave = jj - 2
                           jj = ii + 1
                           GOTO 1800
                        ELSE
                           IF ( .NOT.(.NOT.star .AND. .NOT.pct) ) GOTO 2000
                           WRITE (screen,99020)
99020                      FORMAT (5X,44HPREVIOUS CARD WAS NOT SET UP FOR DUPLICATION)
                           GOTO 3300
                        ENDIF
                     ENDIF
                  ELSEIF ( aii==ic .OR. aii==is .OR. aii==ie ) THEN
                     GOTO 3300
                  ENDIF
               ENDDO
            ELSE
               IF ( aii==ie ) EXIT
               IF ( aii/=ic ) GOTO 3900
               iisave = jj - 2
               jj = ii + 1
               CYCLE
            ENDIF
         ENDIF
      ENDIF
      jj = ii + 1
 1800 ENDDO
!
 1900 kk = 10
   IF ( twodot ) GOTO 3000
   IF ( loop<=0 ) GOTO 2700
 2000 kount = 0
   IF ( notyet ) THEN
      notyet = .FALSE.
      DO kk = 2 , 9
         IF ( l(kk)==none ) THEN
            l(kk) = 0
            f(kk) = (f(kk)-rc(kk))/float(loop)
         ELSEIF ( f(kk)/=xxxx ) THEN
            IF ( l(kk)/=0 ) jc(kk) = jc(kk) - l(kk)
            IF ( f(kk)/=0.0 ) rc(kk) = rc(kk) - f(kk)
         ELSE
            f(kk) = 0.0
            i = (l(kk)-jc(kk))/loop
            IF ( i*loop+jc(kk)/=l(kk) ) GOTO 4300
            l(kk) = i
         ENDIF
      ENDDO
   ENDIF
 2100 kount = kount + 1
   IF ( kount>loop ) GOTO 600
   DO kk = 2 , 9
      IF ( l(kk)/=0 ) THEN
         jc(kk) = jc(kk) + l(kk)
         CALL int2k8(*3700,jc(kk),Card(kk))
      ELSEIF ( f(kk)/=0.0 ) THEN
         rc(kk) = rc(kk) + f(kk)
         CALL fp2k8(*3300,rc(kk),Card(kk))
      ENDIF
   ENDDO
   IF ( prom<0 .AND. kount==loop ) WRITE (screen,99021) loop , Card
99021 FORMAT (/,I5,' ADDITIONAL CARDS WERE GENERATED.  LAST CARD WAS-',/1X,10A8)
   GOTO 2700
 2200 IF ( sp/=ig ) THEN
      IF ( loop<=0 ) THEN
         jc(kk) = jc(kk) + l(kk)
         CALL int2k8(*3700,jc(kk),Card(kk))
      ENDIF
      GOTO 2400
!
   ELSEIF ( loop>0 ) THEN
      i = (l(kk)-jc(kk))/loop
      IF ( i*loop+jc(kk)/=l(kk) ) GOTO 4300
      l(kk) = i
      GOTO 2400
   ELSE
      f(kk) = xxxx
      notyet = .TRUE.
      GOTO 2400
   ENDIF
 2300 IF ( sp==ig ) THEN
      IF ( loop>0 ) THEN
         f(kk) = (f(kk)-rc(kk))/float(loop)
      ELSE
         l(kk) = none
         notyet = .TRUE.
      ENDIF
   ELSEIF ( loop<=0 ) THEN
      rc(kk) = rc(kk) + f(kk)
      CALL fp2k8(*3300,rc(kk),Card(kk))
   ENDIF
 2400 ii = ii + 1
   iisave = jj - 2
   jj = ii + 1
   GOTO 1700
 2500 CALL nk12k8(*3300,c(jj),ii-jj,Card(j),1)
   IF ( kk>=10 ) GOTO 1900
   IF ( ii<ke ) THEN
      iisave = jj - 2
      jj = ii + 1
      GOTO 1700
   ENDIF
!
! ... END OF CARD READ
!
 2600 IF ( kk<10 ) THEN
      DO
         kk = kk + 1
         Card(kk) = blank
         IF ( kk>=10 ) GOTO 1900
      ENDDO
   ELSEIF ( kk==10 ) THEN
      GOTO 1900
   ELSE
      WRITE (screen,99022)
99022 FORMAT (49H *** INPUT ERROR - TOO MANY FIELDS.  REPEAT INPUT)
      GOTO 3400
   ENDIF
!
!     PREPARE TO RETURN
!
 2700 IF ( notyet ) GOTO 100
!
! ... UPDATE CONTINUATION FIELDS IF WE ARE IN A DUPLICATION LOOP
!
   IF ( loop==-1 ) GOTO 3000
   IF ( kount==0 .AND. .NOT.star ) GOTO 3000
   kk = 10
   IF ( save(kk)==blank ) GOTO 2900
 2800 temp = save(kk)
   IF ( tmp(1)==cp ) THEN
      jj = 0
      DO i = 3 , 8
         IF ( tmp(i)==cm ) jj = i
         IF ( tmp(i)==cb ) GOTO 2850
      ENDDO
      i = 9
 2850 IF ( jj/=0 ) THEN
         int = 1
         CALL nk12if(*4600,tmp(jj+1),i-jj-1,j,int)
         IF ( mach==3 ) THEN
!
! ... UNIVAC USES NEXT 5 CARDS INSTEAD OF THE 3 ABOVE
!
            CALL int2k8(*4000,j,spill)
            j = 9 - jj
            DO i = 1 , j
               tmp(jj+i) = tmp(8+i)
            ENDDO
         ELSE
            j = j + 1
            CALL int2k8(*4000,j,tmp(jj+1))
         ENDIF
         j = 9
         IF ( tmp(j)/=cb ) THEN
            WRITE (screen,99023) (tmp(j),j=1,9)
99023       FORMAT (35H *** CONTINUATION FIELD TOO LONG - ,9A1,/5X,25HLAST GENERATED CARD WAS -,/)
            WRITE (screen,99044) save
            GOTO 4400
         ELSE
            Card(kk) = temp
         ENDIF
      ENDIF
   ENDIF
 2900 IF ( kk/=1 ) THEN
      kk = 1
      GOTO 2800
   ENDIF
!
 3000 IF ( ffflag==1234 ) THEN
      IF ( lash==+1 ) Card(1) = slash
      IF ( prom==+1 ) THEN
         IF ( kount<7 .OR. kount>loop4 ) WRITE (screen,99044) Card
         IF ( kount==7 .AND. kount<=loop4 ) WRITE (screen,99024)
99024    FORMAT (9X,1H.,2(/,9X,1H.))
      ENDIF
      IF ( loop/=-1 ) THEN
         DO kk = 1 , 10
            save(kk) = Card(kk)
         ENDDO
      ENDIF
   ENDIF
   IF ( Card(1)==help .AND. Card(2)==blank .AND. iechos==-2 ) CALL ffhelp(*100,*3200,2)
   IF ( Card(1)==stop .AND. Card(2)==blank .AND. iechos/=-2 ) GOTO 3200
   IF ( Card(1)==scale8 .OR. Card(1)==scale1 ) THEN
      IF ( Card(1)==scale8 ) WRITE (nout,99025) (i,i=1,10)
99025 FORMAT (/1X,10(I5,3X),/1X,5('--------++++++++'))
      IF ( Card(1)==scale1 ) WRITE (nout,99026) (i,i=1,8)
99026 FORMAT (/1X,8I10,/1X,8('1234567890'))
      GOTO 100
   ENDIF
!
 3100 RETURN
 3200 STOP
!
!     ERRORS
!
 3300 WRITE (screen,99045)
 3400 IF ( iechos==-2 ) GOTO 600
   IF ( ierr<=15 ) WRITE (screen,99046) a8
   nogo = 1
   ierr = ierr + 1
   IF ( ierr<30 ) GOTO 600
 3500 WRITE (screen,99027)
99027 FORMAT (48H0*** JOB TERMINATED DUE TO TOO MANY INPUT ERRORS)
   STOP
 3600 je = ii - 1
   WRITE (screen,99028) kk , Card(kk) , (a(j),j=jj,je)
99028 FORMAT (5X,5HFIELD,I3,2H (,A8,') OF PREVIOUS CARD SHOULD NOT BE ','USED FOR',/5X,'INCREMENTATION (BY ',8A1,                   &
             &').  ZERO IS ASSUMED')
   IF ( int>0 ) jc(kk) = 0
   IF ( int<0 ) rc(kk) = 0.0
   IF ( int<0 ) GOTO 2300
   IF ( int==0 ) GOTO 3300
   GOTO 2200
 3700 je = ii - 1
   WRITE (screen,99029) kk , (a(j),j=jj,je)
99029 FORMAT (5X,'FIELD',I3,' IS TOO LONG. ONLY 8 DIGITS ALLOWED - ',16A1)
   GOTO 3300
 3800 WRITE (screen,99030) a8
99030 FORMAT (35H *** INDEX ERROR.  NO VALUE AFTER ))
   GOTO 3400
 3900 WRITE (screen,99031)
99031 FORMAT (37H *** INPUT ERROR AFTER EQUAL SIGN (=))
   IF ( iechos/=-2 ) THEN
      WRITE (screen,99046) a8
      nogo = 1
   ENDIF
   GOTO 100
 4000 je = ii - 1
   WRITE (screen,99032) (a(j),j=jj,je)
99032 FORMAT (5X,18HINVALID INTEGER - ,16A1)
   GOTO 3300
 4100 je = ii - 1
   WRITE (screen,99033) (a(j),j=jj,je)
99033 FORMAT (5X,22HINVALID F.P. NUMBER - ,16A1)
   GOTO 3300
 4200 WRITE (screen,99034)
99034 FORMAT (47H *** INPUT ERROR AFTER STAR (*), OR PERCENT (%))
   GOTO 3400
 4300 WRITE (screen,99035) kk , l(kk) , jc(kk) , loop
99035 FORMAT (5X,5HFIELD,I3,2H (,I8,1H-,I8,21H) IS NOT DIVIDABLE BY,I4,/5X,12HRESUME INPUT,/)
 4400 IF ( iechos/=-2 ) nogo = 1
   DO j = 1 , 10
      Card(j) = save(j)
   ENDDO
   GOTO 100
 4500 WRITE (screen,99036)
99036 FORMAT (23H *** BLANK LINE IGNORED)
   GOTO 100
 4600 WRITE (screen,99037) temp
99037 FORMAT (40H *** INTEGER ERROR IN CONTINUATION ID - ,A8)
   IF ( iechos/=-2 ) WRITE (screen,99046) a8
   GOTO 4400
 4700 WRITE (screen,99038)
99038 FORMAT (27H *** TOO MANY LEFT BRACKETS)
   GOTO 3400
 4800 WRITE (nout,99039)
99039 FORMAT (/,20H *** EOF ENCOUNTERED)
   IF ( mach==4 .AND. inflag==5 ) REWIND inflag
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
