
SUBROUTINE nascar
!
!     NASCAR READS THE NASTRAN CARD (IF PRESENT) AND CALLS TTLPGE.
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bandit , Card(20) , Fist(2) , Flag , Idrum , Intap , Itolel , Lfist , Lhpw(4) , Logfl , Mach , Maxfil , Maxopn ,         &
         & Modcom(9) , Mxfl , Nfist , Nogo , Npfist , Outtap , Pghdg(96) , Pltflg , Sysbuf , System(100)
   REAL Rtolel
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Flag , Card
   COMMON /lhpwx / Lhpw , Mxfl
   COMMON /machin/ Mach
   COMMON /output/ Pghdg
   COMMON /system/ System
   COMMON /xfist / Nfist , Lfist , Fist
   COMMON /xmssg / Ufm , Uwm
   COMMON /xpfist/ Npfist
!
! Local variable declarations
!
   INTEGER bdt(7) , blank , buf(75) , files(2) , hdg(14) , i , ii , ixx , j , j1 , jn , k , keywds(2,17) , khr , lkeywd , m ,       &
         & mask1 , mask2 , nstrn(2) , param , topt
   INTEGER complf , orf , rshift
   REAL s1
   CHARACTER*16 s2
   EXTERNAL complf , orf , rshift
!
! End of declarations
!
   EQUIVALENCE (System(1),Sysbuf) , (System(2),Outtap) , (System(3),Nogo) , (System(4),Intap) , (System(7),Logfl) ,                 &
    & (System(20),Pltflg) , (System(29),Maxfil) , (System(30),Maxopn) , (System(34),Idrum) , (System(57),Modcom(1)) ,               &
    & (System(70),Itolel,Rtolel) , (System(77),Bandit)
!
   DATA nstrn/4HNAST , 4HRAN /
   DATA files/4HFILE , 1HS/ , blank/1H /
   DATA lkeywd/17/
   DATA keywds/4HBUFF , 4HSIZE , 4HCONF , 4HIG   , 4HMAXF , 4HILES , 4HMAXO , 4HPEN  , 4HSYST , 4HEM   , 4HKON3 , 4H60   , 4HNLIN , &
       &4HES   , 4HTITL , 4HEOPT , 4HMODC , 4HOM   , 4HHICO , 4HRE   , 4HDRUM , 4H     , 4HTRAC , 4HKS   , 4HSTST , 4H     ,        &
      & 4HBAND , 4HIT   , 4HBULK , 4HDATA , 4HPLOT , 4HOPT  , 4HLOGF , 4HL   /
   DATA hdg/4HN A  , 4HS T  , 4HR A  , 4HN  S , 4H Y S , 4H T E , 4H M   , 4HP A  , 4HR A  , 4HM E  , 4HT E  , 4HR  E , 4H C H ,    &
       &4H O  /
   DATA bdt/4HTCRI , 4HTMTH , 4HTMPC , 4HTDEP , 4HTPCH , 4HTRUN , 4HTDIM/
   DATA topt/ - 9/
!     DATA ADD    /4H@ASG,4H,T  ,4HLOG-,4HFILE,4H.,F ,4H .  /
   DATA s1 , s2/4HWORD , ' OF /SYSTEM/ IS '/
!
!
!     CONMSG (BCD7,1,1)
   mask1 = complf(0)
   mask2 = rshift(mask1,1)
!
!     CALL NSINFO TO OPEN NAINFO FILE AND PICK UP ANY PRESET SYSTEM
!     PARAMETERS FROM THE SECOND SECTION OF THAT FILE
!
   j = 2
   CALL nsinfo(j)
   IF ( j/=2 ) topt = j
   DO
!
!     READ FIRST CARD IN DATA STREAM AND CALL XRCARD TO CONVERT IT.
!     IF INPUT CARD IS BLANK, READ NEXT CARD
!
      CALL xread(*1300,Card)
      IF ( Card(1)/=blank .OR. Card(2)/=blank .OR. Card(3)/=blank .OR. Card(5)/=blank .OR. Card(7)/=blank ) THEN
         CALL xrcard(buf,75,Card)
         Flag = 1
         IF ( buf(1)<0 ) GOTO 1400
         IF ( buf(1)/=0 ) THEN
!
!     IF CARD IS NASTRAN PARAMETER CARD, ECHO IT.
!
            IF ( buf(2)/=nstrn(1) .OR. buf(3)/=nstrn(2) ) GOTO 1400
            DO i = 1 , 14
               Pghdg(i+2) = hdg(i)
            ENDDO
            IF ( System(11)<=0 ) CALL page1
            WRITE (Outtap,99032) (Card(i),i=1,20)
!
!     RETURN IF NO KEYWORD ON NASTRAN CARD
!
            IF ( buf(4)==mask2 ) GOTO 1400
            Flag = 0
!
!     IDENTIFY KEYWORDS AND BRANCH TO APPROPRIATE CODE.
!
            j = 4
            EXIT
         ELSEIF ( Nogo/=0 ) THEN
            Nogo = 0
            GOTO 1400
         ENDIF
      ENDIF
   ENDDO
 100  jn = 2*buf(1) + 1
   j1 = 1
   GOTO 400
 200  IF ( buf(j1)<0 ) GOTO 600
   IF ( buf(j1)/=0 ) GOTO 400
 300  DO
      CALL xread(*1300,Card)
      CALL xrcard(buf,75,Card)
      WRITE (Outtap,99032) Card
      IF ( buf(1)/=0 ) THEN
         j = 2
         GOTO 100
      ENDIF
   ENDDO
 400  DO WHILE ( buf(j1)/=mask2 )
      DO i = 1 , lkeywd
         IF ( buf(j)==keywds(1,i) .AND. buf(j+1)==keywds(2,i) ) GOTO 800
      ENDDO
      IF ( buf(j)==keywds(1,14) ) GOTO 700
      IF ( buf(j)==files(1) ) THEN
!
!     FILES
!
         IF ( buf(j+2)/=mask1 ) GOTO 600
         IF ( j+4>=jn ) GOTO 600
         IF ( buf(j+4)==blank .AND. buf(j+6)==mask1 ) THEN
            j = j + 8
            khr = 7
         ELSE
            j = j + 4
            khr = 0
         ENDIF
         GOTO 1000
      ELSE
         IF ( buf(j)/=blank ) GOTO 500
         j = j + 2
         IF ( buf(j+2)==mask2 ) EXIT
         IF ( buf(j)==0 ) GOTO 300
         IF ( j>=jn ) THEN
            j1 = jn + 1
            IF ( buf(j1)==mask2 ) EXIT
            jn = 2*buf(j1) + 1
            j = j1 + 1
         ENDIF
      ENDIF
   ENDDO
   GOTO 1400
!
!     PRINT MESSAGE FOR UNIDENTIFIED KEYWORD.
!
 500  WRITE (Outtap,99001) Ufm , buf(j) , buf(j+1)
99001 FORMAT (A23,' 17, UNIDENTIFIED NASTRAN CARD KEYWORD ',2A4,'.  ACCEPTABLE KEYWORDS FOLLOW ---',/1H0)
   DO i = 1 , lkeywd
      WRITE (Outtap,99002) keywds(1,i) , keywds(2,i)
99002 FORMAT (5X,2A4)
   ENDDO
   WRITE (Outtap,99003) (bdt(i),i=1,7)
99003 FORMAT (7(5X,4HBAND,A4),/5X,'FILES (MUST BE LAST IN INPUT LIST)')
   Nogo = 1
   GOTO 1400
!
!     PRINT MESSAGE FOR BAD FORMAT.
!
 600  WRITE (Outtap,99004) Ufm
99004 FORMAT (A23,' 43, INCORRECT FORMAT FOR NASTRAN CARD.')
   Nogo = 1
   GOTO 1400
!
!     . BANDIT KEYWORDS.
!
 700  i = 1400
   k = buf(j+1)
!
!     KEYWORD FOUND.
!
 800  j1 = jn + 1
   param = buf(j1+1)
   j1 = j1 + 2
   IF ( buf(j1)/=mask2 ) jn = 2*buf(j1) + j1
   j = j1 + 1
   IF ( buf(j1-2)/=-1 ) THEN
!
!     . CHECK FOR LEGAL REAL NUMBER...
!
      IF ( buf(j1-2)/=-2 ) GOTO 600
      IF ( buf(j1-2)/=-2 ) THEN
         IF ( i/=11 ) GOTO 700
!
!     UNIVAC - DRUM ALLOCATION, 1 BY POSITIONS, 2 BY TRACKS
!              DEFAULT IS 1,  150 POSITIONS  (GOOD FOR LARGE JOB)
!              IF DRUM IS 2, 1280 TRKS. IS ASSIGNED (SUITABLE FOR
!                 SMALLER JOB)
!
!     CDC - IDRUM (34TH WORD OF /SYSTEM/) IS LENGTH OF FET + DUMMY INDEX
!
         Idrum = param
         GOTO 200
      ENDIF
   ENDIF
   IF ( i==1400 ) THEN
!
!     BANDIT (77TH WORD OF SYSTEM)
!     BANDIT KEYWORDS (DEFAULT VALUES IN BRACKETS, SET BY BGRID ROUTINE)
!        BANDTCRI = (1),2,3,4     CRITERION
!        BANDTMTH = 1,2,(3)       METHOD
!        BANDTMPC = (0),1,2       MPC EQUS. AND RIGID ELEMENTS
!        BANDTDEP = (0),1         DEPENDANT GRID
!        BANDTPCH = (0),1         PUNCH SEQGP CARDS
!        BANDTRUN = (0),1         RUN/SEQGP
!        BANDTDIM = (0),1,2,...,9 SCRATCH ARRAY DIMENSION
!        BANDIT   = -1,(0)        BANDIT SKIP FLAG
!     WHERE,
!        CRITERION = 1, USE RMS WAVEFRONT TO DETERMINE BEST RESULT,
!                  = 2, BANDWIDTH,  =3, PROFILE, OR  =4, MAX WAVEFRONT
!        METHOD    = 1, CM METHOD IS USED,   3, GPS, OR     2, BOTH
!        MPC       = 0, MPC'S AND RIGID ELEM ARE NOT CONSIDERED
!                  = 1, MPC'S AND RIGID ELEM ARE USED IN RESEQUENCING
!                  = 2, ONLY RIGID ELEMENTS ARE USED IN RESEQUENCING
!        DEPEND    = 0, DEPENDANT GRID IS OMITTED IN RESEQUENCING
!                       IF MPC IS NON-ZERO
!                  = 1, DEPENDANT GRIDS ARE INCLUDED
!        PUNCH     = 0, NO SEQGP CARDS PUNCHED
!                  = 1, PUNCH OUT BANDIT GENERATED SEQGP CARDS AND
!                       TERMINATE NASTRAN JOB
!        RUN/SEQGP = 0, BANDIT WOULD QUIT IF THERE IS ONE OR MORE SEQGP
!                       CARD IN THE INPUT DECK
!                  = 1, TO FORCE BANDIT TO BE EXECUTED EVEN IF SEQGP
!                       CARDS ARE PRESENT
!        DIM       = 1,2,...,N, TO SET THE SCRATCH AREA, USED ONLY IN
!                       GPS METHOD, TO N*100. (N IS 9 OR LESS)
!                  = 0, DIMENSION IS SET TO 150
!        BANDIT    =-1, BANDIT COMPUTATION IS SKIPPED UNCONDITIONALLY
!                  = 0, BANDIT WOULD BE EXECUTED IF BULK DATA CONTAINS
!                       NO INPUT ERROR
!
      IF ( Bandit>=0 ) THEN
         IF ( k==bdt(7) .AND. param>=100 ) param = param/100
         IF ( param<0 .OR. param>9 ) GOTO 900
         DO i = 1 , 7
            IF ( k==bdt(i) ) THEN
               k = param*10**(i-1)
               GOTO 820
            ENDIF
         ENDDO
         GOTO 500
 820     Bandit = Bandit + k
      ENDIF
      GOTO 200
   ELSE
      IF ( i==2 ) GOTO 200
      IF ( i==3 ) THEN
!
!     MAXFILES UPPER LIMIT
!
         m = Mxfl
         IF ( param>m ) THEN
            WRITE (Outtap,99005) m
99005       FORMAT (' *** MAXFILES IS RESET TO THE LIMIT OF 74')
            param = m
         ENDIF
         Maxfil = param
         GOTO 200
      ELSEIF ( i==4 ) THEN
!
!     MAXOPEN
!
         IF ( param<=Maxfil ) THEN
            Maxopn = param
         ELSEIF ( param>Mxfl ) THEN
!
            m = Mxfl
            WRITE (Outtap,99006) m , m
99006       FORMAT (' *** MAXOPEN EXCEEDS MAXFILES LIMIT OF ',I3,'.  BOTH ','MAXOPEN AND MAXFILES ARE RESET ONLY TO ',I3,' EACH')
            Maxfil = m
            Maxopn = m
         ELSE
            WRITE (Outtap,99007) param
99007       FORMAT (' *** MAXOPEN EXCEEDS MAXFILES. MAXFILES IS AUTOMATICALLY',' EXPANDED TO',I4)
            Maxfil = param
            Maxopn = param
         ENDIF
         GOTO 200
      ELSEIF ( i==5 ) THEN
!
!     SYSTEM
!
         IF ( param<=0 ) GOTO 600
         IF ( param/=24 ) THEN
            DO
!
               IF ( buf(j1)<0 ) THEN
                  IF ( buf(j1)==-2 .AND. param/=70 ) GOTO 600
!
!     IGNORE THE CONFIG PARAMETER
!
                  IF ( param/=28 ) System(param) = buf(j)
!
!     SYSTEM WORD ECHO
!
                  IF ( param>=10 ) THEN
                     k = param/10
                     IF ( k<=9 ) THEN
                        IF ( k==1 .OR. k==2 ) THEN
                           IF ( param==20 ) WRITE (Outtap,99008) s1 , param , s2
99008                      FORMAT (5X,A4,I3,A16,'PLOT OPTION')
                           IF ( param==28 ) WRITE (Outtap,99009) s1 , param , s2
99009                      FORMAT (5X,A4,I3,A16,'MACHINE CONFIGURATION (IGNORED)')
                           IF ( param==29 ) WRITE (Outtap,99010) s1 , param , s2
99010                      FORMAT (5X,A4,I3,A16,'MAX FILES')
                        ELSEIF ( k==3 ) THEN
                           IF ( param==30 ) WRITE (Outtap,99011) s1 , param , s2
99011                      FORMAT (5X,A4,I3,A16,'MAX FILES OPEN')
                           IF ( param==31 ) WRITE (Outtap,99012) s1 , param , s2
99012                      FORMAT (5X,A4,I3,A16,'HI-CORE')
                           IF ( param==34 .AND. Mach/=4 ) WRITE (Outtap,99013) s1 , param , s2
99013                      FORMAT (5X,A4,I3,A16,'DRUM FLAG')
                           IF ( param==34 .AND. Mach==4 ) WRITE (Outtap,99014) s1 , param , s2
99014                      FORMAT (5X,A4,I3,A16,'NOS/NOS-BE FLAG')
                        ELSEIF ( k==4 .OR. k==5 ) THEN
                           IF ( param==42 ) WRITE (Outtap,99015) s1 , param , s2
99015                      FORMAT (5X,A4,I3,A16,'SYSTEM RELEASE DATE')
                           IF ( param==45 ) WRITE (Outtap,99016) s1 , param , s2
99016                      FORMAT (5X,A4,I3,A16,'TAPE BIT')
                           IF ( param==57 ) WRITE (Outtap,99033) s1 , param , s2
                           IF ( param==58 ) WRITE (Outtap,99017) s1 , param , s2
99017                      FORMAT (5X,A4,I3,A16,'MPYAD METHOD SELECTION')
                           IF ( param==59 ) WRITE (Outtap,99018) s1 , param , s2
99018                      FORMAT (5X,A4,I3,A16,'PLOT TAPE TRACK SPEC')
                        ELSEIF ( k==6 .OR. k==7 .OR. k==8 .OR. k==9 ) THEN
                           IF ( param>=60 .AND. param<=65 ) WRITE (Outtap,99033) s1 , param , s2
                           IF ( param==70 ) WRITE (Outtap,99019) s1 , param , s2
99019                      FORMAT (5X,A4,I3,A16,'SMA1 SINGULAR TOLERANCE')
                           IF ( param==77 ) WRITE (Outtap,99020) s1 , param , s2
99020                      FORMAT (5X,A4,I3,A16,'BANDIT/BULKDATA FLAG')
                        ELSE
                           GOTO 822
                        ENDIF
                        GOTO 824
                     ENDIF
 822                 WRITE (Outtap,99021) s1 , param , s2
99021                FORMAT (5X,A4,I3,A16,'NOT AVAILABLE. INPUT IGNORED')
                  ELSE
                     IF ( param==1 ) WRITE (Outtap,99022) s1 , param , s2
99022                FORMAT (5X,A4,I3,A16,'GINO BUFFER SIZE')
                     IF ( param==2 ) WRITE (Outtap,99023) s1 , param , s2
99023                FORMAT (5X,A4,I3,A16,'OUTPUT UNIT')
                     IF ( param==3 ) WRITE (Outtap,99024) s1 , param , s2
99024                FORMAT (5X,A4,I3,A16,'NOGO FLAG')
                     IF ( param==4 ) WRITE (Outtap,99025) s1 , param , s2
99025                FORMAT (5X,A4,I3,A16,'INPUT UNIT')
                     IF ( param==7 ) WRITE (Outtap,99026) s1 , param , s2
99026                FORMAT (5X,A4,I3,A16,'NO. OF CONSOLE LOG MESSAGES')
                     IF ( param==7 .AND. Mach==3 ) WRITE (Outtap,99027)
99027                FORMAT (1H+,31X,'. (95 MAX.)')
                     IF ( param==9 ) WRITE (Outtap,99028) s1 , param , s2
99028                FORMAT (5X,A4,I3,A16,'NO. OF LINES PER PAGE. MINIMUM 10')
                  ENDIF
!
!     SET BOTTOM LIMIT OF 10 TO NUMBER OF LINES PER PAGE
!     AND FOR UNIVAC ONLY, LIMIT THE CONSOLE LOG MESSAGES TO 95 MAXIMUM
!
 824              IF ( param==9 .AND. System(9)<10 ) System(9) = 10
                  IF ( Mach==3 .AND. param==7 .AND. System(7)>95 ) System(7) = 95
                  j1 = j1 + 2
                  j = j1 + 1
                  IF ( buf(j1)==mask2 ) GOTO 1400
                  jn = j1 + 2*buf(j1)
                  GOTO 200
               ELSEIF ( buf(j1)==0 ) THEN
                  GOTO 200
               ELSE
                  j1 = jn + 1
                  j = j1 + 1
                  IF ( buf(j1)==mask2 ) GOTO 600
                  jn = j1 + 2*buf(j1)
               ENDIF
            ENDDO
         ELSE
            WRITE (Outtap,99029)
99029       FORMAT ('0*** FATAL, USER SHOULD NOT CHANGE THE 24TH WORD OF ','/SYSTEM/')
            Nogo = 1
            GOTO 200
         ENDIF
      ELSEIF ( i==6 ) THEN
!
!     KON360/HICORE
!
         System(31) = param
         GOTO 200
      ELSEIF ( i==7 ) THEN
!
!     NLINES - BOTTOM-LIMITED TO 10
!
         System(9) = param
         IF ( System(9)<10 ) System(9) = 10
         GOTO 200
      ELSEIF ( i==8 ) THEN
!
!     TITLEOPT
!
         topt = param
         IF ( Mach==3 .AND. topt<=-2 ) Logfl = 3
         GOTO 200
      ELSEIF ( i==9 ) THEN
!
!     MODCOM COMMUNICATION AREA
!
         IF ( param<=0 ) GOTO 600
         DO
            IF ( buf(j1)<0 ) THEN
               Modcom(param) = buf(j)
               j1 = j1 + 2
               j = j1 + 1
               IF ( buf(j1)==mask2 ) GOTO 1400
               jn = j1 + 2*buf(j1)
               GOTO 200
            ELSEIF ( buf(j1)==0 ) THEN
               GOTO 200
            ELSE
               j1 = jn + 1
               j = j1 + 1
               IF ( buf(j1)==mask2 ) GOTO 600
               jn = j1 + 2*buf(j1)
            ENDIF
         ENDDO
      ELSEIF ( i==10 ) THEN
!
!     HICORE = LENGTH OF CORE ON UNIVAC, VAX, AND UNIX
!
         System(31) = param
         GOTO 200
      ELSEIF ( i==11 ) THEN
         Idrum = param
         GOTO 200
      ELSEIF ( i==12 ) THEN
!
!     PLOT TAPE TRACK SIZE    TRACK=7 IMPLIES 7 TRACK
!                             TRACK=9 IMPLIES 9 TRACK
!
         IF ( param/=7 .AND. param/=9 ) THEN
            WRITE (Outtap,99034) Uwm , param , keywds(1,12) , keywds(2,12)
            Nogo = 1
         ELSE
            IF ( param==7 ) System(59) = 1
            IF ( param==9 ) System(59) = 2
         ENDIF
         GOTO 200
      ELSEIF ( i==13 ) THEN
!
!     . ELEMENT SINGULARITY TOLERANCE (A REAL S.P. NUMBER)...
!
         Itolel = param
         IF ( buf(j1-2)==-1 ) Rtolel = Itolel
         GOTO 200
      ELSEIF ( i==14 ) THEN
         IF ( param<0 ) Bandit = -1
         IF ( param<=0 ) GOTO 200
         k = keywds(2,14)
      ELSEIF ( i==15 ) THEN
!
!     BULK DATA CHECK ONLY
!     TO TERMINATE JOB AFTER BULK DATA CHECK, AND SKIP OVER BANDIT
!     (OPTION TO PRINTOUT TIME CONSTANTS IN /NTIME/, IF BULKDATA=-3)
!
         IF ( param/=0 ) Bandit = -2
         IF ( Bandit==-2 ) Maxfil = 23
         IF ( param==-3 ) Bandit = -3
         GOTO 200
      ELSEIF ( i==16 ) THEN
!
!     PLOT OPTIONS -
!
!     PLTFLG   BULKDATA    PLOT COMMANDS       ACTION TAKEN
!     -----  ------- --   -------------  -------------------------------
!      0      NO ERROR    NO ERROR       EXECUTES ALL LINKS, NO PLOTS
!             NO ERROR       ERROR       STOPS AFTER LNK1 DATA CHECK
!                ERROR  ERR OR NO ERR    STOPS AFTER LINK1 CHECK
!      1      NO ERROR    NO ERROR       GO, ALL LINKS AND PLOTS
!             NO ERROR       ERROR       STOP AFTER LINK1 DATA CHECK
!                ERROR    NO ERROR       STOP AFTER LINK1 DATA CHECK
!                ERROR       ERROR       STOP AFTER LINK1 DATA CHECK
!      2      NO ERROR    NO ERROR       STOP AFTER UNDEFORM PLOT/LINK2
!             NO ERROR       ERROR       STOP AFTER LINK1 DATA CHECK
!                ERROR    NO ERROR       STOP AFTER UNDEFORM PLOT/LINK2
!                ERROR       ERROR       STOP AFTER LINK1 DATA CHECK
!      3      (ERROR OR   (ERROR OR      (ATTEMPT TO PLOT UNDEFORM MODEL
!             NO ERROR)   NO ERROR)      THEN STOP/LINK2)
!      4      NO ERROR    NO ERROR       GO, ALL LINKS AND PLOTS
!             NO ERROR       ERROR       STOP AFTER UNDEFORM PLOT/LINK2
!                ERROR    NO ERROR       STOP AFTER UNDEFORM PLOT/LINK2
!                ERROR       ERROR       STOP AFTER LINK1 DATA CHECK
!      5      NO ERROR    NO ERROR       GO, ALL LINKS AND PLOTS
!             NO ERROR       ERROR       GO, ALL LINKS BUT NO PLOTS
!                ERROR    NO ERROR       STOP AFTER UNDEFORM PLOT/LINK2
!                ERROR       ERROR       STOP AFTER LINK1 DATA CHECK
!     PLTFLG 0 OR 1 IS SET BY THE PRESENCE OF THE PLOT TAPE.
!     PLTFLG WILL BE RESET TO POSITIVE IN IFP1
!     CUT MAXFIL TO HALF AND SKIP BANDIT IF PLOT OPTION IS 2 OR 3
!
         IF ( param<2 .OR. param>5 ) THEN
            WRITE (Outtap,99034) Uwm , param , keywds(1,16) , keywds(2,16)
         ELSE
            IF ( param>=2 ) Pltflg = -param
            IF ( Pltflg==-2 .OR. Pltflg==-3 ) THEN
               Maxfil = 24
               Bandit = -1
            ENDIF
         ENDIF
         GOTO 200
      ELSEIF ( i==17 ) THEN
!
!     LOGFL = LOGFILE MESSAGE CONTROL ON UNIVAC 1100
!
         Logfl = param
         GOTO 200
      ELSE
!
!     BUFFSIZE
!
         Sysbuf = param
!
!     IGNORE THE CONFIG PARAMETER
!
         GOTO 200
      ENDIF
   ENDIF
 900  WRITE (Outtap,99034) Uwm , param , keywds(1,14) , k
   GOTO 200
 1000 IF ( buf(j)==mask1 .OR. khr==1 ) THEN
      j = j + 2
      GOTO 200
   ELSE
      DO ii = 1 , Npfist
         IF ( buf(j)==Fist(2*ii-1) ) GOTO 1100
      ENDDO
      DO i = 1 , lkeywd
         IF ( buf(j)==keywds(1,i) .AND. buf(j+1)==keywds(2,i) ) GOTO 800
      ENDDO
      IF ( buf(j)/=blank ) WRITE (Outtap,99030) Uwm , buf(j)
99030 FORMAT (A25,' 64, ',A4,' IS NOT DEFINED AS A NASTRAN FILE AND ','WILL BE IGNORED.')
      GOTO 1200
   ENDIF
 1100 ixx = 2**(ii-1)
   System(45) = orf(System(45),ixx)
 1200 j = j + 2
   khr = khr + 1
   IF ( j<jn ) GOTO 1000
   j1 = jn + 1
   IF ( buf(j1)==mask2 ) GOTO 1400
   IF ( buf(j1)/=0 ) GOTO 600
   DO
      CALL xread(*1300,Card)
      CALL xrcard(buf,75,Card)
      WRITE (Outtap,99032) (Card(i),i=1,20)
      IF ( buf(1)/=0 ) THEN
         j = 2
         j1 = 1
         jn = 2*buf(1) + 1
         GOTO 1000
      ENDIF
   ENDDO
!
!
!     END-OF-FILE ENCOUNTERED ON INPUT FILE
!
 1300 WRITE (Outtap,99031) Ufm , Intap
99031 FORMAT (A23,' 74, EOF ENCOUNTERED ON UNIT ',I4,' WHILE READING THE INPUT DATA IN SUBROUTINE NASCAR')
   CALL mesage(-61,0,0)
!
!
!     GENERATE TITLE PAGE
!
 1400 DO i = 1 , 14
      Pghdg(i+2) = blank
   ENDDO
   CALL ttlpge(topt)
99032 FORMAT (5X,20A4)
99033 FORMAT (5X,A4,I3,A16,'DATA EXTRACTED FROM ADUM CARDS')
99034 FORMAT (A25,' 65, ILLEGAL VALUE OF ',I7,' IN NASTRAN ',2A4,' CARD')
!
END SUBROUTINE nascar
