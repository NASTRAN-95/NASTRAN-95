!*==nascar.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE nascar
!
!     NASCAR READS THE NASTRAN CARD (IF PRESENT) AND CALLS TTLPGE.
!
   USE c_blank
   USE c_lhpwx
   USE c_machin
   USE c_output
   USE c_system
   USE c_xfist
   USE c_xmssg
   USE c_xpfist
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: bandit , i , idrum , ii , intap , itolel , ixx , j , j1 , jn , k , khr , logfl , m , mask1 , mask2 , maxfil , maxopn ,&
            & nogo , outtap , param , pltflg , sysbuf
   INTEGER , DIMENSION(7) , SAVE :: bdt
   INTEGER , SAVE :: blank , lkeywd , topt
   INTEGER , DIMENSION(75) :: buf
   INTEGER , DIMENSION(2) , SAVE :: files , nstrn
   INTEGER , DIMENSION(14) , SAVE :: hdg
   INTEGER , DIMENSION(2,17) , SAVE :: keywds
   INTEGER , DIMENSION(9) :: modcom
   REAL :: rtolel
   REAL , SAVE :: s1
   CHARACTER(16) , SAVE :: s2
   EXTERNAL complf , mesage , nsinfo , orf , page1 , rshift , ttlpge , xrcard , xread
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (System(1),Sysbuf) , (System(2),Outtap) , (System(3),Nogo) , (System(4),Intap) , (System(7),Logfl) ,                 &
!>>>>    & (System(20),Pltflg) , (System(29),Maxfil) , (System(30),Maxopn) , (System(34),Idrum) , (System(57),Modcom(1)) ,               &
!>>>>    & (System(70),Itolel,Rtolel) , (System(77),Bandit)
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         SPAG_Loop_1_1: DO
!
!     READ FIRST CARD IN DATA STREAM AND CALL XRCARD TO CONVERT IT.
!     IF INPUT CARD IS BLANK, READ NEXT CARD
!
            CALL xread(*20,card)
            IF ( card(1)/=blank .OR. card(2)/=blank .OR. card(3)/=blank .OR. card(5)/=blank .OR. card(7)/=blank ) THEN
               CALL xrcard(buf,75,card)
               flag = 1
               IF ( buf(1)<0 ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( buf(1)/=0 ) THEN
!
!     IF CARD IS NASTRAN PARAMETER CARD, ECHO IT.
!
                  IF ( buf(2)/=nstrn(1) .OR. buf(3)/=nstrn(2) ) THEN
                     spag_nextblock_1 = 14
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  DO i = 1 , 14
                     pghdg(i+2) = hdg(i)
                  ENDDO
                  IF ( system(11)<=0 ) CALL page1
                  WRITE (outtap,99032) (card(i),i=1,20)
!
!     RETURN IF NO KEYWORD ON NASTRAN CARD
!
                  IF ( buf(4)==mask2 ) THEN
                     spag_nextblock_1 = 14
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  flag = 0
!
!     IDENTIFY KEYWORDS AND BRANCH TO APPROPRIATE CODE.
!
                  j = 4
                  EXIT SPAG_Loop_1_1
               ELSEIF ( nogo/=0 ) THEN
                  nogo = 0
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
         jn = 2*buf(1) + 1
         j1 = 1
         spag_nextblock_1 = 5
      CASE (3)
         IF ( buf(j1)<0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( buf(j1)/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         DO
            CALL xread(*20,card)
            CALL xrcard(buf,75,card)
            WRITE (outtap,99032) card
            IF ( buf(1)/=0 ) THEN
               j = 2
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 5
      CASE (5)
         SPAG_Loop_1_2: DO WHILE ( buf(j1)/=mask2 )
            DO i = 1 , lkeywd
               IF ( buf(j)==keywds(1,i) .AND. buf(j+1)==keywds(2,i) ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            IF ( buf(j)==keywds(1,14) ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( buf(j)==files(1) ) THEN
!
!     FILES
!
               IF ( buf(j+2)/=mask1 ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( j+4>=jn ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( buf(j+4)==blank .AND. buf(j+6)==mask1 ) THEN
                  j = j + 8
                  khr = 7
               ELSE
                  j = j + 4
                  khr = 0
               ENDIF
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( buf(j)/=blank ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               j = j + 2
               IF ( buf(j+2)==mask2 ) EXIT SPAG_Loop_1_2
               IF ( buf(j)==0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( j>=jn ) THEN
                  j1 = jn + 1
                  IF ( buf(j1)==mask2 ) EXIT SPAG_Loop_1_2
                  jn = 2*buf(j1) + 1
                  j = j1 + 1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 14
      CASE (6)
!
!     PRINT MESSAGE FOR UNIDENTIFIED KEYWORD.
!
         WRITE (outtap,99001) ufm , buf(j) , buf(j+1)
99001    FORMAT (A23,' 17, UNIDENTIFIED NASTRAN CARD KEYWORD ',2A4,'.  ACCEPTABLE KEYWORDS FOLLOW ---',/1H0)
         DO i = 1 , lkeywd
            WRITE (outtap,99002) keywds(1,i) , keywds(2,i)
99002       FORMAT (5X,2A4)
         ENDDO
         WRITE (outtap,99003) (bdt(i),i=1,7)
99003    FORMAT (7(5X,4HBAND,A4),/5X,'FILES (MUST BE LAST IN INPUT LIST)')
         nogo = 1
         spag_nextblock_1 = 14
      CASE (7)
!
!     PRINT MESSAGE FOR BAD FORMAT.
!
         WRITE (outtap,99004) ufm
99004    FORMAT (A23,' 43, INCORRECT FORMAT FOR NASTRAN CARD.')
         nogo = 1
         spag_nextblock_1 = 14
      CASE (8)
!
!     . BANDIT KEYWORDS.
!
         i = 1400
         k = buf(j+1)
         spag_nextblock_1 = 9
      CASE (9)
!
!     KEYWORD FOUND.
!
         j1 = jn + 1
         param = buf(j1+1)
         j1 = j1 + 2
         IF ( buf(j1)/=mask2 ) jn = 2*buf(j1) + j1
         j = j1 + 1
         IF ( buf(j1-2)/=-1 ) THEN
!
!     . CHECK FOR LEGAL REAL NUMBER...
!
            IF ( buf(j1-2)/=-2 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( buf(j1-2)/=-2 ) THEN
               IF ( i/=11 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     UNIVAC - DRUM ALLOCATION, 1 BY POSITIONS, 2 BY TRACKS
!              DEFAULT IS 1,  150 POSITIONS  (GOOD FOR LARGE JOB)
!              IF DRUM IS 2, 1280 TRKS. IS ASSIGNED (SUITABLE FOR
!                 SMALLER JOB)
!
!     CDC - IDRUM (34TH WORD OF /SYSTEM/) IS LENGTH OF FET + DUMMY INDEX
!
               idrum = param
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
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
            IF ( bandit>=0 ) THEN
               IF ( k==bdt(7) .AND. param>=100 ) param = param/100
               IF ( param<0 .OR. param>9 ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO i = 1 , 7
                  IF ( k==bdt(i) ) THEN
                     k = param*10**(i-1)
                     GOTO 5
                  ENDIF
               ENDDO
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
 5             bandit = bandit + k
            ENDIF
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( i==2 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( i==3 ) THEN
!
!     MAXFILES UPPER LIMIT
!
               m = mxfl
               IF ( param>m ) THEN
                  WRITE (outtap,99005) m
99005             FORMAT (' *** MAXFILES IS RESET TO THE LIMIT OF 74')
                  param = m
               ENDIF
               maxfil = param
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==4 ) THEN
!
!     MAXOPEN
!
               IF ( param<=maxfil ) THEN
                  maxopn = param
               ELSEIF ( param>mxfl ) THEN
!
                  m = mxfl
                  WRITE (outtap,99006) m , m
99006             FORMAT (' *** MAXOPEN EXCEEDS MAXFILES LIMIT OF ',I3,'.  BOTH ','MAXOPEN AND MAXFILES ARE RESET ONLY TO ',I3,     &
                         &' EACH')
                  maxfil = m
                  maxopn = m
               ELSE
                  WRITE (outtap,99007) param
99007             FORMAT (' *** MAXOPEN EXCEEDS MAXFILES. MAXFILES IS AUTOMATICALLY',' EXPANDED TO',I4)
                  maxfil = param
                  maxopn = param
               ENDIF
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==5 ) THEN
!
!     SYSTEM
!
               IF ( param<=0 ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( param/=24 ) THEN
                  DO
!
                     IF ( buf(j1)<0 ) THEN
                        IF ( buf(j1)==-2 .AND. param/=70 ) THEN
                           spag_nextblock_1 = 7
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
!
!     IGNORE THE CONFIG PARAMETER
!
                        IF ( param/=28 ) system(param) = buf(j)
!
!     SYSTEM WORD ECHO
!
                        IF ( param>=10 ) THEN
                           k = param/10
                           IF ( k<=9 ) THEN
                              IF ( k==1 .OR. k==2 ) THEN
                                 IF ( param==20 ) WRITE (outtap,99008) s1 , param , s2
99008                            FORMAT (5X,A4,I3,A16,'PLOT OPTION')
                                 IF ( param==28 ) WRITE (outtap,99009) s1 , param , s2
99009                            FORMAT (5X,A4,I3,A16,'MACHINE CONFIGURATION (IGNORED)')
                                 IF ( param==29 ) WRITE (outtap,99010) s1 , param , s2
99010                            FORMAT (5X,A4,I3,A16,'MAX FILES')
                              ELSEIF ( k==3 ) THEN
                                 IF ( param==30 ) WRITE (outtap,99011) s1 , param , s2
99011                            FORMAT (5X,A4,I3,A16,'MAX FILES OPEN')
                                 IF ( param==31 ) WRITE (outtap,99012) s1 , param , s2
99012                            FORMAT (5X,A4,I3,A16,'HI-CORE')
                                 IF ( param==34 .AND. mach/=4 ) WRITE (outtap,99013) s1 , param , s2
99013                            FORMAT (5X,A4,I3,A16,'DRUM FLAG')
                                 IF ( param==34 .AND. mach==4 ) WRITE (outtap,99014) s1 , param , s2
99014                            FORMAT (5X,A4,I3,A16,'NOS/NOS-BE FLAG')
                              ELSEIF ( k==4 .OR. k==5 ) THEN
                                 IF ( param==42 ) WRITE (outtap,99015) s1 , param , s2
99015                            FORMAT (5X,A4,I3,A16,'SYSTEM RELEASE DATE')
                                 IF ( param==45 ) WRITE (outtap,99016) s1 , param , s2
99016                            FORMAT (5X,A4,I3,A16,'TAPE BIT')
                                 IF ( param==57 ) WRITE (outtap,99033) s1 , param , s2
                                 IF ( param==58 ) WRITE (outtap,99017) s1 , param , s2
99017                            FORMAT (5X,A4,I3,A16,'MPYAD METHOD SELECTION')
                                 IF ( param==59 ) WRITE (outtap,99018) s1 , param , s2
99018                            FORMAT (5X,A4,I3,A16,'PLOT TAPE TRACK SPEC')
                              ELSEIF ( k==6 .OR. k==7 .OR. k==8 .OR. k==9 ) THEN
                                 IF ( param>=60 .AND. param<=65 ) WRITE (outtap,99033) s1 , param , s2
                                 IF ( param==70 ) WRITE (outtap,99019) s1 , param , s2
99019                            FORMAT (5X,A4,I3,A16,'SMA1 SINGULAR TOLERANCE')
                                 IF ( param==77 ) WRITE (outtap,99020) s1 , param , s2
99020                            FORMAT (5X,A4,I3,A16,'BANDIT/BULKDATA FLAG')
                              ELSE
                                 GOTO 6
                              ENDIF
                              GOTO 8
                           ENDIF
 6                         WRITE (outtap,99021) s1 , param , s2
99021                      FORMAT (5X,A4,I3,A16,'NOT AVAILABLE. INPUT IGNORED')
                        ELSE
                           IF ( param==1 ) WRITE (outtap,99022) s1 , param , s2
99022                      FORMAT (5X,A4,I3,A16,'GINO BUFFER SIZE')
                           IF ( param==2 ) WRITE (outtap,99023) s1 , param , s2
99023                      FORMAT (5X,A4,I3,A16,'OUTPUT UNIT')
                           IF ( param==3 ) WRITE (outtap,99024) s1 , param , s2
99024                      FORMAT (5X,A4,I3,A16,'NOGO FLAG')
                           IF ( param==4 ) WRITE (outtap,99025) s1 , param , s2
99025                      FORMAT (5X,A4,I3,A16,'INPUT UNIT')
                           IF ( param==7 ) WRITE (outtap,99026) s1 , param , s2
99026                      FORMAT (5X,A4,I3,A16,'NO. OF CONSOLE LOG MESSAGES')
                           IF ( param==7 .AND. mach==3 ) WRITE (outtap,99027)
99027                      FORMAT (1H+,31X,'. (95 MAX.)')
                           IF ( param==9 ) WRITE (outtap,99028) s1 , param , s2
99028                      FORMAT (5X,A4,I3,A16,'NO. OF LINES PER PAGE. MINIMUM 10')
                        ENDIF
!
!     SET BOTTOM LIMIT OF 10 TO NUMBER OF LINES PER PAGE
!     AND FOR UNIVAC ONLY, LIMIT THE CONSOLE LOG MESSAGES TO 95 MAXIMUM
!
 8                      IF ( param==9 .AND. system(9)<10 ) system(9) = 10
                        IF ( mach==3 .AND. param==7 .AND. system(7)>95 ) system(7) = 95
                        j1 = j1 + 2
                        j = j1 + 1
                        IF ( buf(j1)==mask2 ) THEN
                           spag_nextblock_1 = 14
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        jn = j1 + 2*buf(j1)
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ELSEIF ( buf(j1)==0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        j1 = jn + 1
                        j = j1 + 1
                        IF ( buf(j1)==mask2 ) THEN
                           spag_nextblock_1 = 7
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        jn = j1 + 2*buf(j1)
                     ENDIF
                  ENDDO
               ELSE
                  WRITE (outtap,99029)
99029             FORMAT ('0*** FATAL, USER SHOULD NOT CHANGE THE 24TH WORD OF ','/SYSTEM/')
                  nogo = 1
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( i==6 ) THEN
!
!     KON360/HICORE
!
               system(31) = param
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==7 ) THEN
!
!     NLINES - BOTTOM-LIMITED TO 10
!
               system(9) = param
               IF ( system(9)<10 ) system(9) = 10
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==8 ) THEN
!
!     TITLEOPT
!
               topt = param
               IF ( mach==3 .AND. topt<=-2 ) logfl = 3
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==9 ) THEN
!
!     MODCOM COMMUNICATION AREA
!
               IF ( param<=0 ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO
                  IF ( buf(j1)<0 ) THEN
                     modcom(param) = buf(j)
                     j1 = j1 + 2
                     j = j1 + 1
                     IF ( buf(j1)==mask2 ) THEN
                        spag_nextblock_1 = 14
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     jn = j1 + 2*buf(j1)
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( buf(j1)==0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     j1 = jn + 1
                     j = j1 + 1
                     IF ( buf(j1)==mask2 ) THEN
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     jn = j1 + 2*buf(j1)
                  ENDIF
               ENDDO
            ELSEIF ( i==10 ) THEN
!
!     HICORE = LENGTH OF CORE ON UNIVAC, VAX, AND UNIX
!
               system(31) = param
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==11 ) THEN
               idrum = param
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==12 ) THEN
!
!     PLOT TAPE TRACK SIZE    TRACK=7 IMPLIES 7 TRACK
!                             TRACK=9 IMPLIES 9 TRACK
!
               IF ( param/=7 .AND. param/=9 ) THEN
                  WRITE (outtap,99034) uwm , param , keywds(1,12) , keywds(2,12)
                  nogo = 1
               ELSE
                  IF ( param==7 ) system(59) = 1
                  IF ( param==9 ) system(59) = 2
               ENDIF
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==13 ) THEN
!
!     . ELEMENT SINGULARITY TOLERANCE (A REAL S.P. NUMBER)...
!
               itolel = param
               IF ( buf(j1-2)==-1 ) rtolel = itolel
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==14 ) THEN
               IF ( param<0 ) bandit = -1
               IF ( param<=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               k = keywds(2,14)
            ELSEIF ( i==15 ) THEN
!
!     BULK DATA CHECK ONLY
!     TO TERMINATE JOB AFTER BULK DATA CHECK, AND SKIP OVER BANDIT
!     (OPTION TO PRINTOUT TIME CONSTANTS IN /NTIME/, IF BULKDATA=-3)
!
               IF ( param/=0 ) bandit = -2
               IF ( bandit==-2 ) maxfil = 23
               IF ( param==-3 ) bandit = -3
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
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
                  WRITE (outtap,99034) uwm , param , keywds(1,16) , keywds(2,16)
               ELSE
                  IF ( param>=2 ) pltflg = -param
                  IF ( pltflg==-2 .OR. pltflg==-3 ) THEN
                     maxfil = 24
                     bandit = -1
                  ENDIF
               ENDIF
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==17 ) THEN
!
!     LOGFL = LOGFILE MESSAGE CONTROL ON UNIVAC 1100
!
               logfl = param
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     BUFFSIZE
!
!
!     IGNORE THE CONFIG PARAMETER
!
               sysbuf = param
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
         WRITE (outtap,99034) uwm , param , keywds(1,14) , k
         spag_nextblock_1 = 3
      CASE (11)
         IF ( buf(j)==mask1 .OR. khr==1 ) THEN
            j = j + 2
            spag_nextblock_1 = 3
         ELSE
            DO ii = 1 , npfist
               IF ( buf(j)==fist(2*ii-1) ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            DO i = 1 , lkeywd
               IF ( buf(j)==keywds(1,i) .AND. buf(j+1)==keywds(2,i) ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            IF ( buf(j)/=blank ) WRITE (outtap,99030) uwm , buf(j)
99030       FORMAT (A25,' 64, ',A4,' IS NOT DEFINED AS A NASTRAN FILE AND ','WILL BE IGNORED.')
            spag_nextblock_1 = 13
         ENDIF
      CASE (12)
         ixx = 2**(ii-1)
         system(45) = orf(system(45),ixx)
         spag_nextblock_1 = 13
      CASE (13)
         j = j + 2
         khr = khr + 1
         IF ( j<jn ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j1 = jn + 1
         IF ( buf(j1)==mask2 ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( buf(j1)/=0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO
            CALL xread(*20,card)
            CALL xrcard(buf,75,card)
            WRITE (outtap,99032) (card(i),i=1,20)
            IF ( buf(1)/=0 ) THEN
               j = 2
               j1 = 1
               jn = 2*buf(1) + 1
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!
!     END-OF-FILE ENCOUNTERED ON INPUT FILE
!
 20      WRITE (outtap,99031) ufm , intap
99031    FORMAT (A23,' 74, EOF ENCOUNTERED ON UNIT ',I4,' WHILE READING THE INPUT DATA IN SUBROUTINE NASCAR')
         CALL mesage(-61,0,0)
         spag_nextblock_1 = 14
      CASE (14)
!
!
!     GENERATE TITLE PAGE
!
         DO i = 1 , 14
            pghdg(i+2) = blank
         ENDDO
         CALL ttlpge(topt)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99032 FORMAT (5X,20A4)
99033 FORMAT (5X,A4,I3,A16,'DATA EXTRACTED FROM ADUM CARDS')
99034 FORMAT (A25,' 65, ILLEGAL VALUE OF ',I7,' IN NASTRAN ',2A4,' CARD')
!
END SUBROUTINE nascar
