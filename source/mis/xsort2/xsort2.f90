!*==xsort2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE xsort2
!
!     XSORT2 REPLACES XSORT FOR SPEED AND EFFICIENCY
!
!     XSORT2 REQUIRES IFP MODULE TO USE RCARD2 ROUTINE INSTEAD OF
!     RCARD (DUE TO THE ASTERISK POSITION IN DOUBLE FIELD INPUT
!     CARD HAS NOT BEEN MOVED TO COLUMN 8)
!
!     XSORT2 READS BULKDATA CARDS FROM THE INPUT TAPE, ADJUSTS THE
!     FIELDS, PERFORMS AN ALPHA-NUMERIC SORT ON THE CARD IMAGES FROM
!     LEFT TO RIGHT, INSERTS CONTINUATION CARDS IN THEIR PROPER
!     POSITION, AND PLACES THE RESULTING SORTED IMAGES ON THE NEW
!     PROBLEM TAPE, NPTP.
!
!     THIS ROUTINE DOES NOT USE XRECPS, RPAGE, INITCO, XFADJ, XFADJ1,
!     XBCDBI, XPRETY, EXTINT, INTEXT, CRDFLG, ISFT, AND THE CHARACTER
!     FUNCTIONS KHRFNi.
!     IT CALLS ONLY SORT2K - TO SORT IN-CORE DATA USING TWO SORT KEYS
!              AND  BISLC2 - BINARY SEARCH USING TWO SORTED KEYS
!
!     XSORT2 NEW LOGIC -
!
!     1.  INPUT BULKDATA CARDS ARE READ INTO OPEN CORE, EXCEPT CONTINU-
!         ATION (* OR +), DELETE (/), COMMENT ($), AND BLANK CARDS.
!     2.  WHEN CORE IS FULL, OR LAST INPUT DATA READ, SORT DATA IN CORE
!         AND WRITE THE ENTIRE SORTED DATA TO SEQUENTIAL GINO FILE 303.
!     3.  REPEAT 1 AND 2, AND WRITE DATA TO GINO FILES 304,305,306 ETC.
!         IF NECESSARY. UP TO 30 FILES ARE ALLOWED.
!     4.  ALL CONTINUATION CARDS ARE WRITEN TO GINO FILE 302. ALL
!         DELETES TO 301. BLANK AND COMMENT CARDS ARE IGNORED.
!     5.  WHEN ALL INPUT DATA CARDS ARE READ AND SAVED IN GINO FILE(S),
!         RE-LOAD THE DELETE CARDS FROM 301 INTO OPEN CORE SPACE, AND
!         COPY OPTP TO 301 WITH DESIGNATED CARDS DELETED.
!     6.  COMPUTE BUFFER SPACE (AT THE END OF OPEN CORE) AND THE WORK
!         SPACE (AT THE BEGINNING OF OPEN CORE) NEEDED FOR FILE MERGE
!         OPERATION, AND READ INTO CORE ALL CONTINUATION CARDS USING
!         THE REMAINING CORE SPACE.
!     7.  IF CORE SPACE IS NOT BIG ENOUGH TO HOLD ALL CONTINUATION
!         CARDS, CREATE A CONTINUATION-INDEX TABLE IN CORE, AND MOVE THE
!         CONTINUATION CARDS TO A NEW GINO FILE, WITH LARGE BLOCKS OF
!         CONTINUATION CARDS
!     8.  PRE-MERGE BULKDATA GINO FILES TO SAVE BUFFER SPACE IF MORE
!         THAN 9 GINO FILES WERE USED IN STEP 3.
!         PERFORM A 2-TO-1 MERGE IF 10 TO 17 FILES WERE INVOLVED, OR
!         A 3-TO-1 MERGE IF MORE THAN 17 FILES WERE USED IN STEP 3.
!         THE MERGE FILES ARE SAVED IN 302,303,304,305 ETC.
!     9.  MERGE ALL FILES IN SORTED ORDER, AND INSERT CONTINUATION CARDS
!         WHEN NECESSARY. THE MERGED RESULTS ARE WRITTEN TO NPTP
!     10. ECHO ANY CONTINUATION CARD WHICH HAS NO PARENT AND THEREFORE
!         NOT USED. MAKE SURE NO REDUNDANT MESSAGE FOR THE 'REMAINING'
!         CONTINUATION CARDS OF ONE 'PARENT'
!
!     NOTES FOR XREAD AND FFREAD ROUTINES, WHICH HAVE DONE SOME
!     IMPORTANT PRELIMINARY TASK -
!
!      1. XSORT2 CALLS XREAD WHICH CALLS FFREAD TO READ ALL INPUT DATA,
!         IN BOTH FIXED-FIELD AND FREE-FIELD FORMATS. UNSORTED INPUT
!         DATA IS NOW PRINTED BY FFREAD IF 'ECHO=UNSORT' IS REQUESTED.
!      2. ALL 10 BULKDATA FIELDS ARE LEFT-ADJUSTED IF INPUT ARE IN
!         FREE-FIELD FORMAT. XREAD LEFT-ADJUSTED ALL FIELDS FOR THE
!         FIXED-FIELD INPUT CASE.
!      3. XREAD PRE-CHECK ANY CONTINUATION, COMMENT, DELETE, BLANK, AND
!         ENDDATA CARDS, AND SET APPROPRIATE FLAGS IN BUF4 CONTROL ARRAY
!      4. THE FIRST THREE BULKDATA FIELDS ARE CONVERTED TO INTERNAL
!         INTEGER CODES AND SAVED IN BUF4 CONTROL ARRAY. THESE INTERNAL
!         CODES ARE READY FOR SORTING.
!      5. XREAD HANDLES BOTH SINGLE-FIELD AND/OR DOUBLE-FIELD INPUT
!         AND PASS ON THE FIRST 3 BULKDATA FIELD INFORMATION INDENTI-
!         CALLY TO THE BUF4 CONTROL ARRAY.
!      6. XREAD/FFREAD COMPLETELY ELIMINATE THE REVERSE-STORAGE PROBLEM
!         OF THE VAX MACHINE.  I.E.
!         THE CONSTANT 'ABCD' IS STORED INTERNALLY AS 'DCBA' IN THE VAX
!      7. IN DOUBLE-FIELD INPUT, THE ASTERISK (*) IN FIELD 1 REMAINS
!         WHERE IT IS. (THE OLD XSORT MOVED IT TO COL. 8 THEN TO COL. 1.
!         SUBROUTINE RCARD MUST BE MODIFIED TO HANDLE THIS DOUBLE-FIELD
!         CASE)
!      8. NO LEADING BCD-ZEROS IN FIELD 2 IF THAT FIELD CONTAINS AN
!         INTEGER NUMBER, AND THE NUMBER IS NOT RIGHT ADJUSTED (I.E.
!         XSORT2 TREATS FIELD 2 INTEGER THE SAME WAY AS INTEGERS IN ALL
!         OTHER FILEDS, NAMELY LEFT ADJUSTED WITH TRAILING BLANKS
!      9. IF THE 1ST FIELD OF THE 2ND CARD IS BLANK, A UNIQUE CONTINUA-
!         TION SYMBOL IS INSERTED INTO THE 1ST FIELD, AND THE SAME
!         SYMBOL IS ADDED TO THE 10TH FIELD OF THE PREVIOUS CARD
!
!     SCRATCH FILE LIMITATION IN LINK1 -
!     SEMDBD ALLOCATES ONLY 15 SCRATCH FILES. SINCE XCSA AND XGPI USE
!     THE LAST SCRATCH FILE FOR RIGID FORMAT, XSORT2, PROGRAMMED UP TO
!     30 FILES, IS THEREFORE PHYSICALLY LIMITTED TO 14 SCRATCH FILES.
!
!     WRITTEN BY G.CHAN/UNISYS   10/1987
!
   IMPLICIT NONE
   USE c_ifpx0
   USE c_ifpx1
   USE c_machin
   USE c_names
   USE c_output
   USE c_stapid
   USE c_system
   USE c_two
   USE c_xechox
   USE c_xmssg
   USE c_xsortx
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: base , bk , bsize , buf41 , buf41x , case , cmmt , count , crdflg , empty , exh , file , filea , filex , from ,       &
            & fub41 , full , i , ib , ibuf1 , ibuf2 , ibuf3 , ibufc , ibufl , ic , ii , iibuf , iii , imhere , j , k , kard1 ,      &
            & kard2 , kontn , kount , ksmbi , l , large , left , left24 , len , les1b , loc , m , maxc , method , n , n23 , nbulk , &
            & ncci , ncont , ndele , nfiler , nfiles , nz , nz25 , nzib , nzz , obase , onoff , point , rec , recx , reduce ,       &
            & restr , size , skip , tape , tapecc , tempx , top , wrttn , zpoint
   INTEGER , SAVE :: blank , i25 , maxscr , nptp , optp , tape1 , tape2 , tape3
   INTEGER , DIMENSION(50) :: buf
   INTEGER , DIMENSION(2) , SAVE :: bulkda , name , param
   INTEGER , DIMENSION(3) , SAVE :: cdcnt , ksmb
   LOGICAL , SAVE :: debug
   INTEGER , DIMENSION(25) :: fub
   CHARACTER(56) , DIMENSION(3) , SAVE :: head
   CHARACTER(28) , SAVE :: head4
   INTEGER , DIMENSION(10) :: ibufx , itape
   LOGICAL :: only1
   INTEGER , DIMENSION(2) :: temp
   INTEGER , DIMENSION(25,1) :: y
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!ZZ   COMMON /ZZXST2/ Z(1)
   !>>>>EQUIVALENCE (Y(1,1),Z(1)) , (Buf41,Buf4(1)) , (ibufx(1),buf(26)) , (itape(1),buf(38))
   DATA head , head4/' I N P U T   B U L K   D A T A   D E C K   E C H O      ' ,                                                   &
       &'     S O R T E D   B U L K    D A T A    E C H O        ' , ' ---1--- +++2+++ ---3--- +++4+++ ---5--- +++6+++ ---7---' ,   &
       &' +++8+++ ---9--- +++10+++   '/ , i25/25/
   DATA name , cdcnt , optp , nptp , blank/4HXSOR , 4HT2   , 4HCARD , 4HCOUN , 4HT    , 4HOPTP , 4HNPTP , 4H    /
   DATA tape1 , tape2 , tape3 , maxscr , bulkda , param/301 , 302 , 303 , 314 , 4HBULK , 4HDATA , 4HPARA , 4HM   /
   DATA ksmb/4H+C0N , 4H+CQN , 4H+CON/ , debug/.FALSE./
!
!     DIAG 47 CAN BE RE-ACTIVATED FOR PROGRAM DEBUG CHECKING
!
!     CALL SSWTCH (47,J)
!     IF (J .EQ. 1) DEBUG = .TRUE.
!
!     TURN ON XSORT FLAG AND FREE-FIELD FLAG FOR XREAD AND FFREAD
!
   ixsort = 1
   ffflag = 1234
!
!     CHECK UMF REQUEST
!
   IF ( kumf>0 ) THEN
      WRITE (nout,99001) ufm
99001 FORMAT (A23,' - USER MASTER FILE, UMF, IS  NOT SUPPORTED BY NEW ','XSORT ROUTINE',/5X,                                        &
             &'ADD A ''DIAG 42'' CARD AND RESUBMIT YOUR NASTRAN JOB')
! 100 FORMAT (A23,' - USER MASTER FILE, UMF, IS NO LONGER SUPPORTED BY',
!    1        ' NASTRAN',/5X,'(NOTE - RELEASE 87 WAS THE LAST VERSION ',
!    2        'THAT SUPPORTED UMF OPERATION)')
      CALL mesage(-37,0,name)
   ENDIF
!
!     INITIALIZE XSORT2
!
   echou = 0
   echos = 0
   echop = 0
   ncard = 0
   cmmt = 0
   ncont = 0
   ndele = 0
   full = 0
   exh = 0
   tapecc = 0
   bsize = 3
   restr = 0
   case = 1
   kontn = 10010000
   ksmbi = ksmb(1)
   IF ( apprc<0 ) restr = 1
   IF ( restr==1 ) ksmbi = khrfn3(ksmb(1),date(2),-2,0)
   j = complf(0)
   large = rshift(j,1)
   les1b = rshift(j,nbpc)
   IF ( mod(lqro,10)==1 ) les1b = lshift(j,nbpc)
   IF ( echo>=0 ) THEN
      echou = andf(echo,1)
      echos = andf(echo,2)
      echop = andf(echo,4)
      IF ( cpflg/=0 ) echos = 1
   ENDIF
!
!     SET UP UNSORTED HEADING
!
!     (UNSORTED INPUT DATA IS NOW PRINTED BY FFREAD ROUTINE BECAUSE
!      XREAD HAS BEEN MODIFIED TO RETURN ALL 10 DATA FIELDS LEFT-
!      ADJUSTED)
!
   DO j = 1 , 32
      head2(j) = blank
      head3(j) = blank
      head1(j) = blank
   ENDDO
   imhere = 130
   IF ( debug ) WRITE (nout,99048) imhere , restr , apprc , subs
   READ (head(1),99049) (head1(j),j=11,24)
!WKBR 9/93 READ (HEAD(3),150) (HEAD3(J),J= 7,20)
   READ (head(3),99049) (head3(j),j=8,21)
!WKBR 9/93 READ (HEAD 4 ,150) (HEAD3(J),J=21,27)
   READ (head4,99049) (head3(j),j=22,28)
   IF ( echou/=0 ) CALL page
!
!     GET AVAILABLE CORE
!     IF IBM MACHINE, LIMIT AVAILABLE CORE SIZE TO 1,000,000 WORDS, SUCH
!     THAT DATA WILL BE SAVED IN PRIMARY FILES ONLY, AND NO SPILL INTO
!     SECONDARY FILES.
!
   nzz = korsz(z)
   ibuf1 = nzz - bufsz
   ibuf2 = ibuf1 - bufsz
   ibuf3 = ibuf2 - bufsz
   nz = ibuf3 - 1
   IF ( mach==2 ) nz = min0(nz,1000000)
   IF ( nz<2500 ) CALL mesage(-8,2500,name)
   nz25 = nz/25
!
!     OPEN TAPE1, GINO FILE 301 FOR DELETE (SLASH) CARDS
!     AND  TAPE2, GINO FILE 302 FOR CONTINUATION CARDS
!     SET  TAPE TO TAPE3, GINO FILE 303, FOR BULKDATA CARDS
!     UP TO 30 FILES ARE ALLOWED FOR REGUALR BULKDATA CARDS
!     (CURRENTLY /XFIST/ IN SEMDBD IS SET UP ONLY TO SCRATCH FILE 315.
!     I.E. UP TO 13 (OR 12, IF DECK CONTAINS MANY CONTINUATION CARDS)
!     FILES CAN BE USED HERE)
!
   imhere = 170
   IF ( debug ) WRITE (nout,99048) imhere , nz25
   CALL open(*5600,tape1,z(ibuf1),wrtrew)
   CALL open(*5700,tape2,z(ibuf2),wrtrew)
   tape = tape3 - 1
 100  tape = tape + 1
   IF ( tape>314 ) THEN
      IF ( debug ) WRITE (nout,99056)
      CALL mesage(-8,-nzz,name)
   ENDIF
   CALL open(*6200,tape,z(ibuf3),wrtrew)
   wrttn = 0
!
!
!     START READING INPUT CARDS VIA XREAD/FFREAD.
!
!
!     ADDITIONAL INFORMATION FROM XREAD NOT MENTIONED PREVIOUSLY -
!
!      1. BUF4(1) = BUF4(2) =-1 INDICATE BULKDATA IS A COMMENT CARD
!         BUF4(1) = BUF4(2) =-2 INDICATE BULKDATA IS A CONTINUATION CARD
!         BUF4(1) = BUF4(2) =-3 INDICATE BULKDATA IS A DELETE CARD, WITH
!                   DELETE RANGE SAVED IN BUF4(3) AND BUF4(4)
!         BUF4(1) =-3 AND BUF4(4) =-4 IF TRASH WAS FOUND IN DELETE CARD.
!                   THAT IS, TRASH AFTER SLASH IN BULKDATA FIELD 1
!         BUF4(1) = BUF4(4) =-5 INDICATE A  BLANK   CARD WAS READ
!         BUF4(1) = BUF4(4) =-9 INDICATE AN ENDDATA CARD WAS READ
!      2. IF BULKDATA FIELD 2 IS AN INTEGER INPUT, THE CORRECT INTEGER
!                 VALUE IS SAVED IN BUF4(3)
!         IF BULKDATA FIELD 3 IS AN INTEGER INPUT, THE CORRECT INTEGER
!                 VALUE IS SAVED IN BUF4(4)
!      3. IF THE DATA IN FIELD 2 AND/OR 3 ARE F.P. NUMBER, THEIR INTEGER
!                 VALUES (NOT EXACT) ARE SAVED IN BUF4(3) AND/OR BUF4(4)
!                 THESE VALUES ARE USED ONLY FOR SORTING
!      4. IF BULKDATA FIELD 2 IS NOT NUMERIC, THE FIRST 6 CHARACTERS ARE
!                 CONVERTED TO INTERNAL INTEGER CODE AND SAVED IN BUF4(3
!         IF THE REMAINING 2 CHARACTERS ARE NOT BLANKS, THEY ARE SAVED
!                 IN BUF4(4)
!      5. IF BUF4(4) IS NOT USED BY 4, IT HOLDS THE INTERNAL CODE OR THE
!                 INTEGER VALUE FOR FIELD 3 OF THE ORIGINAL BULKDATA.
!
!     WORK SPACE -                                     NZ
!      1                                               /
!     ------------------------------------------------------------------
!     !                 OPEN CORE, Z                    !    !    !    !
!     ------------------------------------------------------------------
!     !<----------INPUT CARDS, 25 WORDS EACH----------->!<----GINO---->!
!                (20-WORD CARD IMAGE, 4 CONTRL               BUFFERS
!               CONTROL WORDS, 1 INDEX POINTER)
!
!
!     SUMMARY OF COUNTERS -
!
!     NCONT = TOTAL CONTINUATION CARDS COUNT, ON INPUT BULK DATA DECK
!             AND ON RESTART OPTP FILE
!     NDELE = TOTAL COUNT ON RESTART DELETE CARDS
!     CMMT  = TOTAL COUNT ON NON-ESSENTIAL CARDS (COMMENTS, BLANKS, AND
!             RESTART DELETE CARDS) OF INPUT BULK DATA DECK
!     KONTN = SYMBOL COUNTER FOR AUTO-CONTINUAION GENERATION
!     KOUNT = DELETE RANGE COUNTER, USED ONLY IN 800-820 AREA
!     NCARD = TOTAL INPUT BULK DATA CARDS COUNT, INCLUDING NON-ESSENTIAL
!             CARDS; CONTINUATION CARDS AND CARDS ON OPTP ARE EXCLUDED
!     COUNT = CURRENT CORE COUNT ON INPUT CARDS FROM BULK DATA DECK, ALL
!             NON-ESSENTIAL AND CONTINUATION CARDS ARE EXCLUDED
!     NBULK = NO. OF ACTIVE BULK DATA INPUT CARDS
!           = NCARD-CMMT = SUM OF ALL COUNT's
!     NOTE  - NO CARD COUNT ON THE OPTP FILE BEFORE ASSEMBLING NPTP FILE
!
   count = 0
 200  IF ( count<nz25 ) THEN
      IF ( case==1 ) GOTO 600
      IF ( case==2 ) THEN
         case = 1
         GOTO 700
      ELSEIF ( case==3 ) THEN
         GOTO 400
      ELSEIF ( case==4 .OR. case==5 .OR. case==6 ) THEN
!
         DO i = 1 , 25
            buf(i) = fub(i)
         ENDDO
         buf41 = fub41
         IF ( case/=5 ) THEN
            case = 1
            GOTO 700
         ELSE
!
            case = 6
            IF ( buf41/=-9 ) GOTO 400
            GOTO 900
         ENDIF
      ENDIF
   ENDIF
!                                   1,  2,  3,  4,  5,  6 = CASE
   case = 1
   IF ( wasff<=0 ) THEN
!
!     OPEN CORE BUFFER FULL, ENDDATA CARD HAS NOT BEEN ENCOUNTERED
!
      full = 1
      GOTO 1000
   ELSE
!
!     (200 THRU 215) SPECIAL HANDLING OF CONTINUATION CARD(S) WITH FIRST
!     FIELD BLANK DURING FREE-FIELD INPUT.   REGULAR CONTINUATION CARD
!     (FIRST FIELD NOT BLANK) OR FIXED-FIELD INPUT CARDS (BOTH PARENT
!     AND CHILD) ARE NOT CONSIDERED HERE.
!
!        EXAMPLE -     CBAR,10,20, 1 2 3  9)2
!                      ,,, .5 .5 .5
!
!     WE NEED TO CREATE A UNIQUE CONTINUATION SYMBOL FOR THE 1ST FIELD,
!     AND ADD THE SAME SYMBOL TO THE 10TH FIELD OF THE PREVIOUS CARD.
!     SET BUF41 FLAG TO -2.
!                                                                WAITING
!     AT THIS POINT,                                             CARD IN
!        CASE 1, NO CARD IS WAITING FOR PROCESSING               -------
!        CASE 2, CORE WAS FULL AND WAS EMPTIED OUT. A NON-           BUF
!                CONTINUATION CARD WAS READ AND AWAITS PROCESSING
!        CASE 3, CORE WAS FULL AND EMPTIED. A CONTINUATION CARD      BUF
!                WAS READ AND AWAITS PROCESSING.
!        CASE 4, CORE NOT FULL, A CONT.CARD WAS READ. THE NEXT CARD  FUB
!                IS NOT A CONT.CARD. THE CONT.CARD WAS PROCESSED,
!                AND THE NON-CONT. CARD  AWAITS PROCESSING.
!        CASE 5, CORE NOT FULL, A CONT.CARD WAS READ AND THE NEXT    FUB
!                CARD IS ALSO A CONT.CARD. THE FIRST CONT.CARD
!                WAS PROCESSED, AND THE SECOND CONT.CARD AWAITS
!                PROCESSING.
!        CASE 6, CONTINUE FROM PROCESSING CASES=4,5                  FUB
!
! ... CASES 2 AND 3 -
!     CORE IS FULL, READ ONE MORE CARD AND SEE THE NEW CARD IS A SPECIAL
!     CONTINUATION CARD OR NOT
!     IF IT IS, UPDATE THE 10TH FIELD OF THE PARENT CARD BEFORE
!     SENDING THE ENTIRE CORE FOR SORTING
!
      imhere = 202
      DO
         CALL xread(*500,buf)
         IF ( buf41/=-1 .AND. buf41/=-5 ) THEN
            case = 2
            IF ( buf(1)/=blank .OR. buf(2)/=blank ) THEN
               IF ( buf41+2/=0 ) THEN
                  full = 1
                  GOTO 1000
               ENDIF
            ENDIF
            buf41x = -2
            case = 3
            EXIT
         ENDIF
      ENDDO
   ENDIF
 300  kontn = kontn + 1
   IF ( kontn==10020000 ) ksmbi = ksmb(2)
   IF ( kontn==10030000 ) ksmbi = ksmb(3)
   imhere = 205
   IF ( debug ) WRITE (nout,99048) imhere , kontn , count , nz25 , case
   CALL int2a8(*7100,kontn,buf(1))
   buf(1) = ksmbi
   IF ( count<=0 ) GOTO 500
   y(19,count) = buf(1)
   y(20,count) = buf(2)
   IF ( case<=3 ) THEN
      full = 1
      GOTO 1000
   ENDIF
 400  DO
!
      CALL xread(*400,fub)
      IF ( buf41/=-1 .AND. buf41/=-5 ) THEN
         fub41 = buf41
         buf41 = -2
         IF ( fub(1)/=blank .OR. fub(2)/=blank ) GOTO 700
         fub41 = -2
         case = 5
         kontn = kontn + 1
         IF ( kontn==10020000 ) ksmbi = ksmb(2)
         IF ( kontn==10030000 ) ksmbi = ksmb(3)
         imhere = 207
         IF ( debug ) WRITE (nout,99048) imhere , kontn , count , nz25 , case
         CALL int2a8(*7100,kontn,fub(1))
         fub(1) = ksmbi
         buf(19) = ksmbi
         buf(20) = fub(2)
         GOTO 800
      ENDIF
   ENDDO
!
 500  nogo = 1
   WRITE (nout,99002) sfm , imhere
99002 FORMAT (A25,'.  IMHERE =',I6)
   case = 1
   GOTO 700
 600  CALL xread(*7000,buf)
   IF ( buf(1)/=blank .OR. buf(2)/=blank ) THEN
      case = 1
!
! ... CASES 4 AND 5 -
!     CORE IS NOT FULL, A SPECIAL CONTINUATION CARD WAS JUST READ
!
   ELSEIF ( wasff<=0 ) THEN
      case = 1
   ELSE
      case = 4
      buf41 = -2
      GOTO 300
   ENDIF
!
!     IGNORE COMMENT CARD (-1) OR BLANK CARD (-5)
!
 700  IF ( buf41/=-1 .AND. buf41/=-5 ) THEN
!
!     TEST FOR ENDDATA CARD (-9)
!
      IF ( buf41==-9 ) GOTO 900
!
!     IF THIS IS A CONTINUATION CARD (-2), ADD ONE CONTROL WORD ABOUT
!     RESTART, AND WRITE IHE CARD OUT TO TAPE2
!     (THE CONTROL WORD WILL FLAG THE PARENT BIT TO BE SET FOR RESTART
!     WHEN THIS CONTINUATION CARD IS MERGED INTO NPTP)
!
      IF ( buf41/=-2 ) THEN
!
!     IF THIS IS A DELETE CARD (-3), REJECT IT IF EXTRANEOUS DATA IN
!     FIELD 1 OTHERWISE WRITE THE RANGE OF DELETION ON TAPE1
!
         IF ( buf41/=-3 ) THEN
!
!     REGULAR BULKDATA CARDS.
!     SAVE 20 WORDS OF BUF, 4 WORDS FROM BUF4 AND CORE COUNTER IN OPEN
!     CORE SPACE Y (25 WORDS TOTAL)
!     SET RESTART BITS IF THIS IS A RESTART RUN
!     RETURN TO READ NEXT BULKDATA CARD
!
            count = count + 1
            wrttn = 1
            DO i = 1 , 20
               y(i,count) = buf(i)
            ENDDO
            DO i = 1 , 4
               y(i+20,count) = buf4(i)
            ENDDO
            y(25,count) = count
            IF ( debug ) WRITE (nout,99003) count , y(1,count) , y(2,count)
99003       FORMAT (5X,'SAVED IN CORE   COUNT=',I5,3X,2A4)
            IF ( restr==0 ) GOTO 200
            ASSIGN 200 TO crdflg
            from = 330
            GOTO 5400
         ELSE
            cmmt = cmmt + 1
            IF ( buf4(4)==-4 ) THEN
               CALL page2(2)
               WRITE (nout,99004) ufm
99004          FORMAT (A23,' 221, EXTRANEOUS DATA IN FIELD 1 OF BULK DATA ','DELETE CARD.')
               nogo = -2
            ENDIF
!
            IF ( buf4(3)==-3 ) THEN
               WRITE (nout,99005) ufm
99005          FORMAT (A23,' 221, NO DATA IN FIELD 2 OF BULK DATA DELETE CARD')
               nogo = -1
            ELSE
               IF ( buf4(4)==-3 ) buf4(4) = buf4(3)
               buf4(3) = buf4(3) - 2000000000
               buf4(4) = buf4(4) - 2000000000
               CALL write(tape1,buf4(3),2,0)
               IF ( debug ) WRITE (nout,99006) buf4(3) , buf4(4)
99006          FORMAT (5X,'A DELETE CARD -',I11,1H,,I11)
               ndele = ndele + 1
            ENDIF
            GOTO 200
         ENDIF
      ENDIF
   ELSE
      cmmt = cmmt + 1
      GOTO 600
   ENDIF
 800  buf(21) = restr
   CALL write(tape2,buf(1),21,0)
   IF ( debug ) WRITE (nout,99007) buf(1) , buf(2) , buf(21)
99007 FORMAT (5X,'A CONTINUATION CARD - ',2A4,',  CONT.FLAG=',I9)
   ncont = ncont + 1
   GOTO 200
!
!     ENDDATA CARD FOUND, SET FLAG
!
 900  full = -1
   imhere = 350
   ncard = ncard - 1
   nbulk = ncard - cmmt
   IF ( debug ) WRITE (nout,99048) imhere , ncard , ncont , ndele
   CALL page2(2)
   IF ( echou/=1 ) THEN
      WRITE (nout,99008) ncard , cmmt
99008 FORMAT (//24X,'(NO. OF UNSORTED BULK DATA CARDS READ =',I6,', INCLUDING',I4,' COMMENT CARDS)')
   ELSE
      WRITE (nout,99009) ncard
99009 FORMAT (//24X,'TOTAL COUNT=',I7)
   ENDIF
!
!     SORT CARD IMAGES SAVED IN THE OPEN CORE SPACE BY MODIFIED SHELL
!     METHOD.
!     SORT BY 21ST, 22ND, 23RD, AND 24TH CONTROL WORDS ONLY
!     ONLY THE LAST 5 WORDS (21ST THRU 25TH) ARE MOVED INTO SORTED
!     ORDER, THE FIRST 20 WORDS REMAIN STATIONARY.
!
 1000 IF ( wrttn==0 ) GOTO 1300
   IF ( count>nz25 ) CALL mesage(-37,0,name)
   m = count
   imhere = 400
   IF ( debug ) WRITE (nout,99048) imhere , count
 1100 m = m/2
   IF ( m==0 ) THEN
!
!     END OF CORE SORT.
!     WRITE THE SORTED BULKDATA CARDS TO FILE, 24 WORDS EACH RECORD
!     IN ORDER GIVEN BY THE 25TH WORD.
!     IF ONLY ONE SCRATCH FILE (TAPE3) IS USED IN RECEIVING BULKDATA,
!     CHECK ANY DUPLICATE CARD.
!
      imhere = 500
      IF ( debug ) WRITE (nout,99048) imhere , count , maxc
      only1 = .FALSE.
      IF ( full==-1 .AND. tape==tape3 ) only1 = .TRUE.
      base = 25
      DO i = 1 , count
         IF ( only1 ) base = mod(i,2)*25
         j = y(25,i)
         DO k = 1 , 20
            buf(k+base) = y(k,j)
         ENDDO
         DO k = 21 , 24
            buf(k+base) = y(k,i)
         ENDDO
         IF ( only1 ) THEN
            IF ( i/=1 ) THEN
               DO k = 1 , 20
                  IF ( buf(k+base)/=buf(k+obase) ) GOTO 1110
               ENDDO
               buf(21+base) = -6
               buf(22+base) = -6
            ENDIF
 1110       obase = base
         ENDIF
         CALL write(tape,buf(base+1),24,0)
         IF ( debug ) WRITE (nout,99010) tape , (buf(k+base),k=1,8) , (buf(k+base),k=21,24)
99010    FORMAT (5X,'WRITE TO ',I3,4(2X,2A4),/9X,'INT.CODE=',4I12)
      ENDDO
      CALL write(tape,0,0,1)
      GOTO 1300
   ELSE
      j = 1
      k = count - m
      i = j
   ENDIF
 1200 DO
      n = i + m
      IF ( y(21,i)<y(21,n) ) EXIT
      IF ( y(21,i)==y(21,n) ) THEN
         IF ( y(22,i)<y(22,n) ) EXIT
         IF ( y(22,i)==y(22,n) ) THEN
            IF ( y(23,i)<y(23,n) ) EXIT
            IF ( y(23,i)==y(23,n) ) THEN
               IF ( y(24,i)<=y(24,n) ) EXIT
            ENDIF
         ENDIF
      ENDIF
      DO l = 21 , 25
         tempx = y(l,i)
         y(l,i) = y(l,n)
         y(l,n) = tempx
      ENDDO
      i = i - m
      IF ( i<1 ) EXIT
   ENDDO
   j = j + 1
   IF ( j>k ) GOTO 1100
   i = j
   GOTO 1200
 1300 CALL close(tape,rew)
   imhere = 580
   IF ( debug ) WRITE (nout,99048) imhere
!
!     REPEAT READING BULKDATA CARDS INTO CORE IF NECESSARY
!
!     IF NO DATA WRITTEN TO CURRENT FILE (e.g. UN-MODIFIED RESTART),
!     REDUCE TAPE COUNT BY ONE
!
   IF ( full/=-1 ) GOTO 100
   IF ( wrttn==0 ) tape = tape - 1
!
!     CLOSE DELETE CARD FILE, TAPE 1.
!     CONTINUATION CARD FILE, TAPE 2, IS STILL IN USE
!
   CALL write(tape1,0,0,1)
   CALL close(tape1,rew)
!
!     TEST FOR COLD-START WITH NO BULKDATA
!
!     APPRC = APPROACH FLAG (1 DMAP, 2 DISP, 3 HEAT, 4 AERO)
!     SUBS  = SUBSTRUCTURING FLAG
!
   imhere = 585
   IF ( debug ) WRITE (nout,99048) imhere , count , apprc , wrttn , restr , subs
   IF ( wrttn==1 .OR. restr==1 .OR. subs/=0 ) THEN
!
!     IF MODIFIED RESTART - TURN ON SORT ECHO FLAG IF ECHO IS NOT 'NONO'
!     IF NOT A RESTART JOB - JUMP TO 1000
!
      IF ( nbulk>1 .AND. restr==1 ) echos = 1
!     IF (APPRC.EQ.1 .OR.  SUBS .NE.0) ECHOS = 1
      IF ( echo==-2 ) echos = 0
      IF ( restr==0 ) GOTO 2100
!
!     THIS IS A RESTART JOB, PROCESS OPTP FILE -
!
!     OPEN OPTP AND LOCATE WHERE BULK DATA BEGINS
!
      imhere = 610
      IF ( debug ) WRITE (nout,99048) imhere
      CALL open(*6800,optp,z(ibuf3),rdrew)
      DO
         CALL skpfil(optp,+1)
         CALL read(*6600,*6600,optp,buf(1),2,1,j)
         IF ( buf(1)==bulkda(1) .AND. buf(2)==bulkda(2) ) THEN
            IF ( nbulk>0 .OR. ndele/=0 ) THEN
!
!     MODIFIED RESTART WITH NEW BULKDATA CARDS, WITH OR WITHOUT DELETE
!
               imhere = 640
               IF ( debug ) WRITE (nout,99048) imhere
               ic = 1
               left = nz
               IF ( ndele==0 ) GOTO 1600
               IF ( restr==1 ) THEN
!
!     RESTART WITH DELETE CARD(S) -
!     MOVE THE DELETE CARDS  INTO CORE AND FREE TAPE1.
!     SORT THE DELETE CARDS, CHECK FOR AND ELIMINATE OVERLAPS AND
!     REDUNDANCIES
!
                  CALL open(*5600,tape1,z(ibuf1),rdrew)
                  CALL read(*5600,*1500,tape1,z(1),left,1,len)
                  CALL mesage(-8,tape1,name)
                  GOTO 1500
               ELSE
                  CALL page2(-1)
                  WRITE (nout,99011) uwm
99011             FORMAT (A25,' 205, COLD START, DELETE CARDS IGNORED.')
                  GOTO 1600
               ENDIF
            ELSE
!
!     UN-MODIFIED RESTART, WITH NO NEW BULKDATA CARD AND NO DELETE -
!     SETUP SORTED HEADER FOR OLD BULK DATA CARDS IF ECHO FLAG IS ON,
!     COPY THE REST OF OPTP DIRECTLY TO NPTP, AND JOB DONE
!
!
               imhere = 620
               IF ( debug ) WRITE (nout,99048) imhere
               CALL open(*6900,nptp,z(ibuf1),wrt)
               CALL write(nptp,bulkda,2,1)
               ncard = 0
               IF ( echos/=0 ) THEN
                  READ (head(2),99049) (head1(j),j=11,24)
!WKBR 9/93 HEAD2(4) = CDCNT(1)
                  head2(5) = cdcnt(1)
!WKBR 9/93 HEAD3(4) = CDCNT(2)
                  head3(5) = cdcnt(2)
!WKBR 9/93 HEAD3(5) = CDCNT(3)
                  head3(6) = cdcnt(3)
                  CALL page
               ENDIF
               DO
                  CALL read(*1400,*1400,optp,buf(1),20,1,j)
                  CALL write(nptp,buf(1),20,1)
                  ncard = ncard + 1
                  IF ( echop/=0 ) WRITE (lpch,99052) (buf(j),j=1,20)
                  IF ( echos/=0 ) THEN
                     CALL page2(-1)
                     WRITE (nout,99051) ncard , (buf(j),j=1,20)
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDDO
   ELSE
      CALL close(tape2,rew)
      echos = 1
      IF ( apprc==1 ) GOTO 3000
      CALL page2(2)
      WRITE (nout,99012) ufm
99012 FORMAT (A23,' 204, COLD START NO BULK DATA.')
      nogo = -2
      GOTO 7200
   ENDIF
 1400 CALL eof(nptp)
   CALL close(nptp,rew)
   CALL close(optp,norew)
   CALL close(tape2,rew)
   IF ( echop/=0 ) WRITE (lpch,99054)
   CALL page2(-1)
   IF ( echos/=0 ) WRITE (nout,99053)
   IF ( echos==0 ) WRITE (nout,99013) uim , ncard
99013 FORMAT (A29,1H,,I8,' SORTED BULKD DATA CARDS PROCESSED FROM OPTP',' FILE TO NPTP, UN-MODIFIED')
   GOTO 5200
 1500 CALL close(tape1,rew)
!
   CALL sort(0,0,2,1,z(1),len)
   z(len+1) = large
   DO i = 2 , len , 2
      z(i) = z(i) + 1
      IF ( z(i)<z(i-1) ) z(i) = z(i-1)
      IF ( z(i)>=z(i+1) ) THEN
         z(i) = -1
         z(i+1) = -1
      ENDIF
   ENDDO
   j = 0
   DO i = 1 , len
      IF ( z(i)>=0 ) THEN
         j = j + 1
         z(j) = z(i)
      ENDIF
   ENDDO
   IF ( j>0 ) len = j
   left = nz - len
   ic = len + 1
   z(ic) = large
   imhere = 700
   IF ( debug ) WRITE (nout,99014) imhere , (z(i),i=1,len)
99014 FORMAT (/,' *** IMHERE =',I5,(/,3X,10(I7,I5)))
   IF ( mod(len,2)==0 ) GOTO 1700
   GOTO 7100
!
!     IF MODIFIED RESTART WITH NO DELETE, SET DELETE RANGE BEGINNING AT
!     INFINITY
!
 1600 z(1) = large
   imhere = 710
   IF ( debug ) WRITE (nout,99048) imhere
!
!     WE ARE STILL IN PROCESSING RESTART - COPY OPTP TO TAPE1, SKIP
!     APPROPRIATE RECORDS AS SPECIFIED BY THE DELETE CARDS NOW IN
!     OPEN CORE, Z(1) THRU Z(LEN)
!
!     SEND A CARD FROM OPTP TO YREAD (AN ENTRY POINT IN XREAD) FOR
!     RE-PROCESSING. UPON RETURN FROM YREAD, BUF4 ARRAY HOLDS THE
!     INTERNAL INTEGER CODE GOOD FOR SORTING AND OTHER FUNCTIONS.
!
!     IF IT IS A CONTINUATION CARD, COPY THE FULL CARD (20 WORDS)
!     AND ONE CONTROL WORD TO TAPE2.
!     OTHERWISE COPY 24 WORDS (20-BUF AND 4-BUF4) TO TAPE1.
!
!     IF A CONTINUATION CARD IS DELETED, THE RESTART BITS OF THE
!     PARENT CARD SHOULD BE FLAGGED
!
 1700 imhere = 800
   IF ( debug ) WRITE (nout,99048) imhere , restr , tape1
   CALL open(*5600,tape1,z(ibuf1),wrtrew)
   kount = 0
   point = 1
   onoff = 1
   zpoint = z(point)
   buf(19) = 0
 1800 DO
      temp(1) = buf(19)
      temp(2) = buf(20)
      CALL read(*2000,*2000,optp,buf(1),20,1,j)
      kount = kount + 1
      IF ( kount>=zpoint ) THEN
         point = point + 1
         zpoint = z(point)
         onoff = onoff*(-1)
      ENDIF
      CALL yread(*6700,buf)
      imhere = 830
      IF ( debug .AND. onoff==-1 ) WRITE (nout,99015) imhere , kount , (buf(j),j=1,6)
99015 FORMAT (' IMHERE=',I5,'.  DELETED FROM OPTP ==>',I5,2H- ,6A4)
      IF ( buf41==-2 ) THEN
!
!     CONTINUATION CARD FROM OPTP -
!
!     IF BOTH PARENT AND THIS CONTINUATION CARD IN NOT IN DELETE RANGE
!     SEND THIS CONTINUATION CARD TO TAPE2 WITH RESTART CONTROL WORD
!     SET TO ZERO.
!     IF PARENT IS NOT DELETED, BUT THIS CONTINUATION CARD IS, WE NEED
!     TO FLAG PARENT
!     IF PARENT IS ALSO IN DELETE RANGE, SKIP THIS CONTINUATION CARD.
!
         IF ( onoff==+1 ) THEN
            buf(21) = 0
            CALL write(tape2,buf(1),21,0)
            ncont = ncont + 1
         ELSEIF ( kard1/=-1 ) THEN
            IF ( buf(1)/=temp(1) .OR. buf(2)/=temp(2) ) THEN
               from = 860
               ASSIGN 1900 TO crdflg
               GOTO 5500
            ENDIF
         ENDIF
      ELSEIF ( onoff==+1 ) THEN
!
!     REGULAR BULKDATA CARD FROM OPTP -
!     SAVE FIRST FIELD IN KARD1/2 JUST IN CASE THIS IS A PARENT OF
!     A CONTINUATION CARD WHICH FALLS INSIDE A DELETE RANGE.
!
!     NOTE- CARDS FROM OPTP ARE IN SORTED ORDER, AND NO CARD COUNT HERE
!
         DO j = 1 , 4
            buf(j+20) = buf4(j)
         ENDDO
         CALL write(tape1,buf(1),24,0)
         IF ( debug ) WRITE (nout,99016) (buf(j),j=1,6) , buf(21)
99016    FORMAT (' IMHERE=860, OPTP==>TAPE1  ',6A4,'==>',I9)
         kard1 = buf(1)
         kard2 = buf(2)
         IF ( kard1==param(1) .AND. kard2==param(2) ) THEN
            kard1 = buf(3)
            kard2 = buf(4)
         ENDIF
      ELSE
!
!     ANY DELETED CARD, EXCEPT CONTINUATION CARD, MUST RESET
!     RESTART CARD FLAG
!
         ASSIGN 1800 TO crdflg
         from = 830
         GOTO 5400
      ENDIF
   ENDDO
 1900 kard1 = -1
   GOTO 1800
!
!     OPTP IS SUCCESSFULLY MOVED TO TAPT1 AND TAPE2. CLOSE FILES
!
 2000 CALL close(optp,norew)
   CALL write(tape1,0,0,1)
   CALL write(tape2,0,0,1)
   CALL close(tape1,rew)
!
!     PREPARE FOR FILE MERGE -
!
!     SELECT METHOD USED TO BRING CONTINUATION CARDS INTO CORE AND
!     COMPUTE NUMBER OF BUFFERS NEEDED FOR FILE PRE-MERGE.
!
!     METHOD 1 - NO FILE PRE-MERGE IF THERE IS NO CONINUATION CARDS, OR
!                ENOUGH SPACE IN CORE TO HOLD ALL CONTINUATION CARDS,
!                BUFFERS AND SCRATCH ARRAYS FOR ALL SCRATCH DATA FILES
!     METHOD 2 - ALL CONTINUATION CARDS, IN 3-WORD TABLE AND 20-WORD
!                CARD IMAGES, AND ALL GINO BUFFERS, OR REDUCED GINO
!                BUFFERS, FIT INTO CORE
!     METHOD 3 - CONTINUATION 3-WORD TABLE AND ALL GINO BUFFERS, OR
!                REDUCED GINO BUFFERS, FIT INTO CORE
!     METHOD 4 - FATAL, INSUFFICIENT CORE
!
 2100 CALL close(tape2,rew)
   method = 1
   n23 = 1
   nfiles = tape - tape3 + 1
   reduce = 1
   IF ( nfiles>=10 ) reduce = 2
   IF ( nfiles>17 ) reduce = 3
   j = 0
   IF ( restr==1 ) j = 1
   maxc = (nzz-(bufsz+25)*(nfiles+j))/21
   IF ( ncont<=maxc ) reduce = 1
   nfiler = (nfiles+reduce-1)/reduce + j
   imhere = 1010
   IF ( debug ) WRITE (nout,99048) imhere , reduce , nfiles , nfiler
   IF ( ncont/=0 ) THEN
      DO
         method = 2
         n23 = 23
         DO
            size = (nfiler+1)*bufsz + nfiler*25
            size = size + bufsz
            left = nzz - size
            maxc = left/n23
            imhere = 1020
            IF ( debug ) WRITE (nout,99048) imhere , method , nfiles , nfiler , n23 , ncont
            IF ( ncont<=maxc ) EXIT
            IF ( method==2 ) THEN
               method = 3
               n23 = 3
            ELSEIF ( method==3 ) THEN
!
!     INSUFFICIENT CORE, COMPUTE HOW MUCH MORE NEEDED
!
               j = ncont*n23 - left
               CALL mesage(-8,j,name)
               EXIT
            ELSE
               GOTO 2150
            ENDIF
         ENDDO
         EXIT
 2150 ENDDO
   ENDIF
!
!     ALLOCATE BUFFER SPACE AND REDEFINE AVAILABLE CORE SPACE, NZ
!     ALLOCATE SPACES AT THE BEGINNING OF CORE SPACE FOR BULKDATA
!     TO BE BROUGHT BACK FROM VARIOUS FILES.
!
!     IC     = POINTER, WHERE CONTINUATION TABLE BEGINS
!     IB     = POINTER, WHERE CONTINUATION  DATA BEGINS
!     NFILES = TOTAL NUMBER OF FILES USED BEFORE FILE REDUCTION,
!              RESTART TAPE1 NOT INCLUDED
!     NFILER = REDUCED NUMBER OF FILES THAT HOLD BULKDATA INPUT CARDS,
!              RESTART TAPE1 INCLUDED
!     TAPECC = AN ADDITIONAL FILE USED ONLY IN METHOD 3 (NOT INCLUDED
!              IN NFILES AND NFILER)
!
   imhere = 1100
   IF ( debug .OR. nfiles>10 .OR. ncont>1000 ) WRITE (nout,99017) uim , method , nfiler , hicore , ncont
99017 FORMAT (A29,' FROM XSORT -  METHOD',I3,' WAS SELECTED TO PROCESS',' CONTINUATION CARDS',/5X,'NO. OF FILES USED =',I4,4X,      &
             &'HICORE =',I7,' WORDS',4X,'NO. OF CONT. CARDS =',I7)
   nz = ibuf1
   DO i = 1 , nfiler
      nz = nz - bufsz
      ibufx(i) = nz
   ENDDO
   IF ( ncont>0 ) nz = nz - bufsz
   ibufc = nz
   nz = nz - 1
   ic = nfiler*25 + 1
   ib = ic + ncont*3
   nzib = nz - ib + 1
   left = nz - ic + 1
!
!     NEED A STORAGE SPACE FOR AT LEASE 100 CONTINUATION CARDS
!
   IF ( nzib<2100 ) CALL mesage(-8,-2100+nzib,name)
!
!     METHOD 1, NO CONTINUATION CARD IN BULKDATA, SKIP TO 1280
!
   IF ( method==1 ) GOTO 2800
!
!     WORKING SPACE FOR THE CONTINUATION TABLE AND CONTINUATION CARD
!     IMAGES -
!
!                  IC                 IB                   NZ
!                  /                  /                    /
!     ------------------------------------------------------------------
!     ! ! ! !..Y..!                  !                     !  !  !  !  !
!     ------------------------------------------------------------------
!     ! SPACE FOR !<--CONTINUATION-->!<--AVAILABLE SPACE-->!<--GINO--->!
!       DATA FROM     INDEX TABLE        FOR CONTINUATION     BUFFERS
!       FILES 303,   (3 WORDS EACH)      CARD IMAGES
!       304,...                          (21 WORDS EACH)
!       FOR FILE      (PART 1 AERA)
!       MERGE                            (PART 2 AREA)
!
   imhere = 1125
   IF ( debug ) WRITE (nout,99048) imhere , method , n23
   CALL open(*5700,tape2,z(ibuf2),rdrew)
   IF ( method==3 ) THEN
!
!     METHOD 3 -
!
!     COMPUTE NCCI (NO. OF CONTINUATION CARD IMAGES) THAT PART 2 AREA
!     (FROM Z(IB) THRU Z(NZ)) CAN HOLD AT A GIVEN TIME.
!     CREATE IN CORE A CONTINUATION TABLE WITH INDEX POINTERS (SAME
!     AS METHOD 2) IN PART 1 AREA.
!     FILL THE REMAINING PART 2 AREA WITH NCCI CARDS, AND WRITE THIS
!     BLOCK OF CARDS OUT TO A NEW SCRATCH FILE, TAPECC. REPEAT THIS
!     PROCESS FOR THE REST OF THE CONTINUATION CARDS.
!     THE INDEX POINTERS IN PART 1 (METHOD 3 ONLY) ALSO INCLUDE THE
!     DATA BLOCK NUMBER INFORMATION
!
      ncci = nzib/21
      IF ( ncci>=10000000 ) ncci = 10000000 - 1
      nzib = ncci*21
      tapecc = nfiles + tape3
      imhere = 1200
      IF ( debug ) WRITE (nout,99048) imhere , method , tapecc , ncci
      IF ( tapecc>maxscr ) THEN
         WRITE (nout,99056) sfm
         GOTO 7100
      ELSE
         CALL open(*6100,tapecc,z(ibufc),wrtrew)
         bk = 0
         i = ic
         IF ( ncci<750 .AND. mach>2 .AND. nbpw/=64 ) THEN
            j = ((ncont*23-nz+ic+999)/1000)*1000
            WRITE (nout,99018) uim , j , hicore
99018       FORMAT (A29,', DUE TO UNUSUAL LARGE NUMBER OF CONTINUATION CARDS',' PRESENT IN THE BULKDATA DECK',/5X,'AN ADDITION OF', &
                  & I7,' WORDS TO OPEN CORE SPACE COULD MAKE LINK1 MORE EFFICIENT',/5X,'CURRENTLY NASTRAN HICORE IS',I7,' WORDS')
            IF ( ncci<100 ) nogo = -3
         ENDIF
         GOTO 2300
      ENDIF
   ELSE
!
!     METHOD 2 -
!
!     OPEN CORE IS DIVIDED INTO 2 PARTS - A 3-WORD CONTINUATION TABLE
!     IN PART 1, AND 21-WORD CONTINUATION CARD IMAGES IN PART 2.
!
!     3-WORD TABLE IN PART 1 HOLDS THE 2-BCD CONTIUATION SYMBOLS, WITH
!     THE FIRST BYTE (A + OR *) ZERO OUT, AND AN INDEX POINTER. THIS
!     TABLE WILL BE SORTED, AND WILL BE USED BY BISLC2 TO LOCATE THE
!     CARD IMAGES SAVED EITHER IN PART 2, OR IN TAPECC FILE.
!
      imhere = 1130
      IF ( debug ) WRITE (nout,99048) imhere , method , ncont , ic , ib
      CALL read(*6400,*2200,tape2,z(ib),nzib,1,len)
      CALL mesage(-8,0,name)
   ENDIF
 2200 k = len + ib - 1
   i = ic
   DO j = ib , k , 21
      z(i) = andf(z(j),les1b)
      z(i+1) = z(j+1)
      z(i+2) = j
      i = i + 3
   ENDDO
   GOTO 2700
 2300 bk = bk + 10000000
   j = ib
   top = nzib
   CALL read(*2600,*2400,tape2,z(ib),top,0,len)
   GOTO 2500
 2400 top = len
 2500 top = top + ib - 1
   DO
      z(i) = andf(z(j),les1b)
      z(i+1) = z(j+1)
      z(i+2) = j + bk
      i = i + 3
      j = j + 21
      IF ( j>=top ) THEN
         CALL write(tapecc,z(ib),nzib,1)
         GOTO 2300
      ENDIF
   ENDDO
 2600 CALL close(tapecc,rew)
 2700 CALL close(tape2,rew)
   len = i - ic
   IF ( len>3 ) CALL sort2k(0,0,3,1,z(ic),len)
!
!     NO PRE-MERGING FILES IF REDUCE IS 1 (I.E. LESS THAN 10 SCRATCH
!     FILES WERE USED TO HOLD THE RAW BULKDATA, OR ENOUGH CORE TO HOLD
!     EVERYTHING)
!
 2800 IF ( reduce/=1 ) THEN
!
!     PRE-MERGE
!     =========
!
!     AT THIS POINT, CONTINUATION CARD IMAGES ARE EITHER IN CORE OR IN
!     SCRATCH FILE TAPECC, AND TAPE2 IS FREE FOR RE-USE.
!     ALL GINO BUFFERS ARE FREE
!
!     IF TOO MANY FILES WERE USED TO SAVE BULKDATA, MERGE THEM TO REDUCE
!     THE TOTAL NUMBER OF FILES GOING TO BE USED (I.E. TO REDUCE BUFFER
!     SPACE IN THE MERGE PHASE COMING NEXT)
!
!     PERFORM A 2-TO-1 MERGE IF NUMBER OF FILES PRESENTLY IS 10-17.
!
!     FILEB + FILEC == FILEA      E.G.  303 + 304 == 302
!                                       305 + 306 == 303
!                                       307 + 308 == 304  ETC.
!     OR
!     PERFORM A 3-TO-1 MERGE IF NUMBER OF FILES PRESENTLY IS 18-30.
!
!     FILEB+FILEC+FILED == FILEA  E.G.  303+304+305==302
!                                       306+307+308==303
!                                       309+310+311==304  ETC.
!
!     NOTE - 301 IS EITHER NOT USED, OR USED BY THE 'MODIFIED' OPTP
!
      imhere = 1290
      IF ( debug ) WRITE (nout,99048) imhere , nfiles , nfiler , reduce
      filea = 301
      file = 302 - reduce
!
      DO iii = 1 , nfiles , reduce
         file = file + reduce
!
! ... CHECK LAST DO-LOOP CONDITION
!     IF ONE   FILE  LEFT, QUIT MERGING
!     IF TWO   FILES LEFT, DO A 2-TO-1 MERGE
!     IF THREE FILES LEFT, CONTINUE
!
         IF ( nfiles<=iii ) GOTO 2860
!
         filea = filea + 1
         CALL open(*5900,filea,z(ibuf1),wrtrew)
         imhere = 1300
         exh = 0
         DO l = 1 , reduce
            filex = file + l
            ibufl = ibufx(l)
            itape(l) = 1
            IF ( debug ) WRITE (nout,99048) imhere , filex , j
            CALL open(*6000,filex,z(ibufl),rdrew)
            CALL read(*6400,*6300,filex,y(1,l),24,0,i)
         ENDDO
 2820    DO
!
!     PICK THE SMALLEST CONTROL WORDS FROM Y(21,22,23,24 OF A,B,C)
!
            ii = 1
            DO l = 2 , reduce
               IF ( y(21,l)<y(21,ii) ) THEN
               ELSEIF ( y(21,l)==y(21,ii) ) THEN
                  IF ( y(21,l)==large ) CYCLE
                  IF ( y(22,l)<y(22,ii) ) THEN
                  ELSEIF ( y(22,l)==y(22,ii) ) THEN
                     IF ( y(23,l)<y(23,ii) ) THEN
                     ELSEIF ( y(23,l)==y(23,ii) ) THEN
                        IF ( y(24,l)<y(24,ii) ) THEN
                        ELSEIF ( y(24,l)==y(24,ii) ) THEN
!
!     FIRST 3 BULKDATA FIELDS THE SAME, CHECK POSSIBLE DUPLICATE CARD
!     SET 21ST AND 22ND CONTROL WORDS TO -6 IF IT IS A DUPLICATE
!
                           DO j = 7 , 20
                              IF ( y(j,l)/=y(j,ii) ) GOTO 2830
                           ENDDO
                           y(21,ii) = -6
                           y(22,ii) = -6
                           nogo = -1
                        ELSE
                           CYCLE
                        ENDIF
                     ELSE
                        CYCLE
                     ENDIF
                  ELSE
                     CYCLE
                  ENDIF
               ELSE
                  CYCLE
               ENDIF
!
               ii = l
 2830       ENDDO
            imhere = 1380
            IF ( debug ) WRITE (nout,99048) imhere , ii
!
            IF ( y(1,ii)==large ) CALL mesage(-61,0,name)
            CALL write(filea,y(1,ii),24,0)
            filex = ii + file
            CALL read(*6300,*2840,filex,y(1,ii),24,0,j)
            IF ( debug ) WRITE (nout,99019) filex , y(1,ii) , y(2,ii)
99019       FORMAT (5X,'TO PRE-MERGE FILE',I5,3X,2A4)
         ENDDO
!
! ... ONE OF THE FILES IS EXHAUSTED
!
 2840    exh = exh + 1
         itape(ii) = 0
         IF ( exh<reduce-1 ) THEN
            DO j = 1 , 24
               y(j,ii) = large
            ENDDO
            imhere = 1410
            IF ( debug ) WRITE (nout,99048) imhere , exh
            GOTO 2820
         ENDIF
!
! ... ONLY ONE FILE LEFT WHICH HAS NOT BEEN EXHAUSTED
!
 2860    filex = file + 1
         IF ( itape(2)==1 ) filex = file + 2
         IF ( itape(3)==1 ) filex = file + 3
         imhere = 1420
         IF ( debug ) WRITE (nout,99048) imhere , filex
         DO j = 1 , 24
            z(j) = y(j,filex)
         ENDDO
!
!     THIS REMAINING FILE COULD BE VERY BIG. IT COULD BE OPTP
!
         left24 = ((left-24)/24)*24
 2880    full = 1
         CALL read(*6400,*2900,filex,z(i25),left24,0,len)
         full = 0
         len = left24
 2900    IF ( len>=24 ) THEN
!
! ... CHECK ANY DUPLICATE IN THIS GROUP, SET THE 21ST AND 22ND CONTROL
!     WORDS TO -6 IF DUPLICATE
!     THEN WRITE THE REST TO FILEA
!
            DO l = 1 , len , 24
               i = l - 1
               k = i + 24
               DO j = 21 , 24
                  IF ( z(i+j)/=z(k+j) ) GOTO 2905
               ENDDO
               DO j = 7 , 20
                  IF ( z(i+j)/=z(k+j) ) GOTO 2905
               ENDDO
               z(i+21) = -6
               z(i+22) = -6
 2905          CALL write(filea,z(l),24,0)
               IF ( debug ) WRITE (nout,99050) filea , z(l) , z(l+1)
            ENDDO
!
!     IF FILE HAS NOT BEEN EXHAUSTED, GO BACK FOR MORE
!
            IF ( full/=1 ) THEN
               DO j = 1 , 24
                  z(j) = z(len+j)
               ENDDO
               GOTO 2880
            ENDIF
         ENDIF
!
         CALL write(filea,z(len+1),24,1)
         IF ( debug ) WRITE (nout,99050) filea , z(len+1) , z(len+2)
         DO l = 1 , reduce
            filex = file + l
            CALL close(filex,rew)
         ENDDO
!
         file = file + reduce
      ENDDO
   ENDIF
!
!     END OF PRE-MERGE
!
!
!     SET UP SORTED HEADING IF APPLICABLE
!
 3000 IF ( nbulk>1 ) THEN
      CALL page2(2)
      WRITE (nout,99020) uim
99020 FORMAT (A29,' 207, BULK DATA DECK IS NOT SORTED. NASTRAN WILL ','RE-ORDER THE INPUT DECK.')
   ENDIF
   IF ( f3long/=0 .AND. echos/=0 ) THEN
      CALL page2(2)
      WRITE (nout,99021) uim
99021 FORMAT (A29,' 207A, SIX CHARACTERS OF NASTRAN BCD NAME IN THE ','THIRD FIELD WERE USED DURING RE-ORDERING DECK')
   ENDIF
   IF ( echos/=0 ) THEN
      READ (head(2),99049) (head1(j),j=11,24)
!WKBR 9/93 HEAD2(4) = CDCNT(1)
      head2(5) = cdcnt(1)
!WKBR 9/93 HEAD3(4) = CDCNT(2)
      head3(5) = cdcnt(2)
!WKBR 9/93 HEAD3(5) = CDCNT(3)
      head3(6) = cdcnt(3)
      CALL page
   ENDIF
!
!     FINAL FILE MERGE, ADD CONTINUATION CARD AS NEEDED. RESULTS IN NPTP
!           ==========
!
!     ASSIGN BUFFER SPACES FOR THE SCRATCH FILES, RESERVE IBUF1 FOR NPTP
!
!     OPEN SCRATCH DATA FILES (303,304,305... OR        ==METHODS 1,2==
!     PREVIOUSLY SAVED         303,304,305...301  OR
!                              302,303,304,305... OR    ==METHOD  3  ==
!                              302,303,304,305,...,301)
!     AND READ INTO Y SPACE THE FIRST RECORD OF EACH SCRATCH FILE
!
!     OPEN NPTP FOR MERGED RESULT
!
!
   CALL open(*6900,nptp,z(ibuf1),wrt)
   CALL write(nptp,bulkda,2,1)
   IF ( nbulk+ndele==0 ) GOTO 4900
   IF ( tapecc/=0 ) CALL open(*6100,tapecc,z(ibufc),rd)
   recx = large
   ncard = 0
   exh = 0
   imhere = 1700
   IF ( debug ) WRITE (nout,99048) imhere , ncont , nfiler
!
!     IF NO CONTINUATION CARDS, AND ONLY ONE FILE IS USED TO STORE
!     BULKDATA INPUT CARDS, MOVE DATA FROM TAPE3 (COLD START JOB), OR
!     FROM TAPE1 (RESTART JOB WITH DELETE ONLY AND NO NEW BULK DATA)
!     INTO NPTP DIRECTLY. OTHERWISE, JUMP TO 1760
!
   IF ( ncont/=0 .OR. nfiler/=1 ) THEN
!
!     OPEN AND READ IN THE FIRST DATA RECORD FROM ALL FILES
!
      imhere = 1760
      tape = tape2
      IF ( reduce>1 ) tape = tape2 - 1
      IF ( debug ) WRITE (nout,99048) imhere , reduce , nfiler , tape
      empty = 0
      DO ii = 1 , nfiler
         tape = tape + 1
         IF ( ii==nfiler .AND. restr==1 ) tape = tape1
         itape(ii) = tape
         iibuf = ibufx(ii)
         CALL open(*6200,tape,z(iibuf),rdrew)
         CALL read(*6400,*3020,tape,y(1,ii),24,0,j)
         IF ( debug ) WRITE (nout,99022) tape , ii , y(1,ii) , y(2,ii)
99022    FORMAT (5X,'SETTING MERGE TABLE.  TAPE,II =',2I4,2X,2A4)
         CYCLE
 3020    empty = empty + 1
         CALL close(tape,rew)
         DO i = 1 , 24
            y(i,ii) = large
         ENDDO
      ENDDO
      exh = -1
      DO ii = 1 , nfiler
         IF ( y(21,ii)==-6 ) GOTO 3400
      ENDDO
      GOTO 3300
   ELSE
      tape = tape3
      IF ( restr==1 ) tape = tape1
      CALL open(*5800,tape,z(ibuf2),rdrew)
      left24 = ((ibuf2-1)/24)*24
   ENDIF
 3100 full = 1
   k = 1
   CALL read(*6400,*3200,tape,z(1),left24,0,j)
   full = 0
   j = left24
 3200 DO
      CALL write(nptp,z(k),20,1)
      IF ( debug ) WRITE (nout,99023) z(k) , z(k+1)
99023 FORMAT (5X,'WRITE TO NPTP',4X,2A4)
      ncard = ncard + 1
      l = k + 19
      IF ( echos/=0 ) THEN
         CALL page2(-1)
         WRITE (nout,99051) ncard , (z(i),i=k,l)
      ENDIF
      IF ( echop/=0 ) WRITE (lpch,99052) (z(i),i=k,l)
      k = k + 24
      IF ( k>=j ) THEN
         imhere = 1750
         IF ( debug ) WRITE (nout,99048) imhere , full , j
         IF ( full==0 ) GOTO 3100
         CALL eof(nptp)
         CALL close(nptp,rew)
         CALL close(tape,rew)
         IF ( echop/=0 ) WRITE (lpch,99054)
         IF ( echos/=0 ) THEN
            CALL page2(-1)
            WRITE (nout,99053)
         ENDIF
         GOTO 5200
      ENDIF
   ENDDO
 3300 exh = empty
   ii = 1
   IF ( nfiler>1 ) GOTO 3500
   GOTO 3700
 3400 l = ii
   GOTO 4700
!
!     START MERGING FILES
!
!     PICK THE SMALLEST CONTROL WORDS IN 21ST, 22ND, 23RD AND 24TH
!     WORDS OF EACH Y RECORD AND WRITE IT TO MERGE FILE NPTP, 20 WORDS
!     EACH. REPLACE THE CHOSEN RECORD BY NEXT RECORD OF THE SAME FILE
!
 3500 ii = 1
   DO l = 2 , nfiler
      IF ( y(21,l)<y(21,ii) ) THEN
      ELSEIF ( y(21,l)==y(21,ii) ) THEN
         IF ( y(1,l)==large ) CYCLE
         IF ( y(22,l)<y(22,ii) ) THEN
         ELSEIF ( y(22,l)==y(22,ii) ) THEN
            IF ( y(23,l)<y(23,ii) ) THEN
            ELSEIF ( y(23,l)==y(23,ii) ) THEN
               IF ( y(24,l)<y(24,ii) ) THEN
               ELSEIF ( y(24,l)==y(24,ii) ) THEN
!
! ... FIRST 3 BULKDATA FIELDS ARE THE SAME, CHECK POSSIBLE DUPLICATE
!     CARDS
!
                  DO j = 7 , 20
                     IF ( y(j,ii)/=y(j,l) ) GOTO 3600
                  ENDDO
                  GOTO 4700
               ELSE
                  CYCLE
               ENDIF
            ELSE
               CYCLE
            ENDIF
         ELSE
            CYCLE
         ENDIF
      ELSE
         CYCLE
      ENDIF
!
      ii = l
 3600 ENDDO
!
 3700 CALL write(nptp,y(1,ii),20,1)
   ncard = ncard + 1
   IF ( echos/=0 ) THEN
      CALL page2(-1)
      WRITE (nout,99051) ncard , (y(j,ii),j=1,20)
   ENDIF
   IF ( echop/=0 ) WRITE (lpch,99052) (y(j,ii),j=1,20)
   IF ( ncont==0 ) GOTO 4600
   IF ( restr/=0 ) THEN
!
!     IF THIS IS A RESTART JOB, SAVE THE FIRST FIELD, IN CASE THIS IS
!     THE PARENT OF A CONTINUATION CARD THAT CAME FROM NEW BULK DATA
!
      kard1 = y(1,ii)
      kard2 = y(2,ii)
      IF ( kard1==param(1) .AND. kard2==param(2) ) THEN
         kard1 = y(3,ii)
         kard2 = y(4,ii)
      ENDIF
   ENDIF
!
!     INSERT CONTINUATION CARD IF NEEDED
!
   IF ( nogo==-3 ) GOTO 4600
   tempx = y(19,ii)
   temp(1) = andf(tempx,les1b)
   temp(2) = y(20,ii)
 3800 IF ( tempx==blank .AND. temp(2)==blank ) GOTO 4600
   CALL bislc2(*4600,temp(1),z(ic),ncont,bsize,loc)
   k = loc*bsize + ic - 1
   l = z(k)
   IF ( l<0 ) THEN
!
!     DUPLICATE PARENT - ERROR
!
      CALL page2(-1)
      IF ( echos/=0 ) THEN
         WRITE (nout,99024) ufm
99024    FORMAT (A23,' 208, PREVIOUS CARD IS A DUPLICATE PARENT.')
         IF ( debug ) WRITE (nout,99025) loc , bsize , ic , k , l , tempx , temp(2)
99025    FORMAT ('  LOC,BSIZE,IC,K,L =',5I8,2(2H /,A4),1H/)
      ELSE
         WRITE (nout,99026) ufm , z(-l) , z(-l+1)
99026    FORMAT (A23,' 208A, ',2A4,' IS DUPLECATE CONTINUATION MARK.')
      ENDIF
      nogo = -1
      GOTO 4600
   ELSE
      z(k) = -l
      IF ( l>10000000 ) THEN
!
!     READ IN CONTINUATION CARD IMAGE FROM TAPECC FILE
!
         rec = l/10000000
         l = l - rec*10000000
         IF ( rec<recx ) THEN
            CALL rewind(tapecc)
            IF ( rec==1 ) GOTO 4200
            skip = rec - 1
            GOTO 4100
         ELSEIF ( rec/=recx ) THEN
            GOTO 4400
         ENDIF
      ENDIF
   ENDIF
 3900 DO i = 1 , 20
      buf(i) = z(l)
      l = l + 1
   ENDDO
   IF ( restr==0 .OR. kard1==-1 .OR. z(l)==0 ) GOTO 4500
!         ----------     -------------    -----------
!    I.E. NO RESTART     ALREADY DONE     BULKDATA CARD
!                                         NOT FLAGGED
!
!     SET THE PARENT'S RESTART BIT IF ABOVE CONDITIONS NOT MET
!
   ASSIGN 4000 TO crdflg
   from = 2040
   GOTO 5500
 4000 kard1 = -1
   GOTO 4500
 4100 DO j = 1 , skip
      CALL fwdrec(*6500,tapecc)
   ENDDO
 4200 CALL read(*6500,*4300,tapecc,z(ib),nzib,1,len)
   recx = rec
   GOTO 3900
 4300 CALL mesage(-37,0,name)
 4400 skip = rec - recx - 1
   IF ( skip<0 ) GOTO 4300
   IF ( skip==0 ) GOTO 4200
   GOTO 4100
!
!     GOT THE CONTINUATION CARD, WRITE IT OUT TO NPTP
!     CHECK WHETHER IT ASKS FOR MORE CONTINUATION CARD
!
 4500 CALL write(nptp,buf,20,1)
   ncard = ncard + 1
   IF ( echos/=0 ) THEN
      CALL page2(-1)
      WRITE (nout,99051) ncard , (buf(j),j=1,20)
   ENDIF
   IF ( echop/=0 ) WRITE (lpch,99052) (buf(j),j=1,20)
   tempx = buf(19)
   temp(1) = andf(tempx,les1b)
   temp(2) = buf(20)
!
!     CONTINUATION CARD NOT FOUND. ASSUME THE 10TH FIELD IS USER'S
!     COMMENT
!
   GOTO 3800
!
!     REPLACE THE MERGED RECORD BY THE NEXT RECORD OF THE SAME FILE
!
 4600 tape = itape(ii)
   imhere = 2200
   IF ( debug ) WRITE (nout,99048) imhere , tape , ii
   CALL read(*6400,*4800,tape,y(1,ii),24,0,j)
   IF ( debug ) WRITE (nout,99027) tape , ii , y(1,ii) , y(2,ii) , (y(j,ii),j=21,24)
99027 FORMAT (5X,'REPLACING - TAPE,II=',2I4,3X,2A4,4I12)
   IF ( y(21,ii)/=-6 ) THEN
      IF ( exh>=0 ) GOTO 3500
      GOTO 3300
   ENDIF
 4700 CALL page2(-2)
   ncard = ncard + 1
   CALL write(nptp,y(1,ii),20,1)
   WRITE (nout,99051) ncard , (y(j,ii),j=1,20)
   WRITE (nout,99028) uwm
99028 FORMAT (A25,' 208, PREVIOUS CARD IS A DUPLICATE')
!     NOGO = -1
   IF ( debug ) THEN
      DO k = 1 , nfiler
         WRITE (nout,99029) k , (y(j,k),j=1,24)
99029    FORMAT (1X,I2,3H)  ,20A4,2H /,4I8)
      ENDDO
      WRITE (nout,99030) ii , l
99030 FORMAT (//5X,'DUPLICATE  II,L=',2I8)
   ENDIF
   GOTO 4600
!
!     A SCRATCH FILE IS JUST EXHAUSTED, SET THE CORRESPONDING RECORD
!     A SET OF VERY LARGE NUMBERS
!     IF ALL FILES ARE EXHAUSTED, MERGING DONE
!
 4800 exh = exh + 1
   CALL close(tape,rew)
   imhere = 2270
   IF ( debug ) WRITE (nout,99048) imhere , tape , exh , nfiler , ncard
   IF ( exh<nfiler ) THEN
      DO i = 1 , 24
         y(i,ii) = large
      ENDDO
      GOTO 3500
   ENDIF
!
!     MERGING DONE. EVERY THING IN NPTP.
!
 4900 CALL eof(nptp)
   CALL close(nptp,rew)
   imhere = 2290
   IF ( debug ) WRITE (nout,99048) imhere , exh , nfiler
   IF ( echos/=0 ) THEN
      CALL page2(-1)
      WRITE (nout,99053)
   ENDIF
   IF ( echop/=0 ) WRITE (lpch,99054)
!
!     CHECK AND IDENTIFY PARENTLESS CONTINUATION CARDS
!     MAKE SURE TO EXCLUDE ANY BROKEN CONTINUATION CARDS SUPPOSEDLY
!     CONNECTED TO ONE PARENT
!
   IF ( ncont/=0 .AND. nogo/=-3 ) THEN
      imhere = 2330
      IF ( debug ) WRITE (nout,99048) imhere , ncont , ic
      recx = large
      j = ic + bsize - 1
      DO i = 1 , ncont
         l = z(j)
         DO
            IF ( l>=0 ) THEN
               imhere = 2400
               IF ( debug ) WRITE (nout,99055) imhere , z(j-2) , z(j-1) , l
               IF ( l>10000000 ) THEN
                  rec = l/10000000
                  l = l - rec*10000000
                  IF ( rec<recx ) THEN
                     CALL rewind(tapecc)
                     IF ( rec==1 ) GOTO 4902
                     skip = rec - 1
                  ELSEIF ( rec==recx ) THEN
                     GOTO 4905
                  ELSE
                     skip = rec - recx - 1
                     IF ( skip<0 ) THEN
                        CALL mesage(-37,0,name)
                        GOTO 4905
                     ELSEIF ( skip==0 ) THEN
                        GOTO 4902
                     ENDIF
                  ENDIF
                  DO k = 1 , skip
                     CALL fwdrec(*6500,tapecc)
                  ENDDO
 4902             CALL read(*6500,*5100,tapecc,z(ib),nzib,1,len)
                  recx = rec
               ENDIF
 4905          temp(1) = andf(z(l+18),les1b)
               temp(2) = z(l+19)
               imhere = 2470
               IF ( debug ) WRITE (nout,99055) imhere , temp , l
               IF ( temp(1)/=blank .OR. temp(2)/=blank ) THEN
                  loc = loc + 1
                  IF ( temp(1)/=z(loc+ic) .OR. temp(2)/=z(loc*ncont+ic) ) CALL bislc2(*4920,temp(1),z(ic),ncont,bsize,loc)
                  k = loc*bsize + ic - 1
                  l = z(k)
                  z(k) = -iabs(z(k))
                  CYCLE
               ENDIF
            ENDIF
            EXIT
         ENDDO
 4920    j = j + bsize
      ENDDO
!
      j = ic + bsize - 1
      ii = 0
      recx = large
      imhere = 2600
      DO i = 1 , ncont
         IF ( z(j)<0 ) GOTO 5000
         IF ( ii/=1 ) THEN
            ii = 1
            CALL page1
            WRITE (nout,99031) ufm
99031       FORMAT (A23,' 209, THE FOLLOWING CONTINUATION INPUT CARDS HAVE ','NO PARENTS',//)
            nogo = -1
         ENDIF
         CALL page2(1)
         l = z(j)
         IF ( l>10000000 ) THEN
!
            rec = l/10000000
            l = l - rec*10000000
            IF ( rec<recx ) THEN
               CALL rewind(tapecc)
               IF ( rec==1 ) GOTO 4980
               skip = rec - 1
               GOTO 4960
            ELSEIF ( rec/=recx ) THEN
               skip = rec - recx - 1
               IF ( skip<0 ) GOTO 5100
               IF ( skip==0 ) GOTO 4980
               GOTO 4960
            ENDIF
         ENDIF
 4940    m = l + 19
         WRITE (nout,99032) (z(k),k=l,m)
99032    FORMAT (10X,20A4)
         GOTO 5000
 4960    DO k = 1 , skip
            CALL fwdrec(*6500,tapecc)
         ENDDO
 4980    CALL read(*6500,*5100,tapecc,z(ib),nzib,1,len)
         recx = rec
         GOTO 4940
 5000    j = j + bsize
      ENDDO
   ENDIF
   GOTO 5200
 5100 CALL mesage(-2,tapecc,name)
!
!     CLOSE CONTINUAION CARD FILE TAPECC, IF IT WAS OPENED
!     DISABLE FREE-FIELD INPUT OPTION IN XREAD.
!
 5200 IF ( tapecc>0 ) CALL close(tapecc,rew)
   ffflag = 0
   wasff = 0
   IF ( nogo==-3 ) THEN
      WRITE (nout,99033) ufm
99033 FORMAT (A23,' 3008, CONTINUATION CARDS WERE NOT ADDED TO SORTED ','BULKDATA DECK DUE TO INSUFFICIENT CORE CONDITION.')
      IF ( cpflg/=0 ) WRITE (nout,99034)
99034 FORMAT (5X,'THE NPTP FILE OR TAPE GENERATED IN THIS RUN IS NOT ','SUITABLE FOR RESTART')
      CALL mesage(-61,0,0)
   ENDIF
   IF ( nogo/=0 ) nogo = 1
   IF ( .NOT.debug ) GOTO 7200
!
!     DEBUG NPTP ECHO
!
   imhere = 2730
   WRITE (nout,99048) imhere , ffflag , wasff
   CALL open(*6900,nptp,z(ibuf1),rdrew)
   DO
      CALL skpfil(nptp,+1)
      CALL read(*5300,*5300,nptp,buf(1),2,1,j)
      IF ( buf(1)==bulkda(1) .AND. buf(2)==bulkda(2) ) THEN
         DO
            CALL read(*5300,*5300,nptp,buf(1),20,1,j)
            WRITE (nout,99035) (buf(j),j=1,10) , (buf(j),j=17,20)
99035       FORMAT (' ==NPTP==>',5(1X,2A4),'...',2(1X,2A4))
         ENDDO
      ENDIF
   ENDDO
 5300 CALL close(nptp,rew)
   GOTO 7200
!
!
!     INTERNAL ROUTINE TO SET RESTART BITS - CRDFLG
!
!     BITS SET ONLY IF JOB IS A RESTART RUN, AND
!       1. ALL NEW BULK DATA CARDS,   EXCEPT CONTINUATION CARDS
!       2. ALL DELETED CARDS IN OPTP, EXCEPT CONTINUATION CARDS
!       3. THE PARENTS OF THE CONTINUATION CARDS IN 1 AND 2
!
 5400 kard1 = buf(1)
   kard2 = buf(2)
   IF ( kard1==param(1) .AND. kard2==param(2) ) THEN
      kard1 = buf(3)
      kard2 = buf(4)
   ENDIF
 5500 imhere = 2810
   IF ( debug ) WRITE (nout,99036) imhere , from , nogo , kard1 , kard2
99036 FORMAT (/,' *** IMHERE',I5,', FROM',I5,', NOGO=',I3,3X,2A4)
   IF ( nogo==0 ) THEN
      k = numx1*2
      DO i = 1 , k , 2
         IF ( kard1==icards(i) .AND. kard2==icards(i+1) ) THEN
            j = i/2
            m = (j/31) + 1
            n = mod(j,31) + 2
            ibits(m) = orf(ibits(m),itwo(n))
            IF ( debug ) WRITE (nout,99037) kard1 , kard2
99037       FORMAT (5X,'BITS SET SUCCESSFULLY FOR ',2A4)
            EXIT
         ENDIF
      ENDDO
   ENDIF
   GOTO crdflg
!
!     ERRORS
!
 5600 tape = tape1
   GOTO 6200
 5700 tape = tape2
   GOTO 6200
 5800 tape = tape3
   GOTO 6200
 5900 tape = filea
   GOTO 6200
 6000 tape = filex
   GOTO 6200
 6100 tape = tapecc
   IF ( tapecc>maxscr ) THEN
      WRITE (nout,99056) sfm
      GOTO 7100
   ENDIF
 6200 WRITE (nout,99038) sfm , tape
99038 FORMAT (A25,' 210, COULD NOT OPEN SCRATCH FILE',I5)
   GOTO 7100
 6300 WRITE (nout,99039) sfm
99039 FORMAT (A25,' 211, ILLEGAL EOR ON SCRATCH')
   GOTO 7100
 6400 WRITE (nout,99040) sfm , tape
99040 FORMAT (A25,' 212, ILLEGAL EOF ON SCRATCH',I5)
   GOTO 7100
 6500 WRITE (nout,99041)
99041 FORMAT (//26X,'212, TAPECC ERROR')
   tape = tapecc
   GOTO 6400
 6600 WRITE (nout,99042) sfm
99042 FORMAT (A25,' 213, ILLEGAL EOF ON OPTP')
   GOTO 7100
 6700 WRITE (nout,99043) sfm , imhere
99043 FORMAT (A25,' 213X, ILLEGAL DATA ON OPTP.  IMHERE =',I7)
   nogo = 1
   GOTO 1800
 6800 WRITE (nout,99044) sfm
99044 FORMAT (A25,' 214, OPTP COULD NOT BE OPENED')
   GOTO 7100
 6900 WRITE (nout,99045) sfm
99045 FORMAT (A25,' 215, NPTP COULD NOT BE OPENED')
   GOTO 7100
 7000 WRITE (nout,99046) sfm , imhere
99046 FORMAT (A25,' 219, MISSING ENDDATA CARD.  IMHERE =',I7)
   nogo = 1
   GOTO 900
 7100 WRITE (nout,99047) imhere
99047 FORMAT (5X,'IMHERE =',I6)
   CALL mesage(-37,0,name)
!
!     TURN OFF XSORT FLAG AND FREE-FIELD FLAG
!
 7200 ixsort = 0
99048 FORMAT (//,' *** XSORT2/IMHERE =',6I5)
99049 FORMAT (14A4)
99050 FORMAT (5X,'TO FILEA',I5,3X,2A4)
99051 FORMAT (13X,I8,1H-,8X,20A4)
99052 FORMAT (20A4)
99053 FORMAT (30X,'ENDDATA')
99054 FORMAT ('ENDDATA')
99055 FORMAT ('  IMHERE=',I5,'  LOOKING FOR - ',2A4,I14)
99056 FORMAT (A25,' 212, NUMBER OF AVAILABLE SCRATCH FILES EXEEDED.',5X,'RE-RUN JOB WITH MORE CORE')
END SUBROUTINE xsort2
