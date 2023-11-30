
SUBROUTINE xytran
!
   IMPLICIT NONE
   INTEGER Blkcom , Buf(100) , Center , File , Iat , Idin(153) , Idout(300) , Idum1 , Ifile , Ihead(96) , Intr , Ipset , Ipset2 ,   &
         & Ivalue(60) , Knt , Ksystm(81) , L , Major , Nat , Nbots , Ncard , Nframe , Nin , Nogo , Ntops , Steps , Subc(5) ,        &
         & Sysbuf , Tcurve(32) , Vecid(5) , Vector , Xaxis(32) , Yaxis(32) , Ybaxis(32) , Ytaxis(32) , Z(1)
   LOGICAL Outopn , Paplot , Plot , Print , Punch , Random
   REAL Rbuf(100) , Rz(1) , Value(60)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Blkcom , Idum1 , Ipset , Ipset2 , Nframe , Ncard
   COMMON /output/ Ihead
   COMMON /system/ Sysbuf , L , Nogo , Nin , Ksystm , Intr
   COMMON /xmssg / Ufm , Uwm
   COMMON /xywork/ File , Tcurve , Ntops , Print , Ifile , Xaxis , Nbots , Plot , Vector , Yaxis , Vecid , Punch , Major , Ytaxis , &
                 & Subc , Center , Random , Ybaxis , Idin , Buf , Ivalue , Iat , Idout , Outopn , Steps , Nat , Paplot , Knt
   COMMON /zzzzzz/ Z
   INTEGER bcd , blank , card , clea , comp , core , eor , files(11) , flag , fram , go , headsv(96) , i , i1 , i13 , i2 , i23 ,    &
         & i3 , icore , icrq , id , idtot , idz , ier , ifcrv , ifle , ij , indb(5) , inprwd , intrwd , istep , istsv , item ,      &
         & itemp , itry , ival , j , jj , kasknt , ktype , majid(11) , n , name(2) , namev(11) , namevg , nbeg , noeor , nslots ,   &
         & nsubs , nuq , nwds , nwords , openf(5) , outfil , outrwd , place , pset , rand , rewd , routin(2) , size , stop ,        &
         & subcas(200) , tcur , type , vdum , vg , word(58) , xaxi , xy , xycard(20) , xycdb , yaxi , ybax , ytax
   INTEGER korsz
   LOGICAL oomcp , oompp , vgp
   REAL temp , temp1
   !>>>>EQUIVALENCE (Z(1),Rz(1)) , (Buf(1),Rbuf(1)) , (Ivalue(1),Value(1))
!
   DATA stop/4HSTOP/ , go/4HGO  / , vdum/4HVDUM/ , xy/4HXY  / , fram/4HFRAM/ , clea/4HCLEA/ , tcur/4HTCUR/ , xaxi/4HXTIT/ ,         &
       &yaxi/4HYTIT/ , ytax/4HYTTI/ , ybax/4HYBTI/ , blank/4H    / , pset/4HPSET/
!
   DATA eor/1/ , noeor/0/ , outrwd/1/ , inprwd/0/ , rewd/1/
   DATA xycdb/101/ , outfil/201/ , indb/102 , 103 , 104 , 105 , 106/
   DATA nwords/58/ , routin/4HXYTR , 4HAN  / , rand/4HRAND/
   DATA vg/4HVG  / , i3/3/
!
   DATA word/4HXMIN , 4HXMAX , 4HYMIN , 4HYMAX , 4HYTMI , 4HYTMA , 4HYBMI , 4HYBMA , 4HXINT , 4HYINT , 4HYTIN , 4HYBIN , 4HXAXI ,   &
       &4HYAXI , 4HXTAX , 4HXBAX , 4HXDIV , 4HYDIV , 4HYTDI , 4HYBDI , 4HXVAL , 4HYVAL , 4HYTVA , 4HYBVA , 4HUPPE , 4HLOWE ,        &
      & 4HLEFT , 4HRIGH , 4HTLEF , 4HTRIG , 4HBLEF , 4HBRIG , 4HALLE , 4HTALL , 4HBALL , 4HXLOG , 4HYLOG , 4HYTLO , 4HYBLO ,        &
      & 4HCURV , 4HDENS , 4H.... , 4H.... , 4H.... , 4HSKIP , 4HCAME , 4HPLOT , 4HXPAP , 4HYPAP , 4HPENS , 4HXGRI , 4HYGRI ,        &
      & 4HXTGR , 4HYTGR , 4HXBGR , 4HYBGR , 4HCSCA , 4HCOLO/
!
!     DATA FOR THE 11 VECTOR TYPES POSSIBLE
!
!                                                         BASIC
!              VECTOR-NAME         RESIDENT-FILE       MAJOR - ID
!          ******************     ***************   ****************
   DATA namev(1)/4HDISP/ , files(1)/3/ , majid(1)/1/
   DATA namev(2)/4HVELO/ , files(2)/3/ , majid(2)/10/
   DATA namev(3)/4HACCE/ , files(3)/3/ , majid(3)/11/
   DATA namev(4)/4HSPCF/ , files(4)/2/ , majid(4)/3/
   DATA namev(5)/4HLOAD/ , files(5)/1/ , majid(5)/2/
   DATA namev(6)/4HSTRE/ , files(6)/4/ , majid(6)/5/
   DATA namev(7)/4HFORC/ , files(7)/5/ , majid(7)/4/
   DATA namev(8)/4HSDIS/ , files(8)/1/ , majid(8)/15/
   DATA namev(9)/4HSVEL/ , files(9)/1/ , majid(9)/16/
   DATA namev(10)/4HSACC/ , files(10)/1/ , majid(10)/17/
   DATA namev(11)/4HNONL/ , files(11)/2/ , majid(11)/12/
   DATA namevg/4H VG /
!
!     - IDOUT DATA RECORD DISCRIPTION -
!
!     WORD    TYPE   DISCRIPTION
!     ==================================================================
!       1     I/R    SUBCASE ID OR IF RANDOM THE MEAN RESPONSE
!       2      I     FRAME NUMBER
!       3      I     CURVE NUMBER
!       4      I     POINT-ID OR ELEMENT-ID
!       5      I     COMPONENT NUMBER
!       6      I     VECTOR NUMBER  1 THRU 11
!
!       7      I     1 -- CURVE USES TOP HALF OF FRAME
!                    0 -- CURVE USES FULL FRAME
!                   -1 -- CURVE USES LOWER HALF OF FRAME
!
!       8      I     0 -- AXIS,TICS,LABELS,VALUES, ETC. HAVE BEEN DRAWN
!                         AND THIS CURVE IS TO BE SCALED AND PLOTTED
!                         IDENTICALLY AS LAST EXCEPT FOR CURVE SYMBOLS.
!                    1 -- AXIS, TICS, LABELS, SCALEING, ETC. ARE TO BE
!                         PERFORMED OR COMPUTED AND IF IDOUT(7)=0 OR 1
!                         A SKIP TO NEW FRAME IS TO BE MADE.
!
!       9      I     NUMBER OF BLANK FRAMES BETWEEN FRAMES (FRAME-SKIP)
!      10      R     MINIMUM X-INCREMENT
!      11      R     XMIN  *
!      12      R     XMAX   *   DEFINES ACTUAL LIMITS OF DATA OF THIS
!      13      R     YMIN   *   UPPER, LOWER, OR FULL FRAME CURVE.
!      14      R     YMAX  *
!      15      R     ACTUAL VALUE OF FIRST TIC                 *
!      16      R     ACTUAL INCREMENT TO SUCCESSIVE TICS        *
!      17      I     ACTUAL MAXIMUM VALUE OF FRAME               *  X-
!      18      I     MAXIMUM NUMBER OF DIGITS IN ANY PRINT-VALUE  * DIRE
!      19      I     + OR - POWER FOR PRINT VALUES                * TICS
!      20      I     TOTAL NUMBER OF TICS TO PRINT THIS EDGE     *
!      21      I     VALUE PRINT SKIP  0,1,2,3---               *
!      22      I     SPARE                                     *
!      23      R     *
!      24      R      *
!      25      I       *
!      26      I        *  SAME AS  15 THRU 22
!      27      I        *  BUT FOR  Y-DIRECTION TICS
!      28      I       *
!      29      I      *
!      30      I     *
!      31      I     TOP EDGE TICS   **   EACH OF 31 THRU 34 MAY BE
!      32      I     BOTTOM EDGE TICS **  LESS THAN 0 -- TICS W/O VALUES
!      33      I     LEFT EDGE TICS   **  EQUAL TO  0 -- NO TICS HERE
!      34      I     RIGHT EDGE TICS **   GREATER   0 -- TICS W VALUES
!
!      35      I     0 -- X-DIRECTION IS LINEAR
!                    GREATER THAN 0 - NUMBR OF CYCLES AND X-DIREC IS LOG
!      36      I     0 -- Y-DIRECTION IS LINEAR
!                    GREATER THAN 0 - NUMBR OF CYCLES AND Y-DIREC IS LOG
!      37      I     0 -- NO X-AXIS
!                    1 -- DRAW X-AXIS
!
!      38      R     X-AXIS  Y-INTERCEPT
!
!      39      I     0 -- NO Y-AXIS
!                    1 -- DRAW Y-AXIS
!
!      40      R     Y-AXIS  X-INTERCEPT
!
!      41      I     LESS THAN 0 ----- PLOT SYMBOL FOR EACH CURVE POINT.
!                                      SELECT SYMBOL CORRESPONDING TO
!                                      CURVE NUMBER IN IDOUT(3)
!                    EQUAL TO  0 ----- CONNECT POINTS BY LINES WHERE
!                                      POINTS ARE CONTINUOUS I.E.(NO
!                                      INTEGER 1 PAIRS)
!                    GREATER THAN 0 -- DO BOTH OF ABOVE
!
!      42
!       .
!       .
!      50
!      51     BCD    TITLE(32)
!       .     BCD    SUBTITLE(32)
!       .     BCD    LABEL(32)
!       .     BCD    CURVE TITLE(32)
!       .     BCD    X-AXIS TITLE(32)
!     242     BCD    Y-AXIS TITLE(32)
!     243      I     XGRID LINES   0=NO   1=YES
!     244      I     YGRID LINES   0=NO   1=YES
!     245      I     TYPE OF PLOT  1=RESPONSE, 2=PSDF, 3=AUTO
!     246      I     STEPS
!       .
!       .
!     281      I     PAPLOT FRAME NUMBER
!     282      R     CSCALE (REAL NUMBER)
!     283      I     PENSIZE OR DENSITY
!     284      I     PLOTTER (LSHIFT 16) AND MODEL NUMBER.
!     285      R     INCHES PAPER X-DIRECTION
!     286      R     INCHES PAPER Y-DIRECTION
!     287      I     CAMERA FOR SC4020 LESS THAN 0=35MM, 0=F80,
!                                        GREATER 0=BOTH
!     288      I     PRINT FLAG  **
!     289      I     PLOT  FLAG  ** 0=NO, +=YES (PLOT- 2=BOTH, -1=PAPLT)
!     290      I     PUNCH FLAG  **
!     291      R     X-MIN OF ALL DATA
!     292      R     X-MAX OF ALL DATA
!     293      R     Y-MIN WITHIN X-LIMITS OF FRAME
!     294      R     X-VALUE AT THIS Y-MIN
!     295      R     Y-MAX WITHIN X-LIMITS OF FRAME
!     296      R     X-VALUE AT THIS Y-MAX
!     297      R     Y-MIN FOR ALL DATA
!     298      R     X-VALUE AT THIS Y-MIN
!     299      R     Y-MAX FOR ALL DATA
!     300      R     X-VALUE AT THIS Y-MAX
!     ==================================================================
!
!     SAVE OUTPUT HEADING
!
   DO i = 1 , 96
      headsv(i) = Ihead(i)
   ENDDO
!
!     ALLOCATE CORE AND OPEN DATA BLOCKS
!
   oompp = .FALSE.
   vgp = .FALSE.
   oomcp = .FALSE.
   Random = .FALSE.
   ifle = xycdb
   core = korsz(Z) - 1
   DO i = 1 , 32
      Tcurve(i) = blank
      Xaxis(i) = blank
      Yaxis(i) = blank
      Ytaxis(i) = blank
      Ybaxis(i) = blank
   ENDDO
   DO i = 1 , 5
      Subc(i) = 1
   ENDDO
   nsubs = 0
   core = core - Sysbuf
   IF ( core<0 ) THEN
!
!     INSUFFICIENT CORE
!
      CALL mesage(8,-core,routin)
!
!     CALL THE PRINTER-PLOTTER IF ANY REQUESTS FOR PRINTER-PLOTTER
!
      IF ( oompp ) CALL xyprpl
      GOTO 2700
   ELSE
      intrwd = inprwd
      IF ( Intr>0 ) THEN
         intrwd = outrwd
         xycdb = 301
      ENDIF
      CALL open(*2700,xycdb,Z(core+1),intrwd)
      IF ( Intr<=0 ) GOTO 400
      card = 1
      WRITE (L,99001)
!
99001 FORMAT ('  ENTER XYPLOT DEFINITION OR GO TO PLOT OR STOP TO EXIT')
   ENDIF
 100  DO ij = 1 , 20
      xycard(ij) = blank
   ENDDO
   CALL xread(*200,xycard)
   IF ( xycard(1)==stop ) THEN
!
!     INTERACTIVE STOP INITIATED HERE.
!
      Nogo = 1
      RETURN
   ELSE
      IF ( xycard(1)==go ) card = -1
      CALL ifp1xy(card,xycard)
      IF ( xycard(1)==go ) THEN
         CALL close(xycdb,rewd)
         IF ( Intr>10 ) L = 1
         CALL open(*2700,xycdb,Z(core+1),inprwd)
         GOTO 400
      ELSE
         card = 0
         IF ( Nogo==0 ) GOTO 300
         Nogo = 0
      ENDIF
   ENDIF
 200  WRITE (L,99002)
99002 FORMAT ('  BAD CARD TRY AGAIN')
 300  WRITE (L,99003) xycard
99003 FORMAT (20A4)
   GOTO 100
 400  IF ( Intr<=0 ) CALL fwdrec(*600,xycdb)
   Outopn = .FALSE.
   IF ( Blkcom==rand ) Random = .TRUE.
   IF ( Blkcom==vg ) vgp = .TRUE.
   IF ( Blkcom==vg ) namev(5) = namevg
!
   core = core - Sysbuf
   DO i = 1 , 5
      openf(i) = -1
      IF ( core<0 ) GOTO 800
!
      CALL open(*500,indb(i),Z(core),inprwd)
      openf(i) = 0
      Vecid(i) = 0
      core = core - Sysbuf
 500  ENDDO
!
   core = core + Sysbuf - 1
!
!     NOTE - OUTPUT DATA BLOCKS WILL BE OPENED WHEN AND IF REQUIRED
!
!
!
!     READ FIRST BCD WORD FROM -XYCDB- THEN GO INITIALIZE DATA
!
   bcd = clea
   GOTO 2600
 600  ier = 2
   GOTO 900
 700  ier = 3
   GOTO 900
 800  ier = 8
   ifle = -core
 900  CALL mesage(ier,ifle,routin)
!
!     CLOSE ANY OPEN FILES AND RETURN
!
 1000 CALL close(xycdb,rewd)
   DO i = 1 , 5
      CALL close(indb(i),rewd)
   ENDDO
   IF ( .NOT.Outopn ) RETURN
!
!     NO CAMERA PLOTS SO DONT WRITE TRAILER
!
   IF ( oomcp ) THEN
      Buf(1) = outfil
      Buf(2) = 9999999
      CALL wrttrl(Buf(1))
   ENDIF
   CALL close(outfil,rewd)
   IF ( oompp ) CALL xyprpl
   GOTO 2700
!
!     ERROR,  PLOTS REQUESTED AND OUTFIL PURGED.  DO ALL ELSE.
!
 1100 CALL page2(2)
   WRITE (L,99004) Uwm , outfil
99004 FORMAT (A25,' 976, OUTPUT DATA BLOCK',I4,' IS PURGED.','  XYTRAN WILL PROCESS ALL REQUESTS OTHER THAN PLOT')
   Plot = .FALSE.
!
 1200 IF ( Buf(3)/=0 ) Punch = .TRUE.
   type = Buf(4)
   Vector = Buf(5)
   nsubs = Buf(7)
   Knt = 0
   IF ( nsubs>0 ) CALL read(*600,*700,xycdb,subcas(1),nsubs,noeor,flag)
   IF ( nsubs>0 ) CALL sort(0,0,1,1,subcas(1),nsubs)
   IF ( Random .AND. type/=2 .AND. type/=3 ) GOTO 1700
   IF ( (.NOT.Random) .AND. (type==2 .OR. type==3) ) GOTO 1700
   IF ( (.NOT.Random) .AND. Ipset==pset .AND. Vector>7 ) GOTO 1700
   IF ( (.NOT.Random) .AND. Ipset/=pset .AND. Vector<=7 ) GOTO 1700
!
!     INITIALIZE DATA BLOCK POINTERS FOR THIS VECTOR
!
   File = files(Vector)
!
!     CHECK FOR RANDOM
!
   IF ( Random .AND. type==3 ) File = 2
   IF ( Random .AND. type==2 ) File = 1
   Ifile = indb(File)
   IF ( openf(File)<0 ) GOTO 1600
!
!     CHECK TO SEE IF THIS FILES SUBCASE IS TO BE OUTPUT
!
   IF ( openf(File)<0 ) GOTO 1600
   IF ( openf(File)==0 ) THEN
      CALL fwdrec(*1400,Ifile)
      CALL read(*1400,*1300,Ifile,Idin(1),20,eor,flag)
      CALL read(*1400,*1800,Ifile,Idin(1),-core,eor,flag)
      GOTO 800
   ELSE
      GOTO 1900
   ENDIF
!
!     EOR HIT ON IFILE.  SHOULD NOT HAVE HAPPENED
!
 1300 ier = 3
   GOTO 1500
!
!     EOF HIT ON IFILE.  SHOULD NOT HAVE HAPPENED
!
 1400 ier = 2
 1500 CALL mesage(ier,Ifile,routin)
   openf(File) = -1
!
!     FILE IFILE IS NOT SATISFACTORY
!
 1600 CALL fname(Ifile,Buf(1))
   CALL page2(3)
   WRITE (L,99005) Uwm , Buf(1) , Buf(2) , namev(Vector)
99005 FORMAT (A25,' 978',/5X,'XYTRAN MODULE FINDS DATA-BLOCK(',2A4,') PURGED, NULL, OR INADEQUATE, AND IS IGNORING XY-OUTPUT',      &
             &' REQUEST FOR -',A4,'- CURVES')
 1700 DO
!
!     SKIP OVER ANY AND ALL FRAME DATA FOR THIS CARD.
!
      CALL read(*600,*1000,xycdb,bcd,1,noeor,flag)
      IF ( bcd/=fram ) GOTO 2600
      DO
         CALL read(*600,*700,xycdb,Buf(1),3,noeor,flag)
         IF ( Buf(1)==-1 ) EXIT
      ENDDO
   ENDDO
 1800 CALL bckrec(Ifile)
   CALL bckrec(Ifile)
   size = flag/Idin(10)
   ktype = (Idin(2)/1000)*1000
   openf(File) = 1
 1900 kasknt = kasknt + 1
   IF ( nsubs==0 ) THEN
      Subc(File) = 0
   ELSE
      Subc(File) = subcas(kasknt)
   ENDIF
!
!     NOW READY TO PROCEED WITH DATA SELECTION
!
 2000 CALL read(*600,*1000,xycdb,bcd,1,noeor,flag)
   IF ( bcd/=fram ) GOTO 2600
!
!     READ IN THE ID-COMP-COMP SETS AND SORT ON ID-S.
!
   Knt = 0
   itry = 0
   Iat = 0
   DO
      CALL read(*600,*700,xycdb,Z(Iat+1),3,noeor,flag)
      IF ( Z(Iat+1)==-1 ) THEN
!
!     SORT ON ID-S
!
         CALL sort(0,0,3,1,Z(1),Iat)
         EXIT
      ELSE
         Iat = Iat + 3
      ENDIF
   ENDDO
 2100 icore = core - Iat
!
!     COMPUTE FINAL REGIONS
!
   nslots = Iat/3
   Nat = Iat
   IF ( Z(i3)>0 .AND. .NOT.Random ) nslots = nslots + nslots
   DO
      Steps = size
      IF ( vgp ) THEN
         itemp = 0
         nuq = 0
         DO i = 1 , Nat , 3
            IF ( Z(i)/=itemp ) THEN
               nuq = nuq + 1
               itemp = Z(i)
            ENDIF
         ENDDO
         Steps = Steps*nuq
!
!     SET CORE TO 1
!
         j = Iat + 1
         n = j + min0(icore,(nslots+1)*Steps)
         DO i = j , n
            Z(i) = 1
         ENDDO
      ENDIF
      IF ( Steps*(nslots+1)<=icore ) THEN
         Ntops = nslots/2
         Nbots = Ntops
         IF ( .NOT.(Z(i3)>0 .AND. .NOT.Random) ) THEN
            Ntops = nslots
            Nbots = 0
         ENDIF
         Center = Iat + Ntops*Steps
!
!     GET CURVE DATA
!
         Major = ktype + majid(Vector)
         i2 = 0
         ifcrv = -1
         istsv = 0
         idtot = Nat/3
         EXIT
      ELSE
         CALL page2(4)
         WRITE (L,99006) Uwm , Z(Iat-2) , Z(Iat-1) , Z(Iat)
99006    FORMAT (A25,' 980, INSUFFICIENT CORE TO HANDLE ALL DATA FOR ALL ','CURVES OF THIS FRAME',/5X,' ID =',I10,                  &
               & 2(' COMPONENT =',I4,5X),' DELETED FROM OUTPUT')
         icrq = Steps*(nslots+1) - icore
         WRITE (L,99007) icrq
99007    FORMAT (5X,'ADDITIONAL CORE NEEDED =',I9,' WORDS.')
         nslots = nslots - 1
         IF ( Z(i3)>0 .AND. .NOT.Random ) nslots = nslots - 1
         Nat = Nat - 3
         IF ( nslots<=0 ) GOTO 2000
      ENDIF
   ENDDO
!
!     I1 = 1-ST ROW OF NEXT ID
!     I2 = LAST ROW OF NEXT ID
!
 2200 i1 = i2 + 1
   nbeg = 3*i1 - 3
   IF ( nbeg>=Nat ) THEN
!
!     ALL DATA IS NOW IN SLOTS. INTEGER 1-S REMAIN IN VACANT SLOTS.
!
      IF ( nsubs==0 ) Subc(File) = Idin(4)
      CALL xydump(outfil,type)
      Knt = 1
      IF ( nsubs/=0 ) GOTO 2500
      Subc(File) = 0
      itry = itry + 1
      GOTO 2100
   ELSE
      idz = nbeg + 1
      id = Z(idz)
      i2 = i1
      DO WHILE ( .NOT.(i2>=idtot .OR. Random) )
         IF ( Z(3*i2+1)/=id ) EXIT
         i2 = i2 + 1
      ENDDO
!
!     FIND THIS ID ON IFILE
!
      CALL xyfind(*1400,*1300,*2300,majid(1),idz)
      Knt = -1
      IF ( itry==0 .AND. Subc(File)==-1 ) THEN
!
!     NSUBS = 0 AND POINT NOT FOUND START FRAME OVER
!
         CALL page2(3)
         WRITE (L,99011) Uwm , id , namev(Vector) , Ifile
         CALL rewind(Ifile)
         Subc(File) = 0
         Knt = 0
         IF ( Nat/3>i2 ) GOTO 2400
         IF ( i1/=1 ) GOTO 2400
         Subc(File) = 0
         GOTO 2000
!
!     THIS IS THE WAY OUT FOR ALL SUBCASE REQUEST
!
      ELSEIF ( itry/=0 .AND. Subc(File)==-1 ) THEN
         Subc(File) = 0
         GOTO 2000
      ELSE
         ktype = (Idin(2)/1000)*1000
         IF ( ktype==2000 .OR. ktype==3000 ) THEN
!
!     ID FOUND. READ DATA AND DISTRIBUTE INTO SLOTS.
!
            nwds = Idin(10)
            istep = 0
            ifcrv = ifcrv + 1
            IF ( vgp ) istep = istsv
            DO
               CALL read(*1400,*2200,Ifile,Buf(1),nwds,noeor,flag)
               istep = istep + 1
               IF ( istep<=Steps ) THEN
                  itemp = Iat + istep
                  istsv = istep
                  IF ( vgp ) THEN
                     IF ( ifcrv/=0 ) THEN
!
!     SORT X AND MOVE Y TO PROPER SLOTS
!
                        IF ( Rbuf(1)<Rz(itemp-1) ) THEN
                           n = istep - 1
                           DO i = 1 , n
                              IF ( Rbuf(1)<Rz(Iat+i) ) GOTO 2202
                           ENDDO
                        ENDIF
                        GOTO 2204
 2202                   istep = i
                        j = istep
                        itemp = Iat + istep
                        n = nslots + 1
                        jj = istsv - 1
                        DO i = 1 , n
                           item = Iat + (i-1)*Steps + j
                           temp1 = Rz(item)
                           Z(item) = 1
                           DO ij = j , jj
                              item = Iat + (i-1)*Steps + ij + 1
                              temp = Rz(item)
                              Rz(item) = temp1
                              temp1 = temp
                           ENDDO
                        ENDDO
                     ENDIF
                  ENDIF
 2204             Rz(itemp) = Rbuf(1)
!
!     DISTRIBUTE DATA
!
                  DO i = i1 , i2
                     place = i*Steps + istep
!
!     TOP CURVE
!
                     comp = Z(3*i-1)
!
!     SET MEAN RESPONSE IF RANDOM
!
                     IF ( Random ) Z(3*i) = Idin(8)
!
!     SET NUMBER OF ZERO CROSSINGS IF RANDOM
!
                     IF ( Random ) Buf(i+20) = Idin(9)
                     IF ( comp==1000 ) THEN
                        itemp = Iat + place
                        Z(itemp) = 1
                     ELSE
                        IF ( comp==0 ) CYCLE
                        IF ( Random ) comp = 2
                        IF ( comp<=nwds ) THEN
!
                           itemp = Iat + place
                           Z(itemp) = Buf(comp)
                        ELSE
                           Z(3*i-1) = 0
                           CALL page2(2)
                           WRITE (L,99012) Uwm , comp , id
                        ENDIF
                     ENDIF
!
!     BOTTOM CURVE IF DOUBLE FRAME
!
                     IF ( .NOT.(Random) ) THEN
                        comp = Z(3*i)
                        IF ( comp==1000 ) THEN
                           itemp = Center + place
                           Z(itemp) = 1
                        ELSEIF ( comp/=0 ) THEN
                           IF ( comp<=nwds ) THEN
!
                              itemp = Center + place
                              Z(itemp) = Buf(comp)
                           ELSE
                              Z(3*i) = 0
                              CALL page2(2)
                              WRITE (L,99012) Uwm , comp , id
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDDO
                  istep = istsv
               ENDIF
            ENDDO
         ELSE
            CALL page2(2)
            WRITE (L,99008) Uwm
99008       FORMAT (A25,' 977, FOLLOWING NAMED DATA-BLOCK IS NOT IN SORT-II',' FORMAT')
            GOTO 1600
         ENDIF
      ENDIF
   ENDIF
!
!     ID NOT FOUND. PRINT MESSAGE AND SHRINK LIST.
!
!
!     SUBCASE REQUEST EITHER SUBCASE NOT FOUND OR POINT NOT FOUND
!
 2300 IF ( Knt==-1 ) idz = -1
   IF ( idz/=-1 ) GOTO 2500
   CALL page2(3)
   WRITE (L,99011) Uwm , id , namev(Vector) , Ifile
   WRITE (L,99009) Subc(File)
99009 FORMAT (5X,'SUBCASE',I10)
   Knt = 0
   IF ( Nat/3<=i2 .AND. i1==1 ) GOTO 2500
 2400 i13 = 3*i1 - 3
   i23 = 3*i2 + 1
   IF ( i23<Nat ) THEN
      DO i = i23 , Nat
         i13 = i13 + 1
         Z(i13) = Z(i)
      ENDDO
   ENDIF
   idtot = idtot - (i2-i1) - 1
   i2 = i1 - 1
   Nat = i13
   IF ( idz==-1 .AND. i1/=1 .AND. .NOT.vgp ) GOTO 2200
   Iat = Nat
   GOTO 2100
 2500 IF ( kasknt<nsubs ) THEN
      kasknt = kasknt + 1
      Subc(File) = subcas(kasknt)
      DO i = 1 , 5
         Vecid(i) = 0
      ENDDO
      GOTO 2100
   ELSE
      kasknt = 0
      GOTO 1900
   ENDIF
!
!     INITIALIZE PARAMETERS
!
 2600 Plot = .FALSE.
   Punch = .FALSE.
   Print = .FALSE.
   Paplot = .FALSE.
   DO i = 1 , 5
      Vecid(i) = 0
!
!     VALUE DUMP
!
   ENDDO
!
!     BRANCH ON BCD WORD
!
   DO WHILE ( bcd/=xy )
      IF ( bcd==tcur ) THEN
!
!     SET TITLES
!
         CALL read(*600,*700,xycdb,Tcurve(1),32,noeor,flag)
      ELSEIF ( bcd==xaxi ) THEN
         CALL read(*600,*700,xycdb,Xaxis(1),32,noeor,flag)
      ELSEIF ( bcd==yaxi ) THEN
         CALL read(*600,*700,xycdb,Yaxis(1),32,noeor,flag)
      ELSEIF ( bcd==ytax ) THEN
         CALL read(*600,*700,xycdb,Ytaxis(1),32,noeor,flag)
      ELSEIF ( bcd==ybax ) THEN
         CALL read(*600,*700,xycdb,Ybaxis(1),32,noeor,flag)
!
!     SET SINGLE VALUE FLAGS. READ IN VALUE
!
      ELSEIF ( bcd==clea ) THEN
!
!     CLEAR ALL VALUES SET AND RESTORE DEFAULTS
!
         DO i = 1 , 12
            Ivalue(i) = 1
         ENDDO
         DO i = 13 , nwords
            IF ( i/=47 ) Ivalue(i) = 0
         ENDDO
         DO i = 25 , 32
            Ivalue(i) = 1
         ENDDO
!
!     DEFAULT CAMERA TO BOTH
!
         Ivalue(46) = 3
      ELSE
         IF ( bcd/=vdum ) THEN
            CALL read(*600,*700,xycdb,ival,1,noeor,flag)
            DO i = 1 , nwords
               IF ( bcd==word(i) ) GOTO 2620
            ENDDO
!
!     WORD NOT RECOGNIZED
!
            CALL page2(2)
            WRITE (L,99010) Uwm , bcd
!
!
99010       FORMAT (A25,' 975, XYTRAN DOES NOT RECOGNIZE ',A4,' AND IS IGNORING')
         ENDIF
         GOTO 2650
!
!     KEY WORD FOUND
!
 2620    IF ( bcd/=word(58) ) THEN
            Ivalue(i) = ival
         ELSE
            Ivalue(i) = ival
            CALL read(*600,*700,xycdb,ival,1,noeor,flag)
            Ivalue(i+1) = ival
         ENDIF
      ENDIF
!
!     READ NEXT BCD WORD
!
 2650 CALL read(*600,*1000,xycdb,bcd,1,noeor,flag)
   ENDDO
!
!     XY-COMMAND OPERATIONS HIT
!
   CALL read(*600,*700,xycdb,Buf(1),7,noeor,flag)
   IF ( Buf(6)/=0 ) Paplot = .TRUE.
   IF ( Buf(6)/=0 ) oompp = .TRUE.
   IF ( Buf(2)/=0 ) oomcp = .TRUE.
   IF ( Buf(1)/=0 ) Print = .TRUE.
   IF ( Buf(2)/=0 ) Plot = .TRUE.
   kasknt = 0
   IF ( Outopn ) GOTO 1200
   IF ( .NOT.Plot .AND. .NOT.Paplot ) GOTO 1200
!
!     OPEN OUTPUT PLOT DATA BLOCK
!
   core = core - Sysbuf
   IF ( core<=0 ) GOTO 800
!
   CALL open(*1100,outfil,Z(core+1),outrwd)
   CALL fname(outfil,name(1))
   CALL write(outfil,name(1),2,eor)
   Outopn = .TRUE.
   GOTO 1200
!
!     RESTORE OUTPUT HEADING AND RETURN
!
 2700 DO i = 1 , 96
      Ihead(i) = headsv(i)
   ENDDO
   RETURN
99011 FORMAT (A25,' 979, AN XY-OUTPUT REQUEST FOR POINT OR ELEMENT ID',I10,/5X,1H-,A4,'- CURVE IS BEING PASSED OVER.  THE ID ',     &
             &'COULD NOT BE FOUND IN DATA BLOCK',I10)
99012 FORMAT (A25,' 981, COMPONENT =',I10,' FOR ID =',I10,' IS TOO LARGE. THIS COMPONENTS CURVE NOT OUTPUT')
END SUBROUTINE xytran