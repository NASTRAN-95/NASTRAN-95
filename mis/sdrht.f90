
SUBROUTINE sdrht
   IMPLICIT NONE
   REAL Ai(4) , Consts(5) , Raddeg , Rz(1) , Tabs , Twopi
   INTEGER Cls , Clsrew , Eol , Irow , Ksystm(65) , Mach , Outpt , Rd , Rdrew , Sysbuf , Wrt , Wrtrew , Z(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Tabs
   COMMON /condas/ Consts
   COMMON /machin/ Mach
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zntpkx/ Ai , Irow , Eol
   COMMON /zzzzzz/ Z
   INTEGER buf(50) , buf1 , buf2 , buf3 , core , dit , dlt , ecpt(100) , eltype , eor , est , estwds , file , gsize , hbdytp , i ,  &
         & idit , idpos(3) , idrec , ieltab , iflag , ildid , iload , imast , iqgid , irtids , itabid , itype , iug , iugz , j ,    &
         & jcount , jeltab , jid , jj1 , jj2 , jpoint , jword , k , k1 , k2 , kid , kk , kkk , krec , ktype , l , lcore , ldword(16)&
         & , lentry , loadid , lused , lwords , lz , m , mcb(7) , mcbugv(7) , n , name(2) , ncards , ndit , neltab , next , ngo ,   &
         & nldid , nldset , nload , nloads , nmast , noeor , nqgid , nrtids , nsets , ntabid , nug , number , numtab , nwdcrd ,     &
         & nwords , oef1
   REAL c , factor , grids(6) , rbuf(50) , scale , time , tt , yvalue
   LOGICAL cardin , found , havids , lhbdy , mch521 , transt
   INTEGER korsz , numtyp
   INTEGER oef1x , pass , qge , slt , sltat , sltrec , sltyps , subr(2) , tablst(13) , ug
!
!     SPECIAL FLUX-DATA-RECOVERY MODULE FOR HBDY ELEMENTS IN HEAT
!     TRANSFER.
!
!     DMAP CALLING SEQUENCE.
!
!     SDRHT SIL,USET,UGV,OEF1,SLT,EST,DIT,QGE,DLT,/OEF1X/V,N,TABS $
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Outpt) , (Consts(2),Twopi) , (Consts(3),Raddeg) , (Z(1),Rz(1)) , (buf(1),rbuf(1))
   DATA tablst/4 , 1105 , 11 , 1 , 1205 , 12 , 2 , 1305 , 13 , 3 , 1405 , 14 , 4/
   DATA lentry/14/ , eor , noeor/1 , 0/ , subr/4HSDRH , 2HT /
   DATA idpos/2 , 1 , 5/ , hbdytp/52/ , sltyps/16/
   DATA ldword/6 , 6 , 4 , 4 , 6 , 6 , 2 , 5 , 5 , 6 , 6 , 7 , 2 , 2 , 5 , 5/
   DATA ug , oef1 , slt , est , dit , qge , dlt , oef1x/103 , 104 , 105 , 106 , 107 , 108 , 109 , 201/
   DATA grids/1.0 , 2.0 , 2.0 , 3.0 , 4.0 , 2.0/
!
!     SET UP CORE AND BUFFERS
!
   buf1 = korsz(Z) - Sysbuf - 2
   buf2 = buf1 - Sysbuf - 2
   buf3 = buf2 - Sysbuf - 2
   core = buf3 - 1
   idrec = 1
   ieltab = 1
   pass = 0
   havids = .FALSE.
   cardin = .FALSE.
   mch521 = Mach==5 .OR. Mach==21
!
!     OPEN INPUT FORCES -OEF1- AND OUTPUT FORCES -OEF1X-.
!
   CALL open(*2700,oef1,Z(buf1),Rdrew)
   CALL open(*2800,oef1x,Z(buf2),Wrtrew)
   CALL fname(oef1x,name)
   CALL write(oef1x,name,2,eor)
   CALL fwdrec(*2900,oef1)
!
!     COPY RECORD PAIRS OF DATA FROM OEF1 TO OEF1X UNTIL HBDY DATA IS
!     DETECTED.
!
 100  lcore = core - idrec
   IF ( lcore<300 ) CALL mesage(-8,0,subr)
   file = oef1
   CALL read(*2600,*200,oef1,Z(idrec),lcore,noeor,iflag)
   WRITE (Outpt,99001) Swm
99001 FORMAT (A27,' 3063, INPUT FORCES DATA BLOCK HAS INCORRECT DATA.')
   GOTO 2900
!
!     MODIFY ID-RECORD IF THIS IS FOR HBDY ELEMENTS.
!
 200  IF ( Z(idrec+2)/=hbdytp ) THEN
      lhbdy = .FALSE.
   ELSE
      lhbdy = .TRUE.
!
!     SET CONSTANTS FROM OEF1 ID RECORD.
!
      IF ( Z(idrec)/10==6 ) THEN
         loadid = Z(idrec+7)
         time = Rz(idrec+4)
         transt = .TRUE.
      ELSE
         loadid = Z(idrec+7)
         transt = .FALSE.
      ENDIF
      lwords = Z(idrec+9)
      Z(idrec+9) = 5
   ENDIF
!/////
!     CALL BUG (4HTRAN ,90,TRANST,1)
!/////
   CALL write(oef1x,Z(idrec),iflag,eor)
   IF ( lhbdy ) THEN
!
!     HBDY ELEMENT DATA ENCOUNTERED.
!
      pass = pass + 1
!
!     ON FIRST PASS ELEMENT-DATA-TABLE IS FORMED.
!
!     EACH ENTRY WILL CONTAIN,
!
!      1) ELEMENT-ID (HBDY).
!      2) FLUX-RADIATION TERM FOR THIS ELEMENT.
!      3) FLUX-X FROM OEF1 DATA.
!      4) APPLIED LOAD (USING SLT DATA).
!      5) HBDY ELEMENT TYPE (1 TO 6).
!      6) HBDY AREA FACTOR.
!      7) ALPHA VALUE.
!      8) V1(1) *
!      9) V1(2)  *  VECTOR-V1
!     10) V1(3) *
!     11) V2(1) *
!     12) V2(2)  *  VECTOR-V2
!     13) V2(3) *
!     14) OUTPUT ID*10 + DEVICE CODE (FROM OEF1).
!
!     ON PASSES OTHER THAN THE FIRST ONLY THE FLUX-X VALUE IS EXTRACTED
!     FROM OEF1-HBDY-ENTRIES.
!
      jeltab = ieltab - 1
      GOTO 400
   ELSE
      DO
!
!     NOT AN HBDY ELEMENT TYPE THUS COPY DATA ACROSS.
!
         CALL read(*3100,*300,oef1,Z(idrec),lcore,noeor,iflag)
         CALL write(oef1x,Z(idrec),lcore,noeor)
      ENDDO
   ENDIF
 300  CALL write(oef1x,Z(idrec),iflag,eor)
   GOTO 100
 400  DO
!
!     INPUT = ID*10+CODE, NAME1,NAME2,GRD-X,GRD-Y,GRD-Z,FLUX-X,FLUX-Y,
!             FLUX-Z   (TOTAL OF 9 WORDS)
!
      CALL read(*3100,*500,oef1,buf,lwords,noeor,iflag)
      IF ( pass<=1 ) Z(jeltab+1) = buf(1)/10
!
!     STORE (FLUX-X) AND (OUTPUT ID*10 + DEVICE CODE).
!
      Z(jeltab+3) = buf(7)
      Rz(jeltab+2) = 0.0
      Z(jeltab+14) = buf(1)
      jeltab = jeltab + lentry
      IF ( jeltab>core ) CALL mesage(-8,0,subr)
   ENDDO
!
!     END OF DATA.
!
 500  IF ( pass<=1 ) THEN
      neltab = jeltab
      number = (neltab-ieltab+1)/lentry
!
!     OPEN UG FILE FOR INPUT OF UG VECTORS.
!
      CALL open(*2900,ug,Z(buf3),Rdrew)
      CALL fwdrec(*2900,ug)
      mcbugv(1) = ug
      CALL rdtrl(mcbugv)
      gsize = mcbugv(3)
   ELSEIF ( jeltab/=neltab ) THEN
      WRITE (Outpt,99002) Swm , jeltab , neltab
99002 FORMAT (A27,' 3064, INCONSISTANT HBDY DATA RECORDS.  ',2I20)
      GOTO 2900
   ENDIF
!
!     ALL DATA FROM OEF1 IS AT HAND NOW.
!     FILES ARE CLOSED WITHOUT REWIND.
!
   CALL close(oef1,Cls)
   CALL close(oef1x,Cls)
!
!     ALLOCATE UG VECTOR SPACE ON FIRST PASS.
!
   IF ( pass==1 ) THEN
      iug = neltab + 1
      nug = neltab + gsize
      iugz = iug - 1
      IF ( nug+5>core ) CALL mesage(-8,0,subr)
   ENDIF
!
!     BRING NEXT DISPLACEMENT VECTOR INTO CORE.
!
   DO i = iug , nug
      Rz(i) = Tabs
   ENDDO
!
   CALL intpk(*600,ug,0,1,0)
   DO
      CALL zntpki
      kk = iugz + Irow
      Rz(kk) = Rz(kk) + Ai(1)
      IF ( Eol>0 ) EXIT
   ENDDO
!
!     RAISE VECTOR RESULT TO 4TH POWER
!
 600  DO i = iug , nug
      Rz(i) = Rz(i)**4
   ENDDO
!
!     IF TRANSIENT PROBLEM SKIP ACCELERATION AND VELOCITY VECTORS
!
   IF ( transt ) THEN
      DO i = 1 , 2
         CALL fwdrec(*700,ug)
      ENDDO
   ENDIF
!/////
!     CALL BUG (4HUG4  ,200,Z(IUG),NUG-IUG+1)
!/////
!
!     IF NONLINEAR PROBLEM, COMPUTE FLUX RADIATION TERMS.
!
!                                            T           4
!     (FLUX-RADIATION         ) = (Q           )(U +TABS)
!                    EL-SUBSET      G,EL-SUBSET   G
!
 700  file = qge
   CALL open(*800,qge,Z(buf1),Rdrew)
   IF ( pass==1 ) THEN
      CALL read(*3100,*3200,qge,buf,-2,noeor,iflag)
!
!     ON FIRST PASS PICK UP ELEMENT ID LIST.
!
      iqgid = nug + 1
      CALL read(*3100,*900,qge,Z(iqgid),core-iqgid,noeor,iflag)
      CALL mesage(-7,0,subr)
      GOTO 900
   ELSE
      CALL fwdrec(*3100,qge)
      GOTO 1000
   ENDIF
 800  iqgid = nug + 1
   nqgid = nug
   idrec = nqgid
   GOTO 1200
 900  nqgid = nug + iflag
   idrec = nqgid + 1
!/////
!     CALL BUG (4HQGID,410,Z(IQGID),NQGID-IQGID+1)
!/////
!
!     EACH FLUX-RADIATION TERM IN THE ELEMENT TABLE IS CREATED BY
!     FORMING THE DOT-PRODUCT OF THE COLUMN OF -QGE- HAVING THE
!     SAME ELEMENT-ID WITH THE -UG- VECTOR IN CORE.
!
 1000 DO i = iqgid , nqgid , 1
      CALL intpk(*1100,qge,0,1,0)
!
!     FIND OUT IF ID OF THIS VECTOR IS IN ELEMENT TABLE.
!
      kid = Z(i)
      CALL bisloc(*1050,kid,Z(ieltab),lentry,number,jpoint)
      jword = ieltab + jpoint
      Z(jword) = 0
      DO
!
!     FORM DOT PRODUCT
!
         CALL zntpki
         k = iugz + Irow
         Rz(jword) = Rz(jword) - Ai(1)*Rz(k)
         IF ( Eol>0 ) GOTO 1100
      ENDDO
 1050 DO
!
!     ID OF THIS COLUMN NOT IN ELEMENT TABLE
!
         CALL zntpki
         IF ( Eol>0 ) EXIT
      ENDDO
 1100 ENDDO
!/////
!     CALL BUG (4HELTB ,440,Z(IELTAB),NELTAB-IELTAB+1)
!/////
   CALL close(qge,Clsrew)
!
!     ON FIRST PASS, EST IS PASSED AND HBDY ELEMENTS CALLED.
!
 1200 IF ( pass>1 ) GOTO 1400
   file = est
   CALL gopen(est,Z(buf1),Rdrew)
   next = ieltab
   DO
!
!     READ THE ELEMENT TYPE
!
      CALL read(*1300,*3200,est,eltype,1,noeor,iflag)
      IF ( eltype==hbdytp ) THEN
!
!     HBDY ELEMENT-SUMMARY-TABLE DATA FOUND.
!
         estwds = 53
         DO
            CALL read(*3100,*3200,est,ecpt,estwds,noeor,iflag)
!
!     CHECK TO SEE IF THIS ELEMENT IS IN OUTPUT SET.
!
            IF ( Z(next)<ecpt(1) ) GOTO 1300
            IF ( Z(next)==ecpt(1) ) THEN
!
!     THIS ELEMENT IS IN TABLE.
!
               CALL hbdy(ecpt,ecpt,2,rbuf,buf)
!
!     PLANT HBDY OUTPUTS INTO TABLE.
!
               Z(next+4) = ecpt(2)
               Z(next+5) = buf(2)
               Z(next+6) = ecpt(17)
               Z(next+7) = buf(11)
               Z(next+8) = buf(12)
               Z(next+9) = buf(13)
               Z(next+10) = buf(14)
               Z(next+11) = buf(15)
               Z(next+12) = buf(16)
               next = next + lentry
               IF ( next>=neltab ) THEN
                  CALL close(est,Clsrew)
                  GOTO 1400
               ENDIF
            ENDIF
         ENDDO
      ELSE
         CALL fwdrec(*3100,est)
      ENDIF
   ENDDO
 1300 WRITE (Outpt,99003) Swm , Z(next)
99003 FORMAT (A27,' 3065, THERE IS NO EST DATA FOR HBDY ELEMENT ID =',I10)
   GOTO 2900
!
!     LOAD SET PROCESSING IF LOAD-SET-ID IS NON-ZERO.
!
 1400 IF ( loadid<=0 ) GOTO 2400
!
!     OPEN SLT FOR LOAD DATA.
!
!
   file = slt
   CALL open(*3000,slt,Z(buf1),Rdrew)
   IF ( havids ) CALL fwdrec(*3100,slt)
   IF ( havids ) GOTO 2000
   havids = .TRUE.
   ildid = nqgid + 1
   nldid = ildid - 1
!
!     IDS OF LOAD SETS NOT IN CORE THUS BRING IN IDS FROM HEADER RECORD.
!
   imast = ildid
   nsets = 0
   nldset = 3
   CALL read(*3100,*3200,slt,buf,-2,noeor,iflag)
   DO
      IF ( nldid+5>core ) CALL mesage(-8,0,subr)
      CALL read(*3100,*1500,slt,Z(nldid+1),1,noeor,iflag)
      nsets = nsets + 1
      Z(nldid+2) = 1
      Z(nldid+3) = Z(nldid+1)
      Rz(nldid+4) = 1.0
      Z(nldid+5) = nsets
      nldid = nldid + 5
   ENDDO
!
!     IF TRANSIENT PROBLEM THEN DLT OPERATIONS BEGIN
!
 1500 IF ( .NOT.transt ) THEN
!
!     DETERMINE IF -LOADID- IS IN LIST OF LOAD SET IDS.
!
      idrec = nldid + 1
      GOTO 2000
   ELSE
      file = dlt
      CALL open(*3000,dlt,Z(buf2),Rdrew)
      CALL read(*3100,*3200,dlt,buf,3,noeor,iflag)
      m = buf(3)
      found = .FALSE.
      IF ( m>0 ) THEN
         DO i = 1 , m
            CALL read(*3100,*3200,dlt,buf,1,noeor,iflag)
            IF ( buf(1)==loadid ) found = .TRUE.
         ENDDO
      ENDIF
!
!     NOW READ RLOAD1, RLOAD2, TLOAD1, AND TLOAD2 IDS.
!
      irtids = nldid + 1
      CALL read(*3100,*1600,dlt,Z(irtids),core-irtids,noeor,iflag)
      CALL mesage(-7,0,subr)
      GOTO 2900
   ENDIF
 1600 nrtids = nldid + iflag
!
!     IF LOADID WAS FOUND AMONG THE DLOAD IDS, SEARCH IS NOW MADE IN
!     RECORD 1 OF THE DLT FOR THAT ID, AND ITS SUB-IDS.
!
   jj1 = ildid
   jj2 = nldid
   ildid = nrtids + 1
   nldid = nrtids + 2
   Z(ildid) = loadid
   Z(ildid+1) = 0
   IF ( .NOT.found ) THEN
!
!     LOADID NOT AMONG DLOADS FOR THIS TRANSIENT PROBLEM
!
      Z(ildid+1) = 1
      IF ( nldid+13<=core ) THEN
         Z(nldid+1) = Z(ildid)
         Rz(nldid+2) = 0.0
         Z(nldid+3) = 0
         Rz(nldid+4) = 1.0
         Z(nldid+5) = 0
         nldid = nldid + 11
      ELSE
         CALL mesage(8,0,subr)
         GOTO 2900
      ENDIF
   ELSE
      DO
!
!     READ A MASTER DLOAD SET-ID.
!
         CALL read(*3100,*3200,dlt,buf,2,noeor,iflag)
         IF ( buf(1)==loadid ) THEN
!
!     MASTER-ID FOUND.  BUILD LOAD-SET-ID TABLE.
!
            factor = rbuf(2)
            DO WHILE ( nldid+11<=core )
               CALL read(*3100,*3200,dlt,buf,2,noeor,iflag)
               IF ( buf(2)<=0 ) GOTO 1700
               Z(ildid+1) = Z(ildid+1) + 1
               Z(nldid+1) = buf(2)
               Rz(nldid+2) = 0.0
               Z(nldid+3) = 0
               Rz(nldid+4) = rbuf(1)*factor
               Z(nldid+5) = 0
               nldid = nldid + 11
            ENDDO
            CALL mesage(8,0,subr)
            GOTO 2900
         ELSE
            DO
!
!     SKIP SUB-ID DATA OF THIS MASTER
!
               CALL read(*3100,*3200,dlt,buf,2,noeor,iflag)
               IF ( buf(2)<0 ) EXIT
            ENDDO
         ENDIF
      ENDDO
   ENDIF
!
!     IF THERE ARE ANY DLOAD CARDS AT ALL THEN BALANCE OF (OR ALL OF)
!     RECORD 1 IS NOW SKIPPED.
!
 1700 IF ( m>0 ) CALL fwdrec(*3100,dlt)
!
!     NOW PICKING UP DATA NEEDED OF DYNAMIC LOAD SET RECORDS.
!
   k1 = ildid + 2
   k2 = nldid
   DO i = irtids , nrtids
!
!     READ THE LOAD TYPE
!
      CALL read(*3100,*3200,dlt,buf,2,noeor,iflag)
      IF ( buf(1)==3 .OR. buf(1)==4 ) THEN
!
!     CHECK AND SEE IF THIS TLOAD ID IS AMONG THE SUB-IDS
!
         DO j = k1 , k2 , 11
            IF ( Z(j)==Z(i) ) GOTO 1750
         ENDDO
      ENDIF
      CALL fwdrec(*3100,dlt)
      CYCLE
!
!     YES THIS RECORD IS NEEDED.  THUS PUT ITS DATA IN TABLE.
!
 1750 Z(j+4) = buf(1)
!
!     SLT ID INTO TABLE
!
      Z(j) = -buf(2)
!
!     SET SLT RECORD NUMBER
!
      k = 0
      DO l = jj1 , jj2 , 5
         k = k + 1
         IF ( Z(l)==buf(2) ) GOTO 1800
      ENDDO
      k = 0
 1800 Z(j+2) = k
      CALL read(*3100,*3200,dlt,Z(j+5),6,eor,iflag)
      IF ( buf(1)==3 ) Z(j+6) = 0
   ENDDO
!
!     CHECK IS NOW MADE TO INSURE ALL SUB-IDS RECEIVED DLT DATA.
!
!
!     SET SLT IDS POSITIVE
!
   DO i = k1 , k2 , 11
      Z(i) = iabs(Z(i))
   ENDDO
   DO i = k1 , k2 , 11
      IF ( Z(i+4)<=0 ) THEN
!
!     ERROR
!
         WRITE (Outpt,99004) Uwm , Z(i)
99004    FORMAT (A25,' 3066, THERE IS NO TLOAD1 OR TLOAD2 DATA FOR LOAD-','ID =',I9)
      ENDIF
   ENDDO
!
   CALL close(dlt,Clsrew)
   nldset = 11
!
!     SORT SUB-ID TABLE ON SLT RECORD NUMBERS.
!
   CALL sort(0,0,11,3,Z(k1),k2-k1+1)
!/////
!     CALL BUG (4HTABL,640,Z(K1),K2-K1+1)
!
!     CONSTRUCTION OF TABLE-ID LIST.
!
   itabid = nldid + 1
   ntabid = nldid + 1
!
!     FIRST GET TABLE ID-S PRESENT IN THE SUB-ID TABLE.
!
   DO i = k1 , k2 , 11
!
!     CHECK FOR OTHER THAN TLOAD1 TYPE CARD
!
      IF ( Z(i+4)==3 ) THEN
!
!     CHECK FOR ID IN TABLE.
!
         IF ( ntabid>itabid ) THEN
            DO j = itabid , ntabid
               IF ( Z(i+5)==Z(j) ) GOTO 1900
            ENDDO
         ENDIF
         ntabid = ntabid + 1
         IF ( ntabid>core ) CALL mesage(-8,0,subr)
         Z(ntabid) = Z(i+5)
      ENDIF
 1900 ENDDO
!
!     NOW PASS SLT AND GET ANY TABLE IDS PRESENT IN QVECT PORTION OF
!     RECORDS WE WILL BE USING.  (SLT IS CURRENTLY POSITIONED AT FIRST
!     RECORD.)
!
   sltat = 1
   file = slt
   DO i = k1 , k2 , 11
      ngo = Z(i+2) - sltat
      IF ( ngo<0 ) CYCLE
      IF ( ngo/=0 ) THEN
         DO j = 1 , ngo
            CALL fwdrec(*3100,slt)
         ENDDO
         sltat = sltat + ngo
      ENDIF
      DO
!
!     LOOK FOR QVECT CARDS.
!
         CALL read(*3100,*1950,slt,buf,2,noeor,iflag)
         itype = buf(1)
         ncards = buf(2)
         nwords = ldword(itype)
         IF ( itype/=16 ) THEN
            IF ( itype>16 ) GOTO 2500
            nwdcrd = -nwords*ncards
            CALL read(*3100,*3200,slt,buf,nwdcrd,noeor,iflag)
!
!     QVECT CARDS FOUND
!
         ELSEIF ( ncards>0 ) THEN
            DO j = 1 , ncards
               CALL read(*3100,*3200,slt,buf,nwords,noeor,iflag)
               DO k = 2 , 4
                  l = numtyp(buf(k))
                  IF ( mch521 .AND. buf(k)>16000 .AND. buf(k)<=99999999 ) l = 1
                  IF ( buf(k)>0 .AND. l==1 ) THEN
!
!     TABLE ID FOUND.  ADD TO LIST IF NOT YET IN.
!
                     IF ( ntabid>itabid ) THEN
                        DO l = itabid , ntabid
                           IF ( buf(k)==Z(l) ) GOTO 1905
                        ENDDO
                     ENDIF
                     ntabid = ntabid + 1
                     IF ( ntabid>core ) CALL mesage(-8,0,subr)
                     Z(ntabid) = buf(k)
                  ENDIF
 1905          ENDDO
            ENDDO
         ENDIF
      ENDDO
 1950 sltat = sltat + 1
   ENDDO
   numtab = ntabid - itabid
   Z(itabid) = numtab
   numtab = Z(itabid)
!
!     TABLE-ID LIST COMPLETE. NOW SORT IT AND PRIME TAB ROUTINE.
!
   CALL rewind(slt)
   CALL fwdrec(*3100,slt)
!/////
!     CALL BUG (4HTBID,555,Z(ITABID),NTABID-ITABID+1)
!/////
   idit = ntabid + 1
   ndit = idit
   lz = core - idit
   IF ( lz>10 ) THEN
      IF ( numtab/=0 ) THEN
         CALL sort(0,0,1,1,Z(itabid+1),numtab)
         CALL pretab(dit,Z(idit),Z(idit),Z(buf2),lz,lused,Z(itabid),tablst)
         ndit = idit + lused
      ENDIF
!/////
!     CALL BUG (4HDITS,557,Z(IDIT),NDIT-IDIT+1)
!/////
      idrec = ndit + 1
   ELSE
      CALL mesage(8,0,subr)
      GOTO 2900
   ENDIF
!/////
!     CALL BUG (4HLD1   ,360,Z(ILDID),NLDID-ILDID+1)
!/////
 2000 nmast = nldid
   j = ildid
 2100 DO WHILE ( j<=nldid )
!
!     CONTINUE SEARCH FOR LOADID
!
      IF ( loadid==Z(j) ) THEN
!
!     MATCH ON MASTER ID HAS BEEN FOUND.
!
         nloads = Z(j+1)
         iload = j + 2
         nload = iload + nldset*nloads - 1
!
!     PROCESS ALL THE LOAD RECORDS FOR THIS MASTER-ID
!
         sltat = 1
         GOTO 2400
      ELSE
!
!     POSITION -J- TO NEXT LOAD-SET-ID IN TABLE
!
         j = j + nldset*Z(j+1) + 2
      ENDIF
   ENDDO
!
!     LOAD SET ID LIST EXHAUSTED.
!     BRING IN ANY LOAD CARDS IF NOT YET IN.
!
   IF ( cardin ) THEN
!
!     -LOADID- NOT FOUND ANYWHERE.
!
      WRITE (Outpt,99005) Uwm , loadid
99005 FORMAT (A25,' 3067, LOAD SET ID =',I9,' IS NOT PRESENT.')
      GOTO 2900
   ELSE
!
!     THE LOAD-SET-ID TABLE HAS THE FOLLOWING FORMAT.
!
!     MASTER ID                                ******       Z(ILDID)
!     NUMBER OF SUB-IDS FOR THIS MASTER              *
!     SUB-ID             **  3-WORDS  ***             *
!     SCALE FACTOR = F(T)  * ONLY IF     *             *
!     SLT RECORD NUMBER  **  STATICS      * 11 WORDS   * REPEATS FOR
!     CONSTANT SCALE FACTOR               * REPEATS    * EACH MASTER
!     TYPE OF TLOAD =(3 OR 4)             * FOR EACH   * ID PRESENT
!     TYPE3 TABLE ID (OR) TYPE4 T1        * SUB-ID     *
!           0                   T2        * OF THIS    *
!           0                   OMEGA     * MASTER     *
!           0                   PHI       * ID         *
!           0                   N        *             *
!           0                   ALPHA ***              *
!              .                                       *
!              .                                      *
!              .                                     *
!                                              ******
!             ...                    ...
!             ...                    ...
!             ...                    ...                    Z(NLDID)
!
!
      cardin = .TRUE.
!
!     FORWARD SLT TO LOAD CARD RECORD.
!
      IF ( nsets>0 ) THEN
         DO i = 1 , nsets
            CALL fwdrec(*3000,slt)
         ENDDO
      ENDIF
   ENDIF
!
!     READ AND ENTER MASTER ID INTO TABLE
!
 2200 IF ( nldid+2>core ) CALL mesage(-8,0,subr)
   CALL read(*3100,*2300,slt,Z(nldid+1),2,noeor,iflag)
   scale = Rz(nldid+2)
   nldid = nldid + 2
   jcount = nldid
   Z(jcount) = 0
   DO
!
!     READ THE (SID, SCALE-FACTOR)  PAIRS FOR THIS ID.
!
      IF ( nldid+3>core ) CALL mesage(-8,0,subr)
      CALL read(*3100,*3200,slt,Z(nldid+1),2,noeor,iflag)
      IF ( Z(nldid+1)==-1 ) THEN
!
!     SORT ALL SUB-ID 3 WORD GROUPS ON SLT RECORD NUMBER.
!
         CALL sort(0,0,3,3,Z(jcount+1),nldid-jcount)
         GOTO 2200
      ELSE
!
!     MULTIPLY SUBID SCALE FACTOR BY MASTER SCALE FACTOR.
!
         Rz(nldid+2) = Rz(nldid+2)*scale
!
!     DETERMIND SLT RECORD NUMBER OF THIS SUB ID.
!
         krec = 1
         DO i = imast , nmast , nldset
            IF ( Z(nldid+1)==Z(i) ) GOTO 2220
            krec = krec + 1
         ENDDO
         krec = 0
 2220    Z(nldid+3) = krec
         nldid = nldid + 3
         Z(jcount) = Z(jcount) + 1
      ENDIF
   ENDDO
!
!     REPOSITION SLT TO BEGINNING OF FIRST SLT RECORD
!
 2300 idrec = nldid + 1
!/////
!     CALL BUG (4HLDID,460,Z(ILDID),NLDID - ILDID+1)
!/////
   CALL rewind(slt)
   CALL fwdrec(*3100,slt)
   GOTO 2100
!/////
!     CALL BUG (4HLOAD ,500,Z(ILOAD),NLOAD-ILOAD+1)
!/////
!
!     INITIALIZE APPLIED LOAD TO 0.0 FOR ALL ELEMENTS IN TABLE
!
 2400 DO i = ieltab , neltab , lentry
      Rz(i+3) = 0.0
   ENDDO
   IF ( loadid>0 ) THEN
      DO i = iload , nload , nldset
         factor = Rz(i+1)
         IF ( transt ) THEN
!
!     FACTOR HAS TO BE FOUND AS F(TIME)
!
            IF ( Z(i+4)==4 ) THEN
               tt = time - Rz(i+5)
               IF ( tt==0.0 ) THEN
                  IF ( Rz(i+9)/=0.0 ) THEN
                     factor = 0.0
                  ELSE
                     factor = cos(Twopi*Rz(i+7))
                  ENDIF
               ELSEIF ( tt<=0.0 .OR. time>=Rz(i+6) ) THEN
                  factor = 0.0
               ELSE
                  factor = Rz(i+3)*exp(Rz(i+10)*tt)*(tt**Rz(i+9))*cos(Twopi*Rz(i+7)*tt+Rz(i+8)/Raddeg)
               ENDIF
            ELSE
               CALL tab(Z(i+5),time,yvalue)
               factor = Rz(i+3)*yvalue
            ENDIF
         ENDIF
         sltrec = Z(i+2)
         IF ( sltrec>0 .AND. factor/=0.0 ) THEN
            DO
!
!     POSITION SLT TO RECORD DESIRED.
!
               ngo = sltrec - sltat
               IF ( ngo<0 ) THEN
!
!     NEED TO BACK UP ON SLT.
!
                  CALL bckrec(slt)
                  sltat = sltat - 1
                  CYCLE
               ELSEIF ( ngo/=0 ) THEN
!
!     NEED TO GO FORWARD ON SLT
!
                  DO j = 1 , ngo
                     CALL fwdrec(*3100,slt)
                  ENDDO
                  sltat = sltat + ngo
               ENDIF
               EXIT
            ENDDO
            DO
!
!     SLT IS NOW POSITIONED TO LOAD RECORD DESIRED.
!
!
!     GENERATE LOADS FOR THOSE ELEMENTS IN THE TABLE USING ONLY QBDY1,
!     QBDY2, AND QVECT CARDS.
!
               CALL read(*3100,*2450,slt,buf,2,noeor,iflag)
               itype = buf(1)
               IF ( itype<=sltyps ) THEN
                  ncards = buf(2)
                  IF ( ncards>0 ) THEN
                     nwords = ldword(itype)
                     IF ( itype>=14 .AND. itype<=16 ) THEN
                        itype = itype - 13
                        jid = idpos(itype)
                        DO k = 1 , ncards
!
!     READ A QBDY1, QBDY2, OR QVECT ENTRY.
!
                           CALL read(*3100,*3200,slt,buf,nwords,noeor,iflag)
!
!     CHECK FOR ID IN THE TABLE (OTHERWISE SKIP).
!
                           CALL bisloc(*2402,buf(jid),Z(ieltab),lentry,number,jpoint)
                           kk = ieltab + jpoint
!
!     THIS ELEMENT IS IN TABLE, THUS COMPUTE AND SUM IN THE LOAD.
!
                           IF ( itype==2 ) THEN
!
                              ktype = Z(kk+3)
                              Rz(kk+2) = Rz(kk+2) + factor*Rz(kk+4)*(rbuf(2)+rbuf(3)+rbuf(4)+rbuf(5))/grids(ktype)
                           ELSEIF ( itype==3 ) THEN
!
!
!     CALL TAB IF E1,E2,E3 OF QVECT DATA ARE TABLE ID-S IMPLYING
!     TIME DEPENDENCE
!
                              IF ( transt ) THEN
                                 DO kkk = 2 , 4
                                    l = numtyp(buf(kkk))
                                    IF ( mch521 .AND. buf(kkk)>16000 .AND. buf(kkk)<=99999999 ) l = 1
                                    IF ( buf(kkk)>0 .AND. l==1 ) THEN
                                       CALL tab(buf(kkk),time,yvalue)
                                       rbuf(kkk) = yvalue
                                    ENDIF
                                 ENDDO
                              ENDIF
                              ktype = Z(kk+3)
                              c = rbuf(2)*Rz(kk+6) + rbuf(3)*Rz(kk+7) + rbuf(4)*Rz(kk+8)
                              IF ( ktype==6 ) THEN
                                 Rz(kk+2) = Rz(kk+2) + factor*Rz(kk+4)*rbuf(1)*Rz(kk+5)                                             &
                                  & *sqrt(c*c+(rbuf(2)*Rz(kk+9)+rbuf(3)*Rz(kk+10)+rbuf(4)*Rz(kk+11))**2)
                              ELSEIF ( c<=0 ) THEN
                                 Rz(kk+2) = Rz(kk+2) - c*Rz(kk+4)*Rz(kk+5)*rbuf(1)*factor
                              ENDIF
                           ELSE
                              Rz(kk+2) = Rz(kk+2) + Rz(kk+4)*rbuf(1)*factor
                           ENDIF
!
 2402                   ENDDO
                        EXIT
                     ELSE
                        nwdcrd = -nwords*ncards
                        CALL read(*3100,*3200,slt,buf,nwdcrd,noeor,iflag)
                     ENDIF
                  ENDIF
               ELSE
                  WRITE (Outpt,99007) Swm , itype
                  GOTO 2900
               ENDIF
            ENDDO
         ENDIF
!
 2450 ENDDO
      CALL close(slt,Clsrew)
   ENDIF
!/////
!     CALL BUG (4HTELT,670,Z(IELTAB),NELTAB-IELTAB+1)
!/////
!
!     ELEMENT TABLE IS NOW COMPLETE FOR OUTPUT.
!
   file = oef1x
   CALL open(*3000,oef1x,Z(buf1),Wrt)
   DO i = ieltab , neltab , lentry
      buf(1) = Z(i+13)
      rbuf(2) = Rz(i+3)
      rbuf(3) = Rz(i+2)
      rbuf(4) = Rz(i+1)
      rbuf(5) = rbuf(2) + rbuf(3) + rbuf(4)
      CALL write(oef1x,buf(1),5,noeor)
   ENDDO
   CALL write(oef1x,0,0,eor)
   file = oef1
   CALL open(*3000,oef1,Z(buf2),Rd)
   GOTO 100
 2500 WRITE (Outpt,99007) itype
   GOTO 2900
!
!     ALL PROCESSING COMPLETE.
!
 2600 mcb(1) = oef1
   CALL rdtrl(mcb)
   mcb(1) = oef1x
   CALL wrttrl(mcb)
   GOTO 2900
!
!     ERROR CONDITIONS.
!
 2700 RETURN
!
 2800 WRITE (Outpt,99006) Uwm
99006 FORMAT (A25,' 3069, OUTPUT DATA BLOCK FOR FORCES IS PURGED.')
 2900 CALL close(oef1,Clsrew)
   CALL close(oef1x,Clsrew)
   CALL close(ug,Clsrew)
   CALL close(est,Clsrew)
   CALL close(slt,Clsrew)
   CALL close(dlt,Clsrew)
   GOTO 2700
 3000 n = 1
   GOTO 3300
 3100 n = 2
   GOTO 3300
 3200 n = 3
 3300 CALL mesage(n,file,subr)
   GOTO 2900
99007 FORMAT (A27,' 3068, UNRECOGNIZED CARD TYPE =',I9,' FOUND IN -SLT- DATA BLOCK.')
END SUBROUTINE sdrht
