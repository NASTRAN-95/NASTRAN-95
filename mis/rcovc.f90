
SUBROUTINE rcovc
   IMPLICIT NONE
   INTEGER Buf(1) , Buf1 , Buf2 , Buf3 , Buf4 , Dry , Energy , Fss(2) , Icore , Incu , Iopt , Ireq , Iru , Iz(2) , Lbasic , Lcore , &
         & Loop , Lreq , Lui , Mrecvr , Neigv , Norew , Nosort , Nout , Nru , Pa , Qa , Rfno , Rss(2) , Sof1 , Sof2 , Sof3 , Step , &
         & Sysbuf , Ua , Uinms(2,5) , Utypo
   REAL Phi , Pthres , Qthres , Raddeg , Range(2) , Rbuf(1) , Rd , Rdrew , Rew , Twophi , Uimpro , Uthres , Wrt , Wrtrew , Z(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Dry , Loop , Step , Fss , Rfno , Neigv , Lui , Uinms , Nosort , Uthres , Pthres , Qthres
   COMMON /condas/ Phi , Twophi , Raddeg
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew
   COMMON /rcovcm/ Mrecvr , Ua , Pa , Qa , Iopt , Rss , Energy , Uimpro , Range , Ireq , Lreq , Lbasic
   COMMON /rcovcr/ Icore , Lcore , Buf1 , Buf2 , Buf3 , Buf4 , Sof1 , Sof2 , Sof3
   COMMON /system/ Sysbuf , Nout
   COMMON /unpakx/ Utypo , Iru , Nru , Incu
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
   INTEGER acce(3) , casecc(2) , casess , comps(3) , disp(3) , dofs(32) , eqss , file , i , iappro , icode , icomb , idbuf(146) ,   &
         & idc , ieqss , iform , iloc , in , iopst , iout , isc , iseq , iset , isets , isil , iskip , iss , isub , it , item ,     &
         & itype , ivect , j , jeqss , js , jset , jsil , jss , k , kid , kpoint , kset , lcc , lid , lseq , lsets , lskip , mcba(7)&
         & , mode , n , name(2) , namef(2) , nccrec , neqss , next , nfwd(3) , njset , nmodes , np , nreq , ns , nsets , nskip ,    &
         & nss , nsteps , nwds , nword , oload(3) , opg1 , oqg1 , ougv1 , pg , pvec , rc , scr1 , scr2 , scr3 , scr6 , scr7 , scr8
   LOGICAL complx , end , incore , keep , non0 , once , pflag , qflag , supres , uflag
   REAL data(12) , eigen , eigeni , rdbuf(7) , thresh , value
   INTEGER korsz
   INTEGER soln , spcf(3) , srd , substr(4) , uvec , velo(3)
!
!     RCOVC COMPUTES REACTION FORCES AND GENERATES OUTPUT DATA BLOCKS
!     FOR DISPLACEMENTS, APPLIED LOADS, AND REACTION FORCES.
!
   !>>>>EQUIVALENCE (Buf(1),Z(1))
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (Buf(1),Rbuf(1)) , (idbuf(1),rdbuf(1))
   DATA casess , ougv1 , opg1 , oqg1 , scr1 , pg , scr3 , scr6 , scr7 , scr8 , scr2/101 , 201 , 202 , 203 , 301 , 105 , 303 , 306 , &
      & 307 , 308 , 302/
   DATA srd/1/
   DATA eqss , soln , uvec , pvec/4HEQSS , 4HSOLN , 4HUVEC , 4HPVEC/
   DATA name , casecc , substr/4HRCOV , 4HC    , 4HCASE , 4HCC   , 4HSUBS , 4HTRUC , 4HTURE , 4H    /
   DATA comps/4HCOMP , 4HONEN , 4HT   /
!
!     INITIALIZE
!
   IF ( Dry<0 ) RETURN
   Sof1 = korsz(Z) - Lreq - Sysbuf + 1
   Sof2 = Sof1 - Sysbuf - 1
   Sof3 = Sof2 - Sysbuf
   Buf1 = Sof3 - Sysbuf
   Buf2 = Buf1 - Sysbuf
   Buf3 = Buf2 - Sysbuf
   Buf4 = Buf3 - Sysbuf
   Lcore = Buf4 - 1
   IF ( Lcore<=0 ) THEN
      WRITE (Nout,99001) Swm , Rss
!
!     DIAGNOSTICS FORMAT STATEMENTS
!
99001 FORMAT (A27,' 6313, INSUFFICIENT CORE FOR RCOVR MODULE WHILE ','TRYING TO PROCESS',/34X,'PRINTOUT DATA BLOCKS FOR ',          &
             &'SUBSTRUCTURE',2A4)
      GOTO 2000
   ELSE
      CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
!
!     ================================================
!     THIS CARD SHOULD BE ADDED WHEN SDR3 IS FIXED
!
!     IF (RFNO .EQ. 9) NOSORT = 1
!
!     ================================================
      Pa = 0
      Qa = 0
      uflag = .FALSE.
      pflag = .FALSE.
      qflag = .FALSE.
!
!     CHECK OUTPUT REQUESTS ON CASESS
!
      CALL gopen(casess,Z(Buf1),Rdrew)
      nccrec = 1
      file = casess
      DO
         CALL fread(casess,Z,2,1)
         nccrec = nccrec + 1
         IF ( Iz(1)==casecc(1) .AND. Iz(2)==casecc(2) ) THEN
            DO
               CALL read(*100,*1900,casess,idbuf,35,1,i)
               IF ( idbuf(17)/=0 ) pflag = .TRUE.
               IF ( idbuf(20)/=0 ) uflag = .TRUE.
               IF ( idbuf(29)/=0 .AND. Rfno>=8 ) uflag = .TRUE.
               IF ( idbuf(32)/=0 .AND. Rfno>=8 ) uflag = .TRUE.
               IF ( idbuf(35)/=0 ) qflag = .TRUE.
               IF ( pflag .AND. uflag .AND. qflag ) GOTO 100
            ENDDO
         ENDIF
      ENDDO
   ENDIF
 100  CALL close(casess,Rew)
!
   IF ( Buf(Ireq)==1 ) uflag = .TRUE.
   IF ( Buf(Ireq+1)==1 ) pflag = .TRUE.
   IF ( Buf(Ireq+2)==1 ) qflag = .TRUE.
   IF ( Energy/=0 ) THEN
      uflag = .TRUE.
      IF ( Rfno>=3 .AND. Rfno<=8 ) pflag = .TRUE.
      IF ( Rfno>=3 .AND. Rfno<=8 ) qflag = .TRUE.
   ENDIF
!
   IF ( .NOT.(uflag .OR. pflag .OR. qflag) ) GOTO 1500
!
!     COMPUTE THE APPLIED STATIC LOADS FOR THE REQUESTED SUBSTRUCTURE
!     IF WE ARE PRINTING THE SOLUTION SUBSTRUCTURE CHECK IF THE LOADS
!     ARE ON A GINO FILE.
!
   IF ( Rfno==3 ) pflag = .FALSE.
   IF ( .NOT.(.NOT.pflag .AND. (.NOT.qflag .OR. Rfno==3)) ) THEN
      IF ( Rss(1)==Fss(1) .AND. Rss(2)==Fss(2) ) THEN
         Pa = pg
         mcba(1) = pg
         CALL rdtrl(mcba)
         IF ( mcba(1)>0 ) GOTO 200
      ENDIF
      Pa = scr3
      CALL rcovsl(Rss,pvec,0,scr6,scr7,scr8,Pa,Z(1),Z(1),Sof3-1,.FALSE.,Rfno)
      IF ( Pa<=0 ) pflag = .FALSE.
   ENDIF
!
!     GET THE DISPLACEMENT VECTOR AND IF RIGID FORMAT 8 THEN
!     CALCULATE THE VELOCITIES AND ACCELERATIONS.
!
 200  IF ( .NOT.uflag .AND. .NOT.qflag ) GOTO 500
   mcba(1) = Ua
   CALL rdtrl(mcba)
   IF ( mcba(1)>0 ) GOTO 400
   Ua = scr2
   CALL mtrxi(Ua,Rss,uvec,0,rc)
   IF ( rc==1 ) GOTO 400
 300  Ua = 0
   WRITE (Nout,99002) Swm , Rss
99002 FORMAT (A27,' 6319, DISPLACEMENT MATRIX FOR SUBSTRUCTURE ',2A4,' MISSING.'/5X,'DISPLACEMENT OUTPUT REQUESTS CANNOT BE ',      &
             &'HONORED.  SPCFORCE OUTPUT REQUESTS CANNOT BE HONORED UN','LESS THE',/5X,'REACTIONS HAVE BEEN PREVIOUSLY COMPUTED.')
   uflag = .FALSE.
   qflag = .FALSE.
   Energy = 0
!
 400  IF ( .NOT.(Rfno/=8 .OR. .NOT.(uflag .OR. qflag)) ) THEN
      CALL rcovva(Ua,0,scr1,0,0,0,Rss,Z(1),Z(1),Z(1))
      IF ( Ua<=0 ) GOTO 300
      Ua = scr1
   ENDIF
!
!     COMPUTE THE SPCF REACTIONS IF OUTPUT REQUESTS WERE SPECIFIED
!
 500  IF ( qflag ) CALL rcovqv
   IF ( Qa<=0 ) qflag = .FALSE.
!
!     OUTPUT PROCESSING
!
!
!     IF IOPT IS EQUAL TO ONE THEN THE OUTPUT WILL BE SORTED BY SUBCASE
!     IF EQUAL TO TWO IT WILL BE SORTED BY SUBSTRUCTURE
!
   np = Buf(Ireq+3)
   ns = Buf(Ireq+4)
!
!     FIND THE LENGTH AND TYPE OF THE VECTORS TO BE OUTPUT
!
   CALL softrl(Rss,uvec,mcba)
   nmodes = mcba(2)
   nsteps = mcba(2)
   IF ( Rfno==9 ) nsteps = nsteps/3
   complx = .FALSE.
   IF ( mcba(5)>=3 ) complx = .TRUE.
   nword = 1
   IF ( complx ) nword = 2
!
!     PERFORM GENERAL INITIALIZATION OF OFP ID RECORD
!
   idbuf(3) = 0
   idbuf(6) = 0
   idbuf(7) = 0
   idbuf(8) = 0
   idbuf(10) = 8
   IF ( complx ) idbuf(10) = 14
   DO i = 11 , 50
      idbuf(i) = 0
   ENDDO
!
!     INITALIZE THE UNPACK COMMON BLOCK
!
   Utypo = 1
   IF ( complx ) Utypo = 3
   Iru = 1
   Nru = mcba(3)
   Incu = 1
!
!     ALLOCATE OPEN CORE
!
   isets = 1
   lsets = 100
   ivect = isets + lsets
   isil = ivect + (Nru*nword)
   ieqss = isil + np
   IF ( ieqss+2>Lcore ) THEN
      WRITE (Nout,99001) Swm , Rss
      GOTO 2000
   ELSE
!
!
!                          OPEN CORE DIAGRAM FOR /RCOVCX/
!
!                       +----------------------------------+
!          Z(ISETS)     I                                  I
!                       I     CASECC SET INFORMATION       I
!                       I                                  I
!                       +----------------------------------+
!          Z(IVECT)     I                                  I
!                       I     VECTOR TO BE PRINTED         I
!                       I                                  I
!                       +----------------------------------+
!          Z(ISIL )     I                                  I
!                       I     SCALAR INDEX LIST FROM EQSS  I
!                       I                                  I
!                       +----------------------------------+
!          Z(IEQSS)     I                                  I
!                       I     EQSS DATA IN TRIPLES OF      I
!                       I        (1) EXTERNAL GRID ID      I
!                       I        (2) INTERNAL POINT INDEX  I
!                       I        (3) COMPONENT CODE        I
!                       I     DATA FOR EACH BASIC SUB-     I
!                       I     STRUCTURE TERMINATED BY      I
!                       I     THREE (-1)S                  I
!                       I                                  I
!                       I     NOTE  EQSS DATA MAY NOT BE   I
!                       I     IN CORE IF SPILL LOGIC       I
!                       I     INVOKED.                     I
!                       I                                  I
!                       +----------------------------------+
!          Z(ISEQ)      I                                  I
!                       I     SYMMETRY SEQUENCE            I
!                       I                                  I
!                       +----------------------------------+
!          Z(ICOMB)     I                                  I
!                       I     VECTOR CONTRIBUTING TO THE   I
!                       I     LINEAR COMBINATION FOR THE   I
!                       I     SYMMETRY SEQUENCE            I
!                       I                                  I
!                       +----------------------------------+
!
!     READ SIL FROM EQSS INTO OPEN CORE AT ISIL
!
      CALL sfetch(Rss,eqss,srd,rc)
      n = ns + 1
      CALL sjump(n)
      DO i = 1 , np
         CALL suread(Z(isil+i-1),1,nwds,rc)
         CALL suread(j,1,nwds,rc)
      ENDDO
!
!     READ EQSS DATA INTO OPEN CORE AT IEQSS IF IT WILL FIT.  IF IOPT
!     EQUALS 2, READ ONLY ONE GROUP AND PRCESS ONE BASIC SUBSTRUCTURE
!     A TIME.
!
      incore = .FALSE.
      neqss = ieqss + 2
      CALL sfetch(Rss,eqss,srd,rc)
      n = 1
      CALL sjump(n)
      nss = ns
      IF ( Iopt==2 ) nss = 1
      iss = 0
   ENDIF
!
!     TOP OF LOOP OVER BASIC SUBSTRUCTURES WHEN PROCESSING ONE AT A TIME
!
 600  iss = iss + 1
   k = Lcore - ieqss + 1
   j = ieqss
   item = eqss
   DO i = 1 , nss
      CALL suread(Z(j),k,nwds,rc)
      IF ( rc==3 ) GOTO 1600
      IF ( rc/=2 ) GOTO 700
      j = j + nwds
      IF ( j+3>Lcore ) GOTO 700
      Iz(j) = -1
      Iz(j+1) = -1
      Iz(j+2) = -1
      j = j + 3
      neqss = j - 1
      k = k - nwds - 3
      IF ( k<=0 ) GOTO 700
   ENDDO
   incore = .TRUE.
   GOTO 800
!
!     EQSS WILL NOT FIT IN CORE
!
 700  neqss = ieqss + 2
 800  iseq = neqss + 1
!
!     WRITE HEADER RECORDS ON OUTPUT DATA BLOCKS AND POSITION BOTH
!     INPUT AND OUTPUT DATA BLOCKS AFTER THE HEADER RECORD
!
   DO i = 1 , 3
      IF ( i==2 ) THEN
!
!     CHECK LOAD VECTOR
!
         IF ( .NOT.pflag ) CYCLE
         in = Pa
         iout = opg1
      ELSEIF ( i==3 ) THEN
!
!     CHECK READTIONS VECTOR
!
         IF ( .NOT.qflag ) CYCLE
         in = Qa
         iout = oqg1
      ELSE
!
!     CHECK DISPLACEMENT VECTOR
!
         IF ( .NOT.uflag ) CYCLE
         in = Ua
         iout = ougv1
      ENDIF
!
!     POSITION FILES
!
      CALL gopen(in,Z(Buf1),Rdrew)
      CALL close(in,Norew)
      IF ( iss<=1 ) THEN
         CALL open(*850,iout,Z(Buf2),Wrtrew)
         CALL fname(iout,namef)
         CALL write(iout,namef,2,1)
         CALL close(iout,Norew)
      ENDIF
      CYCLE
!
!     OUTPUT FILE PURGED - TURN OFF REQUEST FLAG
!
 850  WRITE (Nout,99003) Swm , iout
99003 FORMAT (A27,' 6314, OUTPUT REQUEST CANNOT BE HONORED.',/34X,'RCOVR MODULE OUTPUT DATA BLOCK',I4,' IS PURGED.')
      IF ( iout==ougv1 ) uflag = .FALSE.
      IF ( iout==opg1 ) pflag = .FALSE.
      IF ( iout==oqg1 ) qflag = .FALSE.
   ENDDO
!
!     SETUP FOR LOOP OVER SUBCASES
!
   isc = 0
   DO i = 1 , 3
      nfwd(i) = 0
   ENDDO
!
!     POSITION CASESS TO FIRST CASECC SUBCASE
!
   file = casess
   CALL open(*1700,casess,Z(Buf3),Rdrew)
   DO i = 1 , nccrec
      CALL fwdrec(*1800,casess)
   ENDDO
   end = .FALSE.
!
!     TOP OF LOOP OVER SUBCASES
!
 900  isc = isc + 1
   itype = 1
   IF ( end ) GOTO 1300
!
!     READ OUTPUT REQUESTS FROM CASECC RECORD
!
   CALL read(*1000,*1900,casess,0,-3,0,nwds)
   CALL fread(casess,lid,1,0)
   CALL fread(casess,0,-12,0)
   CALL fread(casess,oload,3,0)
   CALL fread(casess,disp,3,0)
   CALL fread(casess,0,-6,0)
   CALL fread(casess,acce,3,0)
   CALL fread(casess,velo,3,0)
   CALL fread(casess,spcf,3,0)
   CALL fread(casess,0,-1,0)
!
!     SET OUTPUT TYPE AND MEDIA - IF NO REQUEST IN CASE CONTROL
!     THE DEFAULT VALUES ARE REAL AND PRINTER
!
   iform = 1
   IF ( complx ) iform = 2
   IF ( disp(2)==0 ) disp(2) = 1
   IF ( disp(3)==0 ) disp(3) = iform
   IF ( disp(3)<0 ) Nosort = 1
   IF ( oload(2)==0 ) oload(2) = 1
   IF ( oload(3)==0 ) oload(3) = iform
   IF ( oload(3)<0 ) Nosort = 1
   IF ( spcf(2)==0 ) spcf(2) = 1
   IF ( spcf(3)==0 ) spcf(3) = iform
   IF ( spcf(3)<0 ) Nosort = 1
   IF ( velo(2)==0 ) velo(2) = 1
   IF ( velo(3)==0 ) velo(3) = iform
   IF ( velo(3)<0 ) Nosort = 1
   IF ( acce(2)==0 ) acce(2) = 1
   IF ( acce(3)==0 ) acce(3) = iform
   IF ( acce(3)<0 ) Nosort = 1
!
!     READ TITLE, SUBTITLE, AND LABEL.  WILL REPLACE RIGHTMOST WORDS OF
!     SUBTITLE WITH BASIC SUBSTRUCTURE NAME
!
   CALL fread(casess,idbuf(51),96,0)
   DO i = 1 , 3
      idbuf(i+101) = substr(i)
      idbuf(i+133) = comps(i)
   ENDDO
   idbuf(105) = substr(4)
   idbuf(106) = Rss(1)
   idbuf(107) = Rss(2)
!
!     READ SYMMETRY SEQUENCE AND SET INFORMATION
!
   nwds = -1
   Iz(isets) = 0
   Iz(isets+1) = 0
   CALL fread(casess,0,-31,0)
   CALL fread(casess,lcc,1,0)
   lskip = 167 - lcc
   CALL fread(casess,0,lskip,0)
   CALL read(*1800,*1200,casess,lseq,1,0,n)
   IF ( neqss+lseq>Lcore ) THEN
      WRITE (Nout,99001) Swm , Rss
      GOTO 2000
   ELSE
      IF ( lseq>0 ) CALL read(*1800,*1200,casess,Z(iseq),lseq,0,n)
      icomb = iseq + lseq
      IF ( icomb+Nru>Lcore ) THEN
         WRITE (Nout,99001) Swm , Rss
         GOTO 2000
      ELSE
         CALL read(*1800,*1200,casess,Z(isets),lsets,0,nwds)
         k = lsets
         DO
!
!     MUST EXPAND SETS PORTION OF OPEN CORE
!
            n = Lcore - neqss
            IF ( n>0 ) THEN
               DO i = isil , neqss
                  Iz(Lcore-i+1) = Iz(neqss-i+1)
               ENDDO
               ivect = ivect + n
               isil = isil + n
               ieqss = ieqss + n
               neqss = neqss + n
               CALL read(*1800,*1100,casess,Z(isets+lsets),n,0,nwds)
               k = k + n
            ELSEIF ( .NOT.incore ) THEN
               WRITE (Nout,99001) Swm , Rss
               GOTO 2000
            ELSE
               incore = .FALSE.
               neqss = ieqss + 2
            ENDIF
         ENDDO
      ENDIF
   ENDIF
!
!     END OF CASE CONTROL RECORDS - CHECK IF THIS IS REALLY THE END
!
 1000 end = .TRUE.
   IF ( Rfno<=2 ) GOTO 1400
   IF ( Rfno==3 .AND. isc>nmodes ) GOTO 1400
   IF ( Rfno<8 .OR. isc<=nsteps ) GOTO 1300
   GOTO 1400
 1100 nwds = k + nwds
 1200 nsets = isets + nwds
 1300 DO
!
!     PROCESS OUTPUT ITYPE
!
      once = .FALSE.
      jeqss = ieqss - 3
      iskip = 0
      IF ( .NOT.(itype==1 .AND. .NOT.uflag) ) THEN
         IF ( .NOT.(itype==2 .AND. .NOT.pflag) ) THEN
            IF ( .NOT.(itype==3 .AND. .NOT.qflag) ) THEN
               IF ( .NOT.(itype==4 .AND. .NOT.uflag) ) THEN
                  IF ( .NOT.(itype==5 .AND. .NOT.uflag) ) THEN
!
!     FOR EACH BASIC SUBSTRUCTURE CURRENTLY BEING PROCESSED, CONSTRUCT
!     ONE OFP ID AND DATA RECORD PAIR.  THE BASIC LOOP IS ABOVE THE
!     VECTOR PROCESSING BECAUSE OUTPUT REQUESTS CAN CHANGE FOR EACH
!     BASIC
!
                     DO js = 1 , nss
                        jss = iss + js - 1
                        nreq = Ireq + (jss-1)*Lbasic + 5
                        kpoint = Buf(nreq+12)
!
!     STATICS
!
                        IF ( Rfno>2 ) THEN
!
!     FOR NORMAL MODES GET MODE NUMBER, EIGENVALUE AND FREQUENCY
!
                           IF ( Rfno/=3 ) THEN
!
!     FOR DYNAMICS GET THE TIME OR FREQUENCY
!
                              IF ( Rfno==8 .OR. Rfno==9 ) THEN
                                 IF ( js<=1 ) THEN
                                    CALL sfetch(Fss,soln,srd,rc)
                                    n = 1
                                    CALL sjump(n)
                                    j = isc - 1
                                    IF ( j/=0 ) THEN
                                       DO i = 1 , j
                                         CALL suread(mcba(1),1,nwds,rc)
                                       ENDDO
                                    ENDIF
                                    CALL suread(value,1,nwds,rc)
!
                                    iappro = 5
                                    IF ( Rfno==9 ) iappro = 6
                                    idbuf(4) = isc
                                    rdbuf(5) = value
                                    idbuf(8) = lid
                                 ENDIF
                              ENDIF
                           ELSEIF ( js<=1 ) THEN
                              CALL sfetch(Fss,soln,srd,rc)
                              n = 1
                              CALL sjump(n)
                              j = isc - 1
                              IF ( j/=0 ) THEN
                                 DO i = 1 , j
                                    CALL suread(mcba(1),7,nwds,rc)
                                 ENDDO
                              ENDIF
                              CALL suread(mode,1,nwds,rc)
                              CALL suread(i,1,nwds,rc)
                              CALL suread(eigen,1,nwds,rc)
                              CALL suread(eigeni,1,nwds,rc)
                              CALL suread(value,1,nwds,rc)
!
                              iappro = 2
                              IF ( complx ) iappro = 9
                              idbuf(4) = isc
                              idbuf(5) = mode
                              rdbuf(6) = eigen
                              rdbuf(7) = 0.0
                              IF ( complx ) rdbuf(7) = eigeni
                           ENDIF
                        ELSEIF ( js<=1 ) THEN
                           iappro = 1
                           idbuf(4) = isc
                           idbuf(5) = lid
                        ENDIF
!
!     GET SUBCASE OR MODE REQUEST
!
                        IF ( Rfno<=2 ) THEN
                           isub = isc
                           iloc = 5
                        ELSEIF ( Rfno/=3 ) THEN
                           isub = isc
                           iloc = 11
                        ELSE
                           isub = mode
                           iloc = 6
                        ENDIF
                        iset = Buf(nreq+iloc)
                        IF ( iset>=0 ) THEN
                           IF ( iset==0 ) GOTO 1316
!
!     FIND THE REQUESTED SET
!
                           jset = isets
                           DO WHILE ( iset/=Iz(jset) )
                              jset = jset + Iz(jset+1) + 2
                              IF ( jset>=nsets ) THEN
!
!     SET NOT FOUND, ISSUE WARNING AND PRINT ALL INSTEAD.
!
                                 WRITE (Nout,99004) Uwm , iset
                                 Buf(nreq+iloc) = -1
                                 GOTO 1302
                              ENDIF
                           ENDDO
!
!     FIND IF CURRENT SUBCASE OR MODE IS IN REQUESTED SET
!
                           next = 1
                           kset = Iz(jset+1)
                           CALL setfnd(*1316,Iz(jset+2),kset,isub,next)
                        ENDIF
!
!     SO FAR SO GOOD - IF NORMAL MODES OR DYNAMICS PROBLEM CHECK IF
!     EIGEN VALUE, TIME OR FREQUENCY IS IN REQUESTED RANGE
!
 1302                   IF ( Rfno>=3 ) THEN
                           IF ( value<Rbuf(nreq+7) ) GOTO 1316
                           IF ( value>Rbuf(nreq+8) ) GOTO 1316
                        ENDIF
!
                        IF ( itype==2 ) THEN
!
!     PROCESS OLOAD REQUESTS
!
                           iopst = oload(1)
                           IF ( Buf(nreq+3)>-2 ) iopst = Buf(nreq+3)
                           IF ( iopst==0 .AND. lseq==0 ) GOTO 1316
                           IF ( once ) GOTO 1310
                           once = .TRUE.
!
                           idc = oload(2)
                           iform = iabs(oload(3))
                           thresh = Pthres
                           supres = .TRUE.
                           idbuf(2) = 2
                           in = Pa
                           iout = opg1
                        ELSEIF ( itype==3 ) THEN
!
!     PROCESS SPCFORCE (ACTUALLY, ALL REACTIONS) REQUESTS
!
                           iopst = spcf(1)
                           IF ( Buf(nreq+4)>-2 ) iopst = Buf(nreq+4)
                           IF ( iopst==0 .AND. lseq==0 ) GOTO 1316
                           IF ( once ) GOTO 1310
                           once = .TRUE.
!
                           idc = spcf(2)
                           iform = iabs(spcf(3))
                           thresh = Qthres
                           supres = .TRUE.
                           idbuf(2) = 3
                           in = Qa
                           iout = oqg1
                        ELSEIF ( itype==4 ) THEN
!
!     PROCESS VELOCITY REQUESTS
!
                           iopst = velo(1)
                           IF ( Buf(nreq+9)>-2 ) iopst = Buf(nreq+9)
                           IF ( iopst==0 .AND. lseq==0 ) GOTO 1316
                           IF ( once ) GOTO 1310
                           once = .TRUE.
!
                           idc = velo(2)
                           iform = iabs(velo(3))
                           idbuf(2) = 10
                           thresh = Uthres
                           supres = .FALSE.
                           in = Ua
                           iout = ougv1
                        ELSEIF ( itype==5 ) THEN
!
!     PROCESS ACCELERATION REQUESTS
!
                           iopst = acce(1)
                           IF ( Buf(nreq+10)>-2 ) iopst = Buf(nreq+10)
                           IF ( iopst==0 .AND. lseq==0 ) GOTO 1316
                           IF ( once ) GOTO 1310
                           once = .TRUE.
!
                           idc = acce(2)
                           iform = iabs(acce(3))
                           idbuf(2) = 11
                           thresh = Uthres
                           supres = .FALSE.
                           in = Ua
                           iout = ougv1
                        ELSE
!
!     PROCESS DISPLACEMENT REQUESTS
!
                           iopst = disp(1)
                           IF ( Buf(nreq+2)>-2 ) iopst = Buf(nreq+2)
                           IF ( iopst==0 .AND. lseq==0 ) GOTO 1316
                           IF ( once ) GOTO 1310
                           once = .TRUE.
!
                           idc = disp(2)
                           iform = iabs(disp(3))
                           idbuf(2) = 1
                           IF ( Rfno==3 ) idbuf(2) = 7
                           thresh = Uthres
                           supres = .FALSE.
                           in = Ua
                           iout = ougv1
                        ENDIF
!
!     OPEN FILES AND UNPACK VECTOR TO BE PRINTED
!
                        file = in
                        CALL gopen(in,Z(Buf1),Rd)
                        CALL gopen(iout,Z(Buf2),Wrt)
                        it = itype
                        IF ( itype>3 ) it = 1
                        IF ( lseq>0 ) THEN
!
!     FORM LINEAR COMBINATION FOR SYMMETRY SEQUENCE
!
                           n = nfwd(it) - lseq
                           IF ( n<0 ) THEN
                              n = -n
                              DO i = 1 , n
                                 CALL bckrec(in)
                              ENDDO
                           ELSEIF ( n/=0 ) THEN
                              DO i = 1 , n
                                 CALL fwdrec(*1800,in)
                              ENDDO
                           ENDIF
                           DO i = 1 , Nru
                              Z(ivect+i-1) = 0.0E0
                           ENDDO
                           DO i = 1 , lseq
                              CALL unpack(*1304,in,Z(icomb))
                              DO j = 1 , Nru
                                 Z(ivect+j-1) = Z(ivect+j-1) + Z(iseq+i-1)*Z(icomb+j-1)
                              ENDDO
 1304                      ENDDO
                           nfwd(it) = 0
                        ELSE
                           n = nfwd(it)
                           IF ( n>0 ) THEN
                              DO i = 1 , n
                                 CALL fwdrec(*1800,in)
                              ENDDO
                              nfwd(it) = 0
                           ENDIF
                           CALL unpack(*1306,in,Z(ivect))
                        ENDIF
                        GOTO 1308
 1306                   n = Nru*nword
                        DO i = 1 , n
                           Z(ivect+i-1) = 0.0
                        ENDDO
!
!     IF EQSS DATA NOT IN CORE, POSITION THE SOF
!
 1308                   IF ( .NOT.(incore) ) THEN
                           CALL sfetch(Rss,eqss,srd,rc)
                           nskip = iss + iskip
                           CALL sjump(nskip)
                           jeqss = ieqss
                        ENDIF
!
!     INSERT SUBSTRUCTURE NAME IN IDREC WRITE IT OUT
!
 1310                   idbuf(1) = idc + 10*iappro
                        IF ( complx .AND. js==1 ) idbuf(2) = idbuf(2) + 1000
                        idbuf(9) = iform
                        idbuf(138) = Buf(nreq)
                        idbuf(139) = Buf(nreq+1)
                        keep = .FALSE.
!
!     FIND THE REQUESTED OUTPUT SET
!
                        next = 1
                        jset = isets
                        njset = jset + 1
                        IF ( iopst>=0 ) THEN
                           DO WHILE ( iopst/=Iz(jset) )
                              jset = jset + Iz(jset+1) + 2
                              IF ( jset>=nsets ) THEN
!
!     SET NOT FOUND. ISSUE A WARNING AND PRINT ALL INSTEAD
!
                                 WRITE (Nout,99004) Uwm , iopst
                                 i = itype + 1
                                 IF ( itype>3 ) i = i + 4
                                 Buf(nreq+i) = -1
                                 iopst = -1
                                 EXIT
                              ENDIF
                           ENDDO
                        ENDIF
!
!     FOR EACH GRID POINT ID IN EQSS FOR THE CURRENT SUBSTRUCTURE WHICH
!     IS A MEMBER OF THE REQUESTED OUTPUT SET, WRITE A LINE OF OUTPUT
!
 1312                   IF ( incore ) THEN
                           jeqss = jeqss + 3
                           IF ( Iz(jeqss)<=0 ) GOTO 1314
                        ELSE
                           CALL suread(Z(jeqss),3,nwds,rc)
                           IF ( rc/=1 ) GOTO 1314
                        ENDIF
!
                        IF ( iopst>=0 ) THEN
                           IF ( next>Iz(jset+1) ) GOTO 1314
                           kset = Iz(jset+1)
                           kid = Iz(jeqss)
                           CALL setfnd(*1312,Iz(jset+2),kset,kid,next)
                        ENDIF
!
!     WRITE A LINE OF OUTPUT
!
                        icode = Iz(jeqss+2)
                        CALL decode(icode,dofs(1),n)
                        dofs(n+1) = -1
                        jsil = Iz(jeqss+1) + isil - 1
                        k = 0
                        non0 = .FALSE.
                        DO i = 1 , 6
                           IF ( dofs(k+1)+1==i ) THEN
                              j = ivect + (Iz(jsil)-1)*nword + k*nword
                              k = k + 1
                              data(i) = Z(j)
                              IF ( complx ) THEN
                                 data(6+i) = Z(j+1)
                                 IF ( iform==3 .AND. data(i)+data(6+i)/=0.0 ) THEN
                                    data(i) = sqrt(Z(j)**2+Z(j+1)**2)
                                    data(6+i) = atan2(Z(j+1),Z(j))*Raddeg
                                    IF ( data(6+i)<-.000005 ) data(6+i) = data(6+i) + 360.0
                                 ENDIF
                                 IF ( .NOT.(supres .AND. data(i)+data(6+i)==0.0) ) THEN
                                    IF ( abs(data(i))>=thresh .OR. abs(data(6+i))>=thresh ) THEN
                                       non0 = .TRUE.
                                       CYCLE
                                    ENDIF
                                 ENDIF
                              ELSEIF ( .NOT.(supres .AND. data(i)==0.0) ) THEN
                                 IF ( abs(data(i))>=thresh ) THEN
                                    non0 = .TRUE.
                                    CYCLE
                                 ENDIF
                              ENDIF
                           ENDIF
                           data(i) = 0.0
                           data(6+i) = 0.0
                        ENDDO
                        IF ( non0 ) THEN
                           IF ( .NOT.keep ) CALL write(iout,idbuf,146,1)
                           CALL write(iout,10*Iz(jeqss)+idc,1,0)
                           CALL write(iout,kpoint,1,0)
                           CALL write(iout,data,6*nword,0)
                           keep = .TRUE.
                        ENDIF
                        IF ( next<=Iz(jset+1) .OR. iopst<0 ) GOTO 1312
!
!     IF NO DATA WAS WRITTEN FOR THIS BASIC BACKREC THE OFP FILE
!     OVER THE PREVIOUSLY WRITTEN ID RECORD
!
 1314                   IF ( keep ) CALL write(iout,0,0,1)
                        IF ( Iz(jeqss)<0 .OR. (.NOT.incore .AND. rc/=1) ) CYCLE
!
!     NO MORE OUTPUT FOR THIS BASIC - SKIP EQSS DATA
!
 1316                   IF ( incore ) THEN
                           DO
                              jeqss = jeqss + 3
                              IF ( Iz(jeqss)<=0 ) EXIT
                           ENDDO
                        ELSEIF ( once ) THEN
                           n = 1
                           CALL sjump(n)
                        ELSE
                           iskip = iskip + 1
                        ENDIF
                     ENDDO
!
!     GO BACK AND DO ANOTHER OUTPUT TYPE
!
                     CALL close(in,Norew)
                     CALL close(iout,Norew)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF ( .NOT.(once) ) THEN
         it = itype
         IF ( itype>3 ) it = 1
         nfwd(it) = nfwd(it) + 1
      ENDIF
      itype = itype + 1
      IF ( itype>3 ) THEN
         IF ( itype>5 .OR. Rfno<8 ) THEN
            IF ( .NOT.end ) GOTO 900
            IF ( Rfno==3 .AND. isc<nmodes ) GOTO 900
            IF ( Rfno<8 .OR. isc>=nsteps ) EXIT
            GOTO 900
         ENDIF
      ENDIF
   ENDDO
!
!     ALL SUBCASES PROCESSED,  IF IOPT EQ 2, GO BACK AND PROCESS
!     NEXT BASIC SUBSTRUCTURE
!
 1400 CALL close(casess,Rew)
   IF ( Iopt==1 .OR. iss==ns ) THEN
!
!     WRITE TRAILERS AND EOF ON OUTPUT DATA BLOCKS
!
      DO i = 2 , 7
         mcba(i) = 1
      ENDDO
      IF ( uflag ) THEN
         CALL gopen(ougv1,Z(Buf1),Wrt)
         CALL close(ougv1,Rew)
         mcba(1) = ougv1
         CALL wrttrl(mcba)
      ENDIF
      IF ( pflag ) THEN
         CALL gopen(opg1,Z(Buf1),Wrt)
         CALL close(opg1,Rew)
         mcba(1) = opg1
         CALL wrttrl(mcba)
      ENDIF
      IF ( qflag ) THEN
         CALL gopen(oqg1,Z(Buf1),Wrt)
         CALL close(oqg1,Rew)
         mcba(1) = oqg1
         CALL wrttrl(mcba)
      ENDIF
   ELSE
      CALL sfetch(Rss,eqss,srd,rc)
      n = iss + 1
      CALL sjump(n)
      GOTO 600
   ENDIF
!
!     NORMAL MODULE TERMINATION
!
 1500 CALL sofcls
   RETURN
!
!     ERROR PROCESSING
!
 1600 n = 7
   CALL smsg(n,item,Rss)
   GOTO 2000
 1700 n = 1
   CALL mesage(n,file,name)
   GOTO 2000
 1800 n = 2
   CALL mesage(n,file,name)
   GOTO 2000
 1900 n = 3
   CALL mesage(n,file,name)
 2000 CALL sofcls
   DO i = 101 , 111
      CALL close(i,Rew)
   ENDDO
   DO i = 201 , 203
      CALL close(i,Rew)
   ENDDO
   DO i = 301 , 308
      CALL close(i,Rew)
   ENDDO
   RETURN
99004 FORMAT (A25,' 6365, REQUESTED OUTPUT SET ID',I6,' IS NOT DECLARED',' IN CASE CONTROL, ALL OUTPUT WILL BE PRODUCED.')
END SUBROUTINE rcovc