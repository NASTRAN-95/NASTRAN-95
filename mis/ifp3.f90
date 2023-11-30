
SUBROUTINE ifp3
!
!     DATA PROCESSING AND GENERATION OF THE AXIS-SYMETRIC-CONICAL SHELL
!
!          CARDS          TYPE       REC.ID-BIT CARDS-FILE,  CARDS-FILE
!     ===  =======      ===========  ========== ===========  ==========
!      1   AXIC     --  AX.SY.SHELL     515- 5
!      2   CCONEAX  --  AX.SY.SHELL    8515-85  CCONE-GEOM2,
!      3   FORCEAX  --  AX.SY.SHELL    2115-21  FORCE-GEOM3,
!      4   FORCE    --  STANDARD       4201-42  FORCE-GEOM3,
!      5   GRAV     --  STANDARD       4401-44   GRAV-GEOM3,
!      6   LOAD     --  STANDARD       4551-61   LOAD-GEOM3,
!      7   MOMAX    --  AX.SY.SHELL    3815-38  MOMNT-GEOM3,
!      8   MOMENT   --  STANDARD       4801-48  MOMNT-GEOM3,
!      9   MPCADD   --  STANDARD       4891-60 MPCADD-GEOM4,
!     10   MPCAX    --  AX.SY.SHELL    4015-40    MPC-GEOM4,
!     11   OMITAX   --  AX.SY.SHELL    4315-43   OMIT-GEOM4,
!     12   POINTAX  --  AX.SY.SHELL    4915-49    MPC-GEOM4, GRID-GEOM1
!     13+  RFORCE   --  STANDARD       5509-55 RFORCE-GEOM3,
!     14   RINGAX   --  AX.SY.SHELL    5615-56    SPC-GEOM4, GRID-GEOM1
!     15   SECTAX   --  AX.SY.SHELL    6315-63    MPC-GEOM4, GRID-GEOM1
!     16   SEQGP    --  STANDARD       5301-53  SEQGP-GEOM1,
!     17   SPCADD   --  STANDARD       5491-59 SPCADD-GEOM4,
!     18   SPCAX    --  AX.SY.SHELL    6215-62    SPC-GEOM4,
!     19   SUPAX    --  AX.SY.SHELL    6415-64 SUPORT-GEOM4,
!     20   TEMPAX   --  AX.SY.SHELL    6815-68   TEMP-GEOM3,
!     21   TEMPD    --  STANDARD       5641-65  TEMPD-GEOM3,
!     22   CTRIAAX  --  AX.TR.CR       7012-70  CTRIA-GEOM2
!     23   CTRAPAX  --  AX.TRA.CR      7042-74  CTRAP-GEOM2
!
   IMPLICIT NONE
   REAL A1 , A2 , A3 , A4 , Angle , Coef , Consts(5) , Ni , Nisq , Nphi , Nphi1 , Pi , Raddeg , Rz(1) , Sum , T1 , T2 , Twopi
   INTEGER Axic , Axic1(3) , Axtrl(7) , Buff , Cconex(3) , Cdtype(50) , Clorwd , Compon , Csid , Csset , Ctrapa(3) , Ctriaa(3) ,    &
         & Dum1(26) , Dum37(37) , Dumdum(8) , Dummy(96) , Eor , File(6) , Flag , Force(3) , Forcex(3) , Geom(4) , Grav(3) , Grid(3) &
         & , I1 , I2 , Iamt , Iat , Ibegin , Ibit , Ibitr , Ibuff1 , Ibuff2 , Ibuff3 , Ibufsz , Iconso , Icont , Icore , Icore1 ,   &
         & Iend , Ierrtn , Ifile , Ihead(96) , Iheadb(96) , Iheadr , Iname(12) , Inprwd , Ion , Iphi , Ipiez , Ipt , Iretrn ,       &
         & Iscrat , Istart , It , K3or6 , Last , Load(3) , Momax(3) , Moment(3) , Mpc(3) , Mpcadd(3) , Mpcax(3) , Mpcon , N ,       &
         & N3or5 , Nadd , Nbpc , Nbpw , Ncard , Ncards , Neg111(3) , Nfile , Nlines , Nmove , Nnn , Noaxic , Noeor , Noflag , Non
   LOGICAL Nogo , Recoff
   INTEGER Nopont , Noreg , Nosect , Nout , Nplus1 , Nwords , Omit(3) , Omitax(3) , One , Op , Openfl(6) , Outbuf , Outrwd ,        &
         & Pload(3) , Pointx(3) , Presax(3) , Rec(3) , Rec1(3) , Recid(3) , Recid1(3) , Recidx(3) , Rforce(3) , Ringax(3) , Ringid ,&
         & Scrtch , Sectax(3) , Seqgp(3) , Setid , Sorc , Spc(3) , Spcadd(3) , Spcax(3) , Supax(3) , Suport(3) , T65535(3) , Temp(3)&
         & , Tempax(3) , Tempd(3) , Trail(7) , Two(32) , Veor , Z(8) , Zero , Zpt
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /condas/ Consts
   COMMON /ifp3cm/ File , Iname , Cdtype , Axic1 , Cconex , Forcex , Force , Grav , Load , Momax , Moment , Mpcadd , Mpcax ,        &
                 & Omitax , Pointx , Presax , Ringax , Sectax , Seqgp , Spcax , Supax , Tempax , Tempd , Pload , Mpc , Spc , Grid , &
                 & Suport , Neg111 , T65535 , Temp , Omit , Spcadd , One , Zero , Iheadb , Ctriaa , Ctrapa , Iconso , Rforce
   COMMON /ifp3lv/ Recid , Recid1 , Recidx , Iend , Rec , Rec1 , Trail , It , Axtrl , Openfl , N , A1 , Csid , Ni , Nisq , A2 ,     &
                 & Ibuff1 , Ibuff2 , Ibuff3 , A3 , Buff , Nogo , Op , A4 , Iheadr , Ibitr , Ifile , Noreg , Last , Ierrtn , Icont , &
                 & Noaxic , Ringid , Outbuf , Veor , Istart , Iretrn , Flag , Iamt , Sum , Ibit , Setid , Sorc , Ibegin , Mpcon ,   &
                 & Nwords , Nnn , Angle , K3or6 , Nphi1 , Zpt , Nmove , Csset , Nopont , Non , Iphi , Recoff , Nphi , N3or5 , Ion , &
                 & Nplus1 , Nosect , Coef , Ipt , Compon , Icore , Iscrat , Icore1 , Ncards , I1 , Iat , I2 , T1 , T2 , Nfile ,     &
                 & Nadd , Ncard
   COMMON /output/ Dummy , Ihead
   COMMON /system/ Ibufsz , Nout , Noflag , Dumdum , Nlines , Dum1 , Nbpc , Nbpw , Dum37 , Ipiez
   COMMON /two   / Two
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Rz
   INTEGER andf , complf , korsz , lshift , orf , rshift
   INTEGER b , chr , i , i5 , i6 , iconb , icrq , ifiat , ifist , ii , ij , ik , imsg , in , in1 , inum(11) , ix , j , k , msg1(2) ,&
         & msg2(2) , mus , nh1 , nh2 , num(11) , word
   REAL difphi
   LOGICAL piez , secd
   EXTERNAL andf , complf , lshift , orf , rshift
   !>>>>EQUIVALENCE (Consts(1),Pi) , (Consts(2),Twopi) , (Consts(4),Raddeg) , (Z(1),Rz(1)) , (Geom(1),File(1)) , (Scrtch,File(5)) ,      &
!>>>>    & (Axic,File(6)) , (num(11),b) , (Noeor,Inprwd,Zero) , (Eor,Clorwd,Outrwd,One)
   DATA inum/1H0 , 1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 1H /
   DATA ifiat/4HFIAT/ , ifist/4HFIST/ , i5 , i6/5 , 6/
   DATA msg1/4HIFP3 , 4HBEGN/ , msg2/4HIFP3 , 4HEND /
!
   CALL conmsg(msg1,2,0)
!
!     RIGHT-JUSTIFY INUM AND CALL IT NUM
!
   DO i = 1 , 11
      num(i) = rshift(inum(i),Nbpw-Nbpc)
   ENDDO
!
!     INITIAL CHECK TO MAKE SURE TRAILER BITS ARE ALL OFF FOR GEOM1,
!     GEOM2, GEOM3, GEOM4.
!
   DO i = 1 , 96
      Ihead(i) = Iheadb(i)
   ENDDO
!
   IF ( Noflag/=0 ) THEN
      Nogo = .TRUE.
   ELSE
      Nogo = .FALSE.
   ENDIF
!
   Openfl(1) = 0
   Openfl(2) = 0
   Openfl(3) = 0
   Openfl(4) = 0
   Openfl(5) = 0
   Openfl(6) = 0
   DO i = 1 , 4
      Trail(1) = Geom(i)
      CALL rdtrl(Trail(1))
      IF ( Trail(1)<=0 ) THEN
         CALL page2(3)
         imsg = 1061
         WRITE (Nout,99011) Sfm , imsg
         WRITE (Nout,99012) Geom(i) , Iname(2*i-1) , Iname(2*i) , ifiat
         Nogo = .TRUE.
      ELSE
!
         DO j = 2 , 7
            IF ( Trail(j)/=0 ) THEN
               CALL page2(3)
               imsg = 1062
               WRITE (Nout,99011) Sfm , imsg
               WRITE (Nout,99001) Geom(i) , Iname(2*i-1) , Iname(2*i)
99001          FORMAT (5X,'FILE NUMBER',I4,3H ( ,2A4,') HAS TRAILER BIT ON.  ','FILE SHOULD BE CLEAN AT ENTRY TO IFP3.')
               Nogo = .TRUE.
               EXIT
            ENDIF
         ENDDO
      ENDIF
   ENDDO
!
!     PROCEED TO SETUP CORE AND OPEN AXIC FILE
!     ICORE1 WILL ALWAYS EQUAL THE GROSS OPEN CORE TO IFP3 AT START
!
   Icore1 = korsz(Z)
   Ibuff1 = Icore1 - Ibufsz - 2
   Ibuff2 = Ibuff1 - Ibufsz
   Ibuff3 = Ibuff2 - Ibufsz
   Icore = Ibuff3 - 1
   icrq = 100 - Icore
   IF ( Icore<100 ) GOTO 7300
!
!     OPEN  AXIC FILE
!
   CALL preloc(*7400,Z(Ibuff1),Axic)
   Openfl(6) = 1
   Axtrl(1) = Axic
   CALL rdtrl(Axtrl(1))
!
!     READ AXIC CARD
!
   CALL locate(*100,Z(Ibuff1),Axic1(1),Flag)
   CALL read(*8700,*100,Axic,Z(1),2,Eor,Flag)
   N = Z(1)
   Csid = Z(2)
   Nnn = N
   Ncard = 1
   ASSIGN 200 TO Ierrtn
   GOTO 7800
!
!     MISSING REQUIRED AXIC CARD
!
 100  ASSIGN 200 TO Ierrtn
   Nnn = 0
   Ncard = 1
!
!     MISSING REQUIRED CARD
!
   CALL page2(3)
   imsg = 362
   WRITE (Nout,99013) Ufm , imsg
   WRITE (Nout,99002) Cdtype(2*Ncard-1) , Cdtype(2*Ncard)
99002 FORMAT (5X,'MINIMUM PROBLEM REQUIRES ',2A4,' CARD.  NONE FOUND.')
   Nogo = .TRUE.
   GOTO Ierrtn
 200  N = Nnn
   Nplus1 = N + 1
!
!
!     GEOM2  PROCESSING
!     =================
!
!     OPEN GEOM2
!
   Ifile = Geom(2)
   i = 2
   Op = Outrwd
   Buff = Ibuff2
   ASSIGN 300 TO Iretrn
   GOTO 7500
!
!     CCONEAX CARDS
!
 300  Rec(1) = Cconex(1)
   Rec(2) = Cconex(2)
   Rec(3) = Cconex(3)
   Ncard = 2
!
!     IF THERE IS NO CCONEAX CARD, THEN GO TO 1750 AND LOOK FOR
!     CTRAPAX OR CTRIAAX CARDS
!
   iconb = 0
   Iconso = 0
   CALL locate(*6500,Z(Ibuff1),Rec(1),Flag)
!
!     INPUT IS IN 4-WORD CARDS
!     OUTPUT IS N+1 4-WORD CARDS FOR EACH CARD INPUT
!
!     RECORD HEADER FOR CCONES
!
   ASSIGN 400 TO Iheadr
   GOTO 8100
!
 400  CALL read(*8700,*700,Axic,Z(1),4,Noeor,Iamt)
!
!     CHECK RING ID-S FOR SIZE
!
   Nnn = Z(3)
   ASSIGN 500 TO Ierrtn
   GOTO 7900
 500  Nnn = Z(4)
   ASSIGN 600 TO Ierrtn
   GOTO 7900
!
!     CHECK CCONEAX ID FOR 1-9999 ALLOWABLE RANGE
!
 600  IF ( Z(1)<=0 .OR. Z(1)>=10000 ) THEN
      CALL page2(3)
      imsg = 361
      WRITE (Nout,99013) Ufm , imsg
      WRITE (Nout,99003) Z(1)
99003 FORMAT (5X,'CCONEAX ID =',I10,'.  OUT OF 1 TO 9999 PERMISSIBLE ','RANGE')
      Nogo = .TRUE.
   ENDIF
!
   Z(1) = Z(1)*1000
   DO i = 1 , Nplus1
      Z(1) = Z(1) + 1
      Z(3) = Z(3) + 1000000
      Z(4) = Z(4) + 1000000
      IF ( .NOT.(Nogo) ) CALL write(Geom(2),Z(1),4,Noeor)
   ENDDO
   GOTO 400
!
!     OUT OF CCONEAX CARDS
!
 700  IF ( Iamt/=0 ) THEN
!
!     GO TO 356 FOR RECORD ERROR
!
      ASSIGN 900 TO Ierrtn
      GOTO 8200
   ELSE
!
!     WRITE EOR AND PUT BITS IN TRAILER
!
      ASSIGN 800 TO Iretrn
      Iconso = 1
      GOTO 7000
   ENDIF
 800  iconb = 1
   GOTO 6500
!
!     CLOSE GEOM2
!
 900  i = 2
   ASSIGN 1000 TO Iretrn
   GOTO 7700
!
!     GEOM3 PROCESSING
!     ================
!
!     OPEN GEOM3
!
 1000 Ifile = Geom(3)
   i = 3
   Op = Outrwd
   Buff = Ibuff2
   ASSIGN 1100 TO Iretrn
   GOTO 7500
!
!     FORCE, FORCEAX, MOMNT, AND MOMNTAX CARDS
!
 1100 Recid(1) = Force(1)
   Recid(2) = Force(2)
   Recid(3) = Force(3)
   Recidx(1) = Forcex(1)
   Recidx(2) = Forcex(2)
   Recidx(3) = Forcex(3)
   Ncard = 3
   ASSIGN 3000 TO Icont
!
!     SET NOREG = 0 OR 1, DEPENDING ON PRESSENCE OF RECID
!     SET NOAXIC= 0 OR 1, DEPENDING ON PRESSENCE OF RECIDX
!
 1200 Ibit = Recidx(2)
   ASSIGN 1300 TO Ibitr
   GOTO 8000
 1300 Noaxic = Non
   Ibit = Recid(2)
   ASSIGN 1400 TO Ibitr
   GOTO 8000
 1400 Noreg = Non
!
   Rec(1) = Recid(1)
   Rec(2) = Recid(2)
   Rec(3) = Recid(3)
!
   IF ( Noaxic/=0 ) THEN
!
!     AT 410 READ IN ALL FORCEAX OR MOMNTAX CARDS AND PUT OUT ON GEOM(3)
!     IF NOREG=0,AND ON SCRTCH IF NOREG NON-ZERO.FIRST WRITE 3-WORD-
!     REC ID ON GEOM3.
!
      ASSIGN 1500 TO Iheadr
      GOTO 8100
   ELSE
      IF ( Noreg==0 ) GOTO 2900
!
!     TRANSFER FORCE OR MOMENT RECORD DIRECTLY.
!     THERE ARE NO FORCEAX OR MOMAX CARDS RESPECTIVELY.
!
      ASSIGN 2900 TO Iretrn
      GOTO 6900
   ENDIF
!
!     OPEN SCRATCH IF NEEDED
!
 1500 IF ( Noreg/=0 ) THEN
      i = 5
      Op = Outrwd
      Buff = Ibuff3
      ASSIGN 1600 TO Iretrn
      GOTO 7500
   ENDIF
 1600 CALL locate(*8300,Z(Ibuff1),Recidx(1),Flag)
 1700 CALL read(*8700,*2100,Axic,Z(1),8,Noeor,Iamt)
!
!     CHECK RING ID
!
   ASSIGN 1800 TO Ierrtn
   Nnn = Z(2)
   GOTO 7900
!
!     CHECK HARMONIC NUMBER AND FOR A SEQUENCE OF HARMONICS
!
 1800 IF ( Z(4)==0 ) THEN
      nh1 = Z(3)
      nh2 = Z(3)
   ELSE
      ii = 1
      nh1 = 0
      nh2 = 0
      secd = .TRUE.
      word = 4
      DO ij = 1 , 2
         DO ix = 1 , 4
            chr = rshift(lshift(Z(word),Nbpc*iabs(ix-4)),Nbpw-Nbpc)
            IF ( chr/=b ) THEN
               DO i = 1 , 10
                  k = i - 1
                  IF ( num(i)==chr ) GOTO 1810
               ENDDO
               secd = .FALSE.
               ii = 1
            ENDIF
            CYCLE
 1810       IF ( secd ) THEN
               nh2 = nh2 + ii*k
               ii = ii*10
            ELSE
               nh1 = nh1 + ii*k
               ii = ii*10
            ENDIF
         ENDDO
         word = word - 1
      ENDDO
      IF ( nh1>nh2 ) THEN
         word = nh1
         nh1 = nh2
         nh2 = word
      ENDIF
      Nnn = nh1
      ASSIGN 1900 TO Ierrtn
      GOTO 7800
   ENDIF
 1900 Nnn = nh2
   ASSIGN 2000 TO Ierrtn
   GOTO 7800
 2000 Z(4) = Z(5)
   Z(5) = Z(6)
   Z(6) = Z(7)
   Z(7) = Z(8)
   nh1 = nh1 + 1
   nh2 = nh2 + 1
   Sum = Z(2)
   mus = Z(2)
   DO i = nh1 , nh2
      Z(2) = mus + i*1000000
      Z(3) = 0
!
!     OUTPUT TO GEOM(3) IF NOREG = 0
!     OUTPUT TO SCRTCH  IF NOREG = NON-ZERO
!
      IF ( Nogo ) EXIT
      IF ( Noreg/=0 ) THEN
         Nfile = Scrtch
      ELSE
         Nfile = Geom(3)
      ENDIF
      CALL write(Nfile,Z(1),7,Noeor)
   ENDDO
   GOTO 1700
!
!     OUT OF CARDS
!
 2100 IF ( Iamt/=0 ) THEN
!
!     CHECK FOR RECORD INCONSISTANCY ERROR.
!
      Rec(1) = Recidx(1)
      Rec(2) = Recidx(2)
      Rec(3) = Recidx(3)
      ASSIGN 2200 TO Ierrtn
      GOTO 8200
   ENDIF
!
 2200 IF ( Noreg==0 ) GOTO 2700
!
!     CLOSE THE SCRTCH FILE AND THEN MERGE SCRTCH WITH AXIC
!     ON TO GEOM3
!
   i = 5
   ASSIGN 2300 TO Iretrn
   GOTO 7700
!
!     OPEN SCRTCH FILE FOR INPUT AND LOCATE FORCE OR MOMENT CARDS ON
!     AXIC FILE.
!
 2300 ASSIGN 2400 TO Iretrn
   Op = Inprwd
   GOTO 7500
 2400 CALL locate(*8500,Z(Ibuff1),Recid(1),Flag)
   IF ( Nogo ) GOTO 2900
!
   CALL read(*8700,*2800,Axic,Z(1),7,Noeor,Iamt)
   CALL read(*8800,*8800,Scrtch,Z(8),7,Noeor,Iamt)
   DO
!
      IF ( Z(1)<=Z(8) ) THEN
!
         Nfile = Axic
         Outbuf = 1
      ELSE
!
         Nfile = Scrtch
         Outbuf = 8
      ENDIF
!
      IF ( Nogo ) GOTO 2900
      CALL write(Geom(3),Z(Outbuf),7,Noeor)
      CALL read(*8900,*2500,Nfile,Z(Outbuf),7,Noeor,Iamt)
   ENDDO
!
!     OK ALL WORDS PROCESSED FOR FILE-NFILE
!
 2500 IF ( Nfile==Axic ) THEN
      Nfile = Scrtch
      Outbuf = 8
   ELSE
      Nfile = Axic
      Outbuf = 1
   ENDIF
   DO WHILE ( .NOT.(Nogo) )
      CALL write(Geom(3),Z(Outbuf),7,Noeor)
      CALL read(*8900,*2600,Nfile,Z(Outbuf),7,Noeor,Iamt)
   ENDDO
   GOTO 2900
!
!     CLOSE SCRTCH, WRITE EOR, AND PUT BITS IN TRAILER.
!
 2600 i = 5
   ASSIGN 2700 TO Iretrn
   GOTO 7700
 2700 ASSIGN 2900 TO Iretrn
   GOTO 7000
!
!     RECORD LENGTH ERROR
!
 2800 Rec(1) = Recid(1)
   Rec(2) = Recid(2)
   Rec(3) = Recid(3)
   ASSIGN 2900 TO Ierrtn
   GOTO 8200
!
 2900 GOTO Icont
!
!     GRAV CARD
!
 3000 Rec(1) = Grav(1)
   Rec(2) = Grav(2)
   Rec(3) = Grav(3)
   ASSIGN 3100 TO Iretrn
   GOTO 6900
!
!     LOAD CARD
!
 3100 Rec(1) = Load(1)
   Rec(2) = Load(2)
   Rec(3) = Load(3)
   ASSIGN 3200 TO Iretrn
   GOTO 6900
!
!     MOMENT AND MOMAX CARDS
!
 3200 Recid(1) = Moment(1)
   Recid(2) = Moment(2)
   Recid(3) = Moment(3)
   Recidx(1) = Momax(1)
   Recidx(2) = Momax(2)
   Recidx(3) = Momax(3)
   Ncard = 7
   ASSIGN 3300 TO Icont
   GOTO 1200
!
!     PRESAX CARD
!
 3300 CALL locate(*3800,Z(Ibuff1),Presax(1),Flag)
!
!     RECORD HEADER FOR PRESAX CARDS IS FORMED HERE
!
   Rec(1) = Presax(1)
   Rec(2) = Presax(2)
   Rec(3) = Presax(3)
   Ncard = 13
   ASSIGN 3400 TO Iheadr
   GOTO 8100
!
 3400 CALL read(*8700,*3700,Axic,Z(1),6,Noeor,Iamt)
!
!     CREATE N+1 CARDS OF SAME LENGTH AS INPUT CARD.
!
!     CHECK RING ID-S IN FIELDS 3 AND 4 FOR PROPER SIZE.
!
!     CHECK FOR PIEZOELECTRIC
!
   piez = .FALSE.
   IF ( Ipiez==1 .AND. Z(3)<0 ) piez = .TRUE.
   IF ( piez ) Z(3) = -Z(3)
   Nnn = Z(3)
   ASSIGN 3500 TO Ierrtn
   GOTO 7900
 3500 Nnn = Z(4)
   ASSIGN 3600 TO Ierrtn
   GOTO 7900
!
 3600 difphi = abs(Rz(i6)-Rz(i5))
   DO i = 1 , Nplus1
      Z(7) = i - 1
      Z(3) = Z(3) + 1000000
      IF ( piez ) Z(3) = -Z(3)
      Z(4) = Z(4) + 1000000
      IF ( .NOT.(Nogo) ) THEN
         IF ( difphi/=0.0 ) THEN
            IF ( i<=1 .OR. abs(difphi-360.)>=1.E-6 ) THEN
               CALL write(Geom(3),Z(1),7,Noeor)
               IF ( piez ) Z(3) = -Z(3)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   GOTO 3400
!
!     OUT OF PRESAX CARDS
!
 3700 IF ( Iamt/=0 ) THEN
!
!     CHECK FOR RECORD INCONSISTANCY ERROR.
!
      ASSIGN 3800 TO Ierrtn
      Rec(1) = Presax(1)
      Rec(2) = Presax(2)
      Rec(3) = Presax(3)
      GOTO 8200
   ELSE
!
!     WRITE EOR AND PUT BITS IN TRAILER
!
      ASSIGN 3800 TO Iretrn
      GOTO 7000
   ENDIF
!
!     RFORCE CARD
!
 3800 CALL locate(*4100,Z(Ibuff1),Rforce(1),Flag)
   Rec(1) = Rforce(1)
   Rec(2) = Rforce(2)
   Rec(3) = Rforce(3)
   Ncard = 24
   ASSIGN 3900 TO Iheadr
   GOTO 8100
 3900 DO
!
!     PROCESS RFORCE DATA
!
      CALL read(*8700,*4000,Axic,Z(1),7,Noeor,Iamt)
      IF ( Z(2)==0 .AND. Z(3)==0 .AND. Z(5)==0 .AND. Z(6)==0 ) THEN
         Z(2) = 0
         Z(3) = 0
         Z(5) = 0
         Z(6) = Z(7)
         Z(7) = 0
         CALL write(Geom(3),Z(1),7,Noeor)
      ELSE
         WRITE (Nout,99004) Ufm , Z(1)
99004    FORMAT (A23,' 336, RFORCE DATA IN SET NO.',I8,' CONTAINS ILLEGAL DIRECTION FOR AXISYMMETRIC PROBLEM')
         Nogo = .TRUE.
      ENDIF
   ENDDO
!
!     END OF RFORCE CARDS
!
 4000 IF ( Iamt/=0 ) THEN
!
!     RECORD INCONSISTENCY ERROR
!
      ASSIGN 4100 TO Ierrtn
      Rec(1) = Rforce(1)
      Rec(2) = Rforce(2)
      Rec(3) = Rforce(3)
      GOTO 8200
   ELSE
!
!     WRITE EOR AND BITS IN TRAILER
!
      ASSIGN 4100 TO Iretrn
      GOTO 7000
   ENDIF
!
!     TEMPD CARD
!
 4100 Rec(1) = Tempd(1)
   Rec(2) = Tempd(2)
   Rec(3) = Tempd(3)
   ASSIGN 4500 TO Iretrn
   IF ( Nogo ) GOTO 4500
   CALL locate(*4500,Z(Ibuff1),Rec(1),Flag)
   CALL write(Ifile,Rec(1),3,Noeor)
   Veor = 0
 4200 CALL read(*8700,*4400,Axic,Z(1),Icore,Noeor,Iamt)
   Iamt = Icore
 4300 DO i = 1 , Iamt , 2
      Z(i) = Z(i) + 100000000
   ENDDO
   CALL write(Ifile,Z(1),Iamt,0)
   DO i = 1 , Iamt , 2
      Z(i) = Z(i) + 100000000
   ENDDO
   CALL write(Ifile,Z(1),Iamt,Veor)
   IF ( Veor==0 ) GOTO 4200
   GOTO 7100
 4400 Veor = 1
   GOTO 4300
!
!     TEMPAX CARD
!
 4500 CALL locate(*6000,Z(Ibuff1),Tempax(1),Flag)
!
!     RECORD HEADER ON GEOM3 FOR TEMP CARDS
!
   Rec(1) = Temp(1)
   Rec(2) = Temp(2)
   Rec(3) = Temp(3)
   Ncard = 20
   ASSIGN 4600 TO Iheadr
   GOTO 8100
!
!     AT 604(?) SET UP SCRATCH FILE.
!
 4600 i = 5
   Buff = Ibuff3
   Op = Outrwd
   ASSIGN 4700 TO Iretrn
   GOTO 7500
!
!     PICK UP FIRST TEMPAX CARD = 4 WORDS.
!
 4700 Last = 0
   CALL read(*8700,*5900,Axic,Z(1),4,Noeor,Iamt)
 4800 k = 0
   Setid = Z(1)
   Ringid = Z(2)
!
!     CHECK RING ID FOR PROPER RANGE OF VALUE
!
   Nnn = Ringid
   ASSIGN 4900 TO Ierrtn
   GOTO 7900
!
 4900 Iat = 3
   DO
      k = k + 1
      Iat = Iat + 2
      icrq = Iat + 3 - Icore
      IF ( Icore<Iat+3 ) GOTO 7300
!
!     ALL TEMPAX CARDS HAVING SAME SET AND RING ID MUST BE ABLE TO
!     HAVE 2 WORDS EACH FIT IN CORE.
!
      Z(Iat) = Z(3)
      Z(Iat+1) = Z(4)
!
      CALL read(*8700,*5500,Axic,Z(1),4,Noeor,Iamt)
!
!     DOES THIS CARD HAVE SAME SET AND RING ID AS LAST IN CURRENT SERIES
!
      IF ( Z(1)/=Setid ) EXIT
      IF ( Z(2)/=Ringid ) EXIT
   ENDDO
!
!     WE HAVE A  K X 2  ARRAY OF  PHI-S  AND T-S.
!
!     CONVERT ALL  PHIS SUCH THAT (0.LE. PHI .LT.TWOPI)
!
 5000 Iend = Iat + 1
   Ibegin = 5
!
   DO i = Ibegin , Iend , 2
      Angle = Rz(i)
      IF ( Angle<0 ) THEN
         DO WHILE ( Angle<0 )
            Angle = Angle + 360.0
         ENDDO
      ELSEIF ( Angle/=0 ) THEN
!
         DO WHILE ( Angle>=360.0 )
            Angle = Angle - 360.0
         ENDDO
      ENDIF
!
      Rz(i) = Angle*Raddeg
   ENDDO
!
!     SIMPLE SORT FOR THE K X 2  MATRIX.
!     SORT IS PERFORMED ON COLUMN 1 ONLY
!
   IF ( k==1 ) GOTO 5300
   Istart = Ibegin + 2
   DO i = Istart , Iend , 2
      Iat = i - 2
      IF ( Rz(i)<Rz(Iat) ) THEN
         DO
!
!     ROW NOT HIGH ENOUGH.  MOVE IT UP.
!
            Iat = Iat - 2
            IF ( Iat<=Ibegin ) THEN
               Iat = Ibegin
               EXIT
            ELSEIF ( Rz(i)>=Rz(Iat) ) THEN
               Iat = Iat + 2
               EXIT
            ENDIF
         ENDDO
!
!     THE ELEMENTS (I) AND (I+1) WILL BE MOVED UP TO POSITIONS (IAT) AND
!     (IAT+1) AND ELEMENTS (IAT) THRU (I-1) WILL BE  MOVED DOWN 1 ROW.
!
!     FIRST SAVE THE ROW BEING MOVED UP
!
         Rz(Iend+1) = Rz(i)
         Rz(Iend+2) = Rz(i+1)
         Nmove = i - Iat
         Iat = i + 2
         DO j = 1 , Nmove
            Iat = Iat - 1
            Rz(Iat) = Rz(Iat-2)
         ENDDO
!
!     REPLACE SAVE ROW IN NEW SLOT
!
         Rz(Iat-2) = Rz(Iend+1)
         Rz(Iat-1) = Rz(Iend+2)
      ENDIF
!
   ENDDO
!
!     CHECK FOR ANY DUPLICATE ANGLES AND REMOVE THEM...
!
   Ibegin = Ibegin + 2
 5100 DO i = Ibegin , Iend , 2
      IF ( Z(i)==Z(i-2) ) GOTO 5200
   ENDDO
   GOTO 5300
!
!     DUPLICATE, SHRINK LIST UP OVER IT.
!
 5200 Iend = Iend - 2
   k = k - 1
   DO j = i , Iend , 2
      Z(j) = Z(j+2)
      Z(j+1) = Z(j+3)
   ENDDO
   Ibegin = i
   IF ( Ibegin<Iend ) GOTO 5100
!
!     SET UP K + 1  CARD
!
 5300 Rz(Iend+1) = Rz(i5) + Twopi
   Rz(Iend+2) = Rz(i6)
!
!     THERE ARE K CARDS NOW WITH SETID, AND RINGID, NOT INCLUDING THE
!     K + 1ST CARD
!
!     N+1 TEMP CARDS FOR S SET (PUT ON GEOM3)
!     N+1 TEMP CARDS FOR C SET (PUT ON SCRTCH FOR NOW)
!
!     NOTE FMMS-52  (10/04/67) PAGE -9- FOR FOLLOWING...
!
   Csset = 1
   Setid = Setid + 100000000
!
!     CSSET = 0 FOR C-SET  AND NON-ZERO FOR S-SET.
!
   Ibegin = k + k + 7
   icrq = Ibegin + 2 - Icore
   IF ( (Ibegin+2)>Icore ) GOTO 7300
!
 5400 Nadd = 0
   Z(Ibegin) = Setid
   DO i = 1 , Nplus1
      Nadd = Nadd + 1000000
!
!     NI IS REAL
!
      Ni = i - 1
      Nisq = (i-1)**2
      Z(Ibegin+1) = Ringid + Nadd
      Iphi = 3
      It = 4
      Sum = 0.0E0
      IF ( Ni/=0 ) THEN
!
!     NON-ZERO NI
!
         IF ( k/=1 ) THEN
            DO ik = 1 , k
               Iphi = Iphi + 2
               It = It + 2
               Nphi = Ni*Rz(Iphi)
               Nphi1 = Ni*Rz(Iphi+2)
!
               IF ( Csset/=0 ) THEN
!
!     S-SET
!
                  A1 = -cos(Nphi1)
                  A2 = cos(Nphi)
                  A3 = sin(Nphi1)
                  A4 = -sin(Nphi)
               ELSE
!
!     C-SET
!
                  A1 = sin(Nphi1)
                  A2 = -sin(Nphi)
                  A3 = cos(Nphi1)
                  A4 = -cos(Nphi)
               ENDIF
!
!
               Sum = Sum + (((Rz(It)*Rz(Iphi+2)-Rz(It+2)*Rz(Iphi))*(A1+A2)/Ni)+((Rz(It+2)-Rz(It))*(A3+A4+Nphi1*A1+Nphi*A2)/Nisq))   &
                   & /(Rz(Iphi+2)-Rz(Iphi))
            ENDDO
         ENDIF
!
         Rz(Ibegin+2) = Sum/Pi
      ELSE
         IF ( Csset==0 ) THEN
            DO ik = 1 , k
               Iphi = Iphi + 2
               It = It + 2
               Sum = Sum + (Rz(It)+Rz(It+2))*(Rz(Iphi+2)-Rz(Iphi))
            ENDDO
         ENDIF
         Rz(Ibegin+2) = 0.25*Sum/Pi
      ENDIF
!
      IF ( Nogo ) EXIT
      IF ( Csset/=0 ) THEN
         Nfile = Geom(3)
      ELSE
         Nfile = Scrtch
      ENDIF
      CALL write(Nfile,Z(Ibegin),3,Noeor)
   ENDDO
   IF ( Csset/=0 ) THEN
      Csset = 0
      Setid = Setid + 100000000
      GOTO 5400
   ELSE
!
!     THIS SERIES OF TEMPAX CARDS COMPLETE GO FOR MORE IF LAST = 0
!
      IF ( Last==0 ) GOTO 4800
!
!     ALL TEMPAX CARDS COMPLETE. CLOSE SCRATCH, OPEN SCRATCH
!     AND COPY SCRATCH TO GEOM3.
!
      IF ( Nogo ) GOTO 6000
      CALL write(Scrtch,Z(1),0,Eor)
      CALL close(Scrtch,Clorwd)
      CALL open(*9000,Scrtch,Z(Ibuff3),Inprwd)
!
      Veor = 0
      GOTO 5600
   ENDIF
 5500 Last = 1
   GOTO 5000
 5600 CALL read(*8800,*5800,Scrtch,Z(1),Icore,Noeor,Iamt)
   Iamt = Icore
 5700 CALL write(Geom(3),Z(1),Iamt,Veor)
   IF ( Veor==0 ) GOTO 5600
!
!     ALL  TEMPAX  CARDS  PROCESSED.
!
   CALL close(Scrtch,Clorwd)
!
!     PUT BITS IN TRAILER FOR TEMP CARDS WRITTEN
!
   Rec(1) = Temp(1)
   Rec(2) = Temp(2)
   Rec(3) = Temp(3)
   ASSIGN 6000 TO Iretrn
   GOTO 7100
 5800 Veor = 1
   GOTO 5700
!
!     RECORD LENGTH ERROR
!
 5900 Rec(1) = Tempax(1)
   Rec(2) = Tempax(2)
   Rec(3) = Tempax(3)
   ASSIGN 6000 TO Ierrtn
   GOTO 8200
!
!     CLOSE GEOM3
!
 6000 i = 3
   ASSIGN 6800 TO Iretrn
   GOTO 7700
!
!     CTRIAAX CARD
!
 6100 Rec(1) = Ctriaa(1)
   Rec(2) = Ctriaa(2)
   Rec(3) = Ctriaa(3)
   Ncard = 43
   CALL locate(*6400,Z(Ibuff1),Rec(1),Flag)
!
!     RECORD HEADER FOR CTRIAAX
!
   ASSIGN 6200 TO Iheadr
   iconb = 2
   Iconso = 1
   GOTO 8100
 6200 DO
      CALL read(*8700,*6300,Axic,Z(1),6,Noeor,Iamt)
      Z(1) = Z(1)*1000
      DO i = 1 , Nplus1
         Z(1) = Z(1) + 1
         Z(3) = Z(3) + 1000000
         Z(4) = Z(4) + 1000000
         Z(5) = Z(5) + 1000000
         IF ( .NOT.(Nogo) ) CALL write(Geom(2),Z(1),6,Noeor)
      ENDDO
   ENDDO
!
!     OUT OF CTRIAAX CARD
!
 6300 IF ( Iamt/=0 ) THEN
      ASSIGN 900 TO Ierrtn
      GOTO 8200
   ELSE
!
!     PUT BITS IN TRILER
!
      ASSIGN 900 TO Iretrn
      GOTO 7000
   ENDIF
 6400 IF ( Iconso==1 ) THEN
      ASSIGN 900 TO Iretrn
      GOTO 7000
   ELSE
      ASSIGN 900 TO Ierrtn
!
!     MISSING REQUIRED CCONEAX OR CTRIAAX OR CTRAPAX CARD
!
      CALL page2(3)
      imsg = 362
      WRITE (Nout,99013) Ufm , imsg
      WRITE (Nout,99005) Cdtype(3) , Cdtype(4) , Cdtype(43) , Cdtype(44) , Cdtype(45) , Cdtype(46)
99005 FORMAT (5X,'MINIMUM PROBLEM REQUIRES ',2A4,2H, ,2A4,4H OR ,2A4,' CARD.  NONE FOUND')
      Nogo = .TRUE.
      GOTO Ierrtn
   ENDIF
!
!     CTRAPAX CARD
!     ============
!
 6500 Rec(1) = Ctrapa(1)
   Rec(2) = Ctrapa(2)
   Rec(3) = Ctrapa(3)
   CALL locate(*6100,Z(Ibuff1),Rec(1),Flag)
   iconb = 1
!
!     RECORD HEADER FOR CTRAPAX
!
   ASSIGN 6600 TO Iheadr
   Iconso = 1
   GOTO 8100
 6600 DO
      CALL read(*8700,*6700,Axic,Z(1),7,Noeor,Iamt)
      Z(1) = Z(1)*1000
      DO i = 1 , Nplus1
         Z(1) = Z(1) + 1
         Z(3) = Z(3) + 1000000
         Z(4) = Z(4) + 1000000
         Z(5) = Z(5) + 1000000
         Z(6) = Z(6) + 1000000
         IF ( .NOT.(Nogo) ) CALL write(Geom(2),Z(1),7,Noeor)
      ENDDO
   ENDDO
!
!     OUT OF CTRAPAX CARD
!
 6700 IF ( Iamt/=0 ) THEN
      ASSIGN 900 TO Ierrtn
      GOTO 8200
   ELSE
!
!     PUT BITS IN TRILER
!
      ASSIGN 900 TO Iretrn
      IF ( Nogo ) GOTO 7200
      CALL write(Ifile,Z(1),Iamt,Eor)
      I1 = (Rec(2)-1)/16 + 2
      I2 = Rec(2) - (I1-2)*16 + 16
      Trail(I1) = orf(Trail(I1),Two(I2))
      GOTO 6100
   ENDIF
!
!     GEOM4 AND GEOM1 PROCESSING IS PERFORMED IN IFP3B ROUTINE
!                                                =====
!
 6800 CALL ifp3b
   GOTO 8600
!
!     UTILITY SECTION FOR IFP3
!     AXIS-SYMETRIC-CONICAL-SHELL DATA GENERATOR.
!     ==========================================
!
!     COMMON CODE FOR TRANSFER OF RECORD FROM AXIC FILE TO SOME
!     OTHER FILE
!
 6900 CALL locate(*7200,Z(Ibuff1),Rec(1),Flag)
   IF ( Nogo ) GOTO 7200
   CALL write(Ifile,Rec(1),3,Noeor)
   DO
      CALL read(*8700,*7000,Axic,Z(1),Icore,Noeor,Iamt)
      Iamt = Icore
      CALL write(Ifile,Z(1),Iamt,Noeor)
   ENDDO
 7000 IF ( Nogo ) GOTO 7200
   IF ( Ifile==Geom(3) ) THEN
      CALL write(Ifile,Z(1),Iamt,Eor)
   ELSE
      IF ( Ifile==Geom(2) .AND. iconb==1 ) GOTO 7200
      CALL write(Ifile,Z(1),Iamt,Eor)
   ENDIF
!
!     PUT BITS IN TRAILER
!
 7100 I1 = (Rec(2)-1)/16 + 2
   I2 = Rec(2) - (I1-2)*16 + 16
   Trail(I1) = orf(Trail(I1),Two(I2))
!
 7200 GOTO Iretrn
!
!     OUT OF CORE
!
 7300 CALL page2(4)
   imsg = 363
   WRITE (Nout,99013) imsg
   WRITE (Nout,99006) icrq
99006 FORMAT (5X,'INSUFFICIENT CORE TO PROCESS AXIC DATA IN SUBROUTINE','IFP3',/5X,'ADDITIONAL CORE NEEDED =',I8,' WORDS.')
   Nogo = .TRUE.
!
!     GO TO FATAL ERROR RETURN
!
   GOTO 8600
!
!     AXIC FILE NOT IN FIST
!
 7400 CALL page2(3)
   imsg = 1061
   WRITE (Nout,99011) Sfm , imsg
   WRITE (Nout,99012) Axic , Iname(11) , Iname(12) , ifist
   Nogo = .TRUE.
!
!     GO TO FATAL ERROR RETURN
!
   GOTO 8600
!
!     OPEN A FILE AND GET THE TRAILER
!
 7500 IF ( .NOT.(Nogo) ) THEN
      CALL open(*7600,File(i),Z(Buff),Op)
      Openfl(i) = 1
      IF ( i<=4 ) THEN
!
!     WRITE THE HEADER RECORD
!
         CALL write(File(i),Iname(2*i-1),2,Eor)
         Trail(1) = File(i)
         CALL rdtrl(Trail(1))
      ENDIF
   ENDIF
!
   GOTO Iretrn
!
 7600 CALL page2(3)
   imsg = 1061
   WRITE (Nout,99011) Sfm , imsg
   WRITE (Nout,99012) File(i) , Iname(2*i-1) , Iname(2*i) , ifist
   Nogo = .TRUE.
   GOTO 8600
!
!     CLOSE A FILE
!
 7700 IF ( Openfl(i)/=0 ) THEN
      IF ( i<=4 ) CALL write(File(i),T65535(1),3,Eor)
      CALL close(File(i),Clorwd)
      Openfl(i) = 0
      IF ( i<=4 ) CALL wrttrl(Trail(1))
   ENDIF
   GOTO Iretrn
!
!     HARMONIC NUMBER ... ON CARD TYPE ...... IS OUT OF RANGE 0 TO 998
!
 7800 IF ( Nnn<999 .AND. Nnn>=0 .AND. Nnn<=N ) GOTO Ierrtn
   CALL page2(3)
   imsg = 364
   WRITE (Nout,99013) Ufm , imsg
   WRITE (Nout,99007) Nnn , Cdtype(2*Ncard-1) , Cdtype(2*Ncard) , N
99007 FORMAT (5X,'HARMONIC NUMBER ',I6,4H ON ,2A4,' CARD OUT OF 0 TO ',I4,' ALLOWABLE RANGE.')
   Nogo = .TRUE.
   GOTO Ierrtn
!
!     RING ID OUT OF PERMISSABLE RANGE OF 1 TO 999999
!
 7900 IF ( Nnn>0 .AND. Nnn<=999999 ) GOTO Ierrtn
   CALL page2(3)
   imsg = 365
   WRITE (Nout,99013) Ufm , imsg
   WRITE (Nout,99008) Nnn , Cdtype(2*Ncard-1) , Cdtype(2*Ncard)
99008 FORMAT (5X,'RING ID',I10,4H ON ,2A4,' CARD OUT OF 1 TO 999999 ','ALLOWABLE RANGE')
   Nogo = .TRUE.
   GOTO Ierrtn
!
!     CHECK BIT-IBIT IN TRAILER AND RETURN NON = ZERO OR NON-ZERO...
!
 8000 I1 = (Ibit-1)/16 + 2
   I2 = Ibit - (I1-2)*16 + 16
   Non = andf(Axtrl(I1),Two(I2))
   GOTO Ibitr
!
!     WRITE 3 WORD RECORD HEADER
!
 8100 IF ( .NOT.(Nogo) ) CALL write(Ifile,Rec(1),3,Noeor)
   GOTO Iheadr
!
!     END-OF-RECORD ON AXIC FILE
!
 8200 CALL page2(3)
   imsg = 1063
   WRITE (Nout,99011) Sfm , imsg
   WRITE (Nout,99009) Cdtype(2*Ncard-1) , Cdtype(2*Ncard)
99009 FORMAT (5X,'EOR ON AXIC FILE WHILE READING ',2A4,'CARD RECORDS.')
   Nogo = .TRUE.
   GOTO Ierrtn
!
!     AXIC TRAILER BIT ON BUT CAN NOT LOCATE RECORD
!
 8300 CALL page2(3)
   imsg = 1064
   WRITE (Nout,99011) Sfm , imsg
   WRITE (Nout,99014) Cdtype(2*Ncard-1) , Cdtype(2*Ncard)
 8400 Nogo = .TRUE.
   GOTO 2900
 8500 CALL page2(2)
   WRITE (Nout,99014) Recid(1) , Recid(2) , Recid(3)
   GOTO 8400
!
!     CLOSE ANY OPEN FILES AND RETURN
!
 8600 DO i = 1 , 6
      IF ( Openfl(i)/=0 ) THEN
         CALL close(File(i),Clorwd)
         Openfl(i) = 0
      ENDIF
   ENDDO
   IF ( Nogo ) Noflag = 32767
   CALL conmsg(msg2,2,0)
   RETURN
!
!     EOF ENCOUNTERED READING AXIC FILE.
!
 8700 Nfile = Axic
   in = 11
   in1 = 12
   GOTO 8900
 8800 Nfile = Scrtch
   in = 9
   in1 = 10
 8900 CALL page2(3)
   imsg = 3002
   WRITE (Nout,99011) Sfm , imsg
   WRITE (Nout,99010) Iname(in) , Iname(in1) , Nfile
99010 FORMAT (5X,'EOF ENCOUNTERED WHILE READING DATA SET ',2A4,' (FILE',I4,') IN SUBROUTINE IFP3')
   Nogo = .TRUE.
   GOTO 8600
!
 9000 i = 5
   GOTO 7600
99011 FORMAT (A25,I5)
99012 FORMAT (5X,11HFILE NUMBER,I4,3H ( ,2A4,12H) IS NOT IN ,A4,1H.)
99013 FORMAT (A23,I4)
99014 FORMAT (5X,2A4,' CARD COULD NOT BE LOCATED ON AXIC FILE AS ','EXPECTED')
END SUBROUTINE ifp3