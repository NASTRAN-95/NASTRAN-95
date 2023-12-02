!*==ifp3.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
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
   USE c_condas
   USE c_ifp3cm
   USE c_ifp3lv
   USE c_output
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: axic , b , chr , clorwd , eor , i , iconb , icrq , ii , ij , ik , imsg , in , in1 , inprwd , ix , j , k , mus , nh1 , &
            & nh2 , noeor , outrwd , scrtch , word
   REAL :: difphi , pi , raddeg , twopi
   INTEGER , DIMENSION(4) :: geom
   INTEGER , SAVE :: i5 , i6 , ifiat , ifist
   INTEGER , DIMENSION(11) , SAVE :: inum
   INTEGER , DIMENSION(2) , SAVE :: msg1 , msg2
   INTEGER , DIMENSION(11) :: num
   LOGICAL :: piez , secd
   INTEGER , DIMENSION(8) :: z
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
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
      num(i) = rshift(inum(i),nbpw-nbpc)
   ENDDO
!
!     INITIAL CHECK TO MAKE SURE TRAILER BITS ARE ALL OFF FOR GEOM1,
!     GEOM2, GEOM3, GEOM4.
!
   DO i = 1 , 96
      ihead(i) = iheadb(i)
   ENDDO
!
   IF ( noflag/=0 ) THEN
      nogo = .TRUE.
   ELSE
      nogo = .FALSE.
   ENDIF
!
   openfl(1) = 0
   openfl(2) = 0
   openfl(3) = 0
   openfl(4) = 0
   openfl(5) = 0
   openfl(6) = 0
   DO i = 1 , 4
      trail(1) = geom(i)
      CALL rdtrl(trail(1))
      IF ( trail(1)<=0 ) THEN
         CALL page2(3)
         imsg = 1061
         WRITE (nout,99011) sfm , imsg
         WRITE (nout,99012) geom(i) , iname(2*i-1) , iname(2*i) , ifiat
         nogo = .TRUE.
      ELSE
!
         DO j = 2 , 7
            IF ( trail(j)/=0 ) THEN
               CALL page2(3)
               imsg = 1062
               WRITE (nout,99011) sfm , imsg
               WRITE (nout,99001) geom(i) , iname(2*i-1) , iname(2*i)
99001          FORMAT (5X,'FILE NUMBER',I4,3H ( ,2A4,') HAS TRAILER BIT ON.  ','FILE SHOULD BE CLEAN AT ENTRY TO IFP3.')
               nogo = .TRUE.
               EXIT
            ENDIF
         ENDDO
      ENDIF
   ENDDO
!
!     PROCEED TO SETUP CORE AND OPEN AXIC FILE
!     ICORE1 WILL ALWAYS EQUAL THE GROSS OPEN CORE TO IFP3 AT START
!
   icore1 = korsz(z)
   ibuff1 = icore1 - ibufsz - 2
   ibuff2 = ibuff1 - ibufsz
   ibuff3 = ibuff2 - ibufsz
   icore = ibuff3 - 1
   icrq = 100 - icore
   IF ( icore<100 ) GOTO 7300
!
!     OPEN  AXIC FILE
!
   CALL preloc(*7400,z(ibuff1),axic)
   openfl(6) = 1
   axtrl(1) = axic
   CALL rdtrl(axtrl(1))
!
!     READ AXIC CARD
!
   CALL locate(*100,z(ibuff1),axic1(1),flag)
   CALL read(*8700,*100,axic,z(1),2,eor,flag)
   n = z(1)
   csid = z(2)
   nnn = n
   ncard = 1
   ASSIGN 200 TO ierrtn
   GOTO 7800
!
!     MISSING REQUIRED AXIC CARD
!
 100  ASSIGN 200 TO ierrtn
   nnn = 0
   ncard = 1
!
!     MISSING REQUIRED CARD
!
   CALL page2(3)
   imsg = 362
   WRITE (nout,99013) ufm , imsg
   WRITE (nout,99002) cdtype(2*ncard-1) , cdtype(2*ncard)
99002 FORMAT (5X,'MINIMUM PROBLEM REQUIRES ',2A4,' CARD.  NONE FOUND.')
   nogo = .TRUE.
   GOTO ierrtn
 200  n = nnn
   nplus1 = n + 1
!
!
!     GEOM2  PROCESSING
!     =================
!
!     OPEN GEOM2
!
   ifile = geom(2)
   i = 2
   op = outrwd
   buff = ibuff2
   ASSIGN 300 TO iretrn
   GOTO 7500
!
!     CCONEAX CARDS
!
 300  rec(1) = cconex(1)
   rec(2) = cconex(2)
   rec(3) = cconex(3)
   ncard = 2
!
!     IF THERE IS NO CCONEAX CARD, THEN GO TO 1750 AND LOOK FOR
!     CTRAPAX OR CTRIAAX CARDS
!
   iconb = 0
   iconso = 0
   CALL locate(*6500,z(ibuff1),rec(1),flag)
!
!     INPUT IS IN 4-WORD CARDS
!     OUTPUT IS N+1 4-WORD CARDS FOR EACH CARD INPUT
!
!     RECORD HEADER FOR CCONES
!
   ASSIGN 400 TO iheadr
   GOTO 8100
!
 400  CALL read(*8700,*700,axic,z(1),4,noeor,iamt)
!
!     CHECK RING ID-S FOR SIZE
!
   nnn = z(3)
   ASSIGN 500 TO ierrtn
   GOTO 7900
 500  nnn = z(4)
   ASSIGN 600 TO ierrtn
   GOTO 7900
!
!     CHECK CCONEAX ID FOR 1-9999 ALLOWABLE RANGE
!
 600  IF ( z(1)<=0 .OR. z(1)>=10000 ) THEN
      CALL page2(3)
      imsg = 361
      WRITE (nout,99013) ufm , imsg
      WRITE (nout,99003) z(1)
99003 FORMAT (5X,'CCONEAX ID =',I10,'.  OUT OF 1 TO 9999 PERMISSIBLE ','RANGE')
      nogo = .TRUE.
   ENDIF
!
   z(1) = z(1)*1000
   DO i = 1 , nplus1
      z(1) = z(1) + 1
      z(3) = z(3) + 1000000
      z(4) = z(4) + 1000000
      IF ( .NOT.(nogo) ) CALL write(geom(2),z(1),4,noeor)
   ENDDO
   GOTO 400
!
!     OUT OF CCONEAX CARDS
!
 700  IF ( iamt/=0 ) THEN
!
!     GO TO 356 FOR RECORD ERROR
!
      ASSIGN 900 TO ierrtn
      GOTO 8200
   ELSE
!
!     WRITE EOR AND PUT BITS IN TRAILER
!
      ASSIGN 800 TO iretrn
      iconso = 1
      GOTO 7000
   ENDIF
 800  iconb = 1
   GOTO 6500
!
!     CLOSE GEOM2
!
 900  i = 2
   ASSIGN 1000 TO iretrn
   GOTO 7700
!
!     GEOM3 PROCESSING
!     ================
!
!     OPEN GEOM3
!
 1000 ifile = geom(3)
   i = 3
   op = outrwd
   buff = ibuff2
   ASSIGN 1100 TO iretrn
   GOTO 7500
!
!     FORCE, FORCEAX, MOMNT, AND MOMNTAX CARDS
!
 1100 recid(1) = force(1)
   recid(2) = force(2)
   recid(3) = force(3)
   recidx(1) = forcex(1)
   recidx(2) = forcex(2)
   recidx(3) = forcex(3)
   ncard = 3
   ASSIGN 3000 TO icont
!
!     SET NOREG = 0 OR 1, DEPENDING ON PRESSENCE OF RECID
!     SET NOAXIC= 0 OR 1, DEPENDING ON PRESSENCE OF RECIDX
!
 1200 ibit = recidx(2)
   ASSIGN 1300 TO ibitr
   GOTO 8000
 1300 noaxic = non
   ibit = recid(2)
   ASSIGN 1400 TO ibitr
   GOTO 8000
 1400 noreg = non
!
   rec(1) = recid(1)
   rec(2) = recid(2)
   rec(3) = recid(3)
!
   IF ( noaxic/=0 ) THEN
!
!     AT 410 READ IN ALL FORCEAX OR MOMNTAX CARDS AND PUT OUT ON GEOM(3)
!     IF NOREG=0,AND ON SCRTCH IF NOREG NON-ZERO.FIRST WRITE 3-WORD-
!     REC ID ON GEOM3.
!
      ASSIGN 1500 TO iheadr
      GOTO 8100
   ELSE
      IF ( noreg==0 ) GOTO 2900
!
!     TRANSFER FORCE OR MOMENT RECORD DIRECTLY.
!     THERE ARE NO FORCEAX OR MOMAX CARDS RESPECTIVELY.
!
      ASSIGN 2900 TO iretrn
      GOTO 6900
   ENDIF
!
!     OPEN SCRATCH IF NEEDED
!
 1500 IF ( noreg/=0 ) THEN
      i = 5
      op = outrwd
      buff = ibuff3
      ASSIGN 1600 TO iretrn
      GOTO 7500
   ENDIF
 1600 CALL locate(*8300,z(ibuff1),recidx(1),flag)
 1700 CALL read(*8700,*2100,axic,z(1),8,noeor,iamt)
!
!     CHECK RING ID
!
   ASSIGN 1800 TO ierrtn
   nnn = z(2)
   GOTO 7900
!
!     CHECK HARMONIC NUMBER AND FOR A SEQUENCE OF HARMONICS
!
 1800 IF ( z(4)==0 ) THEN
      nh1 = z(3)
      nh2 = z(3)
   ELSE
      ii = 1
      nh1 = 0
      nh2 = 0
      secd = .TRUE.
      word = 4
      DO ij = 1 , 2
         DO ix = 1 , 4
            chr = rshift(lshift(z(word),nbpc*iabs(ix-4)),nbpw-nbpc)
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
      nnn = nh1
      ASSIGN 1900 TO ierrtn
      GOTO 7800
   ENDIF
 1900 nnn = nh2
   ASSIGN 2000 TO ierrtn
   GOTO 7800
 2000 z(4) = z(5)
   z(5) = z(6)
   z(6) = z(7)
   z(7) = z(8)
   nh1 = nh1 + 1
   nh2 = nh2 + 1
   sum = z(2)
   mus = z(2)
   DO i = nh1 , nh2
      z(2) = mus + i*1000000
      z(3) = 0
!
!     OUTPUT TO GEOM(3) IF NOREG = 0
!     OUTPUT TO SCRTCH  IF NOREG = NON-ZERO
!
      IF ( nogo ) EXIT
      IF ( noreg/=0 ) THEN
         nfile = scrtch
      ELSE
         nfile = geom(3)
      ENDIF
      CALL write(nfile,z(1),7,noeor)
   ENDDO
   GOTO 1700
!
!     OUT OF CARDS
!
 2100 IF ( iamt/=0 ) THEN
!
!     CHECK FOR RECORD INCONSISTANCY ERROR.
!
      rec(1) = recidx(1)
      rec(2) = recidx(2)
      rec(3) = recidx(3)
      ASSIGN 2200 TO ierrtn
      GOTO 8200
   ENDIF
!
 2200 IF ( noreg==0 ) GOTO 2700
!
!     CLOSE THE SCRTCH FILE AND THEN MERGE SCRTCH WITH AXIC
!     ON TO GEOM3
!
   i = 5
   ASSIGN 2300 TO iretrn
   GOTO 7700
!
!     OPEN SCRTCH FILE FOR INPUT AND LOCATE FORCE OR MOMENT CARDS ON
!     AXIC FILE.
!
 2300 ASSIGN 2400 TO iretrn
   op = inprwd
   GOTO 7500
 2400 CALL locate(*8500,z(ibuff1),recid(1),flag)
   IF ( nogo ) GOTO 2900
!
   CALL read(*8700,*2800,axic,z(1),7,noeor,iamt)
   CALL read(*8800,*8800,scrtch,z(8),7,noeor,iamt)
   DO
!
      IF ( z(1)<=z(8) ) THEN
!
         nfile = axic
         outbuf = 1
      ELSE
!
         nfile = scrtch
         outbuf = 8
      ENDIF
!
      IF ( nogo ) GOTO 2900
      CALL write(geom(3),z(outbuf),7,noeor)
      CALL read(*8900,*2500,nfile,z(outbuf),7,noeor,iamt)
   ENDDO
!
!     OK ALL WORDS PROCESSED FOR FILE-NFILE
!
 2500 IF ( nfile==axic ) THEN
      nfile = scrtch
      outbuf = 8
   ELSE
      nfile = axic
      outbuf = 1
   ENDIF
   DO WHILE ( .NOT.(nogo) )
      CALL write(geom(3),z(outbuf),7,noeor)
      CALL read(*8900,*2600,nfile,z(outbuf),7,noeor,iamt)
   ENDDO
   GOTO 2900
!
!     CLOSE SCRTCH, WRITE EOR, AND PUT BITS IN TRAILER.
!
 2600 i = 5
   ASSIGN 2700 TO iretrn
   GOTO 7700
 2700 ASSIGN 2900 TO iretrn
   GOTO 7000
!
!     RECORD LENGTH ERROR
!
 2800 rec(1) = recid(1)
   rec(2) = recid(2)
   rec(3) = recid(3)
   ASSIGN 2900 TO ierrtn
   GOTO 8200
!
 2900 GOTO icont
!
!     GRAV CARD
!
 3000 rec(1) = grav(1)
   rec(2) = grav(2)
   rec(3) = grav(3)
   ASSIGN 3100 TO iretrn
   GOTO 6900
!
!     LOAD CARD
!
 3100 rec(1) = load(1)
   rec(2) = load(2)
   rec(3) = load(3)
   ASSIGN 3200 TO iretrn
   GOTO 6900
!
!     MOMENT AND MOMAX CARDS
!
 3200 recid(1) = moment(1)
   recid(2) = moment(2)
   recid(3) = moment(3)
   recidx(1) = momax(1)
   recidx(2) = momax(2)
   recidx(3) = momax(3)
   ncard = 7
   ASSIGN 3300 TO icont
   GOTO 1200
!
!     PRESAX CARD
!
 3300 CALL locate(*3800,z(ibuff1),presax(1),flag)
!
!     RECORD HEADER FOR PRESAX CARDS IS FORMED HERE
!
   rec(1) = presax(1)
   rec(2) = presax(2)
   rec(3) = presax(3)
   ncard = 13
   ASSIGN 3400 TO iheadr
   GOTO 8100
!
 3400 CALL read(*8700,*3700,axic,z(1),6,noeor,iamt)
!
!     CREATE N+1 CARDS OF SAME LENGTH AS INPUT CARD.
!
!     CHECK RING ID-S IN FIELDS 3 AND 4 FOR PROPER SIZE.
!
!     CHECK FOR PIEZOELECTRIC
!
   piez = .FALSE.
   IF ( ipiez==1 .AND. z(3)<0 ) piez = .TRUE.
   IF ( piez ) z(3) = -z(3)
   nnn = z(3)
   ASSIGN 3500 TO ierrtn
   GOTO 7900
 3500 nnn = z(4)
   ASSIGN 3600 TO ierrtn
   GOTO 7900
!
 3600 difphi = abs(rz(i6)-rz(i5))
   DO i = 1 , nplus1
      z(7) = i - 1
      z(3) = z(3) + 1000000
      IF ( piez ) z(3) = -z(3)
      z(4) = z(4) + 1000000
      IF ( .NOT.(nogo) ) THEN
         IF ( difphi/=0.0 ) THEN
            IF ( i<=1 .OR. abs(difphi-360.)>=1.E-6 ) THEN
               CALL write(geom(3),z(1),7,noeor)
               IF ( piez ) z(3) = -z(3)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   GOTO 3400
!
!     OUT OF PRESAX CARDS
!
 3700 IF ( iamt/=0 ) THEN
!
!     CHECK FOR RECORD INCONSISTANCY ERROR.
!
      ASSIGN 3800 TO ierrtn
      rec(1) = presax(1)
      rec(2) = presax(2)
      rec(3) = presax(3)
      GOTO 8200
   ELSE
!
!     WRITE EOR AND PUT BITS IN TRAILER
!
      ASSIGN 3800 TO iretrn
      GOTO 7000
   ENDIF
!
!     RFORCE CARD
!
 3800 CALL locate(*4100,z(ibuff1),rforce(1),flag)
   rec(1) = rforce(1)
   rec(2) = rforce(2)
   rec(3) = rforce(3)
   ncard = 24
   ASSIGN 3900 TO iheadr
   GOTO 8100
 3900 DO
!
!     PROCESS RFORCE DATA
!
      CALL read(*8700,*4000,axic,z(1),7,noeor,iamt)
      IF ( z(2)==0 .AND. z(3)==0 .AND. z(5)==0 .AND. z(6)==0 ) THEN
         z(2) = 0
         z(3) = 0
         z(5) = 0
         z(6) = z(7)
         z(7) = 0
         CALL write(geom(3),z(1),7,noeor)
      ELSE
         WRITE (nout,99004) ufm , z(1)
99004    FORMAT (A23,' 336, RFORCE DATA IN SET NO.',I8,' CONTAINS ILLEGAL DIRECTION FOR AXISYMMETRIC PROBLEM')
         nogo = .TRUE.
      ENDIF
   ENDDO
!
!     END OF RFORCE CARDS
!
 4000 IF ( iamt/=0 ) THEN
!
!     RECORD INCONSISTENCY ERROR
!
      ASSIGN 4100 TO ierrtn
      rec(1) = rforce(1)
      rec(2) = rforce(2)
      rec(3) = rforce(3)
      GOTO 8200
   ELSE
!
!     WRITE EOR AND BITS IN TRAILER
!
      ASSIGN 4100 TO iretrn
      GOTO 7000
   ENDIF
!
!     TEMPD CARD
!
 4100 rec(1) = tempd(1)
   rec(2) = tempd(2)
   rec(3) = tempd(3)
   ASSIGN 4500 TO iretrn
   IF ( nogo ) GOTO 4500
   CALL locate(*4500,z(ibuff1),rec(1),flag)
   CALL write(ifile,rec(1),3,noeor)
   veor = 0
 4200 CALL read(*8700,*4400,axic,z(1),icore,noeor,iamt)
   iamt = icore
 4300 DO i = 1 , iamt , 2
      z(i) = z(i) + 100000000
   ENDDO
   CALL write(ifile,z(1),iamt,0)
   DO i = 1 , iamt , 2
      z(i) = z(i) + 100000000
   ENDDO
   CALL write(ifile,z(1),iamt,veor)
   IF ( veor==0 ) GOTO 4200
   GOTO 7100
 4400 veor = 1
   GOTO 4300
!
!     TEMPAX CARD
!
 4500 CALL locate(*6000,z(ibuff1),tempax(1),flag)
!
!     RECORD HEADER ON GEOM3 FOR TEMP CARDS
!
   rec(1) = temp(1)
   rec(2) = temp(2)
   rec(3) = temp(3)
   ncard = 20
   ASSIGN 4600 TO iheadr
   GOTO 8100
!
!     AT 604(?) SET UP SCRATCH FILE.
!
 4600 i = 5
   buff = ibuff3
   op = outrwd
   ASSIGN 4700 TO iretrn
   GOTO 7500
!
!     PICK UP FIRST TEMPAX CARD = 4 WORDS.
!
 4700 last = 0
   CALL read(*8700,*5900,axic,z(1),4,noeor,iamt)
 4800 k = 0
   setid = z(1)
   ringid = z(2)
!
!     CHECK RING ID FOR PROPER RANGE OF VALUE
!
   nnn = ringid
   ASSIGN 4900 TO ierrtn
   GOTO 7900
!
 4900 iat = 3
   DO
      k = k + 1
      iat = iat + 2
      icrq = iat + 3 - icore
      IF ( icore<iat+3 ) GOTO 7300
!
!     ALL TEMPAX CARDS HAVING SAME SET AND RING ID MUST BE ABLE TO
!     HAVE 2 WORDS EACH FIT IN CORE.
!
      z(iat) = z(3)
      z(iat+1) = z(4)
!
      CALL read(*8700,*5500,axic,z(1),4,noeor,iamt)
!
!     DOES THIS CARD HAVE SAME SET AND RING ID AS LAST IN CURRENT SERIES
!
      IF ( z(1)/=setid ) EXIT
      IF ( z(2)/=ringid ) EXIT
   ENDDO
!
!     WE HAVE A  K X 2  ARRAY OF  PHI-S  AND T-S.
!
!     CONVERT ALL  PHIS SUCH THAT (0.LE. PHI .LT.TWOPI)
!
 5000 iend = iat + 1
   ibegin = 5
!
   DO i = ibegin , iend , 2
      angle = rz(i)
      IF ( angle<0 ) THEN
         DO WHILE ( angle<0 )
            angle = angle + 360.0
         ENDDO
      ELSEIF ( angle/=0 ) THEN
!
         DO WHILE ( angle>=360.0 )
            angle = angle - 360.0
         ENDDO
      ENDIF
!
      rz(i) = angle*raddeg
   ENDDO
!
!     SIMPLE SORT FOR THE K X 2  MATRIX.
!     SORT IS PERFORMED ON COLUMN 1 ONLY
!
   IF ( k==1 ) GOTO 5300
   istart = ibegin + 2
   DO i = istart , iend , 2
      iat = i - 2
      IF ( rz(i)<rz(iat) ) THEN
         DO
!
!     ROW NOT HIGH ENOUGH.  MOVE IT UP.
!
            iat = iat - 2
            IF ( iat<=ibegin ) THEN
               iat = ibegin
               EXIT
            ELSEIF ( rz(i)>=rz(iat) ) THEN
               iat = iat + 2
               EXIT
            ENDIF
         ENDDO
!
!     THE ELEMENTS (I) AND (I+1) WILL BE MOVED UP TO POSITIONS (IAT) AND
!     (IAT+1) AND ELEMENTS (IAT) THRU (I-1) WILL BE  MOVED DOWN 1 ROW.
!
!     FIRST SAVE THE ROW BEING MOVED UP
!
         rz(iend+1) = rz(i)
         rz(iend+2) = rz(i+1)
         nmove = i - iat
         iat = i + 2
         DO j = 1 , nmove
            iat = iat - 1
            rz(iat) = rz(iat-2)
         ENDDO
!
!     REPLACE SAVE ROW IN NEW SLOT
!
         rz(iat-2) = rz(iend+1)
         rz(iat-1) = rz(iend+2)
      ENDIF
!
   ENDDO
!
!     CHECK FOR ANY DUPLICATE ANGLES AND REMOVE THEM...
!
   ibegin = ibegin + 2
 5100 DO i = ibegin , iend , 2
      IF ( z(i)==z(i-2) ) GOTO 5200
   ENDDO
   GOTO 5300
!
!     DUPLICATE, SHRINK LIST UP OVER IT.
!
 5200 iend = iend - 2
   k = k - 1
   DO j = i , iend , 2
      z(j) = z(j+2)
      z(j+1) = z(j+3)
   ENDDO
   ibegin = i
   IF ( ibegin<iend ) GOTO 5100
!
!     SET UP K + 1  CARD
!
 5300 rz(iend+1) = rz(i5) + twopi
   rz(iend+2) = rz(i6)
!
!     THERE ARE K CARDS NOW WITH SETID, AND RINGID, NOT INCLUDING THE
!     K + 1ST CARD
!
!     N+1 TEMP CARDS FOR S SET (PUT ON GEOM3)
!     N+1 TEMP CARDS FOR C SET (PUT ON SCRTCH FOR NOW)
!
!     NOTE FMMS-52  (10/04/67) PAGE -9- FOR FOLLOWING...
!
   csset = 1
   setid = setid + 100000000
!
!     CSSET = 0 FOR C-SET  AND NON-ZERO FOR S-SET.
!
   ibegin = k + k + 7
   icrq = ibegin + 2 - icore
   IF ( (ibegin+2)>icore ) GOTO 7300
!
 5400 nadd = 0
   z(ibegin) = setid
   DO i = 1 , nplus1
      nadd = nadd + 1000000
!
!     NI IS REAL
!
      ni = i - 1
      nisq = (i-1)**2
      z(ibegin+1) = ringid + nadd
      iphi = 3
      it = 4
      sum = 0.0E0
      IF ( ni/=0 ) THEN
!
!     NON-ZERO NI
!
         IF ( k/=1 ) THEN
            DO ik = 1 , k
               iphi = iphi + 2
               it = it + 2
               nphi = ni*rz(iphi)
               nphi1 = ni*rz(iphi+2)
!
               IF ( csset/=0 ) THEN
!
!     S-SET
!
                  a1 = -cos(nphi1)
                  a2 = cos(nphi)
                  a3 = sin(nphi1)
                  a4 = -sin(nphi)
               ELSE
!
!     C-SET
!
                  a1 = sin(nphi1)
                  a2 = -sin(nphi)
                  a3 = cos(nphi1)
                  a4 = -cos(nphi)
               ENDIF
!
!
               sum = sum + (((rz(it)*rz(iphi+2)-rz(it+2)*rz(iphi))*(a1+a2)/ni)+((rz(it+2)-rz(it))*(a3+a4+nphi1*a1+nphi*a2)/nisq))   &
                   & /(rz(iphi+2)-rz(iphi))
            ENDDO
         ENDIF
!
         rz(ibegin+2) = sum/pi
      ELSE
         IF ( csset==0 ) THEN
            DO ik = 1 , k
               iphi = iphi + 2
               it = it + 2
               sum = sum + (rz(it)+rz(it+2))*(rz(iphi+2)-rz(iphi))
            ENDDO
         ENDIF
         rz(ibegin+2) = 0.25*sum/pi
      ENDIF
!
      IF ( nogo ) EXIT
      IF ( csset/=0 ) THEN
         nfile = geom(3)
      ELSE
         nfile = scrtch
      ENDIF
      CALL write(nfile,z(ibegin),3,noeor)
   ENDDO
   IF ( csset/=0 ) THEN
      csset = 0
      setid = setid + 100000000
      GOTO 5400
   ELSE
!
!     THIS SERIES OF TEMPAX CARDS COMPLETE GO FOR MORE IF LAST = 0
!
      IF ( last==0 ) GOTO 4800
!
!     ALL TEMPAX CARDS COMPLETE. CLOSE SCRATCH, OPEN SCRATCH
!     AND COPY SCRATCH TO GEOM3.
!
      IF ( nogo ) GOTO 6000
      CALL write(scrtch,z(1),0,eor)
      CALL close(scrtch,clorwd)
      CALL open(*9000,scrtch,z(ibuff3),inprwd)
!
      veor = 0
      GOTO 5600
   ENDIF
 5500 last = 1
   GOTO 5000
 5600 CALL read(*8800,*5800,scrtch,z(1),icore,noeor,iamt)
   iamt = icore
 5700 CALL write(geom(3),z(1),iamt,veor)
   IF ( veor==0 ) GOTO 5600
!
!     ALL  TEMPAX  CARDS  PROCESSED.
!
   CALL close(scrtch,clorwd)
!
!     PUT BITS IN TRAILER FOR TEMP CARDS WRITTEN
!
   rec(1) = temp(1)
   rec(2) = temp(2)
   rec(3) = temp(3)
   ASSIGN 6000 TO iretrn
   GOTO 7100
 5800 veor = 1
   GOTO 5700
!
!     RECORD LENGTH ERROR
!
 5900 rec(1) = tempax(1)
   rec(2) = tempax(2)
   rec(3) = tempax(3)
   ASSIGN 6000 TO ierrtn
   GOTO 8200
!
!     CLOSE GEOM3
!
 6000 i = 3
   ASSIGN 6800 TO iretrn
   GOTO 7700
!
!     CTRIAAX CARD
!
 6100 rec(1) = ctriaa(1)
   rec(2) = ctriaa(2)
   rec(3) = ctriaa(3)
   ncard = 43
   CALL locate(*6400,z(ibuff1),rec(1),flag)
!
!     RECORD HEADER FOR CTRIAAX
!
   ASSIGN 6200 TO iheadr
   iconb = 2
   iconso = 1
   GOTO 8100
 6200 DO
      CALL read(*8700,*6300,axic,z(1),6,noeor,iamt)
      z(1) = z(1)*1000
      DO i = 1 , nplus1
         z(1) = z(1) + 1
         z(3) = z(3) + 1000000
         z(4) = z(4) + 1000000
         z(5) = z(5) + 1000000
         IF ( .NOT.(nogo) ) CALL write(geom(2),z(1),6,noeor)
      ENDDO
   ENDDO
!
!     OUT OF CTRIAAX CARD
!
 6300 IF ( iamt/=0 ) THEN
      ASSIGN 900 TO ierrtn
      GOTO 8200
   ELSE
!
!     PUT BITS IN TRILER
!
      ASSIGN 900 TO iretrn
      GOTO 7000
   ENDIF
 6400 IF ( iconso==1 ) THEN
      ASSIGN 900 TO iretrn
      GOTO 7000
   ELSE
      ASSIGN 900 TO ierrtn
!
!     MISSING REQUIRED CCONEAX OR CTRIAAX OR CTRAPAX CARD
!
      CALL page2(3)
      imsg = 362
      WRITE (nout,99013) ufm , imsg
      WRITE (nout,99005) cdtype(3) , cdtype(4) , cdtype(43) , cdtype(44) , cdtype(45) , cdtype(46)
99005 FORMAT (5X,'MINIMUM PROBLEM REQUIRES ',2A4,2H, ,2A4,4H OR ,2A4,' CARD.  NONE FOUND')
      nogo = .TRUE.
      GOTO ierrtn
   ENDIF
!
!     CTRAPAX CARD
!     ============
!
 6500 rec(1) = ctrapa(1)
   rec(2) = ctrapa(2)
   rec(3) = ctrapa(3)
   CALL locate(*6100,z(ibuff1),rec(1),flag)
   iconb = 1
!
!     RECORD HEADER FOR CTRAPAX
!
   ASSIGN 6600 TO iheadr
   iconso = 1
   GOTO 8100
 6600 DO
      CALL read(*8700,*6700,axic,z(1),7,noeor,iamt)
      z(1) = z(1)*1000
      DO i = 1 , nplus1
         z(1) = z(1) + 1
         z(3) = z(3) + 1000000
         z(4) = z(4) + 1000000
         z(5) = z(5) + 1000000
         z(6) = z(6) + 1000000
         IF ( .NOT.(nogo) ) CALL write(geom(2),z(1),7,noeor)
      ENDDO
   ENDDO
!
!     OUT OF CTRAPAX CARD
!
 6700 IF ( iamt/=0 ) THEN
      ASSIGN 900 TO ierrtn
      GOTO 8200
   ELSE
!
!     PUT BITS IN TRILER
!
      ASSIGN 900 TO iretrn
      IF ( nogo ) GOTO 7200
      CALL write(ifile,z(1),iamt,eor)
      i1 = (rec(2)-1)/16 + 2
      i2 = rec(2) - (i1-2)*16 + 16
      trail(i1) = orf(trail(i1),two(i2))
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
 6900 CALL locate(*7200,z(ibuff1),rec(1),flag)
   IF ( nogo ) GOTO 7200
   CALL write(ifile,rec(1),3,noeor)
   DO
      CALL read(*8700,*7000,axic,z(1),icore,noeor,iamt)
      iamt = icore
      CALL write(ifile,z(1),iamt,noeor)
   ENDDO
 7000 IF ( nogo ) GOTO 7200
   IF ( ifile==geom(3) ) THEN
      CALL write(ifile,z(1),iamt,eor)
   ELSE
      IF ( ifile==geom(2) .AND. iconb==1 ) GOTO 7200
      CALL write(ifile,z(1),iamt,eor)
   ENDIF
!
!     PUT BITS IN TRAILER
!
 7100 i1 = (rec(2)-1)/16 + 2
   i2 = rec(2) - (i1-2)*16 + 16
   trail(i1) = orf(trail(i1),two(i2))
!
 7200 GOTO iretrn
!
!     OUT OF CORE
!
 7300 CALL page2(4)
   imsg = 363
   WRITE (nout,99013) imsg
   WRITE (nout,99006) icrq
99006 FORMAT (5X,'INSUFFICIENT CORE TO PROCESS AXIC DATA IN SUBROUTINE','IFP3',/5X,'ADDITIONAL CORE NEEDED =',I8,' WORDS.')
   nogo = .TRUE.
!
!     GO TO FATAL ERROR RETURN
!
   GOTO 8600
!
!     AXIC FILE NOT IN FIST
!
 7400 CALL page2(3)
   imsg = 1061
   WRITE (nout,99011) sfm , imsg
   WRITE (nout,99012) axic , iname(11) , iname(12) , ifist
   nogo = .TRUE.
!
!     GO TO FATAL ERROR RETURN
!
   GOTO 8600
!
!     OPEN A FILE AND GET THE TRAILER
!
 7500 IF ( .NOT.(nogo) ) THEN
      CALL open(*7600,file(i),z(buff),op)
      openfl(i) = 1
      IF ( i<=4 ) THEN
!
!     WRITE THE HEADER RECORD
!
         CALL write(file(i),iname(2*i-1),2,eor)
         trail(1) = file(i)
         CALL rdtrl(trail(1))
      ENDIF
   ENDIF
!
   GOTO iretrn
!
 7600 CALL page2(3)
   imsg = 1061
   WRITE (nout,99011) sfm , imsg
   WRITE (nout,99012) file(i) , iname(2*i-1) , iname(2*i) , ifist
   nogo = .TRUE.
   GOTO 8600
!
!     CLOSE A FILE
!
 7700 IF ( openfl(i)/=0 ) THEN
      IF ( i<=4 ) CALL write(file(i),t65535(1),3,eor)
      CALL close(file(i),clorwd)
      openfl(i) = 0
      IF ( i<=4 ) CALL wrttrl(trail(1))
   ENDIF
   GOTO iretrn
!
!     HARMONIC NUMBER ... ON CARD TYPE ...... IS OUT OF RANGE 0 TO 998
!
 7800 IF ( nnn<999 .AND. nnn>=0 .AND. nnn<=n ) GOTO ierrtn
   CALL page2(3)
   imsg = 364
   WRITE (nout,99013) ufm , imsg
   WRITE (nout,99007) nnn , cdtype(2*ncard-1) , cdtype(2*ncard) , n
99007 FORMAT (5X,'HARMONIC NUMBER ',I6,4H ON ,2A4,' CARD OUT OF 0 TO ',I4,' ALLOWABLE RANGE.')
   nogo = .TRUE.
   GOTO ierrtn
!
!     RING ID OUT OF PERMISSABLE RANGE OF 1 TO 999999
!
 7900 IF ( nnn>0 .AND. nnn<=999999 ) GOTO ierrtn
   CALL page2(3)
   imsg = 365
   WRITE (nout,99013) ufm , imsg
   WRITE (nout,99008) nnn , cdtype(2*ncard-1) , cdtype(2*ncard)
99008 FORMAT (5X,'RING ID',I10,4H ON ,2A4,' CARD OUT OF 1 TO 999999 ','ALLOWABLE RANGE')
   nogo = .TRUE.
   GOTO ierrtn
!
!     CHECK BIT-IBIT IN TRAILER AND RETURN NON = ZERO OR NON-ZERO...
!
 8000 i1 = (ibit-1)/16 + 2
   i2 = ibit - (i1-2)*16 + 16
   non = andf(axtrl(i1),two(i2))
   GOTO ibitr
!
!     WRITE 3 WORD RECORD HEADER
!
 8100 IF ( .NOT.(nogo) ) CALL write(ifile,rec(1),3,noeor)
   GOTO iheadr
!
!     END-OF-RECORD ON AXIC FILE
!
 8200 CALL page2(3)
   imsg = 1063
   WRITE (nout,99011) sfm , imsg
   WRITE (nout,99009) cdtype(2*ncard-1) , cdtype(2*ncard)
99009 FORMAT (5X,'EOR ON AXIC FILE WHILE READING ',2A4,'CARD RECORDS.')
   nogo = .TRUE.
   GOTO ierrtn
!
!     AXIC TRAILER BIT ON BUT CAN NOT LOCATE RECORD
!
 8300 CALL page2(3)
   imsg = 1064
   WRITE (nout,99011) sfm , imsg
   WRITE (nout,99014) cdtype(2*ncard-1) , cdtype(2*ncard)
 8400 nogo = .TRUE.
   GOTO 2900
 8500 CALL page2(2)
   WRITE (nout,99014) recid(1) , recid(2) , recid(3)
   GOTO 8400
!
!     CLOSE ANY OPEN FILES AND RETURN
!
 8600 DO i = 1 , 6
      IF ( openfl(i)/=0 ) THEN
         CALL close(file(i),clorwd)
         openfl(i) = 0
      ENDIF
   ENDDO
   IF ( nogo ) noflag = 32767
   CALL conmsg(msg2,2,0)
   RETURN
!
!     EOF ENCOUNTERED READING AXIC FILE.
!
 8700 nfile = axic
   in = 11
   in1 = 12
   GOTO 8900
 8800 nfile = scrtch
   in = 9
   in1 = 10
 8900 CALL page2(3)
   imsg = 3002
   WRITE (nout,99011) sfm , imsg
   WRITE (nout,99010) iname(in) , iname(in1) , nfile
99010 FORMAT (5X,'EOF ENCOUNTERED WHILE READING DATA SET ',2A4,' (FILE',I4,') IN SUBROUTINE IFP3')
   nogo = .TRUE.
   GOTO 8600
!
 9000 i = 5
   GOTO 7600
99011 FORMAT (A25,I5)
99012 FORMAT (5X,11HFILE NUMBER,I4,3H ( ,2A4,12H) IS NOT IN ,A4,1H.)
99013 FORMAT (A23,I4)
99014 FORMAT (5X,2A4,' CARD COULD NOT BE LOCATED ON AXIC FILE AS ','EXPECTED')
END SUBROUTINE ifp3
