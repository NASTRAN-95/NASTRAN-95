!*==ssght.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssght
   IMPLICIT NONE
   USE C_BLANK
   USE C_FBSX
   USE C_GFBSX
   USE C_HMATDD
   USE C_NAMES
   USE C_PACKX
   USE C_STIME
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZBLPKX
   USE C_ZNTPKX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(5,5) , SAVE :: alibi
   REAL :: alpha , beta , defalt , eps010 , epsold , epst , epsubp , flamda , gamma , pfmag , sn , sum , tau , un , value
   INTEGER , DIMENSION(10) :: buf
   INTEGER :: buf1 , buf2 , core , file , flag , fsize , gsize , i , iat , idelp , idelpz , idelu , ideluz , idfalt , ieqiv ,       &
            & iexit , if , ijk , ipos , iprec1 , iq , iqs , iqsz , iretrn , is , isil , isn , isnz , ium , iume , iumez , iumz ,    &
            & iun , iunat , iuni , iuniat , iuniz , iusz , j , jdelp , jpos , k , k1 , k2 , kdelu , ki , kip1 , kleft , ks , ku ,   &
            & kufi , kufip1 , kuni , lcore , loop , m , mpoint , mpt , mpte , msize , mval , n , ndelp , ndelu , neqiv , npoint ,   &
            & nqs , nsize , nsn , num , number , nume , nun , nuni , nval , outpt , precis , ssize , sysbuf , telaps
   LOGICAL , SAVE :: diagon
   INTEGER , SAVE :: dit , eor , est , gm , gptt , kff , kfs , ksf , kss , lfile , mptfil , noeor , pf , ps , qg , rfn , rsn ,      &
                   & rulv , scrt1 , scrt2 , scrt3 , scrt4 , ufile , ugv , uset
   LOGICAL :: linear , loop1 , nlrad , nogo , noqg , rulvec
   INTEGER , DIMENSION(7) :: mcb , mcb2 , rulmcb
   INTEGER , DIMENSION(2) :: name
   REAL , DIMENSION(10) :: rbuf
   REAL , DIMENSION(1) :: rz
   INTEGER , DIMENSION(2) , SAVE :: subr
   INTEGER :: tend , tloop , tset , tstart , word
   EXTERNAL bldpk , bldpkn , close , fbs , fname , fread , fwdrec , gfbs , gopen , intpk , korsz , makmcb , mesage , open , pack ,  &
          & prehma , rdtrl , read , skprec , ssght1 , ssght2 , ssghtp , sswtch , tmtogo , write , wrttrl , zblpki , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS IS THE STATIC-SOLUTION-GENERATOR FOR HEAT TRANSFER.
!
!     DMAP CALLING SEQUENCE.
!
!     SSGHT  USET,SIL,GPTT,GM,EST,MPT,DIT,PF,PS,KFF,KFS,KSF,KSS,RFN,RSN,
!            LFILE,UFILE/UGV,QG,RULV/V,N,NLK/V,N,NLR/C,Y,EPS0/C,Y,TABS/
!            C,Y,MAXITR/C,Y,IRES/V,N,MPCF1/V,N,SINGLE $
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Outpt) , (Ksystm(10),Tset) , (Ksystm(55),Iprec1) , (buf(1),rbuf(1)) , (Z(1),Rz(1)) , &
!>>>>    & (idfalt,defalt)
   DATA diagon/.FALSE./
   DATA subr/4HSSGH , 4HT   / , eor , noeor/1 , 0/
   DATA uset , gptt , gm , est , mptfil/101 , 103 , 104 , 105 , 106/
!     DATA    SIL   / 102     /
   DATA dit , pf , ps , kff , kfs , ksf , kss/107 , 108 , 109 , 110 , 111 , 112 , 113/
   DATA rfn , rsn , lfile , ufile/114 , 115 , 116 , 117/
   DATA ugv , qg , rulv/201 , 202 , 203/
   DATA scrt1 , scrt2 , scrt3 , scrt4/301 , 302 , 303 , 304/
   DATA alibi/4H NOR , 4HMAL  , 4HCONV , 4HERGE , 4HNCE  , 4H MAX , 4HIMUM , 4H ITE , 4HRATI , 4HONS  , 4H DIV , 4HERGI , 4HNG S ,  &
       &4HOLUT , 4HION  , 4H INS , 4HUFFI , 4HCIEN , 4HT TI , 4HME   , 4H MAX , 4HIMUM , 4H CON , 4HVERG , 4HENCE/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     OPEN CORE
!
!          PRE-ITERATIONS              DURING-ITERATIONS
!         +--------------+
!         I              I  Z(IUNI)
!         I    I         I
!         I  (U ) VECTOR I
!         I    N         I
!         I              I  Z(NUNI)
!         +--------------+
!         I              I  Z(IHMAT)
!         I  HMAT CORE   I
!         I  BLOCK IF    I
!         I  REQUIRED    I
!         I              I  Z(NHMAT)
!         +--------------+
!         I              I  Z(IUN)
!         I (U ) PARTIT. I
!         I   N  VECTOR  I
!         I      FOR F+S I
!         I              I  Z(NUN)
!         +--------------+  -  -  -  -  -  -  -  -  -  -  -  -  -
!         I              I  Z(IEQIV)        MSIZE
!         I              I           -  -  -  *  -  -  EQUIV TABLE
!         I   E          I                   *I*       WILL BE PLACED
!         I(UN )EQIV.TBL I                  * I *      ON SCRATCH FILE
!         I   G          I                    I        DURING ITERATIONS
!         I              I  Z(NEQIV)          I
!         +--------------+  -  -  -   +----------------+
!         I              I  Z(IUM)    I                I  Z(ISN)
!         I (U )PARTIT.  I            I (S ) DIAGONAL  I
!         I   M VECTOR   I            I   N            I
!         I              I  Z(NUM)    I                I
!         +--------------+            I                I  Z(NSN)
!         I              I  Z(IUME)   +----------------+
!         I   E          I            I                I  Z(IDELU)
!         I (U ) TABLE   I            I (DELTA U ) VEC.I
!         I   M          I  Z(NUME)   I         N      I
!         +--------------+            I                I  Z(NDELU)
!                                     +----------------+
!                                     I                I  Z(IDELP)
!                                     I (DELTA P ) VEC.I
!                                     I         N      I
!                                     I                I  Z(NDELP)
!                                     +----------------+
!                                            .
!                                      CORE  . UNUSED
!                           Z(CORE)          .            Z(CORE)
!         - - - - - - - - - - - - - - +----------------+
!                                     I  BUFFER 2      I  Z(BUF2)
!                                     I                I
!                                     +----------------+
!                                     I  BUFFER 1      I  Z(BUF1)
!                                     I                I
!                                     +----------------+
!
!     CORE SIZE AND BUFFERS
!
         lcore = korsz(Z)
         buf1 = lcore - sysbuf - 2
         buf2 = buf1 - sysbuf - 2
         core = buf2 - 1
         IF ( core<100 ) CALL mesage(-8,0,subr)
         precis = 1
!
!     SET MISC. FLAGS.
!
         eps010 = 10.0*Eps0
         epsold = Eps0 + 1.0
         nlrad = .TRUE.
         IF ( Nlr==-1 ) nlrad = .FALSE.
         CALL sswtch(18,k)
         IF ( k==1 ) diagon = .TRUE.
         linear = .TRUE.
         IF ( Nlk==+1 ) linear = .FALSE.
!
!     READ TRAILER OF USET TO GET GSIZE.
!
         mcb(1) = uset
         CALL rdtrl(mcb)
         file = uset
         IF ( mcb(1)<=0 ) GOTO 460
         gsize = mcb(3)
!
!     READ GM TRAILER TO DETERMINE COUNT OF UM POINTS
!
         mcb(1) = gm
         CALL rdtrl(mcb)
         IF ( mcb(1)<=0 ) THEN
            msize = 0
         ELSE
            msize = mcb(3)
         ENDIF
         nsize = gsize - msize
!
!     CORE ALLOCATION.
!
         iuni = 1
         nuni = nsize
         iuniz = iuni - 1
         Ihmat = nuni + 1
         Nhmat = nuni
         IF ( .NOT.linear ) Nhmat = core
         Mptx = mptfil
         Ditx = dit
         IF ( .NOT.linear ) CALL prehma(Z)
         iun = Nhmat + 1
         nun = Nhmat + nsize
         ieqiv = nun + 1
         neqiv = nun + gsize
!
!     EQUIVALENCE TABLE WILL BE PUT ON SCRATCH DURING ITERATIONS.
!
         isn = nun + msize + 1
         nsn = isn + nsize - 1
         IF ( .NOT.nlrad ) nsn = isn - 1
         isnz = isn - 1
         idelu = nsn + 1
         ndelu = nsn + nsize
         ideluz = idelu - 1
         idelp = ndelu + 1
         ndelp = ndelu + nsize
         IF ( ndelp>core ) CALL mesage(-8,0,subr)
         idelpz = idelp - 1
         ium = neqiv + 1
         num = neqiv + msize
         iumz = ium - 1
         iume = num + 1
         nume = num + msize
         iumez = iume - 1
         IF ( nume>core ) CALL mesage(-8,0,subr)
!
!     CONSTRUCTION OF (U ) AND (U ) TABLES.
!                       M        N
!
         file = uset
         CALL gopen(uset,Z(buf1),Rdrew)
         mpoint = iumz
         npoint = iun - 1
         isil = 0
         fsize = 0
         CALL fread(uset,Z(ieqiv),gsize,0)
         CALL close(uset,Clsrew)
         DO i = ieqiv , neqiv
            word = Z(i)
            isil = isil + 1
!
!     CHECK FOR M-POINT
!
            IF ( mod(word,2)<=0 ) THEN
!
!     ASSUME N-POINT
!
               npoint = npoint + 1
!
!     CHECK FOR F OR S POINT
!
               IF ( mod(word/2,2)<=0 ) THEN
!
!     OK N-POINT IS AN F-POINT.
!
                  Z(npoint) = -isil
                  fsize = fsize + 1
               ELSE
!
!     OK N-POINT IS ASSUMED AN S-POINT
!
                  Z(npoint) = +isil
               ENDIF
            ELSE
               mpoint = mpoint + 1
               Z(mpoint) = isil
            ENDIF
         ENDDO
         ssize = nsize - fsize
!
!     U  AND U  ARE COMPLETE.
!      M      N
!
         IF ( isil/=gsize .OR. npoint/=nun .OR. mpoint/=num ) THEN
            WRITE (outpt,99001) Sfm
99001       FORMAT (A25,' 3081, INCONSISTENT USET DATA DETECTED.')
            CALL mesage(-61,0,subr)
         ENDIF
!
!             E
!     BUILD (U ) EQUIVALENCE (U ) POINTS FOR (U ).
!             M                N               M
!
         IF ( nume>=iume ) THEN
            DO i = iume , nume
               Z(i) = 0
            ENDDO
            CALL gopen(gm,Z(buf1),Rdrew)
            DO i = 1 , nsize
!
!     OPERATE ON A COLUMN OF GM.
!
               CALL intpk(*10,gm,0,precis,0)
               SPAG_Loop_2_1: DO
                  CALL zntpki
!
!                             E
!     ROW POSITION -IROW- IN U  GETS COLUMN NUMBER.
!                             M
!
                  ipos = iumez + Irow
                  IF ( Z(ipos)<=0 ) THEN
                     Z(ipos) = i
                  ELSE
!
!     ERROR
!
                     WRITE (outpt,99002) Uwm , Irow , i
99002                FORMAT (A25,' 3082, M =',I10,'  N =',I10)
                  ENDIF
                  IF ( Eol>0 ) EXIT SPAG_Loop_2_1
               ENDDO SPAG_Loop_2_1
 10         ENDDO
            CALL close(gm,Clsrew)
!
!     INSURE ALL UME SLOTS FILLED
!
            nogo = .FALSE.
            DO i = iume , nume
               IF ( Z(i)<=0 ) THEN
                  m = i - iumez
                  isil = iumz + m
                  WRITE (outpt,99003) Ufm , m , Z(isil)
99003             FORMAT (A23,' 3083, UM POSITION =',I10,', SIL =',I10)
                  nogo = .TRUE.
               ENDIF
            ENDDO
            IF ( nogo ) CALL mesage(-61,0,subr)
         ENDIF
!
!                        E
!     CONSTRUCTION OF (UN ) EQUIVALENCE TABLE.
!                        G
!
         mpoint = ium
         mpt = Z(mpoint)
         IF ( mpoint>num ) mpt = 1000000
         mpte = iume
         mval = Z(mpte)
         nval = 1
         k = ieqiv - 1
         DO i = 1 , gsize
            k = k + 1
            IF ( i/=mpt ) THEN
!
!     N-POINT NEXT
!
               Z(k) = nval
               nval = nval + 1
            ELSE
!
!     M-POINT NEXT
!
               Z(k) = mval
               mpte = mpte + 1
               mval = Z(mpte)
               mpoint = mpoint + 1
               mpt = Z(mpoint)
               IF ( mpoint>num ) mpt = 1000000
            ENDIF
         ENDDO
!
!     SET UP RULV IF RESIDUAL LOAD MATRIX IS TO BE FORMED.
!
         rulvec = .FALSE.
         IF ( Ires>0 ) THEN
            CALL makmcb(rulmcb,rulv,fsize,2,precis)
            CALL gopen(rulv,Z(buf1),Wrtrew)
            CALL close(rulv,Cls)
            rulvec = .TRUE.
         ENDIF
!
!     GRID POINT TEMPERATURE DATA IS EXPANDED INTO CORE NOW.  ONLY
!
!      1
!     U  IS FORMED.
!      N
!
!
         IF ( tset<=0 ) THEN
            k = 0
         ELSE
            k = 1
         ENDIF
         DO i = iuni , nuni
            Z(i) = k
         ENDDO
         IF ( tset<=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     POSITION GPTT TO GRID TEMPERATURE DATA SECTION.
!
         file = gptt
         CALL open(*460,gptt,Z(buf1),Rdrew)
         CALL fread(gptt,buf,-2,0)
         number = 0
         DO
            CALL read(*480,*20,gptt,buf,3,noeor,flag)
            number = max0(number,buf(3))
         ENDDO
 20      CALL skprec(gptt,number)
!
!     NOW AT GRID TEMP SECTION HEADER.
!
         CALL fread(gptt,buf,-2,0)
         SPAG_Loop_1_2: DO
            CALL read(*480,*500,gptt,buf,3,noeor,flag)
            IF ( tset==buf(1) ) THEN
!
!     BUF(1)=SET-ID,  BUF(2)=-1 OR DEFAULT TEMP,  BUF(3)=GPTT RECORD.
!
               defalt = rbuf(2)
               IF ( buf(3)<=0 ) GOTO 40
               CALL skprec(gptt,buf(3))
!
!     TEMP PAIRS IN INTERNAL-ID AND TEMPERATURE.
!
               iunat = iun
               isil = iabs(Z(iunat))
               iuniat = iuni
!
!     READ A TEMPERATURE PAIR.
!
               CALL read(*480,*40,gptt,buf,2,noeor,flag)
               EXIT SPAG_Loop_1_2
            ENDIF
         ENDDO SPAG_Loop_1_2
         SPAG_Loop_1_3: DO
            IF ( buf(1)<isil ) THEN
               CALL read(*480,*40,gptt,buf,2,noeor,flag)
               CYCLE
            ELSEIF ( buf(1)==isil ) THEN
               Z(iuniat) = buf(2)
            ENDIF
            iunat = iunat + 1
            isil = iabs(Z(iunat))
            iuniat = iuniat + 1
            IF ( iuniat>nuni ) EXIT SPAG_Loop_1_3
         ENDDO SPAG_Loop_1_3
 40      CALL close(gptt,Clsrew)
!
!     CHECK FOR INTEGER 1-S WHICH GET THE DEFAULT TEMP.
!
         nogo = .FALSE.
         DO i = iuni , nuni
            IF ( Z(i)==1 ) THEN
               IF ( idfalt/=-1 ) THEN
                  rz(i) = defalt
               ELSE
                  nogo = .TRUE.
                  k = iun + i - iuni
                  isil = iabs(Z(k))
                  WRITE (outpt,99004) Ufm , isil
99004             FORMAT (A23,' 3084, THERE IS NO TEMPERATURE DATA FOR SIL NUMBER',I10)
               ENDIF
            ENDIF
         ENDDO
         IF ( nogo ) CALL mesage(-61,0,subr)
         spag_nextblock_1 = 2
      CASE (2)
!
!               1                  1
!     COMPUTE (P ) = (P ) - (K  )(U ) AND SAVE ON SCRATCH-4.
!               F      F      FS   S
!
         k = idelpz + fsize
         DO i = idelp , k
            Z(i) = 0
         ENDDO
         CALL open(*60,pf,Z(buf1),Rdrew)
         CALL fwdrec(*60,pf)
         CALL intpk(*60,pf,0,precis,0)
         SPAG_Loop_1_4: DO
            CALL zntpki
            k = idelpz + Irow
            rz(k) = Ai(1)
            IF ( Eol>0 ) EXIT SPAG_Loop_1_4
         ENDDO SPAG_Loop_1_4
 60      CALL close(pf,Clsrew)
!
!                         1
!     SUBTRACT OFF (K  )(U )
!                    FS   S
!
         iat = iun - 1
         CALL open(*100,kfs,Z(buf1),Rdrew)
         CALL fwdrec(*100,kfs)
         DO i = 1 , ssize
            SPAG_Loop_2_5: DO
!
!     FIND NEXT US POINT TEMPERATURE DATA.
!
               iat = iat + 1
               IF ( Z(iat)>0 ) THEN
                  k = iuniz + iat - iun + 1
                  CALL intpk(*80,kfs,0,precis,0)
                  value = rz(k)
                  DO
                     CALL zntpki
                     k = idelpz + Irow
                     rz(k) = rz(k) - Ai(1)*value
                     IF ( Eol>0 ) EXIT SPAG_Loop_2_5
                  ENDDO
               ENDIF
            ENDDO SPAG_Loop_2_5
 80      ENDDO
 100     CALL close(kfs,Clsrew)
!
!                1
!     PACK OUT (P ) ON SCRATCH-4
!                F
!
         CALL gopen(scrt4,Z(buf1),Wrtrew)
         CALL makmcb(mcb,scrt4,fsize,2,precis)
         Pkin = precis
         Pkout = precis
         Pkirow = 1
         Pknrow = fsize
         Pkincr = 1
         CALL pack(Z(idelp),scrt4,mcb)
         CALL close(scrt4,Clsrew)
         CALL wrttrl(mcb)
!
!     ELEMENT INITIAL PROCESSING PHASE.
!
         CALL gopen(scrt1,Z(buf2),Wrtrew)
         IF ( .NOT.(linear) ) THEN
            CALL gopen(est,Z(buf1),Rdrew)
            CALL ssght1(est,scrt1,Z(ieqiv))
            CALL close(est,Clsrew)
         ENDIF
!
!        E
!     (UN ) EQUIVALENCE TABLE IS NOW APPENDED TO -SCRT1-.
!        G
!
         CALL write(scrt1,0,0,1)
         CALL write(scrt1,Z(ieqiv),gsize,1)
         CALL close(scrt1,Clsrew)
!
!                      1            3
!     FORM (S ) = 4( (U ) + (TABS) )  DIAGONAL MATRIX.
!            N         N
!
         IF ( nlrad ) THEN
            j = iuniz
            DO i = isn , nsn
               j = j + 1
               rz(i) = 4.0*(rz(j)+Tabs)**3
            ENDDO
         ENDIF
!
!     SET PARTITIONING TABLE IN TERMS OF WHERE ELEMENTS ARE TO MOVE TO
!     WHEN GOING FROM N-SET TO F+S SETS.
!
         is = fsize
         if = 0
         DO i = iun , nun
            IF ( Z(i)<=0 ) THEN
!
!     F-POINTER
!
               if = if + 1
               Z(i) = if
            ELSE
!
!     S-POINTER
!
               is = is + 1
               Z(i) = is
            ENDIF
         ENDDO
         loop = 0
         loop1 = .TRUE.
         pfmag = 0.0
         spag_nextblock_1 = 3
      CASE (3)
!
!     == ITERATION SECTION ==
!
!     ITERATIVE LOOPING
!
         loop = loop + 1
!
!     TIME LEFT AT START OF LOOP
!
         CALL tmtogo(tstart)
         DO i = idelp , ndelp
            Z(i) = 0
         ENDDO
         IF ( .NOT.(loop1 .OR. linear) ) THEN
            CALL gopen(scrt1,Z(buf1),Rdrew)
            CALL ssght2(scrt1,Z(idelp),Z(iuni))
            CALL close(scrt1,Clsrew)
!
!     PARTITION DELTA-P VECTOR INTO DELTA-F AND DELTA-S VECTORS.
!
            CALL ssghtp(Z(iun),Z(idelp),nsize)
         ENDIF
!
!                     I
!     GENERATION OF (N ) WILL BE PERFORMED IN CORE SPACE OF (DELTA-P)
!                     F
!       I                          I        4         I
!     (N ) = (DELTA-P ) + (R  )( (U  + TABS)  - (S )(U ) )
!       F            F      FN     N               N  N
!
         IF ( .NOT.nlrad ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL open(*140,rfn,Z(buf2),Rdrew)
         CALL fwdrec(*140,rfn)
         DO i = 1 , nsize
!
!     OPERATE ON A COLUMN OF RFN
!
            CALL intpk(*120,rfn,0,precis,0)
!
!     COMPUTE CONSTANT FOR COLUMN
!
            k = iuniz + i
            un = rz(k)
            k = isnz + i
            sn = rz(k)
            value = (un+Tabs)**4 - sn*un
            SPAG_Loop_2_6: DO
!
!     UNPACK NON-ZERO TERMS OF COLUMN.
!
               CALL zntpki
               k = idelpz + Irow
               rz(k) = rz(k) + Ai(1)*value
               IF ( Eol>0 ) EXIT SPAG_Loop_2_6
            ENDDO SPAG_Loop_2_6
 120     ENDDO
 140     CALL close(rfn,Clsrew)
         spag_nextblock_1 = 4
      CASE (4)
!
!          I      1      I
!     (PBAR ) = (P ) - (N )
!          F      F      F
!                    I
!     FIRST NEGATE (N ) SITTING IN DELTA-P CORE SPACE,
!                    F
!                                     1
!     THEN ADD IN NON-ZERO TERMS OF (P )
!                                     F
!
         k = idelpz + fsize
         DO i = idelp , k
            rz(i) = -rz(i)
         ENDDO
!
!            1
!     OPEN (P ) FOR UNPACKING OF ONE COLUMN.
!            F
!
         CALL open(*160,scrt4,Z(buf2),Rdrew)
         CALL fwdrec(*160,scrt4)
         CALL intpk(*160,scrt4,0,precis,0)
         SPAG_Loop_1_7: DO
            CALL zntpki
            k = idelpz + Irow
            rz(k) = rz(k) + Ai(1)
            IF ( loop1 ) pfmag = pfmag + Ai(1)*Ai(1)
            IF ( Eol>0 ) EXIT SPAG_Loop_1_7
         ENDDO SPAG_Loop_1_7
 160     CALL close(scrt4,Clsrew)
!
!          I
!     (PBAR ) IS NOW PACKED OUT TO SCRATCH-2.
!          F
!
         IF ( loop1 ) THEN
            pfmag = sqrt(pfmag)
            IF ( pfmag<=0 ) THEN
               WRITE (outpt,99005) Ufm
99005          FORMAT (A23,' 3085, THE PF LOAD VECTOR IS EITHER PURGED OR NULL.')
               CALL mesage(-61,0,subr)
            ENDIF
         ENDIF
         CALL makmcb(mcb2,scrt2,fsize,2,2)
         CALL gopen(scrt2,Z(buf2),Wrtrew)
         Pkin = precis
         Pkout = iprec1
         Pkirow = 1
         Pknrow = fsize
         Pkincr = 1
         CALL pack(Z(idelp),scrt2,mcb2)
         CALL close(scrt2,Clsrew)
         CALL wrttrl(mcb2)
!
!                       I           I
!     (DELTA-P ) = (PBAR ) - (K  )(U )
!             F         F      FF   F
!          I
!     (PBAR ) IS SITING IN CORE CURRENTLY.  (IT WILL BE GONE TOMORROW.)
!          F
!                       I       I        I
!     FIRST PARTITION (U ) TO (U ) AND (U )
!                       N       F        S
!
         CALL ssghtp(Z(iun),Z(iuni),nsize)
         CALL open(*200,kff,Z(buf1),Rdrew)
         CALL fwdrec(*200,kff)
         DO i = 1 , fsize
!
!     OPERATE ON ONE COLUMN OF KFF
!
            CALL intpk(*180,kff,0,precis,0)
!
!                                 I
!     LOCATE COLUMN MULTIPLIER = U
!                                 FI
!
            k = iuniz + i
            value = rz(k)
            SPAG_Loop_2_8: DO
               CALL zntpki
!                                                            I
!     SUBTRACT THIS ELEMENT*VALUE FROM IROW POSITION OF (PBAR )
!                                                            F
               k = idelpz + Irow
               rz(k) = rz(k) - Ai(1)*value
               IF ( Eol>0 ) EXIT SPAG_Loop_2_8
            ENDDO SPAG_Loop_2_8
 180     ENDDO
 200     CALL close(kff,Clsrew)
!
!     COMPUTE EPSILON
!                    P
!
         k = idelpz + fsize
         sum = 0.0
         DO i = idelp , k
            sum = sum + rz(i)**2
         ENDDO
         sum = sqrt(sum)
         epsubp = sum/pfmag
         IF ( loop1 .AND. diagon ) WRITE (outpt,99006) epsubp
99006    FORMAT ('1D I A G   1 8   O U T P U T   F R O M   S S G H T',//,' ITERATION    EPSILON-P',9X,'LAMBDA-1',10X,'EPSILON-T',   &
               & /1X,60(1H=),/,6H     1,1P,E19.6)
!
!                                                   I
!     IF -RULV- IS BEING FORMED, THEN WRITE (DELTA-P ) OUT ON -RULV-.
!                                                   F
!
         IF ( rulvec ) THEN
            CALL open(*220,rulv,Z(buf1),Wrt)
            Pkin = precis
            Pkout = precis
            Pkirow = 1
            Pknrow = fsize
            Pkincr = 1
            CALL pack(Z(idelp),rulv,rulmcb)
            CALL close(rulv,Cls)
         ENDIF
!
!                     I+1
!     NOW SOLVE FOR (U   ) IN,
!                     F
!                           I+1         I
!                   (L)(U)(U   ) = (PBAR )
!                           F           F
!
!
 220     Isign = +1
         Iprec = 2
         Lmcb(1) = lfile
         CALL rdtrl(Lmcb)
         Umcb(1) = ufile
         CALL rdtrl(Umcb)
         Bmcb(1) = scrt2
         CALL rdtrl(Bmcb)
         CALL makmcb(Xmcb,scrt3,fsize,2,2)
!
!     INSURE EVEN BOUNDARY (ARRAY WILL BE USED AS DOUBLE PRECISION)
!
         jdelp = ndelp + 1 + mod(ndelp+1,2) + 1
         Lz = lcore - jdelp
!WKBI 3/94
         Jzzz = Lz
         DO ijk = 1 , 31
            Jlmcb(ijk) = Lmcb(ijk)
         ENDDO
         IF ( Umcb(1)>0 ) CALL gfbs(Z(jdelp),Z(jdelp))
         IF ( Umcb(1)<=0 ) CALL fbs(Z(jdelp),Z(jdelp))
         IF ( Umcb(1)>0 ) CALL wrttrl(Xmcb)
         IF ( Umcb(1)<=0 ) CALL wrttrl(Jxmcb)
!
!       I+1
!     (U   ) IS NOW MOVED FROM SCRATCH-3 INTO CORE IN (DELTA-P ) SPACE.
!       F                                                     N
!
         CALL gopen(scrt3,Z(buf1),Rdrew)
         k = idelpz + fsize
         DO i = idelp , k
            Z(i) = 0
         ENDDO
         CALL intpk(*240,scrt3,0,precis,0)
         SPAG_Loop_1_9: DO
            CALL zntpki
            k = idelpz + Irow
            rz(k) = Ai(1)
            IF ( Eol>0 ) EXIT SPAG_Loop_1_9
         ENDDO SPAG_Loop_1_9
 240     CALL close(scrt3,Clsrew)
         IF ( loop1 ) THEN
!
!                   I
!     COMPUTE (DELTA ) TO BE USED ON NEXT LOOP
!                   U
!
            iexit = 2
            IF ( loop<Maxitr ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     == END ITERATION SECTION ==
!
!     ITERATION HALTED, NOW IN EXIT MODE.
!     IF QG FILE IS PRESENT, FORCES OF CONSTRAINT ARE PARTIALLY COMPUTED
!     QS WILL BE FORMED IN THE CORE SPACE USED UP TO NOW FOR (DELTA-U).
!
!                           I
!     (Q ) = -(P ) + (K  )(U ) + (K  )(U ) + (DELTA-P ) + (PRODUCT )
!       S       S      SF   F      SS   S            S            S
!
!                                 I         4        I
!     WHERE (PRODUCT ) = (R  )( (U   + TABS)  - (S  U ) )
!                   S      SN     NJ              NJ N
!
!                                      J = 1,NSIZE
!
!     LOAD (DELTA-P ) INTO QS FORMATION CORE SPACE.
!                  S
!
            WRITE (outpt,99007) Uwm
99007       FORMAT (A25,' 3132, SSGHT RECOVERING FROM SEVERE USER CONVERGENCE',' CRITERIA.')
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!                      I+1       I
!     ALPHA = SUM OF (U   ) (PBAR )          IROW = 1,FSIZE
!                      F         F
!                       IROW      IROW
!
!                           I+1      I
!     BETA = SUM OF (DELTA-U   )(PBAR )      IROW = 1,FSIZE
!                           F        F
!                            IROW     IROW
!
!                      I+1    I       I
!     GAMMA = SUM OF (U    - U  )(PBAR )     IROW = 1,FSIZE
!                      F      F       F
!                       IROW   IROW    IROW
!
!     WHERE I = ITERATION GREATER THAN 1.
!
            CALL gopen(scrt2,Z(buf1),Rdrew)
            alpha = 0.0
            beta = 0.0
            gamma = 0.0
            CALL intpk(*260,scrt2,0,precis,0)
            SPAG_Loop_1_10: DO
!
!     ONLY NON-ZERO TERMS OF (PBAR ) NEED BE CONSIDERED.
!                                 F
               CALL zntpki
               kufip1 = idelpz + Irow
               kdelu = ideluz + Irow
               kufi = iuniz + Irow
               alpha = alpha + rz(kufip1)*Ai(1)
               beta = beta + rz(kdelu)*Ai(1)
               gamma = gamma + (rz(kufip1)-rz(kufi))*Ai(1)
               IF ( Eol>0 ) EXIT SPAG_Loop_1_10
            ENDDO SPAG_Loop_1_10
         ENDIF
 260     CALL close(scrt2,Clsrew)
!
!     CONVERGENCE TESTS ARE MADE HERE.
!
!     WHEN ENTERING EXIT MODE,
!         -IEXIT-        -REASON-
!            1           NORMAL CONVERGENCE
!            2           NO CONVERGENCE AT MAXIMUM ITERATIONS
!            3           NO CONVERGENCE UNSTABLE ITERATION
!            4           NO CONVERGENCE INSUFFICIENT TIME
!            5           MAXIMUM CONVERGENCE, BUT EPSHT NOT SATISFIED
!
         IF ( gamma/=0 ) THEN
            flamda = abs(beta/gamma)
            IF ( alpha==0 ) THEN
               epst = 100.0
            ELSEIF ( flamda/=1.0 ) THEN
               epst = abs(gamma/((flamda-1.0)*alpha))
            ELSE
               epst = 100.0
            ENDIF
         ELSE
            flamda = 100.0
            epst = 0.0
         ENDIF
         CALL tmtogo(kleft)
         telaps = Treqst - kleft
         tau = 1.0 - float(tloop+telaps)/(.8*float(Treqst))
         IF ( diagon ) WRITE (outpt,99008) loop , epsubp , flamda , epst
99008    FORMAT (I6,1P,E19.6,1P,E18.6,1P,E18.6)
         iexit = 1
         IF ( epst<Eps0 .AND. flamda>1.0 .AND. epsubp<eps010 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     TEST FOR TWO SUCCESSIVE CASES PASSING TEST
!
         IF ( epst<Eps0 .AND. epsold<Eps0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         epsold = epst
         iexit = 2
         IF ( loop>=Maxitr ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         iexit = 3
         IF ( flamda<=1.0 .AND. loop>=4 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         iexit = 5
         IF ( gamma==0. ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         iexit = 4
         IF ( tau<0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         k = idelpz + fsize
         kdelu = ideluz
         ki = iuniz
         kip1 = idelpz
         DO i = idelp , k
            kdelu = kdelu + 1
            ki = ki + 1
            kip1 = kip1 + 1
            rz(kdelu) = rz(kip1) - rz(ki)
         ENDDO
!
!                       I+1
!     MOVE (U ) UNDER (U   ) BOTH TO BE IN (DELTA-P ) CORE.
!            S          F                          N
!
         ASSIGN 280 TO iretrn
         spag_nextblock_1 = 6
      CASE (6)
         k1 = iuni + fsize
         k2 = idelpz + fsize
         IF ( ssize>0 ) THEN
            DO i = k1 , nuni
               k2 = k2 + 1
               rz(k2) = rz(i)
            ENDDO
         ENDIF
!
!             I+1                       I+1
!     MERGE (U   ) AND (U ) BACK INTO (U   ) FORM.
!             F          S              N
!
         kuni = iuniz
         DO i = iun , nun
            kuni = kuni + 1
            jpos = idelpz + Z(i)
            Z(kuni) = Z(jpos)
         ENDDO
         GOTO iretrn
!
!     READY NOW FOR ANOTHER LOOP.
!
 280     CALL tmtogo(tend)
         tloop = tstart - tend
         loop1 = .FALSE.
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
         WRITE (outpt,99009) Uim , iexit , (alibi(j,iexit),j=1,5)
99009    FORMAT (A29,' 3086, ENTERING SSGHT EXIT MODE BY REASON NUMBER ',I2,2H (,5A4,1H))
         noqg = .TRUE.
         CALL open(*380,qg,Z(buf2),Wrtrew)
         noqg = .FALSE.
         CALL fname(qg,name)
         CALL write(qg,name,2,eor)
         iqs = idelu
         nqs = ideluz + ssize
         iqsz = ideluz
         k = idelpz + fsize
         DO i = iqs , nqs
            k = k + 1
            Z(i) = Z(k)
         ENDDO
!
!     SUBTRACT OFF NON-ZERO TERMS OF PS VECTOR.
!
         CALL open(*300,ps,Z(buf1),Rdrew)
         CALL fwdrec(*300,ps)
         CALL intpk(*300,ps,0,precis,0)
         SPAG_Loop_1_11: DO
            CALL zntpki
            k = iqsz + Irow
            rz(k) = rz(k) - Ai(1)
            IF ( Eol>0 ) EXIT SPAG_Loop_1_11
         ENDDO SPAG_Loop_1_11
 300     CALL close(ps,Clsrew)
!
!                   I
!     ADD IN (K  )(U )
!              SF   F
!
         CALL open(*340,ksf,Z(buf1),Rdrew)
         CALL fwdrec(*340,ksf)
         DO i = 1 , fsize
            CALL intpk(*320,ksf,0,precis,0)
            k = idelpz + i
            value = rz(k)
            SPAG_Loop_2_12: DO
               CALL zntpki
               k = iqsz + Irow
               rz(k) = rz(k) + Ai(1)*value
               IF ( Eol>0 ) EXIT SPAG_Loop_2_12
            ENDDO SPAG_Loop_2_12
 320     ENDDO
 340     CALL close(ksf,Clsrew)
!
!     ADD IN (K  )(U )
!              SS   S
!
         IF ( ssize/=0 ) THEN
            CALL open(*360,kss,Z(buf1),Rdrew)
            CALL fwdrec(*360,kss)
            iusz = iuniz + fsize
            DO i = 1 , ssize
               CALL intpk(*350,kss,0,precis,0)
               k = iusz + i
               value = rz(k)
               SPAG_Loop_2_13: DO
                  CALL zntpki
                  k = iqsz + Irow
                  rz(k) = rz(k) + Ai(1)*value
                  IF ( Eol>0 ) EXIT SPAG_Loop_2_13
               ENDDO SPAG_Loop_2_13
 350        ENDDO
         ENDIF
 360     CALL close(kss,Clsrew)
!
!                                     I
!     TO COMPUTE ADDITIONAL PRODUCT (U ) IS NOW FORMED.
!                                     N
!                           I
!     THUS MERGE (U ) AND (U )
!                  S        F
!                                  I
!     FIRST MOVE (U ) DOWN UNDER (U ), THEN DO MERGE.
!                  S               F
!
 380     ASSIGN 400 TO iretrn
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!     OK FORM AND ADD (PRODUCT) IN.
!
 400     IF ( .NOT.nlrad ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( noqg ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL open(*440,rsn,Z(buf1),Rdrew)
         CALL fwdrec(*440,rsn)
         DO i = 1 , nsize
            CALL intpk(*420,rsn,0,precis,0)
            ku = iuniz + i
            ks = isnz + i
            value = (rz(ku)+Tabs)**4 - rz(ku)*rz(ks)
            SPAG_Loop_2_14: DO
               CALL zntpki
               k = iqsz + Irow
               rz(k) = rz(k) + Ai(1)*value
               IF ( Eol>0 ) EXIT SPAG_Loop_2_14
            ENDDO SPAG_Loop_2_14
 420     ENDDO
 440     CALL close(rsn,Clsrew)
         spag_nextblock_1 = 8
      CASE (8)
!
!     (QS) IS COMPLETE AND READY FOR EXPANSION TO GSIZE AND OUTPUT.
!
         CALL makmcb(mcb,qg,gsize,2,precis)
         Jrow = 0
         file = uset
         IF ( ssize/=0 ) THEN
            CALL gopen(uset,Z(buf1),Rdrew)
            iq = iqs
            CALL bldpk(precis,precis,qg,0,0)
            SPAG_Loop_1_15: DO
               CALL fread(uset,word,1,0)
               Jrow = Jrow + 1
               IF ( mod(word/2,2)>0 ) THEN
                  Ao(1) = rz(iq)
                  CALL zblpki
                  iq = iq + 1
                  IF ( iq>nqs ) THEN
!
!     QS HAS NOW BEEN EXPANDED TO GSIZE AND OUTPUT ON QG DATA BLOCK.
!
                     CALL bldpkn(qg,0,mcb)
                     CALL close(qg,Clsrew)
                     CALL wrttrl(mcb)
                     CALL close(uset,Clsrew)
                     EXIT SPAG_Loop_1_15
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_15
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
!
!     PACK OUT (U ) USING THE EQUIVALENCE TABLE TO ORDER
!                G
!
!     THE U  POINTS.
!          N
!
!
!     READ EQUIVALENCE TABLE BACK INTO CORE AT THIS TIME.
!
         file = scrt1
         CALL gopen(scrt1,Z(buf1),Rdrew)
         CALL skprec(scrt1,1)
         CALL fread(scrt1,Z(ieqiv),gsize,0)
!
         CALL close(scrt1,Clsrew)
!
!     REPLACE POINTERS WITH THE VALUES.
!
         DO i = ieqiv , neqiv
            k = iuniz + Z(i)
            rz(i) = rz(k)
         ENDDO
!
!     PACK OUT (U )
!                G
!
         CALL makmcb(mcb,ugv,gsize,2,precis)
         CALL gopen(ugv,Z(buf1),1)
         Pkin = precis
         Pkout = precis
         Pkirow = 1
         Pknrow = gsize
         Pkincr = 1
         CALL pack(Z(ieqiv),ugv,mcb)
         CALL close(ugv,Clsrew)
         CALL wrttrl(mcb)
!
!     COMPLETE RULV IF NECESSARY.
!
         IF ( rulvec ) THEN
            CALL gopen(rulv,Z(buf1),3)
            CALL close(rulv,Clsrew)
            CALL wrttrl(rulmcb)
         ENDIF
         RETURN
!
!     ERROR CONDITIONS
!
 460     n = -1
         CALL mesage(n,file,subr)
         GOTO 500
 480     n = -2
         CALL mesage(n,file,subr)
 500     WRITE (outpt,99010) Ufm , tset
99010    FORMAT (A23,' 3087, TEMPERATURE SET',I10,' IS NOT PRESENT IN ','GPTT DATA BLOCK.')
         CALL mesage(-61,0,subr)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ssght
