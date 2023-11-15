
SUBROUTINE cthmck(Nt,Num,Nom,Io,Ig,Ic,Ideg,Idis,Iw,New,Icc,Ild,Ipp,Jump,Un,Nodesl)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Brms0 , Brms1 , Dum3b(3) , Dum6y(6) , Dumb2(2) , Rms0 , Rms1
   INTEGER I77 , Ib , Ibuf1 , Icrit , Ih , Ih0 , Ihe , Isys , Kdim , Knew , Korig , Maxgrd , Maxw0 , Maxw1 , Method , Mm , Ncm ,    &
         & Ngrid , Nlpp , Nn , Nodep , Nompc , Nopch , Norun , Nout
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /banda / Ibuf1 , Nompc , Nodep , Nopch , Norun , Method , Icrit
   COMMON /bandb / Dum3b , Ngrid , Dumb2 , Kdim
   COMMON /bandd / Korig , Knew , Ih0 , Ihe , Ncm
   COMMON /bands / Nn , Mm , Ih , Ib , Maxgrd
   COMMON /bandw / Maxw0 , Rms0 , Maxw1 , Rms1 , I77 , Brms0 , Brms1
   COMMON /system/ Isys , Nout , Dum6y , Nlpp
   COMMON /xmssg / Ufm , Uwm , Uim
!
! Dummy argument declarations
!
   INTEGER Io , Jump , Nom , Nt , Num
   INTEGER Ic(1) , Icc(1) , Ideg(1) , Idis(1) , Ig(1) , Ild(1) , Ipp(1) , Iw(1) , New(1) , Nodesl(1)
   REAL Un(1)
!
! Local variable declarations
!
   REAL averw , brms , crit1 , crit2 , im1 , im2 , rms
   INTEGER i , iajdim , idem , ij , is , j , jmax , k , k2 , ma , mad , maxd , maxlev , maxw , median , mi , mmc , modd , nc , nl , &
         & sumw
   INTEGER kompnt , maxdgr , mindeg
!
! End of declarations
!
!
!     THIS IS THE EXECUTIVE FOR THE CUTHILL-MCKEE GRID POINT RENUMBERING
!     STRATEGY.
!     91 VERSION, WITH REVERSED NEW SEQUENCE LOGIC
!
!     IN SAN ANTONIO, TEXAS, APRIL 27, 1989, THE DOUGLAS MICHEL NASTRAN
!     ACHIEVEMENT AWARD 1989, AN ANNUAL EVENT SPONSORED BY COSMIC AND
!     NASA, WAS GIVEN TO ELIZABETH H. CUTHILL, JAMES M. McKEE AND GORDON
!     C. EVERSTINE FOR THEIR TEAMWORK THAT CREATED BANDIT, A COMPUTER
!     PROGRAM THAT MINIMIZES THE BANDWIDTHS OF NASTRAN MATRICES. THE
!     WIDOW OF DR. McKEE AND HIS FAMILY RECEIVED THE AWARD FOR HIM.
!     DRS. CUTHILL AND EVERSTINE RECEIVED THEIR AWARDS PERSONALLY.
!
!     THE PRINCIPAL INPUTS ARE THE CONNECTIVITY MATRIX IG AND THE NUMBER
!     OF GRID POINTS (NODES) NN.
!
!     INPUT   - NT,NUM,NOM,IO,IP,IG,NN,MAXGRD,ILD
!     OUTPUT  - NEW,ILD,MM,IH0,IHE,KORIG,KNEW,NCM
!     SCRATCH - IC,IDEG,IDIS,IW,ICC,IPP
!
!     SET FOLLOWING DIMENSIONS IN CALLING PROGRAM -
!     IG(II1,M),IC(L),IDEG(L),IDIS(L),IW(L),NEW(L),ICC(L),ILD(L),IP(M)
!
!     L     = HAS THE DIMENSION OF MAXGRD
!             (NEW) MAXGRD EXCEEDS NUMBER OF GRID POINTS
!     II1   = MAXGRD/(PACKING DENSITY IN INTEGERS/WORD)
!           = ROW DIMENSION OF IG
!     M     = MAX NODAL DEGREE DIVIDED BY INTEGER PACKING FACTOR
!             (NEW) EXCEEDS MAX NODAL DEGREE
!     NT    = MAX NUMBER OF STARTING NODES TO BE CONSIDERED (=80)
!     NUM AND NOM GIVE THE FRACTION OF THE RANGE FROM MIN DEGREE TO MAX
!             DEGREE TO CONSIDER FOR STARTING NODES (NUM=1, NOM=2)
!     IO    = RE-SEQUENCING CRITERION , SET BY BANDIT -
!           = 1, RMS WAVEFRONT
!           = 2, BANDWIDTH
!           = 3, PROFILE. (PROFILE IS BANDWIDTH SUM OF ALL ROWS)
!           = 4, WAVEFRONT (MAX)
!     IG(I,J) CONTAINS THE GRID POINT LABEL FOR THE JTH NODE ADJACENT
!             TO NODE I  (THE CONNECTIVITY MATRIX).
!             THE CONNECTION OF A NODE TO ITSELF IS NOT LISTED.
!     NN    = NUMBER OF GRID POINTS (NODES)
!     MM    = COLUMN DIMENSION OF IG ON INPUT,
!             MAX NODAL DEGREE ON OUTPUT
!     MAXGRD= EFFECTIVE IG ROW DIMENSION (NEGLECTING INTEGER PACKING)
!     NEW(I)= OLD LABEL FOR GRID POINT NOW LABELLED I
!     ILD(I)= NEW LABEL FOR GRID POINT ORIGINALLY LABELLED I
!             ILD AND NEW ARE INVERSES
!     ILD MUST BE INPUT TO CTHMCK TO INDICATE AN INITIAL SEQUENCE.
!             NORMALLY, ON INPUT, SET ILD(I)=I FOR ALL I.
!     JUMP  = 1 IF RESEQUENCING ATTEMPTS RESULT IN NO IMPROVEMENT
!           = 0 OTHERWISE.
!     IH0   = ORIG PROFILE
!     IHE   = NEW PROFILE
!     KORIG = ORIG BANDWIDTH
!     KNEW  = NEW BW
!     NCM   = NUMBER OF COMPONENTS
!     NODESL IS SCRATCH SPACE.
!
!     IN CALLING PROGRAM, TRY  CALL CTHMCKL (80,1,2,2,1,...)
!
!     THE FOLLOWING SUBROUTINES WERE WRITTEN BY E. CUTHILL AND J. MCKEE
!     OF NSRDC -
!     DEGREE,DIAM,IDIST,KOMPNT,MAXBND,MAXDGR,MINDEG,RELABL,CTHMCK
!     CTHMCK WAS MODIFIED BY G.C. EVERSTINE, DTRC, AND
!        PUT INTO NASTRAN BY G.C. CHAN/UNISYS
!
!
!     SET UP SCRATCH SPACE NODESL.
!
   idem = Kdim
   k2 = idem + 1
   iajdim = 3*idem
!
!     DETERMINE THE DEGREE OF EACH NODE, THE NUMBER OF COMPONENTS, NCM,
!     AND THE MAXIMUM DEGREE OF ANY NODE.
!
   CALL degree(Ig,Ideg,Un)
   Ncm = kompnt(Ig,Ic,Ideg,Iw,Icc,Un)
   maxd = maxdgr(0,Ic,Ideg)
   mmc = maxd
!
!     INITIALIZE NEW ARRAY FROM THE ILD ARRAY.
!     ILD MUST BE INPUT TO CUTHILL.
!
   DO i = 1 , Nn
      k = Ild(i)
      New(k) = i
   ENDDO
!
!     COMPUTE ORIGINAL BANDWIDTH, PROFILE, WAVEFRONT AND ACTIVE COLUMN
!     IH0 = ORIGINAL PROFILE,  IS = ORIGINAL BW
!
   CALL wavey(Ig,Ild,New,0,Ic,Iw,is,maxw,averw,sumw,rms,brms,Un)
   Ih = sumw
   Maxw0 = maxw
   Rms0 = rms
   Brms0 = brms
   Korig = is
   Ih0 = Ih
   CALL page1
   i = Method + 2
   WRITE (Nout,99001) Uim , Icrit , i , Nompc , Nodep , Nopch
99001 FORMAT (A29,'S FROM RESEQUENCING PROCESSOR - BANDIT     (CRI=',I2,',  MTH=',I2,',  MPC=',I2,',  DEP=',I2,',  PCH=',I2,')',/)
   IF ( Nlpp>50 ) THEN
      WRITE (Nout,99002)
99002 FORMAT (31X,'BEFORE RESEQUENCING - - -')
      WRITE (Nout,99004) is , Ih , maxw , averw , rms , brms
   ENDIF
!
!     COMPUTE NODAL DEGREE STATISTICS.
!
   CALL dist(Ideg,Ipp,median,modd)
   IF ( Method==+1 ) RETURN
!
!     INITIALIZE ILD AND NEW ARRAYS.
!
   Jump = 0
   DO i = 1 , Nn
      New(i) = 0
      Ild(i) = 0
   ENDDO
!
!     GENERATE NUMBERING SCHEME FOR EACH COMPONENT, NC.
!
   DO nc = 1 , Ncm
!
!     DETERMINE THE RANGE OF DEGREES (MI TO MAD) OF NODES OF INTEREST.
!     MAKE SURE MAD DOES NOT EXCEED MEDIAN
!
      mi = mindeg(nc,Ic,Ideg)
      mad = mi
      IF ( Nom/=0 ) THEN
         ma = maxdgr(nc,Ic,Ideg)
         mad = mi + ((ma-mi)*Num)/Nom
         mad = min0(mad,median-1)
         mad = max0(mad,mi)
      ENDIF
!
!     DETERMINE BANDWIDTH OR SUM CRITERION FOR EACH NODE MEETING
!     SPECIFIED CONDITION.
!
      CALL diam(nc,mad,nl,Nodesl,idem,maxlev,Ig,Ic,Ideg,Idis,Iw,Icc,Un)
      jmax = min0(Nt,nl)
      jmax = max0(jmax,1)
      im1 = 1.E+8
      im2 = im1
!
!     CHECK SEQUENCE FOR EACH STARTING NODE SELECTED, AND
!     COMPUTE NEW BANDWIDTH,PROFILE,WAVEFRONT DATA.
!     IB = BANDWIDTH, IH = PROFILE.
!
      DO j = 1 , jmax
         CALL relabl(1,Nodesl(j),Ig,Ic,Ideg,Idis,Iw,New,Icc,Ild,Nodesl(k2),Un,iajdim)
         CALL wavey(Ig,Ild,New,nc,Ic,Iw,Ib,maxw,averw,sumw,rms,brms,Un)
         IF ( Ngrid==-1 ) RETURN
!
         Ih = sumw
         IF ( Io==2 ) THEN
            crit1 = Ib
            crit2 = Ih
         ELSEIF ( Io==3 ) THEN
            crit1 = Ih
            crit2 = Ib
         ELSEIF ( Io==4 ) THEN
            crit1 = maxw
            crit2 = rms
         ELSE
            crit1 = rms
            crit2 = Ih
         ENDIF
         IF ( im1<crit1 ) THEN
         ELSEIF ( im1==crit1 ) THEN
            IF ( im2>crit2 ) THEN
               im2 = crit2
               ij = j
            ENDIF
         ELSE
            im1 = crit1
            im2 = crit2
            ij = j
         ENDIF
!
      ENDDO
!
!     RECOMPUTE SEQUENCE FOR STARTING NODE WHICH IS BEST FOR CRITERION
!     SELECTED.
!
      CALL relabl(1,Nodesl(ij),Ig,Ic,Ideg,Idis,Iw,New,Icc,Ild,Nodesl(k2),Un,iajdim)
      IF ( Ngrid==-1 ) RETURN
!
   ENDDO
!
!     DETERMINE NODES OF ZERO DEGREE AND STACK LAST, AND
!     COMPUTE BANDWIDTH, PROFILE AND WAVEFRONT DATA.
!
   CALL stack(Ideg,New,Ild,Iw)
   CALL wavey(Ig,Ild,New,0,Ic,Iw,Ib,maxw,averw,sumw,rms,brms,Un)
   Ih = sumw
!
   IF ( Nlpp>50 ) THEN
      WRITE (Nout,99003)
99003 FORMAT (/31X,'AFTER RESEQUENCING BY REVERSE CUTHILL-MCKEE (CM)',' ALGORITHM - - -')
      WRITE (Nout,99004) Ib , Ih , maxw , averw , rms , brms
   ENDIF
!
!     CHECK CM LABELING AGAINST ORIGINAL LABELING TO SEE IF BETTER.
!     IB = BANDWIDTH,  IH = PROFILE.
!
   IF ( Io==2 ) THEN
      im1 = is
      im2 = Ih0
      crit1 = Ib
      crit2 = Ih
   ELSEIF ( Io==3 ) THEN
      im1 = Ih0
      im2 = is
      crit1 = Ih
      crit2 = Ib
   ELSEIF ( Io==4 ) THEN
      im1 = Maxw0
      im2 = Rms0
      crit1 = maxw
      crit2 = rms
   ELSE
      im1 = Rms0
      im2 = Ih0
      crit1 = rms
      crit2 = Ih
   ENDIF
   IF ( crit1<im1 ) GOTO 100
   IF ( crit1==im1 ) THEN
      IF ( crit2<im2 ) GOTO 100
   ENDIF
!
!     IF NO IMPROVEMENT RETURN TO ORIGINAL SEQUENCE.
!
   Ib = is
   Ih = Ih0
   maxw = Maxw0
   rms = Rms0
   brms = Brms0
   DO i = 1 , Nn
      Ild(i) = i
      New(i) = i
   ENDDO
   Jump = 1
!
!     SET FINAL VALUES OF B, P, RMS, W.
!
 100  Knew = Ib
   Ihe = Ih
   Maxw1 = maxw
   Rms1 = rms
   Brms1 = brms
99004 FORMAT (40X,'BANDWIDTH',I13,/40X,'PROFILE',I15,/40X,'MAX WAVEFRONT',I9,/40X,'AVG WAVEFRONT',F9.3,/40X,'RMS WAVEFRONT',F9.3,   &
            & /40X,'RMS BANDWIDTH',F9.3)
END SUBROUTINE cthmck
