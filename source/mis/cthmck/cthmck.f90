!*==cthmck.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cthmck(Nt,Num,Nom,Io,Ig,Ic,Ideg,Idis,Iw,New,Icc,Ild,Ipp,Jump,Un,Nodesl)
   USE c_banda
   USE c_bandb
   USE c_bandd
   USE c_bands
   USE c_bandw
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nt
   INTEGER :: Num
   INTEGER :: Nom
   INTEGER :: Io
   INTEGER , DIMENSION(1) :: Ig
   INTEGER , DIMENSION(1) :: Ic
   INTEGER , DIMENSION(1) :: Ideg
   INTEGER , DIMENSION(1) :: Idis
   INTEGER , DIMENSION(1) :: Iw
   INTEGER , DIMENSION(1) :: New
   INTEGER , DIMENSION(1) :: Icc
   INTEGER , DIMENSION(1) :: Ild
   INTEGER , DIMENSION(1) :: Ipp
   INTEGER :: Jump
   REAL , DIMENSION(1) :: Un
   INTEGER , DIMENSION(1) :: Nodesl
!
! Local variable declarations rewritten by SPAG
!
   REAL :: averw , brms , crit1 , crit2 , im1 , im2 , rms
   INTEGER :: i , iajdim , idem , ij , is , j , jmax , k , k2 , ma , mad , maxd , maxlev , maxw , median , mi , mmc , modd , nc ,   &
            & nl , sumw
   EXTERNAL degree , diam , dist , kompnt , maxdgr , mindeg , page1 , relabl , stack , wavey
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         idem = kdim
         k2 = idem + 1
         iajdim = 3*idem
!
!     DETERMINE THE DEGREE OF EACH NODE, THE NUMBER OF COMPONENTS, NCM,
!     AND THE MAXIMUM DEGREE OF ANY NODE.
!
         CALL degree(Ig,Ideg,Un)
         ncm = kompnt(Ig,Ic,Ideg,Iw,Icc,Un)
         maxd = maxdgr(0,Ic,Ideg)
         mmc = maxd
!
!     INITIALIZE NEW ARRAY FROM THE ILD ARRAY.
!     ILD MUST BE INPUT TO CUTHILL.
!
         DO i = 1 , nn
            k = Ild(i)
            New(k) = i
         ENDDO
!
!     COMPUTE ORIGINAL BANDWIDTH, PROFILE, WAVEFRONT AND ACTIVE COLUMN
!     IH0 = ORIGINAL PROFILE,  IS = ORIGINAL BW
!
         CALL wavey(Ig,Ild,New,0,Ic,Iw,is,maxw,averw,sumw,rms,brms,Un)
         ih = sumw
         maxw0 = maxw
         rms0 = rms
         brms0 = brms
         korig = is
         ih0 = ih
         CALL page1
         i = method + 2
         WRITE (nout,99001) uim , icrit , i , nompc , nodep , nopch
99001    FORMAT (A29,'S FROM RESEQUENCING PROCESSOR - BANDIT     (CRI=',I2,',  MTH=',I2,',  MPC=',I2,',  DEP=',I2,',  PCH=',I2,')', &
               & /)
         IF ( nlpp>50 ) THEN
            WRITE (nout,99002)
99002       FORMAT (31X,'BEFORE RESEQUENCING - - -')
            WRITE (nout,99004) is , ih , maxw , averw , rms , brms
         ENDIF
!
!     COMPUTE NODAL DEGREE STATISTICS.
!
         CALL dist(Ideg,Ipp,median,modd)
         IF ( method==+1 ) RETURN
!
!     INITIALIZE ILD AND NEW ARRAYS.
!
         Jump = 0
         DO i = 1 , nn
            New(i) = 0
            Ild(i) = 0
         ENDDO
!
!     GENERATE NUMBERING SCHEME FOR EACH COMPONENT, NC.
!
         DO nc = 1 , ncm
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
               CALL wavey(Ig,Ild,New,nc,Ic,Iw,ib,maxw,averw,sumw,rms,brms,Un)
               IF ( ngrid==-1 ) RETURN
!
               ih = sumw
               IF ( Io==2 ) THEN
                  crit1 = ib
                  crit2 = ih
               ELSEIF ( Io==3 ) THEN
                  crit1 = ih
                  crit2 = ib
               ELSEIF ( Io==4 ) THEN
                  crit1 = maxw
                  crit2 = rms
               ELSE
                  crit1 = rms
                  crit2 = ih
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
            IF ( ngrid==-1 ) RETURN
!
         ENDDO
!
!     DETERMINE NODES OF ZERO DEGREE AND STACK LAST, AND
!     COMPUTE BANDWIDTH, PROFILE AND WAVEFRONT DATA.
!
         CALL stack(Ideg,New,Ild,Iw)
         CALL wavey(Ig,Ild,New,0,Ic,Iw,ib,maxw,averw,sumw,rms,brms,Un)
         ih = sumw
!
         IF ( nlpp>50 ) THEN
            WRITE (nout,99003)
99003       FORMAT (/31X,'AFTER RESEQUENCING BY REVERSE CUTHILL-MCKEE (CM)',' ALGORITHM - - -')
            WRITE (nout,99004) ib , ih , maxw , averw , rms , brms
         ENDIF
!
!     CHECK CM LABELING AGAINST ORIGINAL LABELING TO SEE IF BETTER.
!     IB = BANDWIDTH,  IH = PROFILE.
!
         IF ( Io==2 ) THEN
            im1 = is
            im2 = ih0
            crit1 = ib
            crit2 = ih
         ELSEIF ( Io==3 ) THEN
            im1 = ih0
            im2 = is
            crit1 = ih
            crit2 = ib
         ELSEIF ( Io==4 ) THEN
            im1 = maxw0
            im2 = rms0
            crit1 = maxw
            crit2 = rms
         ELSE
            im1 = rms0
            im2 = ih0
            crit1 = rms
            crit2 = ih
         ENDIF
         IF ( crit1>=im1 ) THEN
            IF ( crit1==im1 ) THEN
               IF ( crit2<im2 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
!     IF NO IMPROVEMENT RETURN TO ORIGINAL SEQUENCE.
!
            ib = is
            ih = ih0
            maxw = maxw0
            rms = rms0
            brms = brms0
            DO i = 1 , nn
               Ild(i) = i
               New(i) = i
            ENDDO
            Jump = 1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     SET FINAL VALUES OF B, P, RMS, W.
!
         knew = ib
         ihe = ih
         maxw1 = maxw
         rms1 = rms
         brms1 = brms
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99004 FORMAT (40X,'BANDWIDTH',I13,/40X,'PROFILE',I15,/40X,'MAX WAVEFRONT',I9,/40X,'AVG WAVEFRONT',F9.3,/40X,'RMS WAVEFRONT',F9.3,   &
            & /40X,'RMS BANDWIDTH',F9.3)
END SUBROUTINE cthmck
