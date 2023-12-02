!*==cdetm.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cdetm(Method,Eed,Mdd,Bdd,Kdd,Lama,Phid,Oceigs,Nfound,Scr1,Scr2,Scr3,Scr4,Scr5,Scr6,Scr7,Scr8)
   USE c_cdcmpx
   USE c_condad
   USE c_machin
   USE c_msgx
   USE c_output
   USE c_saddx
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Method
   INTEGER :: Eed
   INTEGER :: Mdd
   INTEGER :: Bdd
   INTEGER :: Kdd
   INTEGER :: Lama
   INTEGER :: Phid
   INTEGER :: Oceigs
   INTEGER :: Nfound
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Scr4
   INTEGER :: Scr5
   INTEGER :: Scr6
   INTEGER :: Scr7
   INTEGER :: Scr8
!
! Local variable declarations rewritten by SPAG
!
   REAL :: aloc1 , alph1 , alph2 , epsi , lu , rll , w1 , w2 , wloc1 , x , xl , xu , xvr , y , yu , yvr
   REAL(REAL64) , DIMENSION(2) :: amcb , bmcb , cmcb
   REAL(REAL64) :: d1 , d10 , d2 , d3 , d4 , d5 , d6 , d7 , d8 , d9 , ddist2 , ddistx , deltki , deltkr , dt1 , dt2 , dt3 , gki ,   &
                 & gkr , h1bar , h2bar , h3bar , hk1i , hk1r , hki , hkp1i , hkp1r , hkr , lamdki , lamdkr , lamk1i , lamk1r , pi , &
                 & pr , pti , ptr , rl , rooti , rootr , test , zdk , zdkm1 , zz
   REAL(REAL64) , DIMENSION(3) :: deti , detr , dsi , dsr , pki , pkr , psi , psr
   INTEGER :: dretn , file , i , ibuf , icmpx , icnt , id , id1 , id2 , id3 , id4 , idd , ifail , iflag , ifpass , igk , inorm ,    &
            & ip1 , ipoint , ipoles , iprt , ireg1 , irgp , iroot , isave , isil , ising , ispnt , ispt1 , ispt2 , iterm , itime1 , &
            & itime2 , itleft , izk , j , jj , k , kd , kk , kkk , kreg , l , lc , ll , lregn , ls , lspt , m , nchang , nd ,       &
            & ndcomp , ndesrd , ne , nf , nfail , nmoves , nosing , noutsd , npass , npole , nregn , nrorg , nrow , nsbdon ,        &
            & nsbrgn , nxorg , otpe , sysbuf
   INTEGER , DIMENSION(2) , SAVE :: eigc , ipole , name
   INTEGER , DIMENSION(50) , SAVE :: ihead
   INTEGER , DIMENSION(2) :: ilusp , isp
   INTEGER , SAVE :: im1 , iz2 , iz3 , iz4 , iz5 , iz6 , iz7 , iz8 , nit , numint , poin
   INTEGER , DIMENSION(3) :: ipdet , ips
   INTEGER , DIMENSION(7) :: iscr2
   INTEGER , DIMENSION(1) :: iz
   REAL , SAVE :: sign
   REAL(REAL64) , DIMENSION(1) :: zd
   EXTERNAL cdcomp , cdetm2 , cdetm3 , cdtfbs , close , csqrtn , csumm , gopen , klock , korsz , locate , mesage , open , preloc ,  &
          & rdtrl , read , sadd , sswtch , tmtogo , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     SOLVES COMPLEX EIGENVALUE PROBLEM BY DETERMINANT METHOD
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Otpe) , (Amcb(1),Mcba(9)) , (Bmcb(1),Mcbb(9)) , (Cmcb(1),Mcbc(9)) ,                  &
!>>>>    & (Z(1),Iz(1),Zd(1))
   DATA ipole , eigc , ihead , poin , name/257 , 4 , 207 , 2 , 0 , 1009 , 1 , 47*0 , 4HPOIN , 4HCDET , 4HM   /
   DATA nit , im1 , sign , numint , iz2 , iz3 , iz4 , iz5 , iz6 , iz7 , iz8/20 , 1 , -1.0 , 4 , 2 , 3 , 4 , 5 , 6 , 7 , 8/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DEFINITION OF VARIABLES
!
!     METHOD   SELECT SET OF POLES AND EIGC CARDS
!     EED      EIGENVALUE EXTRACTION DATA BLOCK
!     MDD      MASS MATRIX      - DIRECT OR MODAL
!     BDD      DAMPING MATRIX   - DIRECT OR MODAL
!     KDD      STIFFNESS MATRIX - DIRECT OR MODAL
!     LAMA     EIGENVALUE FILE
!     PHID     EIGENVECTOR FILE
!     OEIGS    EIGENVALUE SUMMARY FILE
!     NFOUND   TOTAL NUMBER OF EIGENVALUES FOUND IN ALL REGIONS
!     SCR      SCRATCHES
!     IPOLE    LOCATE WORDS FOR POLES
!     EIGC     LOCATE WORDS FOR EIGC CARDS
!     EPSI     CONVERGENCE CRITERION
!     IBUF     POINTER TO BUFFER
!     NPOLE    NUMBER OF POLES TO BE USED
!     IPOLES   POINTER TO START OF POLES - 4 WORDS PER POLE ID,X,Y,MUL
!     NREGN    NUMBER OF REGIONS
!     IREG1    POINTER TO WORDS DESCRIBING REGIONS
!     INORM    NORMALIZATION METHOD - 1 = MAX,  0 = POINT
!     ISIL     POINTER FOR NORM IF  NORM = 0
!     NE       ESTIMATED NUMBER OF ROOTS IN REGION
!     ND       DESIRED NUMBER OF ROOTS
!     LREGN    LENGTH OF BLOCK DESCRIBING REGION
!     NPASS    NUMBER OF PASSES THROUGH STARTING POINTS
!     NCHANG   NUMBER OF CHANGES OF CONVERGENCE CRITERIA
!     NMOVES   NUMBER OF STARTING POINT MOVES
!     NDCOMP   NUMBER OF DECOMPOSITIONS
!     NFAIL    NUMBER OF FAILURES TO INTERATE TO A ROOT
!     NOUTSD   NUMBER OF PREDICTIONS OUTSIDE REGION
!     ITERM    REASON FOR TERMINATION  0 - FOUND REQUESTED NUMBER
!                                      1 - NO MORE IN REGIONS
!     IRGP     POINTER FOR CURRENT REGION
!     ICNT     NUMBER OF INTERATIONS
!     NIT      MAXIMUM NUMBER OF INTERATIONS/ROOT
!     NUMINT   MAXIMUM NUMBER OF CONVERGENCE  CHANGES
!     NROW     ORDER OF PROBLEM
!     ICMPX    SWITCH IF ROOTS FOUND ARE COMPLEX CONJUGATE -0  NOT-1
!     ISPNT    POINTER TO CURRENT 3 STARTING POINTS
!     PS       SORTED 3 STARTING POINTS
!     DS       SORTED 3 DET OF STARTING POINTS
!     IPS      POWERS OF STARTING POINTS
!     P        TRIAL EIGENVALUE
!     D        SCALED SWEPT DETERMINANT AT P
!     IFPASS   FLAG TO SIGNAL ROOT FROUND ON PASS 1, 0 IF NOT
!     IPOINT   NUMBER OF STARTING POINTS USED IN CURRENT REGION
!     ILUSP    INDEX TO LAST USED STARTING POINT (1ST OF 3) IN EACH
!              SUBRGN
!     NSBRGN   NUMBER OF SUBREGIONS IN PROBLEM THIS REGION
!     NSBDON   FLAG MARKING COMPLETED SUBREGION
!     ISP      POINTS NEAREST AND NEXT NEAREST THE ORIGIN
!
!     STRUCTURE OF REGION
!
!     A1,B1,A2,B2,XL,NE,ND,NF,POINTER TO NEXT REGION (ZERO IF LAST),RL,
!     X  (12  WORDS)
!
!     STARTING POINTS  8NE + 8 WORDS  - D.P. COMPLEX
!     DETERMINANTS     8NE + 8 WORDS    D.P. COMPLEX
!     SCALE FACTORS    4NE + 4 WORDS    2 INTEGERS PER STARTING POINT
!
!     ROOTS   4ND WORDS                 D.P. COMPLEX
!
!
!     DEFINE EPSI (CONVERGENCE CRITERION)
!
         epsi = 1.0E-16
         IF ( mach==5 .OR. mach==21 ) epsi = 1.0E-12
!
         lc = korsz(z)
         ibuf = lc - sysbuf - 1
         lc = (ibuf/2)*2 - 1
         nosing = 1
         CALL sswtch(7,iprt)
         ising = 0
         fa(1) = Kdd
         CALL rdtrl(fa(1))
         IF ( fa(1)<=0 ) THEN
            fa(1) = Mdd
            CALL rdtrl(fa(1))
            IF ( fa(1)<=0 ) THEN
               fa(1) = Bdd
               CALL rdtrl(fa(1))
               IF ( fa(1)<=0 ) GOTO 180
               idd = Bdd
            ELSE
               idd = Mdd
            ENDIF
         ELSE
            idd = Kdd
         ENDIF
         fa(1) = -Scr2
         fa(5) = 4
         fl(1) = idd
         CALL rdtrl(fl(1))
         fl(4) = 4
         fl(5) = 4
         fu(1) = idd
         CALL rdtrl(fu(1))
         fu(4) = 5
         fu(5) = 4
         sr1 = Scr3
         sr2 = Scr4
         sr3 = Scr5
         fl(1) = Scr6
         fu(1) = Scr7
         DO i = 1 , 7
            mcba(i) = 0
            mcbb(i) = 0
            mcbc(i) = 0
            mc(i) = 0
         ENDDO
         mcba(1) = Kdd
         mcbb(1) = Bdd
         mcbc(1) = Mdd
         CALL rdtrl(mcba(1))
         CALL rdtrl(mcbb(1))
         CALL rdtrl(mcbc(1))
!
!     MUST HAVE  B OR M MATRICES
!
         IF ( mcbb(1)<0 ) mcbb(1) = 0
         IF ( mcbc(1)<0 ) mcbc(1) = 0
         IF ( mcbb(1)+mcbc(1)==0 ) GOTO 180
         nrow = max0(mcba(3),mcbb(3),mcbc(3))
         icmpx = 0
         IF ( mcba(5)>2 .OR. mcbb(5)>2 .OR. mcbc(5)>2 ) icmpx = 1
         amcb(1) = 1.0D0
         amcb(2) = 0.D0
         mc(2) = mcba(2)
         mc(3) = mcba(3)
         mc(4) = mcba(4)
         mc(5) = 4
         mcba(8) = 4
         mcbc(8) = 4
         mcbb(8) = 4
         nomat = 3
         mc(1) = Scr2
         ndesrd = 0
!
!     PICK UP AND STORE ANY POLES
!
         file = Eed
         CALL preloc(*120,iz(ibuf),Eed)
         npole = 0
         CALL locate(*20,iz(ibuf),ipole(1),iflag)
!
!     FOUND POLE CARDS
!
         lc = lc - 4
         DO
            CALL read(*140,*20,Eed,iz(lc),4,0,iflag)
            IF ( iz(lc)==Method ) THEN
               npole = npole + 1
               lc = lc - 4
            ENDIF
         ENDDO
 20      ipoles = lc + 4
!
!     STORE REGIONS
!
         nregn = 0
         CALL locate(*180,iz(ibuf),eigc(1),iflag)
         SPAG_Loop_1_1: DO
            CALL read(*140,*180,Eed,iz(1),10,0,iflag)
            IF ( Method==iz(1) .OR. Method==-1 ) THEN
!
!     EIGC CARD FOUND - ALLOCATE CORE + BUILD UP REGIONS
!
               inorm = 0
               IF ( iz(iz4)/=poin ) inorm = 1
               isil = iz(iz6)
               IF ( z(iz8)/=0.0 ) epsi = z(iz8)
               DO
!
!     PROCESS EACH REGION DEFINITION
!
                  CALL read(*140,*160,Eed,z(1),7,0,iflag)
                  IF ( iz(iz7)<0 ) THEN
                     lcadd = lc - 1
                     nx = lcadd
                     iz(lc+8) = 0
                     CALL close(Eed,1)
                     IF ( lc<=4*nrow ) THEN
                        ip1 = -8
                        spag_nextblock_1 = 27
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
!
!     INITIALIZE CUMULATIVE POINTERS
                        ifail = 0
                        Nfound = 0
                        npass = -1
                        nchang = 0
                        nmoves = 0
                        ndcomp = 0
                        nfail = 0
                        noutsd = 0
                        iterm = 1
                        ifpass = 1
                        EXIT SPAG_Loop_1_1
                     ENDIF
                  ELSE
                     nregn = nregn + 1
                     alph1 = z(1)
                     w1 = z(iz2)
                     alph2 = z(iz3)
                     w2 = z(iz4)
                     xl = z(iz5)
                     ne = iz(iz6)
                     nd = iz(iz7)
                     IF ( nd==0 ) nd = 3*ne
                     lregn = 20*ne + 4*nd + 32
                     ndesrd = ndesrd + nd
                     IF ( nregn/=1 ) iz(lc+8) = lc - lregn
                     lc = lc - lregn
                     IF ( lc<=0 ) THEN
                        ip1 = -8
                        spag_nextblock_1 = 27
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        IF ( nregn==1 ) ireg1 = lc
!
!     ZERO REGION
!
                        k = lc - 1
                        DO i = 1 , lregn
                           k = k + 1
                           IF ( i/=9 ) iz(k) = 0
                        ENDDO
!
!     STORE CONSTANTS
                        z(lc) = alph1
                        z(lc+1) = w1
                        z(lc+2) = alph2
                        z(lc+3) = w2
                        z(lc+4) = xl
                        iz(lc+5) = ne
                        iz(lc+6) = nd
!
!     DISTRIBUTE  STARTING POINTS
!
                        d1 = alph2 - alph1
                        d2 = w2 - w1
                        rl = dsqrt(d1*d1+d2*d2)
                        z(lc+9) = rl
                        d1 = d1/rl
                        d2 = d2/rl
                        j = (lc+1)/2 + 6
                        d3 = rl/float(4*ne+4)
                        zd(j) = d1*d3 + alph1
                        zd(j+1) = d2*d3 + w1
                        k = 2*ne + 1
                        d3 = rl/float(k+1)
                        d1 = d1*d3
                        d2 = d2*d3
                        DO i = 1 , k
                           j = j + 2
                           zd(j) = zd(j-2) + d1
                           zd(j+1) = zd(j-1) + d2
                        ENDDO
                     ENDIF
                  ENDIF
               ENDDO
            ELSE
               SPAG_Loop_2_2: DO
!
!     SKIP REMAINDER OF EIGC CARD
!
                  CALL read(*140,*180,Eed,iz(1),7,0,iflag)
                  IF ( iz(iz6)==-1 ) EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
!
!     RETURN HERE TO SEARCH ALL REGIONS AGAIN
!
         npass = npass + 1
         IF ( ifpass==0 ) THEN
            iterm = 2
         ELSE
            ifpass = 0
            irgp = ireg1
!
!     FIND REGION WHICH LACKS ROOTS
!
            DO i = 1 , nregn
               IF ( iz(irgp+6)>iz(irgp+7) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               irgp = iz(irgp+8)
!
!     ALL REGIONS HAVE ENOUGH ROOTS - EXIT
!
            ENDDO
         ENDIF
         spag_nextblock_1 = 24
      CASE (3)
!
!     PICK UP REGION POINTERS AND PARAMETERS
!
         alph1 = z(irgp)
         w1 = z(irgp+1)
         alph2 = z(irgp+2)
         w2 = z(irgp+3)
         xl = z(irgp+4)
         ne = iz(irgp+5)
         nd = iz(irgp+6)
         nf = iz(irgp+7)
         rl = z(irgp+9)
         xvr = (alph2-alph1)/rl
         yvr = (w2-w1)/rl
         ipoint = 0
         ispnt = (irgp+1)/2 + 4
!
!     FIND POINTS CLOSEST TO AND NEXT CLOSEST TO ORIGIN THUS DIVIDING
!     REGION INTO TWO SUBREGIONS
!
         ispt1 = (irgp+13)/2
         lspt = ispt1 + 2*(2*ne+2) - 2
         ddistx = 0.
         nxorg = 0
         nrorg = ispt1
         ddist2 = zd(ispt1)*zd(ispt1) + zd(ispt1+1)*zd(ispt1+1)
         ispt2 = ispt1 + 2
         SPAG_Loop_1_3: DO i = ispt2 , lspt , 2
            zz = zd(i)*zd(i) + zd(i+1)*zd(i+1)
            IF ( zz>ddist2 ) EXIT SPAG_Loop_1_3
            nxorg = nrorg
            nrorg = i
            ddistx = ddist2
            ddist2 = zz
         ENDDO SPAG_Loop_1_3
         IF ( zz>ddistx ) THEN
            IF ( nxorg==0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         nxorg = i
         ddistx = zz
         spag_nextblock_1 = 4
      CASE (4)
!
!     CALCULATE THE NUMBER OR SUBREGIONS, NSBRGN. THERE MUST BE AT LEAST
!     3 POINTS EACH SIDE OF BISECTOR IN ORDER TO HAVE 2 SUBREGIONS.
!          ISPT2+2 .LE. (NRORG+NXORG)/2 .LE. LSPT-4
!
         IF ( 2*(ispt2+2)<=nrorg+nxorg .AND. nrorg+nxorg<=(lspt-4)*2 ) THEN
!
!     TWO SUBREGIONS EXIST. DETERMINE STARTING POINTS FOR EACH
!
            nsbrgn = 2
            nsbdon = 0
            isp(1) = nrorg
            IF ( nrorg<nxorg ) isp(1) = nxorg
            isp(2) = nrorg + nxorg - isp(1)
            kreg = 2
            ilusp(1) = isp(1)
            ilusp(2) = isp(2) - 2
            spag_nextblock_1 = 6
         ELSE
!
!     ONLY ONE SUBREGION
!     FIND FIRST UNEVALUATED POINT
!
            nsbrgn = 1
            k = irgp + 16*ne + 32
            l = 2*ne
            DO j = 1 , l
               IF ( iz(k)==0 ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               k = k + 2
               ipoint = ipoint + 1
               ispnt = ispnt + 2
            ENDDO
!
!     ALL TRIED  GO TO BEGINNING
!
            ipoint = 0
            ispnt = (irgp+1)/2 + 4
            spag_nextblock_1 = 9
         ENDIF
      CASE (5)
!
!     RETURN HERE TO GET NEW STARTING POINT (OR NEW REGION IF NECESSARY)
!     DETINES  ISPNT
!
         IF ( nsbrgn==1 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nsbdon<1 ) THEN
         ELSEIF ( nsbdon==1 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     SUBREGION 2 IS COMPLETE.  IS SUBREGION 1 FINISHED AS WELL
!
            IF ( nsbdon==3 ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     CHANGE SUBREGIONS
!
         kreg = 3 - kreg
         IF ( kreg==2 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     PROCESS FIRST SUBREGION
!
         ispnt = ilusp(1)
         ls = isp(2)
         aloc1 = zd(ls)
         wloc1 = zd(ls+1)
         ilusp(1) = ilusp(1) + 2
         IF ( ispnt+4==lspt ) THEN
!
!     PROCESS LAST SET OF STARTING IN FIRST SUBREGION
!
            nsbdon = nsbdon + 1
            pr = .45*zd(ispnt+4) + .55*alph1 - aloc1
            pi = .45*zd(ispnt+5) + .55*w1 - wloc1
         ELSE
            pr = .45*zd(ispnt+4) + .55*zd(ispnt+6) - aloc1
            pi = .45*zd(ispnt+5) + .55*zd(ispnt+7) - wloc1
         ENDIF
         ipoint = (ispnt-ispt1)/2 + 1
         spag_nextblock_1 = 10
      CASE (8)
!
!     PROCESS SUBREGION 2
!
         ispnt = ilusp(2) - 2
         ilusp(2) = ilusp(2) - 2
         ls = isp(1)
         aloc1 = zd(ls)
         wloc1 = zd(ls+1)
         IF ( ispnt==ispt1 ) THEN
!
!     LAST SET OF STARTING POINTS IN SUBREGION2 TO PROCESS
!
            nsbdon = nsbdon + 2
            pr = -(.45*zd(ispt1)+.55*zd(ispnt)) + aloc1
            pi = -(.45*zd(ispt1+1)+.55*zd(ispnt+1)) + wloc1
         ELSE
            pr = -(.45*zd(ispnt-2)+.55*zd(ispnt)) + aloc1
            pi = -(.45*zd(ispnt-1)+.55*zd(ispnt+1)) + wloc1
         ENDIF
         ipoint = (ispnt-ispt1)/2 + 1
         spag_nextblock_1 = 10
      CASE (9)
!
!     ONLY ONE SUBREGION PROCESS FROM END TO END
!
         ipoint = ipoint + 1
         ispnt = ispnt + 2
         IF ( ipoint>2*ne ) THEN
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     FIND OUT IF ANY DETERMINT EVALUATIONS ARE NECESSARY
!
!     COMPUTE LOCAL SEARCH REGION DESCRITIONS
!
         aloc1 = alph1
         wloc1 = w1
         IF ( ipoint==2*ne ) THEN
            pr = alph2 - aloc1
            pi = w2 - wloc1
         ELSE
            pr = .45*zd(ispnt+4) + .55*zd(ispnt+6) - aloc1
            pi = .45*zd(ispnt+5) + .55*zd(ispnt+7) - wloc1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
         rll = dsqrt(pr*pr+pi*pi)
         k = irgp + 16*ne + 24 + 2*ipoint
         i = 1
         ising = 0
         spag_nextblock_1 = 11
      CASE (11)
         k = k + 2
         IF ( iz(k)/=0 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     EVALUATE DETERMINANT
!
         j = ispnt + 2*i - 2
         pr = zd(j)
         pi = zd(j+1)
         ASSIGN 40 TO dretn
         spag_nextblock_1 = 25
         CYCLE SPAG_DispatchLoop_1
 40      iz(k) = 1
         iz(k+1) = powr
         m = 4*ne + 2 + ispnt + 2*i
         zd(m) = dr
         zd(m+1) = di
         spag_nextblock_1 = 12
      CASE (12)
         i = i + 1
         IF ( i<=3 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ising==3 .AND. npass==0 ) THEN
!
!     SINGULAR MATRIX
!
            iterm = 4
            spag_nextblock_1 = 24
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
!
!     SORT STARTING POINTS BY MAGNITUDE OF DET
!
         CALL klock(itime1)
         k = ispnt + 4*ne + 4
         l = irgp + 16*ne + 26 + 2*ipoint
         CALL cdetm2(zd(ispnt),zd(k),iz(l),psr(1),psi(1),dsr(1),dsi(1),ips(1))
!
!     LOAD STARTING POINTS INTO TRAIL EIGENVALUES
!
         DO i = 1 , 3
            pkr(i) = psr(i)
            pki(i) = psi(i)
            detr(i) = dsr(i)
            deti(i) = dsi(i)
            ipdet(i) = ips(i)
         ENDDO
         dt2 = 1.0D38
!
!     START INTERATION LOOP
!
         icnt = 1
         spag_nextblock_1 = 14
      CASE (14)
         hk1r = pkr(2) - pkr(1)
         hk1i = pki(2) - pki(1)
         hkr = pkr(3) - pkr(2)
         hki = pki(3) - pki(2)
         IF ( hkr==0.0D0 .AND. hki==0.0D0 ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         d1 = hk1r*hk1r + hk1i*hk1i
         lamdkr = (hkr*hk1r+hki*hk1i)/d1
         lamdki = (hki*hk1r-hkr*hk1i)/d1
         deltkr = 1.0D0 + lamdkr
         deltki = lamdki
!
!     COMPUTE GK
!
         d1 = lamdkr*lamdkr - lamdki*lamdki
         d2 = 2.0*lamdkr*lamdki
         d3 = d1*detr(1) - d2*deti(1)
         d4 = d2*detr(1) + d1*deti(1)
         d1 = deltkr*deltkr - deltki*deltki
         d2 = 2.0*deltkr*deltki
         d5 = -d1*detr(2) + d2*deti(2)
         d6 = -d2*detr(2) - d1*deti(2)
         CALL csumm(d3,d4,ipdet(1),d5,d6,ipdet(2),d1,d2,id1)
         d3 = lamdkr + deltkr
         d4 = lamdki + deltki
         d5 = d3*detr(3) - d4*deti(3)
         d6 = d4*detr(3) + d3*deti(3)
         CALL csumm(d1,d2,id1,d5,d6,ipdet(3),gkr,gki,igk)
!
!     COMPUTE TERM UNDER RADICAL IN EQ. 11
!
         d1 = detr(1)*lamdkr - deti(1)*lamdki
         d2 = deti(1)*lamdkr + detr(1)*lamdki
         d3 = -detr(2)*deltkr + deti(2)*deltki
         d4 = -deti(2)*deltkr - detr(2)*deltki
         CALL csumm(d1,d2,ipdet(1),d3,d4,ipdet(2),d5,d6,id1)
         CALL csumm(d5,d6,id1,detr(3),deti(3),ipdet(3),d1,d2,id2)
         d3 = deltkr*lamdkr - deltki*lamdki
         d4 = deltki*lamdkr + deltkr*lamdki
         d5 = d1*d3 - d2*d4
         d6 = d2*d3 + d1*d4
         d1 = -4.0*(detr(3)*d5-deti(3)*d6)
         d2 = -4.0*(deti(3)*d5+detr(3)*d6)
!
!     COMPUTE  GK*GK
!
         d3 = gkr*gkr - gki*gki
         d4 = 2.0*gkr*gki
         CALL csumm(d3,d4,2*igk,d1,d2,ipdet(3)+id2,d5,d6,id1)
         CALL csqrtn(d5,d6,id1,rootr,rooti,iroot)
         CALL csumm(gkr,gki,igk,rootr,rooti,iroot,d9,d10,id3)
         CALL csumm(gkr,gki,igk,-rootr,-rooti,iroot,d7,d8,id4)
         IF ( icnt==1 ) THEN
!
!     COMPUTE  NUMERATOR  EQ. 11
!
            d1 = d9
            d2 = d10
            id1 = id3
            m = 2
         ELSE
            d1 = d9
            d2 = d10
            id1 = id3
            d5 = d9*d9 + d10*d10
            d6 = d7*d7 + d8*d8
            IF ( d5<d6 ) THEN
               d1 = d7
               d2 = d8
               id1 = id4
            ENDIF
         ENDIF
         SPAG_Loop_1_4: DO
            d3 = -2.0*(detr(3)*deltkr-deti(3)*deltki)
            d4 = -2.0*(deti(3)*deltkr+detr(3)*deltki)
            d5 = d1*d1 + d2*d2
            d6 = 10.0**(ipdet(3)-id1)
            lamk1r = d6*(d3*d1+d4*d2)/d5
            lamk1i = d6*(d4*d1-d3*d2)/d5
            hkp1r = lamk1r*hkr - lamk1i*hki
            hkp1i = lamk1i*hkr + lamk1r*hki
            pr = pkr(3) + hkp1r
            pi = pki(3) + hkp1i
            IF ( icnt/=1 ) EXIT SPAG_Loop_1_4
            dt3 = 0.0D0
            DO i = 1 , 3
               dt3 = dt3 + dsqrt((pkr(i)-pr)**2+(pki(i)-pi)**2)
            ENDDO
            IF ( dt3<=dt2 ) THEN
               ptr = pr
               pti = pi
               dt2 = dt3
            ENDIF
            IF ( m==2 ) THEN
               d1 = d7
               d2 = d8
               id1 = id4
               m = 1
            ELSE
               pr = ptr
               pi = pti
               EXIT SPAG_Loop_1_4
            ENDIF
         ENDDO SPAG_Loop_1_4
!
!     DO RANGE CHECKS
!
!
!     COMPUTE U VECTOR
!
         xu = pr - alph1
         yu = pi - w1
         lu = sqrt(xu*xu+yu*yu)
         IF ( lu/=0.0 ) THEN
            xu = xu/lu
            yu = yu/lu
            x = lu*(xu*xvr+yu*yvr)
            y = lu*(yu*xvr-xu*yvr)
            IF ( abs(y)>xl/2.0 .OR. x<0.0 .OR. x>rl ) THEN
!
!     PREDICTED OUTSIDE BIG REGION
!
               noutsd = noutsd + 1
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     SEE IF POINT IS IN LOCAL REGION
!
         xu = pr - aloc1
         yu = pi - wloc1
         lu = sqrt(xu*xu+yu*yu)
         IF ( lu==0.0 ) THEN
!
!     TRY FOR CONVERGENCE
!
            ASSIGN 60 TO dretn
            spag_nextblock_1 = 25
         ELSE
            xu = xu/lu
            yu = yu/lu
            y = lu*(yu*xvr-xu*yvr)
            x = lu*(xu*xvr+yu*yvr)
            IF ( abs(y)>xl/2.0 .OR. x<0.0 .OR. x>rll ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 60 TO dretn
            spag_nextblock_1 = 25
         ENDIF
         CYCLE
!
!     BEGIN CONVERGENCE TESTS
!
 60      IF ( icnt<=2 ) THEN
!
!     CONTINUE INTERATIONS
!
            icnt = icnt + 1
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ELSE
            h1bar = dsqrt(hk1r*hk1r+hk1i*hk1i)
            h2bar = dsqrt(hkr*hkr+hki*hki)
            h3bar = dsqrt(hkp1r*hkp1r+hkp1i*hkp1i)
         ENDIF
         spag_nextblock_1 = 15
      CASE (15)
         test = epsi*rl
         IF ( h1bar<=test*1.0E7 ) THEN
            IF ( h2bar<=test*1.0E4 ) THEN
               IF ( h3bar>h2bar ) THEN
                  IF ( h2bar<=1.0E-7*rl ) THEN
                     spag_nextblock_1 = 17
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSEIF ( h3bar<=test ) THEN
                  spag_nextblock_1 = 17
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         icnt = icnt + 1
         IF ( icnt>=nit ) THEN
            IF ( icnt==nit ) THEN
               IF ( nchang<numint .AND. ifail==1 ) THEN
                  epsi = epsi*10.0
                  nchang = nchang + 1
                  spag_nextblock_1 = 15
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            ifail = 1
            nfail = nfail + 1
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 16
      CASE (16)
         DO i = 1 , 2
            pkr(i) = pkr(i+1)
            pki(i) = pki(i+1)
            ipdet(i) = ipdet(i+1)
            detr(i) = detr(i+1)
            deti(i) = deti(i+1)
         ENDDO
         pkr(3) = pr
         pki(3) = pi
         detr(3) = dr
         deti(3) = di
         ipdet(3) = powr
         spag_nextblock_1 = 14
      CASE (17)
!
!     ACCEPT CURRENT EIGENVALUE
!
         file = Lama
         Nfound = Nfound + 1
         ifpass = 1
         IF ( Nfound>1 ) im1 = 3
         CALL open(*120,Lama,iz(ibuf),im1)
         zd(1) = pr
         zd(iz2) = pi
         CALL write(Lama,zd(1),4,1)
         CALL close(Lama,2)
!
!     BUILD LOAD FOR FBS
!
         IF ( minda==0.0D0 ) minda = 1.0D-8
         sign = -sign
         d1 = nrow
         d2 = Nfound
         j = 2*nrow
         DO i = 1 , j , 2
            k = (i+1)/2
            zd(i) = sign*minda/(1.0D0+(1.0D0-float(k)/d1)*d2)
            zd(i+1) = 0.0D0
         ENDDO
         iscr2(1) = sr2
         iscr2(7) = fu(7)
         CALL cdtfbs(zd(1),zd(j+1),iz(ibuf),iscr2,nrow)
!
!     NORMALIZE
!
         d1 = 0.0D0
         DO i = 1 , j , 2
            d2 = zd(i)*zd(i) + zd(i+1)*zd(i+1)
            IF ( d2>=d1 ) THEN
               d3 = zd(i)
               d4 = zd(i+1)
               d1 = d2
            ENDIF
         ENDDO
         IF ( inorm==0 ) THEN
            jj = 2*isil
            d2 = zd(jj)*zd(jj) + zd(jj-1)*zd(jj-1)
            IF ( d2/=0.0D0 .AND. d1/d2<=1.0D6 ) THEN
               d3 = zd(jj-1)
               d4 = zd(jj)
               d1 = d2
            ENDIF
         ENDIF
         DO i = 1 , j , 2
            d5 = (zd(i)*d3+zd(i+1)*d4)/d1
            zd(i+1) = (d3*zd(i+1)-d4*zd(i))/d1
            zd(i) = d5
         ENDDO
!
!     WRITE OUT NORMALIZED VECTOR
!
         file = Phid
         CALL open(*120,Phid,iz(ibuf),im1)
         CALL write(Phid,zd(1),4*nrow,1)
         CALL close(Phid,2)
!
!     STORE ACCEPTED VALUE
!
         iz(irgp+7) = iz(irgp+7) + 1
         nf = nf + 1
         j = (irgp+1)/2 + 2*nf + 10*ne + 14
         zd(j) = pr
         zd(j+1) = pi
         ifail = 0
!
!     CHECK FOR STARTING POINT MOVES
!
         j = ireg1
         i = 1
         spag_nextblock_1 = 18
      CASE (18)
         dt1 = 200.0*epsi*epsi*z(j+9)
         m = 2*iz(j+5) + 2
         k = (j+1)/2 + 5
         l = 1
         spag_nextblock_1 = 19
      CASE (19)
         k = k + 2
         kkk = j + 16*iz(j+5) + 26 + 2*l
         IF ( dsqrt((zd(k)-pi)**2+(zd(k-1)-pr)**2)>=dt1 ) THEN
!
!     SWEEP ACCEPTED VALUE FROM STORED  DETM-S
!
            kk = k + 4*iz(j+5) + 4
            d2 = zd(k-1) - pr
            d3 = zd(k) - pi
            d4 = d2*d2 + d3*d3
            d5 = (zd(kk-1)*d2+zd(kk)*d3)/d4
            zd(kk) = (zd(kk)*d2-zd(kk-1)*d3)/d4
            zd(kk-1) = d5
!
!     SWEEP CONJUGATES S
!
            IF ( icmpx/=1 .AND. dabs(pi)>=1000.0*z(j+9)*epsi ) THEN
               d3 = zd(k) + pi
               d4 = d2*d2 + d3*d3
               d5 = (zd(kk-1)*d2+zd(kk)*d3)/d4
               zd(kk) = (zd(kk)*d2-zd(kk-1)*d3)/d4
               zd(kk-1) = d5
            ENDIF
            spag_nextblock_1 = 20
         ELSE
!
!     SHIFT STARTING POINT
!
            d2 = 1000.0*epsi*epsi*z(j+9)
            zd(k-1) = dsign((z(j+2)-z(j))/z(j+9)*d2+zd(k-1),zd(k-1))
            zd(k) = dsign((z(j+3)-z(j+1))/z(j+9)*d2+zd(k),zd(k))
            nmoves = nmoves + 1
!
!     IF  DETERMINANT EVALUATED - REEVALUATE FOR SHIFT
!
            IF ( iz(kkk)==0 ) THEN
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            dt2 = pr
            dt3 = pi
            pr = zd(k-1)
            pi = zd(k)
            ASSIGN 80 TO dretn
            spag_nextblock_1 = 25
         ENDIF
         CYCLE
 80      pr = dt2
         pi = dt3
         kk = k + 4*iz(j+5) + 4
         zd(kk) = di
         zd(kk-1) = dr
         iz(kkk+1) = powr
         spag_nextblock_1 = 20
      CASE (20)
         zdkm1 = zd(kk-1)
         zdk = zd(kk)
         izk = iz(kkk+1)
         CALL cdetm3(zdkm1,zdk,izk)
         zd(kk-1) = zdkm1
         zd(kk) = zdk
         iz(kkk+1) = izk
         spag_nextblock_1 = 21
      CASE (21)
         l = l + 1
         IF ( l<=m ) THEN
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = iz(j+8)
         i = i + 1
         IF ( i<=nregn ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL klock(itime2)
         CALL tmtogo(itleft)
         IF ( 2*(itime2-itime1)>itleft .AND. Nfound/=ndesrd ) THEN
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nf<nd ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 22
      CASE (22)
!
!     FIND NEXT REGION LACKING ROOTS
!
         DO WHILE ( iz(irgp+8)/=0 )
            irgp = iz(irgp+8)
            IF ( iz(irgp+6)>iz(irgp+7) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 2
      CASE (23)
!
!     INSUFFICIENT TIME
!
         IF ( nmsgx>=maxgx ) nmsgx = maxgx - 1
         CALL mesage(45,ndesrd-Nfound,name)
         iterm = 3
         spag_nextblock_1 = 24
      CASE (24)
!
!     END OF ROUTINE  PUT OUT SUMMARY
!
         CALL gopen(Oceigs,iz(ibuf),1)
         CALL write(Oceigs,ihead(1),10,0)
         iz(1) = Nfound
         iz(iz2) = npass
         iz(iz3) = nchang
         iz(iz4) = nmoves
         iz(iz5) = ndcomp
         iz(iz6) = nfail
         iz(iz7) = noutsd
         iz(iz8) = iterm
         CALL write(Oceigs,iz(1),40,0)
         CALL write(Oceigs,head(1),96,1)
         ihead(3) = 3
         ihead(10) = 6
         CALL write(Oceigs,ihead,50,0)
         CALL write(Oceigs,head,96,1)
         j = ireg1
         DO i = 1 , nregn
            ne = iz(j+5)
            k = (j+1)/2 + 6
            kk = 4*ne + 4
            kd = j + 27 + 16*ne
            ne = 2*ne + 2
            DO l = 1 , ne
               iz(1) = l
               z(iz2) = zd(k)
               z(iz3) = zd(k+1)
               m = k + kk
               kd = kd + 2
               iz(iz6) = iz(kd)
!
!     CONVERT TO MAGNITUDE AND PHASE  SCALE ON MAGNITIDE
!     PHASE IN DEGRESS BETWEEN 0 AND 360
!
               d1 = dsqrt(zd(m)*zd(m)+zd(m+1)*zd(m+1))
               IF ( d1==0.0D0 ) THEN
!
!     NOT  EVALUATED
!
                  z(iz4) = 0.0
                  z(iz5) = 0.0
               ELSE
                  DO WHILE ( d1>10.0D0 )
                     d1 = d1*0.1D0
                     iz(iz6) = iz(iz6) + 1
                  ENDDO
                  DO WHILE ( d1<1.0D0 )
                     d1 = d1*10.0D0
                     iz(iz6) = iz(iz6) - 1
                  ENDDO
                  z(iz4) = d1
!
!     COMPUTE PHASE
!
                  z(iz5) = datan2(zd(m+1),zd(m))*raddeg
!
!     DETERMINE QUADRANT
!
                  IF ( z(iz5)<0. ) z(iz5) = z(iz5) + 360.0
               ENDIF
               CALL write(Oceigs,iz(1),6,0)
               k = k + 2
            ENDDO
            j = iz(j+8)
         ENDDO
         CALL close(Oceigs,1)
         fa(1) = Oceigs
         CALL wrttrl(fa(1))
         RETURN
      CASE (25)
!
!     INTERNAL SUBROUTINE TO EVALUATE DR,DI AT PR,PI
!
         ndcomp = ndcomp + 1
!
!     SET UP FOR ADD
!
         bmcb(1) = pr
         bmcb(2) = pi
         cmcb(1) = pr*pr - pi*pi
         cmcb(2) = 2.*pr*pi
         CALL sadd(z(1),z(1))
         fa(1) = -iabs(fa(1))
         IF ( nosing/=0 ) THEN
            isave = sr2
            sr2 = Scr8
            Scr8 = isave
         ENDIF
         CALL tmtogo(kk)
         IF ( kk<=0 ) THEN
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ib = 0
         CALL cdcomp(*100,z(1),z(1),z(1))
         nosing = 1
         IF ( iprt/=0 ) WRITE (otpe,99001) pr , pi , dr , di , powr
!
!     SCALE DETERMINANT BY POLES AND EIGENVALUES PREVIOUSLY FOUND
!
         id1 = ireg1
         DO id = 1 , nregn
            id2 = iz(id1+5)
            kk = iz(id1+7)
            IF ( kk/=0 ) THEN
               kd = 14 + 10*id2 + (id1+1)/2
               DO ll = 1 , kk
                  kd = kd + 2
                  d1 = pr - zd(kd)
                  d2 = pi - zd(kd+1)
                  d3 = d1*d1 + d2*d2
                  d4 = (dr*d1+di*d2)/d3
                  d5 = (di*d1-dr*d2)/d3
                  dr = d4
                  di = d5
                  IF ( icmpx/=1 ) THEN
!
!     SWEEP COMPLEX CONJUGATE ROOTS
!
                     IF ( dabs(zd(kd+1))>=1000.0*z(id1+9)*epsi ) THEN
                        d2 = pi + zd(kd+1)
                        d3 = d1*d1 + d2*d2
                        d4 = (dr*d1+di*d2)/d3
                        d5 = (di*d1-dr*d2)/d3
                        dr = d4
                        di = d5
                     ENDIF
                  ENDIF
                  CALL cdetm3(dr,di,powr)
               ENDDO
            ENDIF
            id1 = iz(id1+8)
         ENDDO
!
!     SWEEP POLES
!
         IF ( npole/=0 ) THEN
            id1 = ipoles
            DO id = 1 , npole
               d1 = pr - z(id1+1)
               d2 = pi - z(id1+2)
               d3 = 1.0D0
               d4 = 0.0D0
               kd = iz(id1+3)
               DO id2 = 1 , kd
                  d5 = d1*d3 - d2*d4
                  d6 = d2*d3 + d1*d4
                  d3 = d5
                  d4 = d6
               ENDDO
               d1 = d3*d3 + d4*d4
               d2 = (dr*d3+di*d4)/d1
               d5 = (di*d3-dr*d4)/d1
               dr = d2
               di = d5
               id1 = id1 + 4
!
!     SCALE AGAIN
!
               CALL cdetm3(dr,di,powr)
            ENDDO
         ENDIF
         spag_nextblock_1 = 26
         CYCLE SPAG_DispatchLoop_1
!
!     SINGLULAR MATRIX
!
 100     dr = 0.0D0
         di = 0.0D0
         powr = 0
         ising = ising + 1
         minda = 1.0E-11
         IF ( nosing/=0 ) THEN
            nosing = 0
            isave = sr2
            sr2 = Scr8
            Scr8 = isave
         ENDIF
         spag_nextblock_1 = 26
      CASE (26)
!
!     RETURN
!
         IF ( iprt/=0 ) WRITE (otpe,99001) pr , pi , dr , di , powr
         GOTO dretn
!
!     ERROR  MESAGES
!
 120     ip1 = -1
         spag_nextblock_1 = 27
      CASE (27)
         CALL mesage(ip1,file,name)
 140     ip1 = -2
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
 160     ip1 = -3
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
 180     ip1 = -7
         spag_nextblock_1 = 27
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99001 FORMAT (10X,4D16.7,I8)
END SUBROUTINE cdetm
