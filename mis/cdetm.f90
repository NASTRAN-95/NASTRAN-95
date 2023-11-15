
SUBROUTINE cdetm(Method,Eed,Mdd,Bdd,Kdd,Lama,Phid,Oceigs,Nfound,Scr1,Scr2,Scr3,Scr4,Scr5,Scr6,Scr7,Scr8)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION Amcb(2) , Bmcb(2) , Cmcb(2) , D2pi , D4pisq , Degrad , Di , Dpi , Dr , Minda , Raddeg , Zd(1)
   INTEGER Fa(7) , Fl(7) , Fu(7) , Ib , Iz(1) , Ksystm(65) , Lcadd , Mach , Maxgx , Mc(7) , Mcba(12) , Mcbb(12) , Mcbc(12) ,        &
         & Mcbd(12) , Mcbe(12) , Nmsgx , Nomat , Nx , Otpe , Powr , Sr1 , Sr2 , Sr3 , Sysbuf
   REAL Head(1) , Z(1)
   COMMON /cdcmpx/ Fa , Fl , Fu , Sr1 , Sr2 , Sr3 , Dr , Di , Powr , Nx , Minda , Ib
   COMMON /condad/ Dpi , D2pi , Raddeg , Degrad , D4pisq
   COMMON /machin/ Mach
   COMMON /msgx  / Nmsgx , Maxgx
   COMMON /output/ Head
   COMMON /saddx / Nomat , Lcadd , Mcba , Mcbb , Mcbc , Mcbd , Mcbe , Mc
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Bdd , Eed , Kdd , Lama , Mdd , Method , Nfound , Oceigs , Phid , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Scr6 , Scr7 , Scr8
!
! Local variable declarations
!
   REAL aloc1 , alph1 , alph2 , epsi , lu , rll , sign , w1 , w2 , wloc1 , x , xl , xu , xvr , y , yu , yvr
   DOUBLE PRECISION d1 , d10 , d2 , d3 , d4 , d5 , d6 , d7 , d8 , d9 , ddist2 , ddistx , deltki , deltkr , deti(3) , detr(3) ,      &
                  & dsi(3) , dsr(3) , dt1 , dt2 , dt3 , gki , gkr , h1bar , h2bar , h3bar , hk1i , hk1r , hki , hkp1i , hkp1r ,     &
                  & hkr , lamdki , lamdkr , lamk1i , lamk1r , pi , pki(3) , pkr(3) , pr , psi(3) , psr(3) , pti , ptr , rl , rooti ,&
                  & rootr , test , zdk , zdkm1 , zz
   INTEGER dretn , eigc(2) , file , i , ibuf , icmpx , icnt , id , id1 , id2 , id3 , id4 , idd , ifail , iflag , ifpass , igk ,     &
         & ihead(50) , ilusp(2) , im1 , inorm , ip1 , ipdet(3) , ipoint , ipole(2) , ipoles , iprt , ips(3) , ireg1 , irgp , iroot ,&
         & isave , iscr2(7) , isil , ising , isp(2) , ispnt , ispt1 , ispt2 , iterm , itime1 , itime2 , itleft , iz2 , iz3 , iz4 ,  &
         & iz5 , iz6 , iz7 , iz8 , izk , j , jj , k , kd , kk , kkk , kreg , l , lc , ll , lregn , ls , lspt , m , name(2) ,        &
         & nchang , nd , ndcomp , ndesrd , ne , nf , nfail , nit , nmoves , nosing , noutsd , npass , npole , nregn
   INTEGER korsz
   INTEGER nrorg , nrow , nsbdon , nsbrgn , numint , nxorg , poin
!
! End of declarations
!
!
!     SOLVES COMPLEX EIGENVALUE PROBLEM BY DETERMINANT METHOD
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Otpe) , (Amcb(1),Mcba(9)) , (Bmcb(1),Mcbb(9)) , (Cmcb(1),Mcbc(9)) ,                  &
    & (Z(1),Iz(1),Zd(1))
   DATA ipole , eigc , ihead , poin , name/257 , 4 , 207 , 2 , 0 , 1009 , 1 , 47*0 , 4HPOIN , 4HCDET , 4HM   /
   DATA nit , im1 , sign , numint , iz2 , iz3 , iz4 , iz5 , iz6 , iz7 , iz8/20 , 1 , -1.0 , 4 , 2 , 3 , 4 , 5 , 6 , 7 , 8/
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
   IF ( Mach==5 .OR. Mach==21 ) epsi = 1.0E-12
!
   lc = korsz(Z)
   ibuf = lc - Sysbuf - 1
   lc = (ibuf/2)*2 - 1
   nosing = 1
   CALL sswtch(7,iprt)
   ising = 0
   Fa(1) = Kdd
   CALL rdtrl(Fa(1))
   IF ( Fa(1)<=0 ) THEN
      Fa(1) = Mdd
      CALL rdtrl(Fa(1))
      IF ( Fa(1)<=0 ) THEN
         Fa(1) = Bdd
         CALL rdtrl(Fa(1))
         IF ( Fa(1)<=0 ) GOTO 3500
         idd = Bdd
      ELSE
         idd = Mdd
      ENDIF
   ELSE
      idd = Kdd
   ENDIF
   Fa(1) = -Scr2
   Fa(5) = 4
   Fl(1) = idd
   CALL rdtrl(Fl(1))
   Fl(4) = 4
   Fl(5) = 4
   Fu(1) = idd
   CALL rdtrl(Fu(1))
   Fu(4) = 5
   Fu(5) = 4
   Sr1 = Scr3
   Sr2 = Scr4
   Sr3 = Scr5
   Fl(1) = Scr6
   Fu(1) = Scr7
   DO i = 1 , 7
      Mcba(i) = 0
      Mcbb(i) = 0
      Mcbc(i) = 0
      Mc(i) = 0
   ENDDO
   Mcba(1) = Kdd
   Mcbb(1) = Bdd
   Mcbc(1) = Mdd
   CALL rdtrl(Mcba(1))
   CALL rdtrl(Mcbb(1))
   CALL rdtrl(Mcbc(1))
!
!     MUST HAVE  B OR M MATRICES
!
   IF ( Mcbb(1)<0 ) Mcbb(1) = 0
   IF ( Mcbc(1)<0 ) Mcbc(1) = 0
   IF ( Mcbb(1)+Mcbc(1)==0 ) GOTO 3500
   nrow = max0(Mcba(3),Mcbb(3),Mcbc(3))
   icmpx = 0
   IF ( Mcba(5)>2 .OR. Mcbb(5)>2 .OR. Mcbc(5)>2 ) icmpx = 1
   Amcb(1) = 1.0D0
   Amcb(2) = 0.D0
   Mc(2) = Mcba(2)
   Mc(3) = Mcba(3)
   Mc(4) = Mcba(4)
   Mc(5) = 4
   Mcba(8) = 4
   Mcbc(8) = 4
   Mcbb(8) = 4
   Nomat = 3
   Mc(1) = Scr2
   ndesrd = 0
!
!     PICK UP AND STORE ANY POLES
!
   file = Eed
   CALL preloc(*3100,Iz(ibuf),Eed)
   npole = 0
   CALL locate(*100,Iz(ibuf),ipole(1),iflag)
!
!     FOUND POLE CARDS
!
   lc = lc - 4
   DO
      CALL read(*3300,*100,Eed,Iz(lc),4,0,iflag)
      IF ( Iz(lc)==Method ) THEN
         npole = npole + 1
         lc = lc - 4
      ENDIF
   ENDDO
 100  ipoles = lc + 4
!
!     STORE REGIONS
!
   nregn = 0
   CALL locate(*3500,Iz(ibuf),eigc(1),iflag)
   DO
      CALL read(*3300,*3500,Eed,Iz(1),10,0,iflag)
      IF ( Method==Iz(1) .OR. Method==-1 ) THEN
!
!     EIGC CARD FOUND - ALLOCATE CORE + BUILD UP REGIONS
!
         inorm = 0
         IF ( Iz(iz4)/=poin ) inorm = 1
         isil = Iz(iz6)
         IF ( Z(iz8)/=0.0 ) epsi = Z(iz8)
         DO
!
!     PROCESS EACH REGION DEFINITION
!
            CALL read(*3300,*3400,Eed,Z(1),7,0,iflag)
            IF ( Iz(iz7)<0 ) THEN
               Lcadd = lc - 1
               Nx = Lcadd
               Iz(lc+8) = 0
               CALL close(Eed,1)
               IF ( lc<=4*nrow ) THEN
                  ip1 = -8
                  GOTO 3200
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
                  GOTO 200
               ENDIF
            ELSE
               nregn = nregn + 1
               alph1 = Z(1)
               w1 = Z(iz2)
               alph2 = Z(iz3)
               w2 = Z(iz4)
               xl = Z(iz5)
               ne = Iz(iz6)
               nd = Iz(iz7)
               IF ( nd==0 ) nd = 3*ne
               lregn = 20*ne + 4*nd + 32
               ndesrd = ndesrd + nd
               IF ( nregn/=1 ) Iz(lc+8) = lc - lregn
               lc = lc - lregn
               IF ( lc<=0 ) THEN
                  ip1 = -8
                  GOTO 3200
               ELSE
                  IF ( nregn==1 ) ireg1 = lc
!
!     ZERO REGION
!
                  k = lc - 1
                  DO i = 1 , lregn
                     k = k + 1
                     IF ( i/=9 ) Iz(k) = 0
                  ENDDO
!
!     STORE CONSTANTS
                  Z(lc) = alph1
                  Z(lc+1) = w1
                  Z(lc+2) = alph2
                  Z(lc+3) = w2
                  Z(lc+4) = xl
                  Iz(lc+5) = ne
                  Iz(lc+6) = nd
!
!     DISTRIBUTE  STARTING POINTS
!
                  d1 = alph2 - alph1
                  d2 = w2 - w1
                  rl = dsqrt(d1*d1+d2*d2)
                  Z(lc+9) = rl
                  d1 = d1/rl
                  d2 = d2/rl
                  j = (lc+1)/2 + 6
                  d3 = rl/float(4*ne+4)
                  Zd(j) = d1*d3 + alph1
                  Zd(j+1) = d2*d3 + w1
                  k = 2*ne + 1
                  d3 = rl/float(k+1)
                  d1 = d1*d3
                  d2 = d2*d3
                  DO i = 1 , k
                     j = j + 2
                     Zd(j) = Zd(j-2) + d1
                     Zd(j+1) = Zd(j-1) + d2
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
      ELSE
         DO
!
!     SKIP REMAINDER OF EIGC CARD
!
            CALL read(*3300,*3500,Eed,Iz(1),7,0,iflag)
            IF ( Iz(iz6)==-1 ) EXIT
         ENDDO
      ENDIF
   ENDDO
!
!     RETURN HERE TO SEARCH ALL REGIONS AGAIN
!
 200  npass = npass + 1
   IF ( ifpass==0 ) THEN
      iterm = 2
   ELSE
      ifpass = 0
      irgp = ireg1
!
!     FIND REGION WHICH LACKS ROOTS
!
      DO i = 1 , nregn
         IF ( Iz(irgp+6)>Iz(irgp+7) ) GOTO 300
         irgp = Iz(irgp+8)
!
!     ALL REGIONS HAVE ENOUGH ROOTS - EXIT
!
      ENDDO
   ENDIF
   GOTO 2700
!
!     PICK UP REGION POINTERS AND PARAMETERS
!
 300  alph1 = Z(irgp)
   w1 = Z(irgp+1)
   alph2 = Z(irgp+2)
   w2 = Z(irgp+3)
   xl = Z(irgp+4)
   ne = Iz(irgp+5)
   nd = Iz(irgp+6)
   nf = Iz(irgp+7)
   rl = Z(irgp+9)
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
   ddist2 = Zd(ispt1)*Zd(ispt1) + Zd(ispt1+1)*Zd(ispt1+1)
   ispt2 = ispt1 + 2
   DO i = ispt2 , lspt , 2
      zz = Zd(i)*Zd(i) + Zd(i+1)*Zd(i+1)
      IF ( zz>ddist2 ) EXIT
      nxorg = nrorg
      nrorg = i
      ddistx = ddist2
      ddist2 = zz
   ENDDO
   IF ( zz>ddistx ) THEN
      IF ( nxorg==0 ) GOTO 400
   ENDIF
   nxorg = i
   ddistx = zz
!
!     CALCULATE THE NUMBER OR SUBREGIONS, NSBRGN. THERE MUST BE AT LEAST
!     3 POINTS EACH SIDE OF BISECTOR IN ORDER TO HAVE 2 SUBREGIONS.
!          ISPT2+2 .LE. (NRORG+NXORG)/2 .LE. LSPT-4
!
 400  IF ( 2*(ispt2+2)<=nrorg+nxorg .AND. nrorg+nxorg<=(lspt-4)*2 ) THEN
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
      GOTO 600
   ELSE
!
!     ONLY ONE SUBREGION
!     FIND FIRST UNEVALUATED POINT
!
      nsbrgn = 1
      k = irgp + 16*ne + 32
      l = 2*ne
      DO j = 1 , l
         IF ( Iz(k)==0 ) GOTO 900
         k = k + 2
         ipoint = ipoint + 1
         ispnt = ispnt + 2
      ENDDO
!
!     ALL TRIED  GO TO BEGINNING
!
      ipoint = 0
      ispnt = (irgp+1)/2 + 4
      GOTO 900
   ENDIF
!
!     RETURN HERE TO GET NEW STARTING POINT (OR NEW REGION IF NECESSARY)
!     DETINES  ISPNT
!
 500  IF ( nsbrgn==1 ) GOTO 900
   IF ( nsbdon<1 ) THEN
   ELSEIF ( nsbdon==1 ) THEN
      GOTO 800
   ELSE
!
!     SUBREGION 2 IS COMPLETE.  IS SUBREGION 1 FINISHED AS WELL
!
      IF ( nsbdon/=3 ) GOTO 700
      GOTO 2500
   ENDIF
!
!     CHANGE SUBREGIONS
!
 600  kreg = 3 - kreg
   IF ( kreg==2 ) GOTO 800
!
!     PROCESS FIRST SUBREGION
!
 700  ispnt = ilusp(1)
   ls = isp(2)
   aloc1 = Zd(ls)
   wloc1 = Zd(ls+1)
   ilusp(1) = ilusp(1) + 2
   IF ( ispnt+4==lspt ) THEN
!
!     PROCESS LAST SET OF STARTING IN FIRST SUBREGION
!
      nsbdon = nsbdon + 1
      pr = .45*Zd(ispnt+4) + .55*alph1 - aloc1
      pi = .45*Zd(ispnt+5) + .55*w1 - wloc1
   ELSE
      pr = .45*Zd(ispnt+4) + .55*Zd(ispnt+6) - aloc1
      pi = .45*Zd(ispnt+5) + .55*Zd(ispnt+7) - wloc1
   ENDIF
   ipoint = (ispnt-ispt1)/2 + 1
   GOTO 1000
!
!     PROCESS SUBREGION 2
!
 800  ispnt = ilusp(2) - 2
   ilusp(2) = ilusp(2) - 2
   ls = isp(1)
   aloc1 = Zd(ls)
   wloc1 = Zd(ls+1)
   IF ( ispnt==ispt1 ) THEN
!
!     LAST SET OF STARTING POINTS IN SUBREGION2 TO PROCESS
!
      nsbdon = nsbdon + 2
      pr = -(.45*Zd(ispt1)+.55*Zd(ispnt)) + aloc1
      pi = -(.45*Zd(ispt1+1)+.55*Zd(ispnt+1)) + wloc1
   ELSE
      pr = -(.45*Zd(ispnt-2)+.55*Zd(ispnt)) + aloc1
      pi = -(.45*Zd(ispnt-1)+.55*Zd(ispnt+1)) + wloc1
   ENDIF
   ipoint = (ispnt-ispt1)/2 + 1
   GOTO 1000
!
!     ONLY ONE SUBREGION PROCESS FROM END TO END
!
 900  ipoint = ipoint + 1
   ispnt = ispnt + 2
   IF ( ipoint>2*ne ) GOTO 2500
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
      pr = .45*Zd(ispnt+4) + .55*Zd(ispnt+6) - aloc1
      pi = .45*Zd(ispnt+5) + .55*Zd(ispnt+7) - wloc1
   ENDIF
 1000 rll = dsqrt(pr*pr+pi*pi)
   k = irgp + 16*ne + 24 + 2*ipoint
   i = 1
   ising = 0
 1100 k = k + 2
   IF ( Iz(k)/=0 ) GOTO 1300
!
!     EVALUATE DETERMINANT
!
   j = ispnt + 2*i - 2
   pr = Zd(j)
   pi = Zd(j+1)
   ASSIGN 1200 TO dretn
   GOTO 2800
 1200 Iz(k) = 1
   Iz(k+1) = Powr
   m = 4*ne + 2 + ispnt + 2*i
   Zd(m) = Dr
   Zd(m+1) = Di
 1300 i = i + 1
   IF ( i<=3 ) GOTO 1100
   IF ( ising==3 .AND. npass==0 ) THEN
!
!     SINGULAR MATRIX
!
      iterm = 4
      GOTO 2700
   ENDIF
!
!     SORT STARTING POINTS BY MAGNITUDE OF DET
!
 1400 CALL klock(itime1)
   k = ispnt + 4*ne + 4
   l = irgp + 16*ne + 26 + 2*ipoint
   CALL cdetm2(Zd(ispnt),Zd(k),Iz(l),psr(1),psi(1),dsr(1),dsi(1),ips(1))
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
 1500 hk1r = pkr(2) - pkr(1)
   hk1i = pki(2) - pki(1)
   hkr = pkr(3) - pkr(2)
   hki = pki(3) - pki(2)
   IF ( hkr==0.0D0 .AND. hki==0.0D0 ) GOTO 1900
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
   DO
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
      IF ( icnt/=1 ) EXIT
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
         EXIT
      ENDIF
   ENDDO
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
         GOTO 500
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
      ASSIGN 1600 TO dretn
      GOTO 2800
   ELSE
      xu = xu/lu
      yu = yu/lu
      y = lu*(yu*xvr-xu*yvr)
      x = lu*(xu*xvr+yu*yvr)
      IF ( abs(y)>xl/2.0 .OR. x<0.0 .OR. x>rll ) GOTO 500
      ASSIGN 1600 TO dretn
      GOTO 2800
   ENDIF
!
!     BEGIN CONVERGENCE TESTS
!
 1600 IF ( icnt<=2 ) THEN
!
!     CONTINUE INTERATIONS
!
      icnt = icnt + 1
      GOTO 1800
   ELSE
      h1bar = dsqrt(hk1r*hk1r+hk1i*hk1i)
      h2bar = dsqrt(hkr*hkr+hki*hki)
      h3bar = dsqrt(hkp1r*hkp1r+hkp1i*hkp1i)
   ENDIF
 1700 test = epsi*rl
   IF ( h1bar<=test*1.0E7 ) THEN
      IF ( h2bar<=test*1.0E4 ) THEN
         IF ( h3bar>h2bar ) THEN
            IF ( h2bar<=1.0E-7*rl ) GOTO 1900
         ELSEIF ( h3bar<=test ) THEN
            GOTO 1900
         ENDIF
      ENDIF
   ENDIF
   icnt = icnt + 1
   IF ( icnt<nit ) GOTO 1800
   IF ( icnt==nit ) THEN
      IF ( nchang<numint .AND. ifail==1 ) THEN
         epsi = epsi*10.0
         nchang = nchang + 1
         GOTO 1700
      ENDIF
   ENDIF
   ifail = 1
   nfail = nfail + 1
   GOTO 500
 1800 DO i = 1 , 2
      pkr(i) = pkr(i+1)
      pki(i) = pki(i+1)
      ipdet(i) = ipdet(i+1)
      detr(i) = detr(i+1)
      deti(i) = deti(i+1)
   ENDDO
   pkr(3) = pr
   pki(3) = pi
   detr(3) = Dr
   deti(3) = Di
   ipdet(3) = Powr
   GOTO 1500
!
!     ACCEPT CURRENT EIGENVALUE
!
 1900 file = Lama
   Nfound = Nfound + 1
   ifpass = 1
   IF ( Nfound>1 ) im1 = 3
   CALL open(*3100,Lama,Iz(ibuf),im1)
   Zd(1) = pr
   Zd(iz2) = pi
   CALL write(Lama,Zd(1),4,1)
   CALL close(Lama,2)
!
!     BUILD LOAD FOR FBS
!
   IF ( Minda==0.0D0 ) Minda = 1.0D-8
   sign = -sign
   d1 = nrow
   d2 = Nfound
   j = 2*nrow
   DO i = 1 , j , 2
      k = (i+1)/2
      Zd(i) = sign*Minda/(1.0D0+(1.0D0-float(k)/d1)*d2)
      Zd(i+1) = 0.0D0
   ENDDO
   iscr2(1) = Sr2
   iscr2(7) = Fu(7)
   CALL cdtfbs(Zd(1),Zd(j+1),Iz(ibuf),iscr2,nrow)
!
!     NORMALIZE
!
   d1 = 0.0D0
   DO i = 1 , j , 2
      d2 = Zd(i)*Zd(i) + Zd(i+1)*Zd(i+1)
      IF ( d2>=d1 ) THEN
         d3 = Zd(i)
         d4 = Zd(i+1)
         d1 = d2
      ENDIF
   ENDDO
   IF ( inorm==0 ) THEN
      jj = 2*isil
      d2 = Zd(jj)*Zd(jj) + Zd(jj-1)*Zd(jj-1)
      IF ( d2/=0.0D0 .AND. d1/d2<=1.0D6 ) THEN
         d3 = Zd(jj-1)
         d4 = Zd(jj)
         d1 = d2
      ENDIF
   ENDIF
   DO i = 1 , j , 2
      d5 = (Zd(i)*d3+Zd(i+1)*d4)/d1
      Zd(i+1) = (d3*Zd(i+1)-d4*Zd(i))/d1
      Zd(i) = d5
   ENDDO
!
!     WRITE OUT NORMALIZED VECTOR
!
   file = Phid
   CALL open(*3100,Phid,Iz(ibuf),im1)
   CALL write(Phid,Zd(1),4*nrow,1)
   CALL close(Phid,2)
!
!     STORE ACCEPTED VALUE
!
   Iz(irgp+7) = Iz(irgp+7) + 1
   nf = nf + 1
   j = (irgp+1)/2 + 2*nf + 10*ne + 14
   Zd(j) = pr
   Zd(j+1) = pi
   ifail = 0
!
!     CHECK FOR STARTING POINT MOVES
!
   j = ireg1
   i = 1
 2000 dt1 = 200.0*epsi*epsi*Z(j+9)
   m = 2*Iz(j+5) + 2
   k = (j+1)/2 + 5
   l = 1
 2100 k = k + 2
   kkk = j + 16*Iz(j+5) + 26 + 2*l
   IF ( dsqrt((Zd(k)-pi)**2+(Zd(k-1)-pr)**2)>=dt1 ) THEN
!
!     SWEEP ACCEPTED VALUE FROM STORED  DETM-S
!
      kk = k + 4*Iz(j+5) + 4
      d2 = Zd(k-1) - pr
      d3 = Zd(k) - pi
      d4 = d2*d2 + d3*d3
      d5 = (Zd(kk-1)*d2+Zd(kk)*d3)/d4
      Zd(kk) = (Zd(kk)*d2-Zd(kk-1)*d3)/d4
      Zd(kk-1) = d5
!
!     SWEEP CONJUGATES S
!
      IF ( icmpx/=1 .AND. dabs(pi)>=1000.0*Z(j+9)*epsi ) THEN
         d3 = Zd(k) + pi
         d4 = d2*d2 + d3*d3
         d5 = (Zd(kk-1)*d2+Zd(kk)*d3)/d4
         Zd(kk) = (Zd(kk)*d2-Zd(kk-1)*d3)/d4
         Zd(kk-1) = d5
      ENDIF
      GOTO 2300
   ELSE
!
!     SHIFT STARTING POINT
!
      d2 = 1000.0*epsi*epsi*Z(j+9)
      Zd(k-1) = dsign((Z(j+2)-Z(j))/Z(j+9)*d2+Zd(k-1),Zd(k-1))
      Zd(k) = dsign((Z(j+3)-Z(j+1))/Z(j+9)*d2+Zd(k),Zd(k))
      nmoves = nmoves + 1
!
!     IF  DETERMINANT EVALUATED - REEVALUATE FOR SHIFT
!
      IF ( Iz(kkk)==0 ) GOTO 2400
      dt2 = pr
      dt3 = pi
      pr = Zd(k-1)
      pi = Zd(k)
      ASSIGN 2200 TO dretn
      GOTO 2800
   ENDIF
 2200 pr = dt2
   pi = dt3
   kk = k + 4*Iz(j+5) + 4
   Zd(kk) = Di
   Zd(kk-1) = Dr
   Iz(kkk+1) = Powr
 2300 zdkm1 = Zd(kk-1)
   zdk = Zd(kk)
   izk = Iz(kkk+1)
   CALL cdetm3(zdkm1,zdk,izk)
   Zd(kk-1) = zdkm1
   Zd(kk) = zdk
   Iz(kkk+1) = izk
 2400 l = l + 1
   IF ( l<=m ) GOTO 2100
   j = Iz(j+8)
   i = i + 1
   IF ( i<=nregn ) GOTO 2000
   CALL klock(itime2)
   CALL tmtogo(itleft)
   IF ( 2*(itime2-itime1)>itleft .AND. Nfound/=ndesrd ) GOTO 2600
   IF ( nf<nd ) GOTO 1400
!
!     FIND NEXT REGION LACKING ROOTS
!
 2500 DO WHILE ( Iz(irgp+8)/=0 )
      irgp = Iz(irgp+8)
      IF ( Iz(irgp+6)>Iz(irgp+7) ) GOTO 300
   ENDDO
   GOTO 200
!
!     INSUFFICIENT TIME
!
 2600 IF ( Nmsgx>=Maxgx ) Nmsgx = Maxgx - 1
   CALL mesage(45,ndesrd-Nfound,name)
   iterm = 3
!
!     END OF ROUTINE  PUT OUT SUMMARY
!
 2700 CALL gopen(Oceigs,Iz(ibuf),1)
   CALL write(Oceigs,ihead(1),10,0)
   Iz(1) = Nfound
   Iz(iz2) = npass
   Iz(iz3) = nchang
   Iz(iz4) = nmoves
   Iz(iz5) = ndcomp
   Iz(iz6) = nfail
   Iz(iz7) = noutsd
   Iz(iz8) = iterm
   CALL write(Oceigs,Iz(1),40,0)
   CALL write(Oceigs,Head(1),96,1)
   ihead(3) = 3
   ihead(10) = 6
   CALL write(Oceigs,ihead,50,0)
   CALL write(Oceigs,Head,96,1)
   j = ireg1
   DO i = 1 , nregn
      ne = Iz(j+5)
      k = (j+1)/2 + 6
      kk = 4*ne + 4
      kd = j + 27 + 16*ne
      ne = 2*ne + 2
      DO l = 1 , ne
         Iz(1) = l
         Z(iz2) = Zd(k)
         Z(iz3) = Zd(k+1)
         m = k + kk
         kd = kd + 2
         Iz(iz6) = Iz(kd)
!
!     CONVERT TO MAGNITUDE AND PHASE  SCALE ON MAGNITIDE
!     PHASE IN DEGRESS BETWEEN 0 AND 360
!
         d1 = dsqrt(Zd(m)*Zd(m)+Zd(m+1)*Zd(m+1))
         IF ( d1==0.0D0 ) THEN
!
!     NOT  EVALUATED
!
            Z(iz4) = 0.0
            Z(iz5) = 0.0
         ELSE
            DO WHILE ( d1>10.0D0 )
               d1 = d1*0.1D0
               Iz(iz6) = Iz(iz6) + 1
            ENDDO
            DO WHILE ( d1<1.0D0 )
               d1 = d1*10.0D0
               Iz(iz6) = Iz(iz6) - 1
            ENDDO
            Z(iz4) = d1
!
!     COMPUTE PHASE
!
            Z(iz5) = datan2(Zd(m+1),Zd(m))*Raddeg
!
!     DETERMINE QUADRANT
!
            IF ( Z(iz5)<0. ) Z(iz5) = Z(iz5) + 360.0
         ENDIF
         CALL write(Oceigs,Iz(1),6,0)
         k = k + 2
      ENDDO
      j = Iz(j+8)
   ENDDO
   CALL close(Oceigs,1)
   Fa(1) = Oceigs
   CALL wrttrl(Fa(1))
   RETURN
!
!     INTERNAL SUBROUTINE TO EVALUATE DR,DI AT PR,PI
!
 2800 ndcomp = ndcomp + 1
!
!     SET UP FOR ADD
!
   Bmcb(1) = pr
   Bmcb(2) = pi
   Cmcb(1) = pr*pr - pi*pi
   Cmcb(2) = 2.*pr*pi
   CALL sadd(Z(1),Z(1))
   Fa(1) = -iabs(Fa(1))
   IF ( nosing/=0 ) THEN
      isave = Sr2
      Sr2 = Scr8
      Scr8 = isave
   ENDIF
   CALL tmtogo(kk)
   IF ( kk<=0 ) GOTO 2600
   Ib = 0
   CALL cdcomp(*2900,Z(1),Z(1),Z(1))
   nosing = 1
   IF ( iprt/=0 ) WRITE (Otpe,99001) pr , pi , Dr , Di , Powr
!
!     SCALE DETERMINANT BY POLES AND EIGENVALUES PREVIOUSLY FOUND
!
   id1 = ireg1
   DO id = 1 , nregn
      id2 = Iz(id1+5)
      kk = Iz(id1+7)
      IF ( kk/=0 ) THEN
         kd = 14 + 10*id2 + (id1+1)/2
         DO ll = 1 , kk
            kd = kd + 2
            d1 = pr - Zd(kd)
            d2 = pi - Zd(kd+1)
            d3 = d1*d1 + d2*d2
            d4 = (Dr*d1+Di*d2)/d3
            d5 = (Di*d1-Dr*d2)/d3
            Dr = d4
            Di = d5
            IF ( icmpx/=1 ) THEN
!
!     SWEEP COMPLEX CONJUGATE ROOTS
!
               IF ( dabs(Zd(kd+1))>=1000.0*Z(id1+9)*epsi ) THEN
                  d2 = pi + Zd(kd+1)
                  d3 = d1*d1 + d2*d2
                  d4 = (Dr*d1+Di*d2)/d3
                  d5 = (Di*d1-Dr*d2)/d3
                  Dr = d4
                  Di = d5
               ENDIF
            ENDIF
            CALL cdetm3(Dr,Di,Powr)
         ENDDO
      ENDIF
      id1 = Iz(id1+8)
   ENDDO
!
!     SWEEP POLES
!
   IF ( npole/=0 ) THEN
      id1 = ipoles
      DO id = 1 , npole
         d1 = pr - Z(id1+1)
         d2 = pi - Z(id1+2)
         d3 = 1.0D0
         d4 = 0.0D0
         kd = Iz(id1+3)
         DO id2 = 1 , kd
            d5 = d1*d3 - d2*d4
            d6 = d2*d3 + d1*d4
            d3 = d5
            d4 = d6
         ENDDO
         d1 = d3*d3 + d4*d4
         d2 = (Dr*d3+Di*d4)/d1
         d5 = (Di*d3-Dr*d4)/d1
         Dr = d2
         Di = d5
         id1 = id1 + 4
!
!     SCALE AGAIN
!
         CALL cdetm3(Dr,Di,Powr)
      ENDDO
   ENDIF
   GOTO 3000
!
!     SINGLULAR MATRIX
!
 2900 Dr = 0.0D0
   Di = 0.0D0
   Powr = 0
   ising = ising + 1
   Minda = 1.0E-11
   IF ( nosing/=0 ) THEN
      nosing = 0
      isave = Sr2
      Sr2 = Scr8
      Scr8 = isave
   ENDIF
!
!     RETURN
!
 3000 IF ( iprt/=0 ) WRITE (Otpe,99001) pr , pi , Dr , Di , Powr
   GOTO dretn
!
!     ERROR  MESAGES
!
 3100 ip1 = -1
 3200 CALL mesage(ip1,file,name)
 3300 ip1 = -2
   GOTO 3200
 3400 ip1 = -3
   GOTO 3200
 3500 ip1 = -7
   GOTO 3200
99001 FORMAT (10X,4D16.7,I8)
END SUBROUTINE cdetm
