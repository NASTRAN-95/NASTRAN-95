
SUBROUTINE alg02
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Bblock(30) , Bdist(30) , C1 , Conmx , Contr , Cppg(21) , Cr(21) , Data1(100) , Data2(100) , Data3(100) , Data4(100) ,       &
      & Data5(100) , Data6(100) , Data7(100) , Data8(100) , Data9(100) , Datac(100) , Delc(100) , Delf(30) , Delh(30) , Delt(30) ,  &
      & Delta(100) , Delw(21) , Diff(15,4) , Dm(11,5,2) , Drdm2(30) , Ej , Fdhub(15,4) , Fdmid(15,4) , Fdtip(15,4) , Flow(10) ,     &
      & Fm2 , G , Gama(21) , H(21,30) , Hkeep(21) , Hmin , Lami(21) , Lamim1(21) , Lamip1(21) , Loss(21) , Phi(21) , Pi , Plow ,    &
      & Pscale , R(21,30) , Rconst , Rim1(30) , Rlow , Rstn(150) , S(21,30) , Sclfac , Shape , Skeep(21) , Spdfac(10) , Speed(30) , &
      & Sppg(21) , Taneps(21) , Tbeta(21,30) , Terad(5,2) , Title(18) , Tolnce , Visk , Vm(21,30) , Vv(21) , Vw(21,30) , Vwkeep(21) &
      & , Wblock(30) , Wfrac(11,5,2) , Work(21) , Wwbl(30) , X(21,30) , Xi(21) , Xim1(30) , Xl(21,30) , Xmmax , Xscale , Xstn(150)
   INTEGER I , Icase , Ifail , Ifailo , Iffail , Ii(21,30) , Iloss , Imid , Ipass , Iprint , Iprtc , Is1(30) , Is2(30) , Is3(30) ,  &
         & Istag , Iter , Itub , Ivfail , Jj(21,30) , Limit , Lnct , Log1 , Log2 , Log3 , Log4 , Log5 , Log6 , Lq , Nbl , Nblade(30)&
         & , Ncase , Ncurve(30) , Ndata(30) , Ndel(30) , Ndiff(4) , Ndimen(30) , Neqn , Neval(30) , Nforce , Nl1(30) , Nl2(30) ,    &
         & Nliter(30) , Nloss(30) , Nm(2) , Nmach(30) , Nmany , Nmax , Nmix , Nout1(30) , Nout2(30) , Nout3(30) , Npage , Nplot ,   &
         & Npunch , Nrad(2) , Nread , Nset1 , Nset2 , Nsign , Nspec(30) , Nsplit , Nstns , Nstplt , Nstrms , Nterp(30) , Ntrans ,   &
         & Nwhich(30) , Nwork(30)
   COMMON /ud300c/ Nstns , Nstrms , Nmax , Nforce , Nbl , Ncase , Nsplit , Nread , Npunch , Npage , Nset1 , Nset2 , Istag , Icase , &
                 & Ifailo , Ipass , I , Ivfail , Iffail , Nmix , Ntrans , Nplot , Iloss , Lnct , Itub , Imid , Ifail , Iter , Log1 ,&
                 & Log2 , Log3 , Log4 , Log5 , Log6 , Iprint , Nmany , Nstplt , Neqn , Nspec , Nwork , Nloss , Ndata , Nterp ,      &
                 & Nmach , Nl1 , Nl2 , Ndimen , Is1 , Is2 , Is3 , Neval , Ndiff , Ndel , Nliter , Nm , Nrad , Ncurve , Nwhich ,     &
                 & Nout1 , Nout2 , Nout3 , Nblade , Dm , Wfrac , R , Xl , X , H , S , Vm , Vw , Tbeta , Diff , Fdhub , Fdmid ,      &
                 & Fdtip , Terad , Datac , Data1 , Data2 , Data3 , Data4 , Data5 , Data6 , Data7 , Data8 , Data9 , Flow , Speed ,   &
                 & Spdfac , Bblock , Bdist , Wblock , Wwbl , Xstn , Rstn , Delf , Delc , Delta , Title , Drdm2 , Rim1 , Xim1 ,      &
                 & Work , Loss , Taneps , Xi , Vv , Delw , Lami , Lamim1 , Lamip1 , Phi , Cr , Gama , Sppg , Cppg , Hkeep , Skeep , &
                 & Vwkeep , Delh , Delt , Visk , Shape , Sclfac , Ej , G , Tolnce , Xscale , Pscale , Plow , Rlow , Xmmax , Rconst ,&
                 & Fm2 , Hmin , C1 , Pi , Contr , Conmx
   COMMON /ud3prt/ Iprtc
   COMMON /udsign/ Nsign
   COMMON /upage / Limit , Lq
!
! Local variable declarations
!
   LOGICAL debug
   INTEGER idata(24) , j , k , l , l1 , l2 , last , lastd , logn , name(2) , next , nle , nte
   REAL rdata(6)
!
! End of declarations
!
!
   EQUIVALENCE (H(1,1),Ii(1,1)) , (S(1,1),Jj(1,1))
   DATA name/4HALG0 , 4H2   /
!
   debug = .FALSE.
   CALL sswtch(20,j)
   IF ( j==1 ) debug = .TRUE.
   Neval(1) = 0
   CALL fread(Log1,Title,18,1)
   IF ( Iprtc==1 ) WRITE (Log2,99001) Title
99001 FORMAT (10X,10HINPUT DATA,/10X,10(1H*),//10X,5HTITLE,34X,2H= ,18A4)
   Lnct = Lnct + 4
   CALL alg1(Lnct)
   CALL fread(Log1,idata,21,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',111,IDATA,21)
   Nstns = idata(1)
   Nstrms = idata(2)
   Nmax = idata(3)
   Nforce = idata(4)
   Nbl = idata(5)
   Ncase = idata(6)
   Nsplit = idata(7)
   Nset1 = idata(8)
   Nset2 = idata(9)
   Nread = idata(10)
   Npunch = idata(11)
   Nplot = idata(12)
   Npage = idata(13)
   Ntrans = idata(14)
   Nmix = idata(15)
   Nmany = idata(16)
   Nstplt = idata(17)
   Neqn = idata(18)
   nle = idata(19)
   nte = idata(20)
   Nsign = idata(21)
   IF ( Nstrms==0 ) Nstrms = 11
   IF ( Nmax==0 ) Nmax = 40
   IF ( Nforce==0 ) Nforce = 10
   IF ( Ncase==0 ) Ncase = 1
   IF ( Npage==0 ) Npage = 60
   Lq = Log2
   Limit = Npage
   CALL alg03(Lnct,19)
   IF ( Iprtc==1 ) WRITE (Log2,99002) Nstns , Nstrms , Nmax , Nforce , Nbl , Ncase , Nsplit , Nset1 , Nset2 , Nread , Npunch ,      &
                                    & Nplot , Npage , Ntrans , Nmix , Nmany , Nstplt , Neqn , nle , nte , Nsign
99002 FORMAT (//10X,'NUMBER OF STATIONS',21X,1H=,I3,/10X,'NUMBER OF ','STREAMLINES',18X,1H=,I3,/10X,20HMAX NUMBER OF PASSES,19X,1H=,&
            & I3,/10X,30HMAX NUMBER OF ARBITRARY PASSES,9X,1H=,I3,/10X,29HBOUNDARY LAYER CALC INDICATOR,10X,1H=,I3,/10X,            &
             &24HNUMBER OF RUNNING POINTS,15X,1H=,I3,/10X,33HSTREAMLINE DISTRIBUTION INDICATOR,6X,1H=,I3,/10X,                      &
             &34HNUMBER OF LOSS/D-FACTOR CURVE SETS,5X,1H=,I3,/10X,34HNUMBER OF LOSS/T.E.LOSS CURVE SETS,5X,1H=,I3,/10X,            &
             &26HSTREAMLINE INPUT INDICATOR,13X,1H=,I3,/10X,27HSTREAMLINE OUTPUT INDICATOR,12X,1H=,I3,/10X,                         &
             &24HPRECISION PLOT INDICATOR,15X,1H=,I3,/10X,24HMAX NUMBER OF LINES/PAGE,15X,1H=,I3,/10X,                              &
             &29HWAKE TRANSPORT CALC INDICATOR,10X,1H=,I3,/10X,32HMAINSTREAM MIXING CALC INDICATOR,7X,1H=,I3,/10X,                  &
             &33HNO OF STATIONS FROM ANALYTIC SECN,6X,1H=,I3,/10X,27HLINE-PRINTER PLOT INDICATOR,12X,1H=,I3,/10X,                   &
             &32HMOMENTUM EQUATION FORM INDICATOR,7X,1H=,I3,/10X,30HSTATION NUMBER AT LEADING EDGE,9X,1H=,I3,/10X,                  &
             &31HSTATION NUMBER AT TRAILING EDGE,8X,1H=,I3,/10X,37HCOMPRESSOR DIR. OF ROTATION INDICATOR,2X,1H=,I3)
   Itub = Nstrms - 1
   Imid = Nstrms/2 + 1
   IF ( Nmany/=0 ) THEN
      CALL fread(Log1,Nwhich,Nmany,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',132,NWHICH,NMANY)
      CALL alg03(Lnct,2)
      IF ( Iprtc==1 ) WRITE (Log2,99003) (Nwhich(I),I=1,Nmany)
99003 FORMAT (//10X,'GEOMETRY COMES FROM ANALYTIC SECTION FOR STATIONS',23I3)
   ENDIF
   CALL alg03(Lnct,7)
   CALL fread(Log1,rdata,6,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',136,RDATA,6)
   G = rdata(1)
   Ej = rdata(2)
   Sclfac = rdata(3)
   Tolnce = rdata(4)
   Visk = rdata(5)
   Shape = rdata(6)
   IF ( G==0.0 ) G = 32.174
   IF ( Ej==0.0 ) Ej = 778.16
   IF ( Sclfac==0. ) Sclfac = 12.0
   IF ( Tolnce==0. ) Tolnce = 0.001
   IF ( Visk==0.0 ) Visk = 0.00018
   IF ( Shape==0.0 ) Shape = 0.7
   IF ( Iprtc==1 ) WRITE (Log2,99004) G , Ej , Sclfac , Tolnce , Visk , Shape
99004 FORMAT (//10X,22HGRAVITATIONAL CONSTANT,17X,1H=,F8.4,/10X,17HJOULES EQUIVALENT,22X,1H=,F8.3,/10X,                             &
             &29HLINEAR DIMENSION SCALE FACTOR,10X,1H=,F8.4,/10X,15HBASIC TOLERANCE,24X,1H=,F8.5,/10X,19HKINEMATIC VISCOSITY,20X,   &
            & 1H=,F8.5,/10X,17HB.L. SHAPE FACTOR,22X,1H=,F8.5)
   CALL alg03(Lnct,7)
   CALL fread(Log1,rdata,6,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',151,RDATA,6)
   Xscale = rdata(1)
   Pscale = rdata(2)
   Rlow = rdata(3)
   Plow = rdata(4)
   Xmmax = rdata(5)
   Rconst = rdata(6)
   IF ( Xmmax==0.0 ) Xmmax = 0.6
   IF ( Rconst==0.0 ) Rconst = 6.0
   IF ( Iprtc==1 ) WRITE (Log2,99005) Xscale , Pscale , Rlow , Plow , Xmmax , Rconst
99005 FORMAT (//10X,29HPLOTTING SCALE FOR DIMENSIONS,10X,1H=,F7.3,/10X,28HPLOTTING SCALE FOR PRESSURES,11X,1H=,F7.3,/10X,           &
             &22HMINIMUM RADIUS ON PLOT,17X,1H=,F7.3,/10X,24HMINIMUM PRESSURE ON PLOT,15X,1H=,F7.3,/10X,                            &
             &40HMAXIMUM M-SQUARED IN RELAXATION FACTOR =,F8.4,/10X,29HCONSTANT IN RELAXATION FACTOR,10X,1H=,F8.4)
   CALL alg03(Lnct,3)
   CALL fread(Log1,rdata,2,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',162,RDATA,2)
   Contr = rdata(1)
   Conmx = rdata(2)
   IF ( Iprtc==1 ) WRITE (Log2,99006) Contr , Conmx
99006 FORMAT (//10X,22HWAKE TRANSFER CONSTANT,17X,1H=,F8.5,/10X,25HTURBULENT MIXING CONSTANT,14X,1H=,F8.5)
   CALL alg03(Lnct,5+Ncase)
   DO k = 1 , Ncase
      CALL fread(Log1,Flow(k),1,0)
      CALL fread(Log1,Spdfac(k),1,1)
   ENDDO
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',171,FLOW,NCASE)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',172,SPDFAC,NCASE)
   IF ( Iprtc==1 ) WRITE (Log2,99007) (k,Flow(k),Spdfac(k),k=1,Ncase)
99007 FORMAT (//10X,21HPOINTS TO BE COMPUTED,//10X,2HNO,6X,8HFLOWRATE,4X,12HSPEED FACTOR,//,(10X,I2,F13.3,F14.3))
   CALL fread(Log1,l1,1,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',180,L1,1)
   DO k = 1 , l1
      CALL fread(Log1,Xstn(k),1,0)
      CALL fread(Log1,Rstn(k),1,1)
   ENDDO
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',191,XSTN,L1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',192,RSTN,L1)
   Istag = 0
   IF ( Rstn(1)==0.0 ) Istag = 1
   Nspec(1) = l1
   CALL alg03(Lnct,7+l1)
   IF ( Iprtc==1 ) WRITE (Log2,99008) l1 , (Xstn(k),Rstn(k),k=1,l1)
99008 FORMAT (//10X,'ANNULUS / COMPUTING STATION GEOMETRY',//10X,24HSTATION  1  SPECIFIED BY,I3,7H POINTS,//17X,4HXSTN,8X,4HRSTN,//,&
            & (F22.4,F12.4))
   Is1(1) = 1
   last = l1
   DO I = 2 , Nstns
      CALL fread(Log1,l1,1,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',210,L1,1)
      next = last + 1
      last = last + l1
      IF ( last>150 ) GOTO 200
      DO k = next , last
         CALL fread(Log1,Xstn(k),1,0)
         CALL fread(Log1,Rstn(k),1,1)
      ENDDO
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',215,XSTN(NEXT),LAST-NEXT+1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',216,RSTN(NEXT),LAST-NEXT+1)
      IF ( Rstn(next)==0.0 ) Istag = I
      CALL alg03(Lnct,5+l1)
      Is1(I) = next
      Nspec(I) = l1
      IF ( Iprtc==1 ) WRITE (Log2,99009) I , l1 , (Xstn(k),Rstn(k),k=next,last)
99009 FORMAT (//10X,7HSTATION,I3,14H  SPECIFIED BY,I3,7H POINTS,//17X,4HXSTN,8X,4HRSTN,//,(F22.4,F12.4))
   ENDDO
   Speed(1) = 0.0
   CALL fread(Log1,idata,4,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',233,IDATA,4)
   l1 = idata(1)
   Nterp(1) = idata(2)
   Ndimen(1) = idata(3)
   Nmach(1) = idata(4)
   DO k = 1 , l1
      CALL fread(Log1,rdata,4,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',234,RDATA,4)
      Datac(k) = rdata(1)
      Data1(k) = rdata(2)
      Data2(k) = rdata(3)
      Data3(k) = rdata(4)
   ENDDO
   CALL alg03(Lnct,7+l1)
   Is2(1) = 1
   Ndata(1) = l1
   last = l1
   IF ( Iprtc==1 ) WRITE (Log2,99010) l1 , Nterp(1) , Ndimen(1) , Nmach(1) , (Datac(k),Data1(k),Data2(k),Data3(k),k=1,l1)
99010 FORMAT (//10X,24HSTATION CALCULATION DATA,//7X,18HSTATION  1  NDATA=,I3,7H NTERP=,I2,8H NDIMEN=,I2,7H NMACH=,I2,//11X,5HDATAC,&
            & 6X,14HTOTAL PRESSURE,4X,17HTOTAL TEMPERATURE,4X,11HWHIRL ANGLE,//,(5X,F12.4,F15.4,F19.3,F18.3))
   DO k = 1 , l1
      Data1(k) = Data1(k)*Sclfac**2
   ENDDO
   lastd = 0
   Nout1(1) = 0
   Nout2(1) = 0
   DO I = 2 , Nstns
      logn = Log1
      IF ( Nmany/=0 ) THEN
         DO l1 = 1 , Nmany
            IF ( Nwhich(l1)==I ) GOTO 50
         ENDDO
      ENDIF
      GOTO 100
 50   logn = Log5
 100  CALL fread(logn,idata,16,1)
!WKBD IF (DEBUG .AND. LOGN.EQ.LOG1) CALL BUG1 ('ALG02   ',258,IDATA,16)
      Ndata(I) = idata(1)
      Nterp(I) = idata(2)
      Ndimen(I) = idata(3)
      Nmach(I) = idata(4)
      Nwork(I) = idata(5)
      Nloss(I) = idata(6)
      Nl1(I) = idata(7)
      Nl2(I) = idata(8)
      Neval(I) = idata(9)
      Ncurve(I) = idata(10)
      Nliter(I) = idata(11)
      Ndel(I) = idata(12)
      Nout1(I) = idata(13)
      Nout2(I) = idata(14)
      Nout3(I) = idata(15)
      Nblade(I) = idata(16)
      l1 = 3
      IF ( Ndata(I)/=0 ) l1 = l1 + 5 + Ndata(I)
      IF ( Ndel(I)/=0 ) l1 = l1 + 3 + Ndel(I)
      CALL alg03(Lnct,l1)
      IF ( Iprtc==1 ) WRITE (Log2,99011) I , Ndata(I) , Nterp(I) , Ndimen(I) , Nmach(I) , Nwork(I) , Nloss(I) , Nl1(I) , Nl2(I) ,   &
                           & Neval(I) , Ncurve(I) , Nliter(I) , Ndel(I) , Nout1(I) , Nout2(I) , Nout3(I) , Nblade(I)
99011 FORMAT (//7X,7HSTATION,I3,8H  NDATA=,I3,7H NTERP=,I2,8H NDIMEN=,I2,7H NMACH=,I2,7H NWORK=,I2,7H NLOSS=,I2,5H NL1=,I3,5H NL2=, &
            & I3,7H NEVAL=,I2,8H NCURVE=,I2,8H NLITER=,I3,6H NDEL=,I3,/19X,6HNOUT1=,I2,7H NOUT2=,I2,7H NOUT3=,I2,8H NBLADE=,I3)
      Speed(I) = 0.0
      IF ( Ndata(I)/=0 ) THEN
         next = last + 1
         last = last + Ndata(I)
         Is2(I) = next
         IF ( last>100 ) GOTO 200
         CALL fread(logn,Speed(I),1,1)
!WKBD IF (DEBUG .AND.LOGN.EQ.LOG1) CALL BUG1 ('ALG02   ',271,SPEED(I),1)
         DO k = next , last
            CALL fread(logn,rdata,6,1)
!WKBD IF (DEBUG .AND. LOGN.EQ.LOG1) CALL BUG1 ('ALG02   ',272,RDATA,6)
            Datac(k) = rdata(1)
            Data1(k) = rdata(2)
            Data2(k) = rdata(3)
            Data3(k) = rdata(4)
            Data4(k) = rdata(5)
            Data5(k) = rdata(6)
            CALL fread(logn,rdata,4,1)
!WKBD IF (DEBUG .AND. LOGN.EQ.LOG1) CALL BUG1 ('ALG02   ',273,RDATA,4)
            Data6(k) = rdata(1)
            Data7(k) = rdata(2)
            Data8(k) = rdata(3)
            Data9(k) = rdata(4)
         ENDDO
         IF ( Iprtc==1 ) WRITE (Log2,99012) Speed(I) , (Datac(k),Data1(k),Data2(k),Data3(k),Data4(k),Data5(k),Data6(k),Data7(k),    &
                              & Data8(k),Data9(k),k=next,last)
99012    FORMAT (//10X,7HSPEED =,F9.2,//13X,5HDATAC,7X,5HDATA1,7X,5HDATA2,7X,5HDATA3,7X,5HDATA4,7X,5HDATA5,7X,5HDATA6,7X,5HDATA7,7X,&
                &5HDATA8,7X,5HDATA9,//,(10X,F9.4,F12.3,F13.6,F11.4,F12.5,F12.5,4F12.4))
         IF ( Nwork(I)==1 ) THEN
            DO k = next , last
               Data1(k) = Data1(k)*Sclfac**2
            ENDDO
         ENDIF
         IF ( Neval(I)>0 .AND. Nstrms>Ndata(I) ) last = last + Nstrms - Ndata(I)
         IF ( Ndel(I)/=0 ) THEN
            next = lastd + 1
            lastd = lastd + Ndel(I)
            Is3(I) = next
            IF ( lastd>100 ) GOTO 200
            DO k = next , lastd
               CALL fread(Log1,Delc(k),1,0)
               CALL fread(Log1,Delta(k),1,1)
            ENDDO
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',298,DELC(NEXT),LASTD-NEXT+1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',299,DELTA(NEXT),LASTD-NEXT+1)
            IF ( Iprtc==1 ) WRITE (Log2,99013) (Delc(k),Delta(k),k=next,lastd)
99013       FORMAT (//13X,4HDELC,8X,5HDELTA,//,(10X,F9.4,F12.4))
         ENDIF
      ENDIF
   ENDDO
   CALL alg03(Lnct,5+Nstns)
   DO I = 1 , Nstns
      CALL fread(Log1,rdata,3,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',323,RDATA,3)
      Wblock(I) = rdata(1)
      Bblock(I) = rdata(2)
      Bdist(I) = rdata(3)
   ENDDO
   IF ( Iprtc==1 ) WRITE (Log2,99014) (I,Wblock(I),Bblock(I),Bdist(I),I=1,Nstns)
99014 FORMAT (//10X,'BLOCKAGE FACTOR SPECIFICATIONS',//10X,'STATION  ',' WALL BLOCKAGE   WAKE BLOCKAGE   WAKE DISTRIBUTION FACTOR', &
            & //,(10X,I4,F16.5,F16.5,F19.3))
   IF ( Nset1/=0 ) THEN
      DO k = 1 , Nset1
         CALL fread(Log1,l1,1,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',342,L1,1)
         DO j = 1 , l1
            CALL fread(Log1,rdata,4,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',343,RDATA,4)
            Diff(j,k) = rdata(1)
            Fdhub(j,k) = rdata(2)
            Fdmid(j,k) = rdata(3)
            Fdtip(j,k) = rdata(4)
         ENDDO
         CALL alg03(Lnct,6+l1)
         IF ( Iprtc==1 ) WRITE (Log2,99015) k , l1 , (Diff(j,k),Fdhub(j,k),Fdmid(j,k),Fdtip(j,k),j=1,l1)
99015    FORMAT (//10X,'LOSS PARAMETER / DIFFUSION FACTOR CURVES FOR BLADE',' TYPE',I2,I5,' D-FACTORS GIVEN',//15X,9HDIFFUSION,5X,  &
                &'L O S S   P A R A M E T E R S',/16X,7HFACTORS,8X,3HHUB,9X,3HMID,8X,3HTIP,//,(15X,F8.3,F13.5,F12.5,F11.5))
         Ndiff(k) = l1
      ENDDO
   ENDIF
   IF ( Nset2/=0 ) THEN
      DO k = 1 , Nset2
         CALL fread(Log1,idata,2,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',385,IDATA,2)
         l1 = idata(1)
         l2 = idata(2)
         CALL alg03(Lnct,7+l1)
         Nm(k) = l1
         Nrad(k) = l2
         CALL fread(Log1,Terad(1,k),1,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',391,TERAD(1,K),1)
         DO j = 1 , l1
            CALL fread(Log1,rdata,2,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',398,RDATA,2)
            Dm(j,1,k) = rdata(1)
            Wfrac(j,1,k) = rdata(2)
         ENDDO
         IF ( Iprtc==1 ) WRITE (Log2,99016) k , l1 , l2 , Terad(1,k) , (Dm(j,1,k),Wfrac(j,1,k),j=1,l1)
99016    FORMAT (//10X,'FRACTIONAL LOSS DISTRIBUTION CURVES FOR BLADE ','CLASS',I2,I5,' POINTS GIVEN AT',I3,' RADIAL LOCATIONS',    &
               & //10X,'FRACTION OF COMPUTING STATION LENGTH AT BLADE EXIT =',F7.4,//10X,'FRACTION OF MERIDIONAL CHORD',4X,         &
                &'LOSS/LOSS AT TRAILING EDGE',//,(15X,F11.4,20X,F11.4))
         IF ( l2/=1 ) THEN
            DO l = 2 , l2
               CALL alg03(Lnct,5+l1)
               CALL fread(Log1,Terad(l,k),1,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',411,TERAD(L,K),1)
               DO j = 1 , l1
                  CALL fread(Log1,rdata,2,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',412,RDATA,2)
                  Dm(j,l,k) = rdata(1)
                  Wfrac(j,l,k) = rdata(2)
               ENDDO
               IF ( Iprtc==1 ) WRITE (Log2,99017) Terad(l,k) , (Dm(j,l,k),Wfrac(j,l,k),j=1,l1)
99017          FORMAT (//10X,'FRACTION OF COMPUTING STATION LENGTH AT BLADE ','EXIT =',F7.4,//10X,'FRACTION OF MERIDIONAL CHORD',4X,&
                      &'LOSS/LOSSAT TRAILING EDGE',//,(15X,F11.4,20X,F11.4))
            ENDDO
         ENDIF
      ENDDO
   ENDIF
   IF ( Nsplit/=0 .OR. Nread/=0 ) THEN
      DO j = 1 , Nstrms , 6
         CALL fread(Log1,Delf(j),6,1)
      ENDDO
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',455,DELF,NSTRMS)
      l1 = 5
      IF ( Nstrms>=16 ) l1 = 8
      CALL alg03(Lnct,l1)
      IF ( Iprtc==1 ) WRITE (Log2,99018)
99018 FORMAT (//10X,'PROPORTIONS OF TOTAL FLOW BETWEEN HUB AND EACH ','STREAMLINE ARE TO BE AS FOLLOWS')
      l1 = Nstrms
      IF ( Nstrms>15 ) l1 = 15
      IF ( Iprtc==1 ) WRITE (Log2,99022) (j,j=1,l1)
      IF ( Iprtc==1 ) WRITE (Log2,99023) (Delf(j),j=1,l1)
      IF ( Nstrms>15 ) THEN
         l1 = l1 + 1
         IF ( Iprtc==1 ) WRITE (Log2,99022) (j,j=l1,Nstrms)
         IF ( Iprtc==1 ) WRITE (Log2,99023) (Delf(j),j=l1,Nstrms)
      ENDIF
      IF ( Nread/=0 ) THEN
         DO I = 1 , Nstns
            DO j = 1 , Nstrms
               CALL fread(Log1,rdata,3,0)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',501,RDATA,3)
               R(j,I) = rdata(1)
               X(j,I) = rdata(2)
               Xl(j,I) = rdata(3)
               CALL fread(Log1,idata,2,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',502,IDATA,2)
               Ii(j,I) = idata(1)
               Jj(j,I) = idata(2)
            ENDDO
         ENDDO
         CALL alg03(Lnct,5+Nstrms)
         IF ( Iprtc==1 ) WRITE (Log2,99019)
99019    FORMAT (//10X,'ESTIMATED STREAMLINE COORDINATES')
         DO I = 1 , Nstns
            IF ( I>1 ) CALL alg03(Lnct,3+Nstrms)
            IF ( Iprtc==1 ) WRITE (Log2,99020) (I,j,R(j,I),X(j,I),Xl(j,I),Ii(j,I),Jj(j,I),j=1,Nstrms)
99020       FORMAT (//10X,'STATION  STREAMLINE   RADIUS  AXIAL COORDINATE  ','L -COORDINATE    CHECKS-  I    J',//,                 &
                  & (3X,2I11,F14.4,F12.4,F16.4,I17,I5))
         ENDDO
      ENDIF
   ENDIF
   GOTO 99999
 200  WRITE (Log2,99021)
99021 FORMAT (////10X,'JOB STOPPED - TOO MUCH INPUT DATA')
   CALL mesage(-37,0,name)
99022 FORMAT (//10X,'STREAMLINE',I5,14I7)
99023 FORMAT (10X,4HFLOW,7X,15F7.4)
99999 END SUBROUTINE alg02
