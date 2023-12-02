!*==alg02.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg02
   USE c_ud300c
   USE c_ud3prt
   USE c_udsign
   USE c_upage
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: debug
   INTEGER , DIMENSION(24) :: idata
   INTEGER , DIMENSION(21,30) :: ii , jj
   INTEGER :: j , k , l , l1 , l2 , last , lastd , logn , next , nle , nte
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(6) :: rdata
   EXTERNAL alg03 , alg1 , fread , mesage , sswtch
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
   !>>>>EQUIVALENCE (H(1,1),Ii(1,1)) , (S(1,1),Jj(1,1))
   DATA name/4HALG0 , 4H2   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         debug = .FALSE.
         CALL sswtch(20,j)
         IF ( j==1 ) debug = .TRUE.
         neval(1) = 0
         CALL fread(log1,title,18,1)
         IF ( iprtc==1 ) WRITE (log2,99001) title
99001    FORMAT (10X,10HINPUT DATA,/10X,10(1H*),//10X,5HTITLE,34X,2H= ,18A4)
         lnct = lnct + 4
         CALL alg1(lnct)
         CALL fread(log1,idata,21,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',111,IDATA,21)
         nstns = idata(1)
         nstrms = idata(2)
         nmax = idata(3)
         nforce = idata(4)
         nbl = idata(5)
         ncase = idata(6)
         nsplit = idata(7)
         nset1 = idata(8)
         nset2 = idata(9)
         nread = idata(10)
         npunch = idata(11)
         nplot = idata(12)
         npage = idata(13)
         ntrans = idata(14)
         nmix = idata(15)
         nmany = idata(16)
         nstplt = idata(17)
         neqn = idata(18)
         nle = idata(19)
         nte = idata(20)
         nsign = idata(21)
         IF ( nstrms==0 ) nstrms = 11
         IF ( nmax==0 ) nmax = 40
         IF ( nforce==0 ) nforce = 10
         IF ( ncase==0 ) ncase = 1
         IF ( npage==0 ) npage = 60
         lq = log2
         limit = npage
         CALL alg03(lnct,19)
         IF ( iprtc==1 ) WRITE (log2,99002) nstns , nstrms , nmax , nforce , nbl , ncase , nsplit , nset1 , nset2 , nread , npunch ,&
                              & nplot , npage , ntrans , nmix , nmany , nstplt , neqn , nle , nte , nsign
99002    FORMAT (//10X,'NUMBER OF STATIONS',21X,1H=,I3,/10X,'NUMBER OF ','STREAMLINES',18X,1H=,I3,/10X,20HMAX NUMBER OF PASSES,19X, &
                &1H=,I3,/10X,30HMAX NUMBER OF ARBITRARY PASSES,9X,1H=,I3,/10X,29HBOUNDARY LAYER CALC INDICATOR,10X,1H=,I3,/10X,     &
                &24HNUMBER OF RUNNING POINTS,15X,1H=,I3,/10X,33HSTREAMLINE DISTRIBUTION INDICATOR,6X,1H=,I3,/10X,                   &
                &34HNUMBER OF LOSS/D-FACTOR CURVE SETS,5X,1H=,I3,/10X,34HNUMBER OF LOSS/T.E.LOSS CURVE SETS,5X,1H=,I3,/10X,         &
                &26HSTREAMLINE INPUT INDICATOR,13X,1H=,I3,/10X,27HSTREAMLINE OUTPUT INDICATOR,12X,1H=,I3,/10X,                      &
                &24HPRECISION PLOT INDICATOR,15X,1H=,I3,/10X,24HMAX NUMBER OF LINES/PAGE,15X,1H=,I3,/10X,                           &
                &29HWAKE TRANSPORT CALC INDICATOR,10X,1H=,I3,/10X,32HMAINSTREAM MIXING CALC INDICATOR,7X,1H=,I3,/10X,               &
                &33HNO OF STATIONS FROM ANALYTIC SECN,6X,1H=,I3,/10X,27HLINE-PRINTER PLOT INDICATOR,12X,1H=,I3,/10X,                &
                &32HMOMENTUM EQUATION FORM INDICATOR,7X,1H=,I3,/10X,30HSTATION NUMBER AT LEADING EDGE,9X,1H=,I3,/10X,               &
                &31HSTATION NUMBER AT TRAILING EDGE,8X,1H=,I3,/10X,37HCOMPRESSOR DIR. OF ROTATION INDICATOR,2X,1H=,I3)
         itub = nstrms - 1
         imid = nstrms/2 + 1
         IF ( nmany/=0 ) THEN
            CALL fread(log1,nwhich,nmany,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',132,NWHICH,NMANY)
            CALL alg03(lnct,2)
            IF ( iprtc==1 ) WRITE (log2,99003) (nwhich(i),i=1,nmany)
99003       FORMAT (//10X,'GEOMETRY COMES FROM ANALYTIC SECTION FOR STATIONS',23I3)
         ENDIF
         CALL alg03(lnct,7)
         CALL fread(log1,rdata,6,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',136,RDATA,6)
         g = rdata(1)
         ej = rdata(2)
         sclfac = rdata(3)
         tolnce = rdata(4)
         visk = rdata(5)
         shape = rdata(6)
         IF ( g==0.0 ) g = 32.174
         IF ( ej==0.0 ) ej = 778.16
         IF ( sclfac==0. ) sclfac = 12.0
         IF ( tolnce==0. ) tolnce = 0.001
         IF ( visk==0.0 ) visk = 0.00018
         IF ( shape==0.0 ) shape = 0.7
         IF ( iprtc==1 ) WRITE (log2,99004) g , ej , sclfac , tolnce , visk , shape
99004    FORMAT (//10X,22HGRAVITATIONAL CONSTANT,17X,1H=,F8.4,/10X,17HJOULES EQUIVALENT,22X,1H=,F8.3,/10X,                          &
                &29HLINEAR DIMENSION SCALE FACTOR,10X,1H=,F8.4,/10X,15HBASIC TOLERANCE,24X,1H=,F8.5,/10X,19HKINEMATIC VISCOSITY,20X,&
                &1H=,F8.5,/10X,17HB.L. SHAPE FACTOR,22X,1H=,F8.5)
         CALL alg03(lnct,7)
         CALL fread(log1,rdata,6,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',151,RDATA,6)
         xscale = rdata(1)
         pscale = rdata(2)
         rlow = rdata(3)
         plow = rdata(4)
         xmmax = rdata(5)
         rconst = rdata(6)
         IF ( xmmax==0.0 ) xmmax = 0.6
         IF ( rconst==0.0 ) rconst = 6.0
         IF ( iprtc==1 ) WRITE (log2,99005) xscale , pscale , rlow , plow , xmmax , rconst
99005    FORMAT (//10X,29HPLOTTING SCALE FOR DIMENSIONS,10X,1H=,F7.3,/10X,28HPLOTTING SCALE FOR PRESSURES,11X,1H=,F7.3,/10X,        &
                &22HMINIMUM RADIUS ON PLOT,17X,1H=,F7.3,/10X,24HMINIMUM PRESSURE ON PLOT,15X,1H=,F7.3,/10X,                         &
                &40HMAXIMUM M-SQUARED IN RELAXATION FACTOR =,F8.4,/10X,29HCONSTANT IN RELAXATION FACTOR,10X,1H=,F8.4)
         CALL alg03(lnct,3)
         CALL fread(log1,rdata,2,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',162,RDATA,2)
         contr = rdata(1)
         conmx = rdata(2)
         IF ( iprtc==1 ) WRITE (log2,99006) contr , conmx
99006    FORMAT (//10X,22HWAKE TRANSFER CONSTANT,17X,1H=,F8.5,/10X,25HTURBULENT MIXING CONSTANT,14X,1H=,F8.5)
         CALL alg03(lnct,5+ncase)
         DO k = 1 , ncase
            CALL fread(log1,flow(k),1,0)
            CALL fread(log1,spdfac(k),1,1)
         ENDDO
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',171,FLOW,NCASE)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',172,SPDFAC,NCASE)
         IF ( iprtc==1 ) WRITE (log2,99007) (k,flow(k),spdfac(k),k=1,ncase)
99007    FORMAT (//10X,21HPOINTS TO BE COMPUTED,//10X,2HNO,6X,8HFLOWRATE,4X,12HSPEED FACTOR,//,(10X,I2,F13.3,F14.3))
         CALL fread(log1,l1,1,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',180,L1,1)
         DO k = 1 , l1
            CALL fread(log1,xstn(k),1,0)
            CALL fread(log1,rstn(k),1,1)
         ENDDO
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',191,XSTN,L1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',192,RSTN,L1)
         istag = 0
         IF ( rstn(1)==0.0 ) istag = 1
         nspec(1) = l1
         CALL alg03(lnct,7+l1)
         IF ( iprtc==1 ) WRITE (log2,99008) l1 , (xstn(k),rstn(k),k=1,l1)
99008    FORMAT (//10X,'ANNULUS / COMPUTING STATION GEOMETRY',//10X,24HSTATION  1  SPECIFIED BY,I3,7H POINTS,//17X,4HXSTN,8X,4HRSTN,&
               & //,(F22.4,F12.4))
         is1(1) = 1
         last = l1
         DO i = 2 , nstns
            CALL fread(log1,l1,1,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',210,L1,1)
            next = last + 1
            last = last + l1
            IF ( last>150 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO k = next , last
               CALL fread(log1,xstn(k),1,0)
               CALL fread(log1,rstn(k),1,1)
            ENDDO
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',215,XSTN(NEXT),LAST-NEXT+1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',216,RSTN(NEXT),LAST-NEXT+1)
            IF ( rstn(next)==0.0 ) istag = i
            CALL alg03(lnct,5+l1)
            is1(i) = next
            nspec(i) = l1
            IF ( iprtc==1 ) WRITE (log2,99009) i , l1 , (xstn(k),rstn(k),k=next,last)
99009       FORMAT (//10X,7HSTATION,I3,14H  SPECIFIED BY,I3,7H POINTS,//17X,4HXSTN,8X,4HRSTN,//,(F22.4,F12.4))
         ENDDO
         speed(1) = 0.0
         CALL fread(log1,idata,4,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',233,IDATA,4)
         l1 = idata(1)
         nterp(1) = idata(2)
         ndimen(1) = idata(3)
         nmach(1) = idata(4)
         DO k = 1 , l1
            CALL fread(log1,rdata,4,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',234,RDATA,4)
            datac(k) = rdata(1)
            data1(k) = rdata(2)
            data2(k) = rdata(3)
            data3(k) = rdata(4)
         ENDDO
         CALL alg03(lnct,7+l1)
         is2(1) = 1
         ndata(1) = l1
         last = l1
         IF ( iprtc==1 ) WRITE (log2,99010) l1 , nterp(1) , ndimen(1) , nmach(1) , (datac(k),data1(k),data2(k),data3(k),k=1,l1)
99010    FORMAT (//10X,24HSTATION CALCULATION DATA,//7X,18HSTATION  1  NDATA=,I3,7H NTERP=,I2,8H NDIMEN=,I2,7H NMACH=,I2,//11X,     &
                &5HDATAC,6X,14HTOTAL PRESSURE,4X,17HTOTAL TEMPERATURE,4X,11HWHIRL ANGLE,//,(5X,F12.4,F15.4,F19.3,F18.3))
         DO k = 1 , l1
            data1(k) = data1(k)*sclfac**2
         ENDDO
         lastd = 0
         nout1(1) = 0
         nout2(1) = 0
         DO i = 2 , nstns
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  logn = log1
                  IF ( nmany/=0 ) THEN
                     DO l1 = 1 , nmany
                        IF ( nwhich(l1)==i ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                  ENDIF
                  spag_nextblock_2 = 3
               CASE (2)
                  logn = log5
                  spag_nextblock_2 = 3
               CASE (3)
                  CALL fread(logn,idata,16,1)
!WKBD IF (DEBUG .AND. LOGN.EQ.LOG1) CALL BUG1 ('ALG02   ',258,IDATA,16)
                  ndata(i) = idata(1)
                  nterp(i) = idata(2)
                  ndimen(i) = idata(3)
                  nmach(i) = idata(4)
                  nwork(i) = idata(5)
                  nloss(i) = idata(6)
                  nl1(i) = idata(7)
                  nl2(i) = idata(8)
                  neval(i) = idata(9)
                  ncurve(i) = idata(10)
                  nliter(i) = idata(11)
                  ndel(i) = idata(12)
                  nout1(i) = idata(13)
                  nout2(i) = idata(14)
                  nout3(i) = idata(15)
                  nblade(i) = idata(16)
                  l1 = 3
                  IF ( ndata(i)/=0 ) l1 = l1 + 5 + ndata(i)
                  IF ( ndel(i)/=0 ) l1 = l1 + 3 + ndel(i)
                  CALL alg03(lnct,l1)
                  IF ( iprtc==1 ) WRITE (log2,99011) i , ndata(i) , nterp(i) , ndimen(i) , nmach(i) , nwork(i) , nloss(i) , nl1(i) ,&
                     & nl2(i) , neval(i) , ncurve(i) , nliter(i) , ndel(i) , nout1(i) , nout2(i) , nout3(i) , nblade(i)
99011             FORMAT (//7X,7HSTATION,I3,8H  NDATA=,I3,7H NTERP=,I2,8H NDIMEN=,I2,7H NMACH=,I2,7H NWORK=,I2,7H NLOSS=,I2,5H NL1=,&
                        & I3,5H NL2=,I3,7H NEVAL=,I2,8H NCURVE=,I2,8H NLITER=,I3,6H NDEL=,I3,/19X,6HNOUT1=,I2,7H NOUT2=,I2,         &
                        & 7H NOUT3=,I2,8H NBLADE=,I3)
                  speed(i) = 0.0
                  IF ( ndata(i)/=0 ) THEN
                     next = last + 1
                     last = last + ndata(i)
                     is2(i) = next
                     IF ( last>100 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     CALL fread(logn,speed(i),1,1)
!WKBD IF (DEBUG .AND.LOGN.EQ.LOG1) CALL BUG1 ('ALG02   ',271,SPEED(I),1)
                     DO k = next , last
                        CALL fread(logn,rdata,6,1)
!WKBD IF (DEBUG .AND. LOGN.EQ.LOG1) CALL BUG1 ('ALG02   ',272,RDATA,6)
                        datac(k) = rdata(1)
                        data1(k) = rdata(2)
                        data2(k) = rdata(3)
                        data3(k) = rdata(4)
                        data4(k) = rdata(5)
                        data5(k) = rdata(6)
                        CALL fread(logn,rdata,4,1)
!WKBD IF (DEBUG .AND. LOGN.EQ.LOG1) CALL BUG1 ('ALG02   ',273,RDATA,4)
                        data6(k) = rdata(1)
                        data7(k) = rdata(2)
                        data8(k) = rdata(3)
                        data9(k) = rdata(4)
                     ENDDO
                     IF ( iprtc==1 ) WRITE (log2,99012) speed(i) , (datac(k),data1(k),data2(k),data3(k),data4(k),data5(k),data6(k), &
                        & data7(k),data8(k),data9(k),k=next,last)
99012                FORMAT (//10X,7HSPEED =,F9.2,//13X,5HDATAC,7X,5HDATA1,7X,5HDATA2,7X,5HDATA3,7X,5HDATA4,7X,5HDATA5,7X,5HDATA6,  &
                           & 7X,5HDATA7,7X,5HDATA8,7X,5HDATA9,//,(10X,F9.4,F12.3,F13.6,F11.4,F12.5,F12.5,4F12.4))
                     IF ( nwork(i)==1 ) THEN
                        DO k = next , last
                           data1(k) = data1(k)*sclfac**2
                        ENDDO
                     ENDIF
                     IF ( neval(i)>0 .AND. nstrms>ndata(i) ) last = last + nstrms - ndata(i)
                     IF ( ndel(i)/=0 ) THEN
                        next = lastd + 1
                        lastd = lastd + ndel(i)
                        is3(i) = next
                        IF ( lastd>100 ) THEN
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        DO k = next , lastd
                           CALL fread(log1,delc(k),1,0)
                           CALL fread(log1,delta(k),1,1)
                        ENDDO
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',298,DELC(NEXT),LASTD-NEXT+1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',299,DELTA(NEXT),LASTD-NEXT+1)
                        IF ( iprtc==1 ) WRITE (log2,99013) (delc(k),delta(k),k=next,lastd)
99013                   FORMAT (//13X,4HDELC,8X,5HDELTA,//,(10X,F9.4,F12.4))
                     ENDIF
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL alg03(lnct,5+nstns)
         DO i = 1 , nstns
            CALL fread(log1,rdata,3,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',323,RDATA,3)
            wblock(i) = rdata(1)
            bblock(i) = rdata(2)
            bdist(i) = rdata(3)
         ENDDO
         IF ( iprtc==1 ) WRITE (log2,99014) (i,wblock(i),bblock(i),bdist(i),i=1,nstns)
99014    FORMAT (//10X,'BLOCKAGE FACTOR SPECIFICATIONS',//10X,'STATION  ',                                                          &
                &' WALL BLOCKAGE   WAKE BLOCKAGE   WAKE DISTRIBUTION FACTOR',//,(10X,I4,F16.5,F16.5,F19.3))
         IF ( nset1/=0 ) THEN
            DO k = 1 , nset1
               CALL fread(log1,l1,1,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',342,L1,1)
               DO j = 1 , l1
                  CALL fread(log1,rdata,4,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',343,RDATA,4)
                  diff(j,k) = rdata(1)
                  fdhub(j,k) = rdata(2)
                  fdmid(j,k) = rdata(3)
                  fdtip(j,k) = rdata(4)
               ENDDO
               CALL alg03(lnct,6+l1)
               IF ( iprtc==1 ) WRITE (log2,99015) k , l1 , (diff(j,k),fdhub(j,k),fdmid(j,k),fdtip(j,k),j=1,l1)
99015          FORMAT (//10X,'LOSS PARAMETER / DIFFUSION FACTOR CURVES FOR BLADE',' TYPE',I2,I5,' D-FACTORS GIVEN',//15X,           &
                     & 9HDIFFUSION,5X,'L O S S   P A R A M E T E R S',/16X,7HFACTORS,8X,3HHUB,9X,3HMID,8X,3HTIP,//,                 &
                     & (15X,F8.3,F13.5,F12.5,F11.5))
               ndiff(k) = l1
            ENDDO
         ENDIF
         IF ( nset2/=0 ) THEN
            DO k = 1 , nset2
               CALL fread(log1,idata,2,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',385,IDATA,2)
               l1 = idata(1)
               l2 = idata(2)
               CALL alg03(lnct,7+l1)
               nm(k) = l1
               nrad(k) = l2
               CALL fread(log1,terad(1,k),1,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',391,TERAD(1,K),1)
               DO j = 1 , l1
                  CALL fread(log1,rdata,2,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',398,RDATA,2)
                  dm(j,1,k) = rdata(1)
                  wfrac(j,1,k) = rdata(2)
               ENDDO
               IF ( iprtc==1 ) WRITE (log2,99016) k , l1 , l2 , terad(1,k) , (dm(j,1,k),wfrac(j,1,k),j=1,l1)
99016          FORMAT (//10X,'FRACTIONAL LOSS DISTRIBUTION CURVES FOR BLADE ','CLASS',I2,I5,' POINTS GIVEN AT',I3,                  &
                      &' RADIAL LOCATIONS',//10X,'FRACTION OF COMPUTING STATION LENGTH AT BLADE EXIT =',F7.4,//10X,                 &
                      &'FRACTION OF MERIDIONAL CHORD',4X,'LOSS/LOSS AT TRAILING EDGE',//,(15X,F11.4,20X,F11.4))
               IF ( l2/=1 ) THEN
                  DO l = 2 , l2
                     CALL alg03(lnct,5+l1)
                     CALL fread(log1,terad(l,k),1,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',411,TERAD(L,K),1)
                     DO j = 1 , l1
                        CALL fread(log1,rdata,2,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',412,RDATA,2)
                        dm(j,l,k) = rdata(1)
                        wfrac(j,l,k) = rdata(2)
                     ENDDO
                     IF ( iprtc==1 ) WRITE (log2,99017) terad(l,k) , (dm(j,l,k),wfrac(j,l,k),j=1,l1)
99017                FORMAT (//10X,'FRACTION OF COMPUTING STATION LENGTH AT BLADE ','EXIT =',F7.4,//10X,                            &
                            &'FRACTION OF MERIDIONAL CHORD',4X,'LOSS/LOSSAT TRAILING EDGE',//,(15X,F11.4,20X,F11.4))
                  ENDDO
               ENDIF
            ENDDO
         ENDIF
         IF ( nsplit/=0 .OR. nread/=0 ) THEN
            DO j = 1 , nstrms , 6
               CALL fread(log1,delf(j),6,1)
            ENDDO
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',455,DELF,NSTRMS)
            l1 = 5
            IF ( nstrms>=16 ) l1 = 8
            CALL alg03(lnct,l1)
            IF ( iprtc==1 ) WRITE (log2,99018)
99018       FORMAT (//10X,'PROPORTIONS OF TOTAL FLOW BETWEEN HUB AND EACH ','STREAMLINE ARE TO BE AS FOLLOWS')
            l1 = nstrms
            IF ( nstrms>15 ) l1 = 15
            IF ( iprtc==1 ) WRITE (log2,99022) (j,j=1,l1)
            IF ( iprtc==1 ) WRITE (log2,99023) (delf(j),j=1,l1)
            IF ( nstrms>15 ) THEN
               l1 = l1 + 1
               IF ( iprtc==1 ) WRITE (log2,99022) (j,j=l1,nstrms)
               IF ( iprtc==1 ) WRITE (log2,99023) (delf(j),j=l1,nstrms)
            ENDIF
            IF ( nread/=0 ) THEN
               DO i = 1 , nstns
                  DO j = 1 , nstrms
                     CALL fread(log1,rdata,3,0)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',501,RDATA,3)
                     r(j,i) = rdata(1)
                     x(j,i) = rdata(2)
                     xl(j,i) = rdata(3)
                     CALL fread(log1,idata,2,1)
!WKBD IF (DEBUG) CALL BUG1 ('ALG02   ',502,IDATA,2)
                     ii(j,i) = idata(1)
                     jj(j,i) = idata(2)
                  ENDDO
               ENDDO
               CALL alg03(lnct,5+nstrms)
               IF ( iprtc==1 ) WRITE (log2,99019)
99019          FORMAT (//10X,'ESTIMATED STREAMLINE COORDINATES')
               DO i = 1 , nstns
                  IF ( i>1 ) CALL alg03(lnct,3+nstrms)
                  IF ( iprtc==1 ) WRITE (log2,99020) (i,j,r(j,i),x(j,i),xl(j,i),ii(j,i),jj(j,i),j=1,nstrms)
99020             FORMAT (//10X,'STATION  STREAMLINE   RADIUS  AXIAL COORDINATE  ','L -COORDINATE    CHECKS-  I    J',//,           &
                        & (3X,2I11,F14.4,F12.4,F16.4,I17,I5))
               ENDDO
            ENDIF
         ENDIF
         RETURN
      CASE (2)
         WRITE (log2,99021)
99021    FORMAT (////10X,'JOB STOPPED - TOO MUCH INPUT DATA')
         CALL mesage(-37,0,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99022 FORMAT (//10X,'STREAMLINE',I5,14I7)
99023 FORMAT (10X,4HFLOW,7X,15F7.4)
END SUBROUTINE alg02
