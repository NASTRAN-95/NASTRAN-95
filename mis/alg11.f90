
SUBROUTINE alg11
   IMPLICIT NONE
   REAL Bblock(30) , Bdist(30) , C1 , Chordd(21) , Conmx , Contr , Cppg(21) , Cr(21) , Data1(100) , Data2(100) , Data3(100) ,       &
      & Data4(100) , Data5(100) , Data6(100) , Data7(100) , Data8(100) , Data9(100) , Datac(100) , Delc(100) , Delf(30) , Delh(30) ,&
      & Delt(30) , Delta(100) , Delw(21) , Diff(15,4) , Dm(11,5,2) , Drdm2(30) , Ej , Fdhub(15,4) , Fdmid(15,4) , Fdtip(15,4) ,     &
      & Flow(10) , Fm2 , G , Gama(21) , H(21,30) , Hkeep(21) , Hmin , Lami(21) , Lamim1(21) , Lamip1(21) , Loss(21) , Phi(21) , Pi ,&
      & Plow , Pscale , R(21,30) , Rconst , Rim1(30) , Rlow , Rstn(150) , S(21,30) , Sclfac , Shape , Skeep(21) , Spdfac(10) ,      &
      & Speed(30) , Sppg(21) , Stag(21) , Taneps(21) , Tbeta(21,30) , Terad(5,2) , Title(18) , Tolnce , Visk , Vm(21,30) , Vv(21) , &
      & Vw(21,30) , Vwkeep(21) , Wblock(30) , Wfrac(11,5,2) , Work(21) , Wwbl(30) , X(21,30) , Xi(21) , Xim1(30) , Xl(21,30) ,      &
      & Xmmax , Xscale , Xstn(150)
   INTEGER I , Icase , Ifail , Ifailo , Iffail , Iloss , Imid , Ipass , Ipgeom , Iprint , Iprtc , Is1(30) , Is2(30) , Is3(30) ,     &
         & Iscr , Istag , Istrml , Iter , Itub , Ivfail , Ksystm(90) , Lnct , Log1 , Log2 , Log3 , Log4 , Log5 , Log6 , Lpunch ,    &
         & Nbl , Nblade(30) , Nbldes , Ncase , Ncurve(30) , Ndata(30) , Ndel(30) , Ndiff(4) , Ndimen(30) , Neqn , Neval(30) ,       &
         & Nforce , Nl1(30) , Nl2(30) , Nliter(30) , Nloss(30) , Nm(2) , Nmach(30) , Nmany , Nmax , Nmix , Nout1(30) , Nout2(30) ,  &
         & Nout3(30) , Npage , Nplot , Npunch , Nrad(2) , Nread , Nset1 , Nset2 , Nsign , Nspec(30) , Nsplit , Nstns , Nstplt ,     &
         & Nstrms , Nterp(30) , Ntrans , Nwhich(30) , Nwork(30)
   COMMON /algino/ Iscr
   COMMON /system/ Ksystm , Lpunch
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
   COMMON /ud3prt/ Iprtc , Istrml , Ipgeom
   COMMON /udsign/ Nsign
   COMMON /udstr2/ Nbldes , Stag , Chordd
   REAL alg2 , alg3 , alg4 , alg5 , alg7 , alg9
   REAL alpha , bbeta , beta , bspace , coef , delp , deltb(21) , deltp(21,30) , dh1 , dhn , dif , dpq , eff1 , effn , fi , ga ,    &
      & h1bar , hbar , hnbar , hri , hs , oeff , opr , p1(21) , p1bar , pbar , pfac , pload , pn(21) , pnbar , power , ppg(21) ,    &
      & pr , prd , prl1 , prl2 , ps(21) , psins , psl1 , psl2 , pt(21) , ptins , q , radius , rbar1 , rbarn , rdata(6) , rmdv(21,6) &
      & , s1bar , snbar , solid(21) , tr(21,30) , ts(21) , tt , u , v(21) , va , vr , wt(21) , x1 , x2 , x3 , x4 , xblade , xm(21) ,&
      & xmr , xn , xsign
   INTEGER ible , idata(6) , ielem , ifle , ifte , ii , ilast , ileb , inode , ip , itrleb , j , k , l1 , l1keep , l2 , l3 , l4 ,   &
         & l5 , l6 , ledgeb , name1(2) , name2(2) , nout3s , nout3t , nstnsx
!
   EQUIVALENCE (idata(1),rdata(1))
   DATA name1 , name2/4HPLOA , 4HD2   , 4HTEMP , 4H    /
!
   opr = 0.0
   oeff = 1.0
   pfac = 550.0
   ilast = Nstns
!
!     LOCATE COMPUTING STATION NUMBER AT THE BLADE LEADING EDGE AND
!     AT THE BLADE TRAILING EDGE.
!
   ledgeb = 0
   itrleb = 0
   DO ible = 1 , Nstns
      nout3s = Nout3(ible)/10
      IF ( Nout3(ible)==1 .OR. nout3s==1 ) ledgeb = ible
      IF ( Nout3(ible)==2 .OR. nout3s==2 ) itrleb = ible
   ENDDO
   IF ( Ifailo/=0 ) ilast = Ifailo
   DO I = 1 , ilast
      CALL alg03(Lnct,7+Nstrms)
      IF ( Iprtc==1 ) WRITE (Log2,99001) I
99001 FORMAT (//10X,'STATION',I3,'  FLOW-FIELD DESCRIPTION',/10X,34(1H*),//,'  STREAM      -----MESH-POINT COORDS------',3X,16(1H-),&
             &'V E L O C I T I E S,16(1H-)    RADIUS OF  ','STREAMLINE   STATION',/,'  -LINE       RADIUS    X-COORD',              &
             &'    L-COORD   MERIDIONAL TANGENTIAL   AXIAL',6X,'RADIAL',4X,'TOTAL    CURVATURE SLOPE ANGLE LEAN ANGLE',/)
      CALL alg01(R(1,I),X(1,I),Nstrms,R(1,I),x1,Gama,Nstrms,0,1)
      IF ( I/=1 .AND. I/=Nstns ) THEN
         DO j = 1 , Nstrms
            x1 = sqrt((R(j,I+1)-R(j,I))**2+(X(j,I+1)-X(j,I))**2)
            x2 = sqrt((R(j,I)-R(j,I-1))**2+(X(j,I)-X(j,I-1))**2)
            x3 = atan2(R(j,I+1)-R(j,I),X(j,I+1)-X(j,I))
            x4 = atan2(R(j,I)-R(j,I-1),X(j,I)-X(j,I-1))
            Cr(j) = (x3-x4)/(x1+x2)*2.0
            IF ( Cr(j)/=0.0 ) Cr(j) = 1.0/Cr(j)
            Phi(j) = (x3+x4)/2.0
         ENDDO
      ELSE
         l1 = 1
         l2 = 2
         IF ( I/=1 ) THEN
            l2 = Nstns
            l1 = l2 - 1
         ENDIF
         DO j = 1 , Nstrms
            Cr(j) = 0.0
            Phi(j) = atan2(R(j,l2)-R(j,l1),X(j,l2)-X(j,l1))
         ENDDO
      ENDIF
      DO j = 1 , Nstrms
         va = Vm(j,I)*cos(Phi(j))
         vr = Vm(j,I)*sin(Phi(j))
         fi = Phi(j)*C1
         ga = atan(Gama(j))*C1
         ppg(j) = fi + ga
         v(j) = sqrt(Vm(j,I)**2+Vw(j,I)**2)
!
!     STORE RADIUS AT BLADE LEADING AND TRAILING EDGES, ALL STREAMLINES
!
         IF ( Icase==1 .AND. I==ledgeb ) rmdv(j,5) = R(j,I)
         IF ( Icase==1 .AND. I==itrleb ) rmdv(j,6) = R(j,I)
         IF ( Iprtc==1 ) WRITE (Log2,99002) j , R(j,I) , X(j,I) , Xl(j,I) , Vm(j,I) , Vw(j,I) , va , vr , v(j) , Cr(j) , fi , ga
99002    FORMAT (I6,F14.4,2F11.4,5F11.2,1X,F10.2,2F11.3)
      ENDDO
      CALL alg03(Lnct,Nstrms+4)
      IF ( Iprtc==1 ) WRITE (Log2,99003)
99003 FORMAT (/8H  STREAM,7X,4HMACH,6X,4(1H-),9HPRESSURES,4(1H-),5X,17H---TEMPERATURES--,4X,8HSPECIFIC,4X,17H---ENTHALPIES----,4X,  &
             &7HENTROPY,6X,4HFLOW,3X,11H(PHI+GAMMA),/,7H  -LINE,7X,6HNUMBER,5X,5HTOTAL,6X,6HSTATIC,5X,5HTOTAL,6X,6HSTATIC,5X,       &
             &6HWEIGHT,5X,5HTOTAL,6X,6HSTATIC,16X,5HANGLE,/)
      DO j = 1 , Nstrms
         deltb(j) = 0.0
         hs = H(j,I) - v(j)**2/(2.0*G*Ej)
         IF ( hs<Hmin ) hs = Hmin
         xm(j) = sqrt(alg9(hs,S(j,I),v(j)**2))
         pt(j) = alg4(H(j,I),S(j,I))
         ptins = pt(j)/Sclfac**2
         ps(j) = alg4(hs,S(j,I))
         psins = ps(j)/Sclfac**2
         tt = alg7(H(j,I),S(j,I))
         ts(j) = alg7(hs,S(j,I))
         wt(j) = alg5(hs,S(j,I))
         alpha = 0.0
         IF ( I/=Istag .OR. j/=1 ) alpha = C1*atan(Vw(j,I)/Vm(j,I))
!
!     STORE DENSITY AT BLADE LEADING EDGE FOR ALL STREAMLINES
!
         IF ( Icase==1 .AND. I==ledgeb ) rmdv(j,2) = wt(j)
         IF ( Iprtc==1 ) WRITE (Log2,99004) j , xm(j) , ptins , psins , tt , ts(j) , wt(j) , H(j,I) , hs , S(j,I) , alpha , ppg(j)
99004    FORMAT (I6,F14.4,2F11.4,2F11.3,F12.6,F10.3,F11.3,F12.6,F10.3,F11.3)
      ENDDO
      IF ( I/=1 ) THEN
         ifle = 0
         ifte = 0
         IF ( Nwork(I)/=0 ) THEN
            ifte = 1
            IF ( I/=Nstns .AND. Nwork(I+1)/=0 .AND. Speed(I)/=Speed(I+1) ) ifle = 1
         ELSEIF ( I/=Nstns .AND. Nwork(I+1)/=0 ) THEN
            ifle = 1
         ENDIF
         IF ( ifte/=0 ) THEN
            CALL alg03(Lnct,Nstrms+8)
            xn = Speed(I)*Spdfac(Icase)
            xblade = 10.0
            IF ( Nblade(I)/=0 ) xblade = abs(float(Nblade(I)))
            l1 = xblade
            IF ( Iprtc==1 ) WRITE (Log2,99005) I , xn , l1
99005       FORMAT (/10X,'STATION',I3,' IS WITHIN OR AT THE TRAILING EDGE OF',' A BLADE ROTATING AT',F8.1,                          &
                   &' RPM  NUMBER OF BLADES IN ','ROW =',I3,/10X,109(1H*),//,'  STREAM      BLADE     ',                            &
                   &'RELATIVE    RELATIVE   RELATIVE  DEVIATION    BLADE    ','  LEAN    PRESS DIFF    LOSS     DIFFUSION   DELTA P'&
                  & ,/,'  -LINE',7X,'SPEED     VELOCITY    MACH NO.  FLOW',' ANGLE   ANGLE      ANGLE      ANGLE   ACROSS BLADE  ', &
                   &'COEFF      FACTOR     ON Q',/)
            q = 1.0
            IF ( Speed(I)>=0.0 ) THEN
               IF ( Speed(I)>0.0 ) THEN
                  q = -1.0
               ELSEIF ( I>=3 ) THEN
                  ii = I - 1
                  DO WHILE ( Speed(ii)==0.0 )
                     IF ( ii==2 ) GOTO 10
                     ii = ii - 1
                  ENDDO
                  IF ( Speed(ii)<0.0 ) q = -1.0
               ENDIF
            ENDIF
 10         l1 = Ndimen(I) + 1
            IF ( l1==2 ) THEN
               DO j = 1 , Nstrms
                  Taneps(j) = R(j,I)/R(Nstrms,I)
               ENDDO
            ELSEIF ( l1==3 ) THEN
               DO j = 1 , Nstrms
                  Taneps(j) = Xl(j,I)
               ENDDO
            ELSEIF ( l1==4 ) THEN
               DO j = 1 , Nstrms
                  Taneps(j) = Xl(j,I)/Xl(Nstrms,I)
               ENDDO
            ELSE
               DO j = 1 , Nstrms
                  Taneps(j) = R(j,I)
               ENDDO
            ENDIF
            l1 = Is2(I)
            IF ( Nwork(I)==5 .OR. Nwork(I)==6 ) CALL alg01(Datac(l1),Data6(l1),Ndata(I),Taneps,deltb,x1,Nstrms,Nterp(I),0)
            CALL alg01(Datac(l1),Data5(l1),Ndata(I),Taneps,solid,x1,Nstrms,Nterp(I),0)
            CALL alg01(Datac(l1),Data3(l1),Ndata(I),Taneps,Taneps,x1,Nstrms,Nterp(I),0)
            l1 = I + Nl1(I)
            l2 = l1
            IF ( Nloss(I)==1 .OR. Nloss(I)==4 .OR. Nwork(I)==7 ) l2 = I + Nl2(I)
            xn = xn*Pi/(30.0*Sclfac)
            DO j = 1 , Nstrms
               u = xn*R(j,I)
               vr = sqrt(Vm(j,I)**2+(Vw(j,I)-u)**2)
               xmr = xm(j)*vr/v(j)
               beta = atan(Tbeta(j,I))*C1
               bbeta = 0.0
               IF ( Nwork(I)==5 .OR. Nwork(I)==6 ) bbeta = beta - deltb(j)
               deltb(j) = deltb(j)*q
               delp = 0.0
               IF ( I/=Nstns .AND. Nwork(I+1)/=0 .AND. Speed(I)==Speed(I+1) ) THEN
                  x1 = sqrt((R(j,I+1)-R(j,I))**2+(X(j,I+1)-X(j,I))**2)
                  x2 = sqrt((R(j,I)-R(j,I-1))**2+(X(j,I)-X(j,I-1))**2)
                  x3 = xblade
                  delp = Pi*R(j,I)*wt(j)/(Sclfac**2*x3*G)                                                                           &
                       & *(Tbeta(j,I)/(1.0+Tbeta(j,I)**2)*ts(j)*G*Ej*((S(j,I+1)-S(j,I))/x1+(S(j,I)-S(j,I-1))/x2)+Vm(j,I)/R(j,I)     &
                       & *((R(j,I+1)*Vw(j,I+1)-R(j,I)*Vw(j,I))/x1+(R(j,I)*Vw(j,I)-R(j,I-1)*Vw(j,I-1))/x2))
                  deltp(j,I) = delp
               ENDIF
               hri = H(j,I) - (v(j)**2-vr**2)/(2.0*G*Ej)
               prd = alg4(hri,S(j,l1))
               pr = alg4(hri,S(j,I))
               tr(j,I) = alg7(hri,S(j,I))
               prl2 = pr
               psl2 = ps(j)*Sclfac**2
               IF ( l2/=I ) THEN
                  prl2 = H(j,l2) - (Vw(j,l2)**2-(Vw(j,l2)-xn*R(j,l2))**2)/(2.0*G*Ej)
                  prl2 = alg4(prl2,S(j,l2))
                  psl2 = H(j,l2) - (Vw(j,l2)**2+Vm(j,l2)**2)/(2.0*G*Ej)
                  psl2 = alg4(psl2,S(j,l2))
               ENDIF
               coef = (prd-pr)/(prl2-psl2)
               dif = 0.0
               IF ( solid(j)/=0.0 ) THEN
                  x2 = Vw(j,l1) - xn*R(j,l1)
                  x1 = sqrt(Vm(j,l1)**2+x2**2)
                  x3 = Vw(j,I) - u
                  dif = 1.0 - vr/x1 + (x2-x3)/(2.0*x1*solid(j))*q
               ENDIF
               prl1 = prl2
               psl1 = psl2
               IF ( l2/=l1 ) THEN
                  psl1 = H(j,l1) - (Vw(j,l1)**2+Vm(j,l1)**2)/(2.0*G*Ej)
                  prl1 = psl1 + (Vm(j,l1)**2+(Vw(j,l1)-xn*R(j,l1))**2)/(2.0*G*Ej)
                  psl1 = alg4(psl1,S(j,l1))
                  prl1 = alg4(prl1,S(j,l1))
               ENDIF
               dpq = (ps(j)-psl1)/(prl1-psl1)
               IF ( Iprtc==1 ) WRITE (Log2,99006) j , u , vr , xmr , beta , deltb(j) , bbeta , Taneps(j) , delp , coef , dif , dpq
99006          FORMAT (I6,F14.2,F11.2,F11.4,4F11.3,F11.4,F11.5,F10.4,F11.4)
            ENDDO
            CALL alg03(Lnct,Nstrms+5)
            pbar = 0.0
            hbar = 0.0
            DO j = 1 , Itub
               x1 = (Delf(j+1)-Delf(j))/2.0
               pbar = pbar + x1*(pt(j)+pt(j+1))
               hbar = hbar + x1*(H(j,I)+H(j+1,I))
            ENDDO
            rbar1 = pbar/p1bar
            dh1 = (hbar-h1bar)/h1bar
            eff1 = 0.0
            IF ( hbar/=h1bar ) eff1 = (alg2(s1bar,pbar)-h1bar)/(hbar-h1bar)
            opr = rbar1
            IF ( eff1/=0.0 ) oeff = eff1
            IF ( l1/=l1keep ) THEN
               l1keep = l1
               pnbar = 0.0
               hnbar = 0.0
               DO j = 1 , Nstrms
                  pn(j) = alg4(H(j,l1),S(j,l1))
               ENDDO
               DO j = 1 , Itub
                  x1 = (Delf(j+1)-Delf(j))/2.0
                  pnbar = pnbar + x1*(pn(j)+pn(j+1))
                  hnbar = hnbar + x1*(H(j,l1)+H(j+1,l1))
               ENDDO
               snbar = alg3(pnbar,hnbar)
            ENDIF
            effn = 0.0
            IF ( hnbar/=hbar ) effn = (alg2(snbar,pbar)-hnbar)/(hbar-hnbar)
            rbarn = pbar/pnbar
            dhn = (hbar-hnbar)/hnbar
            IF ( Iprtc==1 ) WRITE (Log2,99007) I , l1 , I , I , l1 , I , rbar1 , rbarn , eff1 , effn , dh1 , dhn
99007       FORMAT (/,'  STREAM',7X,'INLET THROUGH STATION',I3,7X,'STATION',I3,' THROUGH STATION',I3,5X,'MEAN VALUES',6X,           &
                   &'INLET TO STA.',I2,'   STA.',I2,' TO STA.',I2,/,'  -LINE',6X,'PRESSURE  ISENTROPIC  DELTA H    PRESSURE  ',     &
                   &'ISENTROPIC  DELTA H     PRESSURE RATIO',F14.4,F19.4,/15X,                                                      &
                   &'RATIO   EFFICIENCY  ON H1        RATIO   EFFICIENCY  ON ','H1       ISEN EFFY',2F19.4,/80X,'DELTA H ON H1',    &
                  & F15.4,F19.4)
            DO j = 1 , Nstrms
               rbar1 = pt(j)/p1(j)
               eff1 = 0.0
               IF ( H(j,I)/=H(j,1) ) eff1 = (alg2(S(j,1),pt(j))-H(j,1))/(H(j,I)-H(j,1))
               dh1 = (H(j,I)-H(j,1))/H(j,1)
               rbarn = pt(j)/pn(j)
               effn = 0.0
               IF ( H(j,I)/=H(j,l1) ) effn = (alg2(S(j,l1),pt(j))-H(j,l1))/(H(j,I)-H(j,l1))
               dhn = (H(j,I)-H(j,l1))/H(j,l1)
               IF ( Iprtc==1 ) WRITE (Log2,99008) j , rbar1 , eff1 , dh1 , rbarn , effn , dhn
99008          FORMAT (I6,F14.4,F10.4,F11.4,F12.4,F10.4,F11.4)
            ENDDO
         ENDIF
         IF ( ifle/=0 ) THEN
            CALL alg03(Lnct,Nstrms+8)
            xn = Speed(I+1)*Spdfac(Icase)
            ip = I + 1
            xblade = 10.0
            IF ( Nblade(ip)/=0 ) xblade = abs(float(Nblade(ip)))
            l1 = xblade
            IF ( Iprtc==1 ) WRITE (Log2,99009) I , xn , l1
99009       FORMAT (/10X,'STATION',I3,' IS AT THE LEADING EDGE OF A BLADE ','ROATING AT',F9.1,' RPM  NUMBER OF BLADES IN ROW =',I3, &
                  & /10X,99(1H*),//,'  STREAM      BLADE     RELATIVE   ','RELATIVE   RELATIVE  INCIDENCE    BLADE      LEAN    ',  &
                   &'PRESS DIFF',/,'  -LINE       SPEED     VELOCITY   MACH',                                                       &
                   &' NO.  FLOW ANGLE   ANGLE      ANGLE      ANGLE   ACROSS',' BLADE',/)
            xn = xn*Pi/(30.0*Sclfac)
            q = 1.0
            IF ( Speed(ip)>=0.0 ) THEN
               IF ( Speed(ip)>0.0 ) THEN
                  q = -1.0
               ELSEIF ( ip>=3 ) THEN
                  ii = ip - 1
                  DO WHILE ( Speed(ii)==0.0 )
                     IF ( ii==2 ) GOTO 20
                     ii = ii - 1
                  ENDDO
                  IF ( Speed(ii)<0.0 ) q = -1.0
               ENDIF
            ENDIF
 20         DO j = 1 , Nstrms
               Cr(j) = 0.0
               Taneps(j) = 0.0
            ENDDO
            IF ( Nwork(I)==0 .AND. Ndata(I)/=0 ) THEN
               l1 = Ndimen(I) + 1
               IF ( l1==2 ) THEN
                  DO j = 1 , Nstrms
                     Taneps(j) = R(j,I)/R(Nstrms,I)
                  ENDDO
               ELSEIF ( l1==3 ) THEN
                  DO j = 1 , Nstrms
                     Taneps(j) = Xl(j,I)
                  ENDDO
               ELSEIF ( l1==4 ) THEN
                  DO j = 1 , Nstrms
                     Taneps(j) = Xl(j,I)/Xl(Nstrms,I)
                  ENDDO
               ELSE
                  DO j = 1 , Nstrms
                     Taneps(j) = R(j,I)
                  ENDDO
               ENDIF
               l1 = Is2(I)
               CALL alg01(Datac(l1),Data1(l1),Ndata(I),Taneps,Cr,x1,Nstrms,Nterp(I),0)
               CALL alg01(Datac(l1),Data3(l1),Ndata(I),Taneps,Taneps,x1,Nstrms,Nterp(I),0)
            ENDIF
            bbeta = 0.0
            DO j = 1 , Nstrms
               u = xn*R(j,I)
               vr = sqrt(Vm(j,I)**2+(Vw(j,I)-u)**2)
               xmr = xm(j)*vr/v(j)
               tr(j,I) = alg7(H(j,I)-(v(j)**2-vr**2)/(2.0*G*Ej),S(j,I))
               beta = atan((Vw(j,I)-u)/Vm(j,I))*C1
!
!     STORE REL. MACH, REL. VEL AND REL. FLOW ANGLE FOR ALL STREAMLINES
!     AT THE BLADE LEADING EDGE
!
               IF ( Icase==1 .AND. I==ledgeb ) THEN
                  rmdv(j,1) = xmr
                  rmdv(j,3) = vr
                  rmdv(j,4) = beta
               ENDIF
               deltb(j) = 0.0
               IF ( Nwork(I)==0 .AND. Ndata(I)/=0 ) THEN
                  bbeta = atan((tan(Cr(j)/C1)*(1.0-Gama(j)*tan(Phi(j)))-tan(Phi(j))*tan(Taneps(j)/C1)*sqrt(1.0+Gama(j)**2))         &
                        & *cos(Phi(j)))*C1
                  deltb(j) = (beta-bbeta)*q
               ENDIF
               x1 = sqrt((R(j,I+1)-R(j,I))**2+(X(j,I+1)-X(j,I))**2)
               delp = Pi*R(j,I)*2.0*wt(j)/(Sclfac**2*xblade*G)*(sin(beta/C1)*cos(beta/C1)*G*Ej*ts(j)*(S(j,I+1)-S(j,I))/x1+Vm(j,I)   &
                    & /(R(j,I)*x1)*(R(j,I+1)*Vw(j,I+1)-R(j,I)*Vw(j,I)))
               deltp(j,I) = delp
               IF ( Iprtc==1 ) WRITE (Log2,99010) j , u , vr , xmr , beta , deltb(j) , bbeta , Taneps(j) , delp
99010          FORMAT (I6,F14.2,F11.2,F11.4,4F11.3,F11.4)
            ENDDO
         ENDIF
      ELSE
         p1bar = 0.0
         h1bar = 0.0
         p1(1) = pt(1)
         pn(1) = pt(1)
         DO j = 1 , Itub
            p1(j+1) = pt(j+1)
            pn(j+1) = pt(j+1)
            x1 = (Delf(j+1)-Delf(j))/2.0
            p1bar = p1bar + x1*(pt(j)+pt(j+1))
            h1bar = h1bar + x1*(H(j,1)+H(j+1,1))
         ENDDO
         hbar = h1bar
         s1bar = alg3(p1bar,h1bar)
         pnbar = p1bar
         hnbar = h1bar
         snbar = s1bar
         l1keep = 1
      ENDIF
   ENDDO
   IF ( Nbl/=0 ) THEN
      l1 = (ilast-1)/10 + 1
      CALL alg03(Lnct,3+5*l1)
      IF ( Iprtc==1 ) THEN
         WRITE (Log2,99011)
99011    FORMAT (/10X,'ANNULUS WALL BOUNDARY LAYER CALCULATION RESULTS',/10X,47(1H*))
         DO k = 1 , l1
            l2 = 10*(k-1) + 1
            l3 = l2 + 9
            IF ( l3>ilast ) l3 = ilast
            WRITE (Log2,99012) (I,I=l2,l3)
99012       FORMAT (/,' STATION NUMBER',14X,10I10)
            WRITE (Log2,99013) (Delh(I),I=l2,l3)
99013       FORMAT (' HUB DISPLACEMENT THICKNESS',4X,10F10.5)
            WRITE (Log2,99014) (Delt(I),I=l2,l3)
99014       FORMAT (' CASE DISPLACEMENT THICKNESS',3X,10F10.5)
            WRITE (Log2,99015) (Wwbl(I),I=l2,l3)
99015       FORMAT (' BLOCKAGE AREA FRACTION',8X,10F10.5)
         ENDDO
      ENDIF
   ENDIF
   CALL alg03(Lnct,4)
   IF ( Iprtc==1 .AND. Ivfail==0 .AND. Iffail==0 ) WRITE (Log2,99016) Icase , Ipass
99016 FORMAT (/10X,'POINT NO',I3,'   PASS',I3,'   THE CALCULATION IS ','CONVERGED',/10X,52(1H*))
   IF ( Ifailo/=0 ) WRITE (Log2,99017) Icase , Ipass , Ifailo
99017 FORMAT (/10X,'POINT NO',I3,'   PASS',I3,'   THE CALCULATION FAIL','ED AT STATION',I3,/10X,60(1H*))
   IF ( Ifailo==0 .AND. (Ivfail/=0 .OR. Iffail/=0) ) WRITE (Log2,99018) Icase , Ipass , Ivfail , Iffail
99018 FORMAT (/10X,'POINT NO',I3,'   PASS',I3,'   THE CALCULATION IS ','NOT FULLY CONVERGED  IVFAIL =',I3,'  IFFAIL =',I3,/10X,     &
             &88(1H*))
   power = Flow(Icase)*(hbar-h1bar)*Ej/pfac
   IF ( Iprtc==1 ) WRITE (Log2,99019) Spdfac(Icase) , Flow(Icase) , opr , oeff , power
99019 FORMAT (10X,'SPEED FACTOR =',F10.3,'  FLOW =',F8.3,'  TOTAL PRES','SURE RATIO =',F7.3,'  ISENTROPIC EFFICIENCY =',F6.4,       &
             &'  POWER =',E11.4)
   IF ( Iprtc==0 ) WRITE (Log2,99020) Icase , Ipass , Spdfac(Icase) , Flow(Icase) , opr , oeff , power
99020 FORMAT (18H     FOR POINT NO.,I3,5H PASS,I3,15H - SPEED FACTOR,10X,1H=,F10.4/32X,4HFLOW,18X,1H=,F10.4,/32X,                   &
             &23HTOTAL PRESSURE RATIO  =,F10.4,/32X,'ISENTROPIC ','EFFICIENCY =',F10.4,/32X,'POWER',17X,1H=,E10.4)
   IF ( Ifailo/=0 ) GOTO 300
   l1 = 2
 100  DO I = l1 , Nstns
      nout3s = Nout3(I)/10
      IF ( nout3s==0 ) nout3s = Nout3(I)
      IF ( nout3s==1 .OR. nout3s==3 ) GOTO 200
   ENDDO
   GOTO 300
 200  l2 = I
   l3 = I + 1
   DO I = l3 , Nstns
      nout3s = Nout3(I)/10
      nout3t = Nout3(I) - nout3s*10
      IF ( nout3s==0 ) nout3t = 1
      IF ( nout3s==0 ) nout3s = Nout3(I)
      IF ( nout3s==2 .OR. nout3s==3 ) EXIT
   ENDDO
   l3 = I
   CALL alg03(Lnct,10)
   IF ( Iprtc==1 ) WRITE (Log2,99021) l2 , l3
99021 FORMAT (/10X,'DATA FOR NASTRAN PROGRAM FOR BLADE BETWEEN STATIONS',I3,' AND',I3,/10X,61(1H*),//)
   IF ( nout3t/=2 ) THEN
      IF ( Iprtc==1 ) WRITE (Log2,99022)
99022 FORMAT (' NAME   CODE    DELTA P   ELEMENT',7X,'MESHPOINTS -  J   I',9X,'J   I',9X,'J   I',/)
      Lnct = Lnct - 4
      ielem = 0
      xsign = -float(Nsign)
      l4 = l2 + 1
      idata(1) = name1(1)
      idata(2) = name1(2)
      idata(3) = 60
      DO j = 1 , Itub
         DO I = l4 , l3
            CALL alg03(Lnct,2)
            ielem = ielem + 1
            l5 = I - 1
            l6 = j + 1
            IF ( I==l3 ) THEN
               pload = xsign*((deltp(j,l5)+deltp(l6,l5))/3.0)
               IF ( Nblade(I)<0 ) pload = pload*0.75
               IF ( Iprtc==1 ) WRITE (Log2,99028) pload , ielem , j , l5 , l6 , l5 , l6 , I
               rdata(4) = pload
               idata(5) = ielem
               CALL write(Iscr,idata,5,1)
               ielem = ielem + 1
               IF ( Nblade(I)>=0 ) pload = xsign*(deltp(j,l5)/3.0)
               IF ( Iprtc==1 ) WRITE (Log2,99028) pload , ielem , j , l5 , l6 , I , j , I
               rdata(4) = pload
               idata(5) = ielem
               CALL write(Iscr,idata,5,1)
            ELSE
               pload = xsign*((deltp(j,l5)+deltp(l6,l5)+deltp(l6,I))/3.0)
               IF ( Nblade(I)<0 ) pload = xsign*((deltp(j,l5)+deltp(j,I)+deltp(l6,l5)+deltp(l6,I))*0.25)
               IF ( Iprtc==1 ) WRITE (Log2,99028) pload , ielem , l6 , l5 , l6 , I , j , l5
               rdata(4) = pload
               idata(5) = ielem
               CALL write(Iscr,idata,5,1)
               ielem = ielem + 1
               IF ( Nblade(I)>=0 ) pload = xsign*((deltp(j,l5)+deltp(l6,I)+deltp(j,I))/3.0)
               IF ( Iprtc==1 ) WRITE (Log2,99028) pload , ielem , j , l5 , l6 , I , j , I
               rdata(4) = pload
               idata(5) = ielem
               CALL write(Iscr,idata,5,1)
            ENDIF
         ENDDO
      ENDDO
      l1 = l3
   ENDIF
   IF ( nout3t/=1 ) THEN
!
!     OUTPUT RELATIVE TOTAL TEMPERATURES AT NODES ON *TEMP* CARDS
!
      CALL alg03(Lnct,10)
      Lnct = Lnct - 6
      IF ( Iprtc==1 ) WRITE (Log2,99023)
99023 FORMAT (//,' NAME   CODE    DELTA T   NODE',10X,'MESHPOINTS -  ','J   I   COORDINATES -   RADIAL       AXIAL',/)
      inode = 1
      idata(1) = name2(1)
      idata(2) = name2(2)
      idata(3) = 70
      DO j = 1 , Nstrms
         DO I = l2 , l3
            CALL alg03(Lnct,1)
            idata(4) = inode
            rdata(5) = tr(j,I)
            CALL write(Iscr,idata,5,1)
            IF ( Iprtc==1 ) WRITE (Log2,99024) tr(j,I) , inode , j , I , R(j,I) , X(j,I)
99024       FORMAT (' TEMP     70',F12.5,I6,21X,2I4,16X,F10.4,2X,F10.4)
            inode = inode + 1
         ENDDO
      ENDDO
   ENDIF
   GOTO 100
!
!     PUNCH STREAML2 BULK DATA CARDS FOR EACH STREAMLINE
!     CHANGE THE SIGN ON THE STAGGER AND FLOW ANGLES FOR STREAML2 CARDS.
!     THIS CHANGE IS NECESSARY BECAUSE OF THE AERODYNAMIC PROGRAMS IN
!     NASTRAN MODULE AMG THAT USE THESE ANGLES.
!
 300  IF ( ledgeb*itrleb/=0 ) THEN
      IF ( Istrml/=-1 .AND. Istrml/=1 ) THEN
         WRITE (Log2,99025)
99025    FORMAT (//10X,47HNASTRAN - STREAML2 - COMPRESSOR BLADE BULK DATA,/10X,49(1H*),/,'  SLN  NSTNS  STAGGER    CHORD    RADIUS',&
                &'    BSPACE     MACH       DEN       VEL      FLOWA',/)
         nstnsx = itrleb - ledgeb + 1
         DO ileb = 1 , Nstrms
            radius = (rmdv(ileb,5)+rmdv(ileb,6))/2.0
            bspace = (6.283185*radius)/float(Nbldes)
            Stag(ileb) = -1.0*Stag(ileb)
            rmdv(ileb,4) = -1.0*rmdv(ileb,4)
            WRITE (Lpunch,99026) ileb , nstnsx , Stag(ileb) , Chordd(ileb) , radius , bspace , rmdv(ileb,1) , rmdv(ileb,2) , ileb , &
                               & ileb , rmdv(ileb,3) , rmdv(ileb,4)
99026       FORMAT (8HSTREAML2,2I8,F8.3,3F8.5,2F8.6,5H+STRL,I2,5H+STRL,I2,F8.1,F8.3)
            WRITE (Log2,99027) ileb , nstnsx , Stag(ileb) , Chordd(ileb) , radius , bspace , rmdv(ileb,1) , rmdv(ileb,2) ,          &
                             & rmdv(ileb,3) , rmdv(ileb,4)
99027       FORMAT (I5,I6,2X,F8.3,3(2X,F8.5),2(2X,F8.6),2X,F8.1,2X,F8.3)
         ENDDO
      ENDIF
   ENDIF
99028 FORMAT (' PLOAD2   60',F12.5,I7,14X,3(I10,I4))
END SUBROUTINE alg11
