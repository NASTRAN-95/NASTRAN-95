
SUBROUTINE alg08
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
   INTEGER I , Icase , Ifail , Ifailo , Iffail , Iloss , Imid , Ipass , Iprint , Is1(30) , Is2(30) , Is3(30) , Istag , Iter , Itub ,&
         & Ivfail , Lnct , Log1 , Log2 , Log3 , Log4 , Log5 , Log6 , Nbl , Nblade(30) , Ncase , Ncurve(30) , Ndata(30) , Ndel(30) , &
         & Ndiff(4) , Ndimen(30) , Neqn , Neval(30) , Nforce , Nl1(30) , Nl2(30) , Nliter(30) , Nloss(30) , Nm(2) , Nmach(30) ,     &
         & Nmany , Nmax , Nmix , Nout1(30) , Nout2(30) , Nout3(30) , Npage , Nplot , Npunch , Nrad(2) , Nread , Nset1 , Nset2 ,     &
         & Nspec(30) , Nsplit , Nstns , Nstplt , Nstrms , Nterp(30) , Ntrans , Nwhich(30) , Nwork(30)
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
!
! Local variable declarations
!
   REAL afun(20) , bfun(20) , dl(21) , dladm(21) , dphidl(21) , drvwdm(21) , dsdl(21) , dsdm(21) , dv , dv2dl , dvmdvm(20) , dwdv , &
      & fx1(21) , fx2(21) , hs(20) , tbip1(21) , teip1(21) , vav , vmax , vmin , vmmax(21) , vold , vvold(21) , w , x1 , x10 , x11 ,&
      & x12 , x2 , x3 , x4 , x5 , x6 , x7 , x8 , x9 , xm2(20) , xn , xq , xx1(21)
   REAL alg4 , alg5 , alg7 , alg8 , alg9
   INTEGER iconf1 , iconf2 , ifaie , itmax , j , jinc , jj , jold , k , l1 , l2 , loop , lpmax
!
! End of declarations
!
!
!
!
!
   itmax = 20
   lpmax = 10
   k = 1
   IF ( I==Istag ) k = 2
   xn = Speed(I)*Spdfac(Icase)*Pi/(30.0*Sclfac)
   IF ( I/=1 ) THEN
      DO j = 1 , Nstrms
         Lamim1(j) = Lami(j)
         Lami(j) = Lamip1(j)
         Lamip1(j) = 1.0
      ENDDO
      IF ( I/=Nstns ) THEN
         IF ( Ndata(I+1)/=0 ) THEN
            l1 = Ndimen(I+1) + 1
            IF ( l1==2 ) THEN
               DO j = 1 , Nstrms
                  xx1(j) = R(j,I+1)/R(Nstrms,I+1)
               ENDDO
            ELSEIF ( l1==3 ) THEN
               DO j = 1 , Nstrms
                  xx1(j) = Xl(j,I+1)
               ENDDO
            ELSEIF ( l1==4 ) THEN
               DO j = 1 , Nstrms
                  xx1(j) = Xl(j,I+1)/Xl(Nstrms,I+1)
               ENDDO
            ELSE
               DO j = 1 , Nstrms
                  xx1(j) = R(j,I+1)
               ENDDO
            ENDIF
            l1 = Is2(I+1)
            CALL alg01(Datac(l1),Data4(l1),Ndata(I+1),xx1,xx1,x1,Nstrms,Nterp(I+1),0)
            DO j = 1 , Nstrms
               Lamip1(j) = 1.0 - xx1(j)
            ENDDO
         ENDIF
         DO j = 1 , Nstrms
            x1 = sqrt((R(j,I+1)-R(j,I))**2+(X(j,I+1)-X(j,I))**2)
            x2 = sqrt((R(j,I)-Rim1(j))**2+(X(j,I)-Xim1(j))**2)
            x3 = atan2(R(j,I+1)-R(j,I),X(j,I+1)-X(j,I))
            x4 = atan2(R(j,I)-Rim1(j),X(j,I)-Xim1(j))
            Phi(j) = (x3+x4)/2.0
            Cr(j) = (x3-x4)/(x1+x2)*2.0
            dsdm(j) = 0.0
            drvwdm(j) = 0.0
            dladm(j) = ((Lamip1(j)-Lami(j))/x1+(Lami(j)-Lamim1(j))/x2)/2.0
            IF ( Ipass/=1 ) THEN
               dsdm(j) = ((S(j,I+1)-S(j,I))/x1+(S(j,I)-S(j,I-1))/x2)/2.0*G*Ej
               drvwdm(j) = ((R(j,I+1)*Vw(j,I+1)-R(j,I)*Vw(j,I))/x1+(R(j,I)*Vw(j,I)-Rim1(j)*Vw(j,I-1))/x2)/(2.0*R(j,I))
            ENDIF
         ENDDO
         IF ( Ipass/=1 .AND. Ndata(I)/=0 .AND. Neqn/=1 .AND. Nwork(I)==0 .AND. Nwork(I+1)/=0 ) THEN
            l1 = Ndimen(I) + 1
            IF ( l1==2 ) THEN
               DO j = 1 , Nstrms
                  teip1(j) = R(j,I)/R(Nstrms,I)
               ENDDO
            ELSEIF ( l1==3 ) THEN
               DO j = 1 , Nstrms
                  teip1(j) = Xl(j,I)
               ENDDO
            ELSEIF ( l1==4 ) THEN
               DO j = 1 , Nstrms
                  teip1(j) = Xl(j,I)/Xl(Nstrms,I)
               ENDDO
            ELSE
               DO j = 1 , Nstrms
                  teip1(j) = R(j,I)
               ENDDO
            ENDIF
            l1 = Is2(I)
            CALL alg01(Datac(l1),Data3(l1),Ndata(I),teip1,teip1,x1,Nstrms,Nterp(I),0)
            x1 = Speed(I+1)*Spdfac(Icase)*Pi/(30.0*Sclfac)
            DO j = 1 , Nstrms
               teip1(j) = tan(teip1(j)/C1)
               tbip1(j) = (Vw(j,I)-x1*R(j,I))/Vm(j,I)
            ENDDO
         ENDIF
         GOTO 100
      ENDIF
   ENDIF
   DO j = 1 , Nstrms
      dladm(j) = 0.0
      dsdm(j) = 0.0
      drvwdm(j) = 0.0
      Cr(j) = 0.0
   ENDDO
   IF ( I==1 ) THEN
      DO j = 1 , Nstrms
         Phi(j) = atan2(R(j,2)-R(j,1),X(j,2)-X(j,1))
      ENDDO
      DO j = 1 , Nstrms
         Xi(j) = H(j,1)
         Lami(j) = 1.0
         Lamip1(j) = 1.0
      ENDDO
      IF ( Ndata(2)/=0 ) THEN
         l2 = Ndimen(2) + 1
         IF ( l2==2 ) THEN
            DO j = 1 , Nstrms
               xx1(j) = R(j,2)/R(Nstrms,2)
            ENDDO
         ELSEIF ( l2==3 ) THEN
            DO j = 1 , Nstrms
               xx1(j) = Xl(j,2)
            ENDDO
         ELSEIF ( l2==4 ) THEN
            DO j = 1 , Nstrms
               xx1(j) = Xl(j,2)/Xl(Nstrms,2)
            ENDDO
         ELSE
            DO j = 1 , Nstrms
               xx1(j) = R(j,2)
            ENDDO
         ENDIF
         l1 = Is2(2)
         CALL alg01(Datac(l1),Data4(l1),Ndata(2),xx1,xx1,x1,Nstrms,Nterp(2),0)
         DO j = 1 , Nstrms
            Lamip1(j) = 1.0 - xx1(j)
         ENDDO
      ENDIF
   ELSE
      DO j = 1 , Nstrms
         Phi(j) = atan2(R(j,I)-Rim1(j),X(j,I)-Xim1(j))
      ENDDO
   ENDIF
 100  CALL alg01(R(1,I),X(1,I),Nstrms,R(1,I),x1,Gama,Nstrms,0,1)
   DO j = 1 , Nstrms
      Gama(j) = atan(Gama(j))
      Sppg(j) = Gama(j) + Phi(j)
      Cppg(j) = cos(Sppg(j))
      Sppg(j) = sin(Sppg(j))
      Vv(j) = Vm(j,I)
   ENDDO
   DO j = 1 , Itub
      dl(j) = Xl(j+1,I) - Xl(j,I)
      dsdl(j) = (S(j+1,I)-S(j,I))/dl(j)*G*Ej
      dphidl(j) = (Phi(j+1)-Phi(j))/dl(j)
   ENDDO
   IF ( I==1 .OR. Nwork(I)>=5 ) THEN
      DO j = 1 , Itub
         fx1(j) = (Tbeta(j+1,I)+Tbeta(j,I))/(R(j+1,I)+R(j,I))*(R(j+1,I)*Tbeta(j+1,I)-R(j,I)*Tbeta(j,I))/dl(j)
         fx2(j) = (Xi(j+1)-Xi(j))/dl(j)*G*Ej
      ENDDO
      DO j = 1 , Nstrms
         x1 = Xi(j) + (xn*R(j,I))**2/(2.0*G*Ej)
         x1 = 1.0/(alg9(x1,S(j,I),1.0)*(1.0+(alg8(x1,S(j,I))-1.0)*(1.0+Tbeta(j,I)**2)/2.0))
         IF ( x1<=1.0 ) THEN
            IF ( Ipass>Nforce ) THEN
               CALL alg03(Lnct,1)
               WRITE (Log2,99014) Ipass , I , j , x1
            ENDIF
            x1 = 6250000.0
            IF ( Ifailo==0 ) Ifailo = I
         ENDIF
         vmmax(j) = sqrt(x1)
      ENDDO
   ELSE
      DO j = 1 , Itub
         dvmdvm(j) = 0.0
         fx1(j) = (Vw(j+1,I)+Vw(j,I))/(R(j+1,I)+R(j,I))*(R(j+1,I)*Vw(j+1,I)-R(j,I)*Vw(j,I))/dl(j)
         fx2(j) = (H(j+1,I)-H(j,I))/dl(j)*G*Ej
      ENDDO
      DO j = 1 , Nstrms
         x1 = alg8(H(j,I),S(j,I))
         x1 = (2.0/alg9(H(j,I),S(j,I),1.0)-Vw(j,I)**2*(x1-1.0))/(x1+1.0)
         IF ( x1<=1.0 ) THEN
            IF ( Ipass>Nforce ) THEN
               CALL alg03(Lnct,1)
               WRITE (Log2,99014) Ipass , I , j , x1
            ENDIF
            x1 = 6250000.0
            IF ( Ifailo==0 ) Ifailo = I
         ENDIF
         vmmax(j) = sqrt(x1)
      ENDDO
   ENDIF
   vmax = 0.0
   vmin = 1.05*vmmax(Imid)
   Iter = 0
 200  Iter = Iter + 1
   Ifail = 0
   iconf1 = 0
   DO j = 1 , Nstrms
      vvold(j) = Vv(j)
   ENDDO
   IF ( I==1 .OR. Nwork(I)>=5 ) THEN
      j = Imid
      jinc = 1
      GOTO 400
   ELSE
      DO j = 1 , Itub
         x1 = (H(j,I)+H(j+1,I))/2.0 - (((vvold(j)+vvold(j+1))/2.0)**2+((Vw(j,I)+Vw(j+1,I))/2.0)**2)/(2.0*G*Ej)
         IF ( x1<Hmin ) THEN
            IF ( Ipass>Nforce ) THEN
               IF ( Lnct>=Npage ) THEN
                  WRITE (Log2,99015)
                  Lnct = 1
               ENDIF
               Lnct = Lnct + 1
               WRITE (Log2,99001) Ipass , I , Iter , j , x1
99001          FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,                                            &
                      &53H  STATIC ENTHALPY BELOW LIMIT IN MOMENTUM EQUATION AT,E13.5)
            ENDIF
            Ifail = 1
            x1 = Hmin
         ENDIF
         x2 = (S(j,I)+S(j+1,I))/2.0
         x6 = alg8(x1,x2)
         x7 = alg7(x1,x2)
         x1 = alg9(x1,x2,((vvold(j)+vvold(j+1))/2.0)**2)
         xq = x1
         IF ( x1>0.9801 ) THEN
            IF ( Ipass>Nforce ) THEN
               IF ( Lnct>=Npage ) THEN
                  WRITE (Log2,99015)
                  Lnct = 1
               ENDIF
               Lnct = Lnct + 1
               x1 = sqrt(x1)
               WRITE (Log2,99002) Ipass , I , Iter , j , x1
99002          FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,39H  MERIDIONAL MACH NUMBER ABOVE LIMIT AT, &
                     & E13.5)
            ENDIF
            Ifail = 1
            x1 = 0.9801
         ENDIF
         x2 = (Cppg(j)+Cppg(j+1))/2.0
         x3 = (Sppg(j)+Sppg(j+1))/2.0
         afun(j) = -2.0/(1.0-x1)*((1.0-x2*x2*xq)*(Cr(j)+Cr(j+1))/(2.0*x2)-x3/x2*dphidl(j)                                           &
                 & -x3*(sin((Phi(j)+Phi(j+1))/2.0)/(R(j,I)+R(j+1,I))*2.0*(1.0+x1*((Vw(j,I)+Vw(j+1,I))/(vvold(j)+vvold(j+1)))**2)    &
                 & +(dladm(j)+dladm(j+1))/(Lami(j)+Lami(j+1))))
         bfun(j) = 2.0*(fx2(j)-x7*dsdl(j)-fx1(j))
         IF ( I/=Nstns .AND. Ipass/=1 ) THEN
            IF ( Neqn==1 .OR. Ndata(I)==0 .OR. (Nwork(I)==0 .AND. Nwork(I+1)==0) ) THEN
               bfun(j) = bfun(j) + x7*(dsdm(j)+dsdm(j+1))*x3*(1.0-x1*(x6-1.0))/(1.0-x1)
            ELSE
               IF ( Nwork(I)==0 ) THEN
                  x4 = (tbip1(j)+tbip1(j+1))*0.5
                  x5 = (teip1(j)+teip1(j+1))*0.5
               ELSE
                  x4 = (Tbeta(j,I)+Tbeta(j+1,I))/2.0
                  x5 = (Taneps(j)+Taneps(j+1))/2.0
               ENDIF
               bfun(j) = bfun(j) + 2.0*(x7*(dsdm(j)+dsdm(j+1))/2.0*(x3*(1.0/(1.0+x4*x4)+x6*x1/(1.0-x1))-x5*x4/(1.0+x4*x4))          &
                       & -(vvold(j)+vvold(j+1))*.25*(drvwdm(j)+drvwdm(j+1))*(x5-x3*x1/(1.0-x1)*x4))
            ENDIF
         ENDIF
      ENDDO
      Vv(Imid) = vvold(Imid)**2
      j = Imid
      jinc = 1
   ENDIF
 300  jold = j
   j = j + jinc
   jj = jold
   IF ( jinc==-1 ) jj = j
   IF ( abs(afun(jj))<=1.0E-5 ) THEN
      Vv(j) = Vv(jold) + bfun(jj)*(Xl(j,I)-Xl(jold,I))
   ELSE
      x1 = -afun(jj)*(Xl(j,I)-Xl(jold,I))
      IF ( x1>88.0 ) THEN
         IF ( Ipass>Nforce ) THEN
            IF ( Lnct>=Npage ) THEN
               WRITE (Log2,99015)
               Lnct = 1
            ENDIF
            Lnct = Lnct + 1
            WRITE (Log2,99003) Ipass , I , Iter , jj , x1
99003       FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,43H  MOMENTUM EQUATION EXPONENT ABOVE LIMIT AT,&
                  & E13.5)
         ENDIF
         Ifail = 1
         x1 = 88.0
      ENDIF
      x1 = exp(x1)
      Vv(j) = Vv(jold)*x1 + (1.0-x1)*bfun(jj)/afun(jj)
   ENDIF
   IF ( j==k ) THEN
      DO j = k , Nstrms
         IF ( Vv(j)>4.0*vvold(Imid)**2 ) THEN
            Ifail = 1
            IF ( Ipass>Nforce ) THEN
               CALL alg03(Lnct,1)
               WRITE (Log2,99004) Ipass , I , Iter , j
99004          FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,                                            &
                      &50H  MERIDIONAL VELOCITY GREATER THAN TWICE MID VALUE)
            ENDIF
            Vv(j) = 4.0*vvold(Imid)**2
         ENDIF
         IF ( Vv(j)>=1.0 ) THEN
            Vv(j) = sqrt(Vv(j))
            IF ( Vv(j)>vmmax(j) ) THEN
               Ifail = 1
               IF ( Ipass>Nforce ) THEN
                  CALL alg03(Lnct,1)
                  WRITE (Log2,99016) Ipass , I , Iter , j , Vv(j) , vmmax(j)
               ENDIF
               Vv(j) = vmmax(j)
            ENDIF
         ELSE
            IF ( Ipass>Nforce ) THEN
               IF ( Lnct>=Npage ) THEN
                  WRITE (Log2,99015)
                  Lnct = 1
               ENDIF
               Lnct = Lnct + 1
               WRITE (Log2,99005) Ipass , I , Iter , j , Vv(j)
99005          FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,                                            &
                      &46H  (MERIDIONAL VELOCITY) SQUARED BELOW LIMIT AT,E13.5)
            ENDIF
            Vv(j) = 1.0
            Ifail = 1
         ENDIF
      ENDDO
      x1 = 0.0
      DO j = k , Itub
         x1 = x1 + (Xl(j+1,I)-Xl(j,I))*abs((Vv(j+1)+Vv(j))/(vvold(j+1)+vvold(j))-1.0)
      ENDDO
      x1 = x1/(Xl(Nstrms,I)-Xl(k,I))
      x2 = 0.1
      IF ( x1<0.2 ) x2 = exp(-11.52*x1)
      DO j = k , Nstrms
         Vv(j) = vvold(j) + x2*(Vv(j)-vvold(j))
      ENDDO
      IF ( Nloss(I)==1 .AND. Nl2(I)==0 ) CALL alg07
      DO j = 1 , Itub
         hs(j) = (H(j,I)+H(j+1,I))/2.0 - (((Vv(j)+Vv(j+1))/2.0)**2+((Vw(j,I)+Vw(j+1,I))/2.0)**2)/(2.0*G*Ej)
         IF ( hs(j)<Hmin ) THEN
            IF ( Ipass>Nforce ) THEN
               IF ( Lnct>=Npage ) THEN
                  WRITE (Log2,99015)
                  Lnct = 1
               ENDIF
               Lnct = Lnct + 1
               WRITE (Log2,99017) Ipass , I , Iter , j , hs(j)
            ENDIF
            Ifail = 1
            hs(j) = Hmin
         ENDIF
         xm2(j) = alg9(hs(j),(S(j,I)+S(j+1,I))/2.0,((Vv(j)+Vv(j+1))/2.0)**2)
      ENDDO
      GOTO 600
   ELSE
      IF ( j==Nstrms ) THEN
         j = Imid
         jinc = -1
      ENDIF
      GOTO 300
   ENDIF
 400  loop = 1
   jold = j
   j = j + jinc
   jj = jold
   IF ( jinc==-1 ) jj = j
 500  vold = Vv(j)
   vav = (vold+Vv(jold))/2.0
   ifaie = 0
   iconf2 = 0
   x2 = (Tbeta(j,I)+Tbeta(jold,I))/2.0
   x1 = (Xi(j)+Xi(jold))/2.0 + ((xn*(R(j,I)+R(jold,I))/2.0)**2-vav**2*(1.0+x2*x2))/(2.0*G*Ej)
   IF ( x1<Hmin ) THEN
      IF ( Ipass>Nforce ) THEN
         IF ( Lnct>=Npage ) THEN
            WRITE (Log2,99015)
            Lnct = 1
         ENDIF
         Lnct = Lnct + 1
         WRITE (Log2,99006) Ipass , I , Iter , jj , loop , x1
99006    FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,6H  LOOP,I3,                                      &
                &43H  STATIC H IN MOMENTUM EQUN. BELOW LIMIT AT,E13.5)
      ENDIF
      ifaie = 1
      iconf2 = 1
      x1 = Hmin
   ENDIF
   x3 = (S(j,I)+S(jold,I))/2.0
   x6 = alg8(x1,x3)
   x7 = alg7(x1,x3)
   x1 = alg9(x1,x3,vav*vav)
   IF ( x1>0.9801 ) THEN
      IF ( Ipass>Nforce ) THEN
         IF ( Lnct>=Npage ) THEN
            WRITE (Log2,99015)
            Lnct = 1
         ENDIF
         Lnct = Lnct + 1
         x1 = sqrt(x1)
         WRITE (Log2,99007) Ipass , I , Iter , jj , loop , x1
99007    FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,6H  LOOP,I3,                                      &
                &39H  MERIDIONAL MACH NUMBER ABOVE LIMIT AT,E13.5)
      ENDIF
      ifaie = 1
      iconf2 = 1
      x1 = 0.9801
   ENDIF
   x4 = (Sppg(j)+Sppg(jold))/2.0
   x5 = (Cppg(j)+Cppg(jold))/2.0
   x9 = (R(j,I)+R(jold,I))*0.5
   x10 = sin((Phi(j)+Phi(jold))*0.5)
   x11 = (1.0-x5*x5*x1)*(Cr(j)+Cr(jold))*0.5/x5 - x4/x5*dphidl(jj) - x4*(x10/x9*(1.0+x1*(x2+xn*x9/vav)**2)+(dladm(j)+dladm(jold))   &
       & /(Lami(j)+Lami(jold)))
   dv2dl = fx2(jj) - x7*dsdl(jj) - 2.0*xn*vav*x2*cos((Gama(j)+Gama(jold))*0.5) + vav*vav*(x11/(1.0-x1)-fx1(jj))
   x12 = 1.0/(1.0+x2*x2)
   dvmdvm(jj) = x12*((x7*dsdl(jj)-fx2(jj))/vav**2-fx1(jj)+x11/(1.0-x1))
   IF ( I/=1 .AND. I/=Nstns .AND. Ipass/=1 ) THEN
      IF ( Neqn==1 ) THEN
         x5 = 0.5*(dsdm(j)+dsdm(jold))*x7*x4*(1.0-x1*(x6-1.0))/(1.0-x1)
         dv2dl = dv2dl + x5
         dvmdvm(jj) = dvmdvm(jj) - x5*x12/vav**2
      ELSE
         x8 = (Taneps(j)+Taneps(jold))*0.5
         x5 = 0.5*(dsdm(j)+dsdm(jold))*x7*(x4*(x12+x6*x1/(1.0-x1))-x8*x2*x12)
         dv2dl = dv2dl + x5 - vav*(drvwdm(j)+drvwdm(jold))*0.5*(x8-x4*x1*x2/(1.0-x1))
         dvmdvm(jj) = dvmdvm(jj) - x5*x12/vav**2
      ENDIF
   ENDIF
   dv2dl = dv2dl*2.0*x12
   x1 = Vv(jold)**2 + dv2dl*(Xl(j,I)-Xl(jold,I))
   IF ( x1>9.0*vvold(Imid)**2 ) THEN
      iconf2 = 1
      ifaie = 1
      IF ( Ipass>Nforce ) THEN
         CALL alg03(Lnct,1)
         x1 = sqrt(x1)
         x2 = 3.0*vvold(Imid)
         WRITE (Log2,99008) Ipass , I , Iter , j , loop , x1 , x2
99008    FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,6H  LOOP,I3,33H  MERIDIONAL VELOCITY ABOVE LIMIT, &
               & E13.5,9H  LIMIT =,E13.5)
      ENDIF
      x1 = 9.0*vvold(Imid)**2
   ENDIF
   IF ( x1<1.0 ) THEN
      IF ( Ipass>Nforce ) THEN
         IF ( Lnct>=Npage ) THEN
            WRITE (Log2,99015)
            Lnct = 1
         ENDIF
         Lnct = Lnct + 1
         WRITE (Log2,99009) Ipass , I , Iter , j , loop , x1
99009    FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,6H  LOOP,I3,                                      &
                &46H  (MERIDIONAL VELOCITY) SQUARED BELOW LIMIT AT,E13.5)
      ENDIF
      x1 = 1.0
      ifaie = 1
      iconf2 = 1
   ENDIF
   Vv(j) = sqrt(x1)
   IF ( Vv(j)>vmmax(j) ) THEN
      ifaie = 1
      iconf2 = 1
      IF ( Ipass>Nforce ) THEN
         CALL alg03(Lnct,1)
         WRITE (Log2,99016) Ipass , I , Iter , j , Vv(j) , vmmax(j)
      ENDIF
      Vv(j) = vmmax(j)
   ENDIF
   IF ( abs(Vv(j)/vold-1.0)>Tolnce*0.2 ) THEN
      IF ( loop>=lpmax ) THEN
         iconf2 = 1
         IF ( Ipass>Nforce ) THEN
            IF ( Lnct>=Npage ) THEN
               WRITE (Log2,99015)
               Lnct = 1
            ENDIF
            Lnct = Lnct + 1
            WRITE (Log2,99010) Ipass , I , Iter , j , Vv(j) , vold
99010       FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,38H  MERIDIONAL VELOCITY UNCONVERGED  VM=,     &
                  & E13.6,9H VM(OLD)=,E13.6)
         ENDIF
      ELSE
         loop = loop + 1
         GOTO 500
      ENDIF
   ENDIF
   IF ( ifaie==1 ) Ifail = 1
   IF ( iconf2==1 ) iconf1 = 1
   IF ( j==Nstrms ) THEN
      j = Imid
      jinc = -1
      GOTO 400
   ELSE
      IF ( j/=1 ) GOTO 400
      IF ( I/=1 ) THEN
         IF ( Nloss(I)==2 .OR. (Nloss(I)==1 .AND. Nl2(I)==0) ) CALL alg07
      ENDIF
      DO j = 1 , Itub
         x1 = ((Vv(j)+Vv(j+1))/2.0)**2*(1.0+((Tbeta(j,I)+Tbeta(j+1,I))/2.0)**2)
         hs(j) = (Xi(j)+Xi(j+1))/2.0 + ((xn*(R(j,I)+R(j+1,I))/2.0)**2-x1)/(2.0*G*Ej)
         IF ( hs(j)<Hmin ) THEN
            IF ( Ipass>Nforce ) THEN
               IF ( Lnct>=Npage ) THEN
                  WRITE (Log2,99015)
                  Lnct = 1
               ENDIF
               Lnct = Lnct + 1
               WRITE (Log2,99017) Ipass , I , Iter , j , hs(j)
            ENDIF
            Ifail = 1
            hs(j) = Hmin
         ENDIF
         xm2(j) = alg9(hs(j),(S(j,I)+S(j+1,I))/2.0,x1)
         IF ( I/=1 .AND. Nloss(I)==1 .AND. Nl2(I)==0 ) THEN
            x1 = (S(j,I)+S(j+1,I))/2.0
            x2 = alg4(hs(j),x1)
            x4 = alg8(hs(j),x1)
            x3 = (Xi(j)+Xi(j))/2.0 + (xn*((R(j,I)+R(j+1,I))/2.0))**2/(2.0*G*Ej)
            x3 = alg4(x3,x1)
            xm2(j) = xm2(j)*(1.0+x4*(Loss(j)+Loss(j+1))/2.0*x2/(x3*(1.0+(Loss(j)+Loss(j+1))/2.0*(1.0-x2/x3))))
         ENDIF
      ENDDO
   ENDIF
 600  Delw(1) = 0.0
   dwdv = 0.0
   x2 = Bblock(I)*Bdist(I)
   x3 = Bblock(I)*(1.0-Bdist(I))*2.0/Xl(Nstrms,I)
   DO j = 1 , Itub
      x1 = dl(j)*(R(j+1,I)+R(j,I))*alg5(hs(j),(S(j,I)+S(j+1,I))/2.0)*(Vv(j)+Vv(j+1))*(Cppg(j)+Cppg(j+1))*Pi/(4.0*Sclfac**2)
      x1 = x1*((Lami(j)+Lami(j+1))/2.0-Wwbl(I)-x2-x3*(Xl(j,I)+Xl(j+1,I)))
      Delw(j+1) = Delw(j) + x1
      x4 = 0.0
      IF ( j>=Imid ) THEN
         l1 = Imid + 1
         DO
            x4 = x4 + dvmdvm(l1)
            IF ( l1>=j ) THEN
               x4 = x4/float(j-Imid+1)
               EXIT
            ELSE
               l1 = l1 + 1
            ENDIF
         ENDDO
      ELSE
         l1 = j
         DO
            x4 = x4 + dvmdvm(l1)
            IF ( l1>=Imid-1 ) THEN
               x4 = x4/float(Imid-j)
               EXIT
            ELSE
               l1 = l1 + 1
            ENDIF
         ENDDO
      ENDIF
      dwdv = dwdv + x1*(1.0-xm2(j))*2.0/((Vv(j)+Vv(j+1))*(1.0-((Xl(j,I)+Xl(j+1,I))*0.5-Xl(Imid,I))*x4))
   ENDDO
   w = Delw(Nstrms)
   Fm2 = dwdv/w*Vv(Imid)
   DO j = 2 , Nstrms
      Delw(j) = Delw(j)/w
   ENDDO
   IF ( dwdv<=0.0 ) THEN
      IF ( Nmach(I)==0 ) THEN
         IF ( Vv(Imid)<vmin .AND. iconf1==0 ) vmin = Vv(Imid)
         dv = -.1*Vv(Imid)
         GOTO 800
      ELSE
         IF ( w<Flow(Icase) .AND. iconf1==0 ) vmin = Vv(Imid)
      ENDIF
   ELSEIF ( Nmach(I)==1 ) THEN
      IF ( Vv(Imid)>vmax .AND. iconf1==0 ) vmax = Vv(Imid)
      dv = 0.1*Vv(Imid)
      GOTO 800
   ELSE
      IF ( w<Flow(Icase) .AND. iconf1==0 ) vmax = Vv(Imid)
   ENDIF
   dv = (Flow(Icase)-w)/dwdv
   IF ( dv<-0.1*Vv(Imid) ) dv = -0.1*Vv(Imid)
   IF ( dv>0.1*Vv(Imid) ) dv = 0.1*Vv(Imid)
 700  IF ( .NOT.(Ipass==1 .OR. (I/=1 .AND. Nwork(I)<=4)) ) THEN
      IF ( Vv(Imid)+dv>=vmin ) dv = (vmin-Vv(Imid))*0.5
      IF ( Vv(Imid)+dv<=vmax ) dv = (vmax-Vv(Imid))*0.5
   ENDIF
   DO j = k , Nstrms
      Vv(j) = Vv(j) + dv
      IF ( Vv(j)>vmmax(j) ) THEN
         Ifail = 1
         Vv(j) = vmmax(j)
      ENDIF
      IF ( Vv(j)<1.0 ) THEN
         IF ( Ipass>Nforce ) THEN
            IF ( Lnct>=Npage ) THEN
               WRITE (Log2,99015)
               Lnct = 1
            ENDIF
            Lnct = Lnct + 1
            WRITE (Log2,99011) Ipass , I , Iter , j , Vv(j)
99011       FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,                                               &
                   &50H  MERIDIONAL VELOCITY BELOW LIMIT IN CONTINUITY AT,E13.5)
         ENDIF
         Vv(j) = 1.0
         Ifail = 1
      ENDIF
   ENDDO
   x1 = Tolnce/5.0
   IF ( Neval(I)>0 ) x1 = x1/2.0
   IF ( abs(w/Flow(Icase)-1.0)>x1 ) GOTO 900
   DO j = k , Nstrms
      IF ( abs(Vv(j)/vvold(j)-1.0)>x1 ) GOTO 900
   ENDDO
   GOTO 1000
 800  Ifail = 1
   IF ( Ipass>Nforce ) THEN
      IF ( Lnct>=Npage ) THEN
         WRITE (Log2,99015)
         Lnct = 1
      ENDIF
      Lnct = Lnct + 1
      WRITE (Log2,99012) Ipass , I , Iter
99012 FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,43H  OTHER CONTINUITY EQUATION BRANCH REQUIRED)
   ENDIF
   GOTO 700
 900  IF ( Iter<itmax ) THEN
      IF ( I/=1 ) THEN
         IF ( (Nloss(I)==1 .AND. Nl2(I)==0) .OR. (Nwork(I)>=5 .AND. Nloss(I)==2) ) CALL alg07
      ENDIF
      GOTO 200
   ELSEIF ( Ipass>Nforce ) THEN
      IF ( Lnct>=Npage ) THEN
         WRITE (Log2,99015)
         Lnct = 1
      ENDIF
      Lnct = Lnct + 1
      x1 = w/Flow(Icase)
      x2 = Vv(k)/vvold(k)
      x3 = Vv(Imid)/vvold(Imid)
      x4 = Vv(Nstrms)/vvold(Nstrms)
      WRITE (Log2,99013) Ipass , I , x1 , x2 , x3 , x4
99013 FORMAT (5X,4HPASS,I3,9H  STATION,I3,49H  MOMENTUM AND/OR CONTINUITY UNCONVERGED W/WSPEC=,F8.5,16H VM/VM(OLD) HUB=,F8.5,       &
            & 5H MID=,F8.5,5H TIP=,F8.5)
   ENDIF
 1000 IF ( Ifail/=0 .AND. Ifailo==0 ) Ifailo = I
   DO j = 1 , Nstrms
      Vm(j,I) = Vv(j)
   ENDDO
   IF ( I/=1 ) THEN
      IF ( Nmix==1 ) THEN
         DO j = 1 , Nstrms
            S(j,I-1) = Skeep(j)
            H(j,I-1) = Hkeep(j)
            Vw(j,I-1) = Vwkeep(j)
         ENDDO
      ENDIF
      IF ( Nwork(I)>=5 ) THEN
         DO j = 1 , Nstrms
            Vw(j,I) = Vv(j)*Tbeta(j,I) + xn*R(j,I)
            H(j,I) = Xi(j) + xn*R(j,I)*Vw(j,I)/(G*Ej)
         ENDDO
      ELSE
         Tbeta(1,I) = 0.0
         DO j = k , Nstrms
            Tbeta(j,I) = (Vw(j,I)-xn*R(j,I))/Vv(j)
         ENDDO
      ENDIF
   ELSE
      DO j = 1 , Nstrms
         Vw(j,1) = Vv(j)*Tbeta(j,1)
      ENDDO
   ENDIF
99014 FORMAT (5X,4HPASS,I3,9H  STATION,I3,12H  STREAMLINE,I3,40H  LIMITING MERIDIONAL VELOCITY SQUARED =,E12.5)
99015 FORMAT (1H1)
99016 FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,44H  MERIDIONAL VELOCITY ABOVE SOUND SPEED  VM=,F8.2,&
             &3H A=,F8.2)
99017 FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,                                                     &
             &55H  STATIC ENTHALPY BELOW LIMIT IN CONTINUITY EQUATION AT,E13.5)
END SUBROUTINE alg08
