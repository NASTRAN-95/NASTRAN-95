
SUBROUTINE alg10
   IMPLICIT NONE
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
   REAL c1h , c1t , v5 , vinh , vint , x1
!
!
!
   IF ( I>1 ) THEN
      vint = vint + sqrt((X(Nstrms,I)-X(Nstrms,I-1))**2+(R(Nstrms,I)-R(Nstrms,I-1))**2)*((Vm(Nstrms,I)+Vm(Nstrms,I-1))/2.0)         &
           & **4/Sclfac
      Delt(I) = v5*(c1t+0.016*vint)**0.8/Vm(Nstrms,I)**3.4*Sclfac*Shape
      Delh(I) = 0.0
      IF ( I>Istag ) THEN
         vinh = vinh + sqrt((X(1,I)-X(1,I-1))**2+(R(1,I)-R(1,I-1))**2)*((Vm(1,I)+Vm(1,I-1))/2.0)**4/Sclfac
         Delh(I) = v5*(c1h+0.016*vinh)**0.8/Vm(1,I)**3.4*Sclfac*Shape
      ENDIF
      Wwbl(I) = 0.5*Wwbl(I) + 0.5*(((2.0*R(Nstrms,I)-Delt(I)*cos(Phi(Nstrms)))*Delt(I)/Cppg(Nstrms)+(2.0*R(1,I)+Delh(I)*cos(Phi(1)))&
              & *Delh(I)/Cppg(1))/((R(Nstrms,I)+R(1,I))*Xl(Nstrms,I)))
      IF ( Wwbl(I)>0.3 ) Wwbl(I) = 0.3
      IF ( Wwbl(I)<0.0 ) Wwbl(I) = 0.3
   ELSE
      v5 = Visk**0.2
      vinh = 0.0
      vint = 0.0
      IF ( Wwbl(1)<=0.0 ) THEN
         c1h = 0.0
         c1t = 0.0
         Delh(1) = 0.0
         Delt(1) = 0.0
      ELSEIF ( Istag>0 ) THEN
         Delh(1) = 0.0
         c1h = 0.0
         IF ( abs(Phi(Nstrms))>Pi/2.0-0.00015 .AND. abs(Phi(Nstrms))<Pi/2.0+0.00015 ) THEN
            Delt(1) = Wwbl(1)*Xl(Nstrms,1)/Cppg(Nstrms)
            c1t = (Delt(1)*Vm(Nstrms,1)**3.4/(v5*Sclfac*Shape))**1.25
         ELSE
            x1 = (R(Nstrms,1)-sqrt(R(Nstrms,1)**2-cos(Phi(Nstrms))*Cppg(Nstrms)*Wwbl(1)*(R(Nstrms,1)+R(1,1))*Xl(Nstrms,I)))         &
               & /cos(Phi(Nstrms))
            Delt(1) = x1
            c1t = (x1/(Shape*Sclfac*v5)*Vm(Nstrms,1)**3.4)**1.25
         ENDIF
      ELSE
         x1 = Wwbl(1)*Xl(Nstrms,1)*(Cppg(1)+Cppg(Nstrms))/4.0
         Delh(1) = x1
         Delt(1) = x1
         x1 = x1/(Sclfac*Shape)
         c1h = (x1*Vm(1,1)**3.4/v5)**1.25
         c1t = (x1*Vm(Nstrms,1)**3.4/v5)**1.25
      ENDIF
   ENDIF
END SUBROUTINE alg10