
SUBROUTINE alg12
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
   REAL alg4
   REAL delx , hs , pstat(32) , xmax , xmin , xx(32)
   INTEGER j , k , l1
!
! End of declarations
!
!
!
!
!
   xmax = X(1,Nstns)
   xmin = X(1,1)
   DO j = 2 , Nstrms
      IF ( X(j,1)<xmin ) xmin = X(j,1)
      IF ( X(j,Nstns)>xmax ) xmax = X(j,Nstns)
   ENDDO
   IF ( xmin<0.0 ) xmin = xmin - 1.0
   l1 = xmin - 1.0
   xmin = float(l1)
   l1 = xmax + 1.0
   xmax = float(l1)
   delx = (xmax-xmin)/Xscale + 0.01
   xx(Nstns+1) = xmin
   xx(Nstns+2) = Xscale
   IF ( Nplot/=2 ) THEN
      pstat(Nstns+1) = Plow
      pstat(Nstns+2) = Pscale
      j = 1
      k = 1
      DO
         DO I = 1 , Nstns
            hs = H(j,I) - (Vw(j,I)**2+Vm(j,I)**2)/(2.0*G*Ej)
            IF ( hs<Hmin ) hs = Hmin
            pstat(I) = alg4(hs,S(j,I))/Sclfac**2
            xx(I) = X(j,I)
         ENDDO
         IF ( j==Nstrms ) THEN
            IF ( Nplot/=1 ) EXIT
            GOTO 99999
         ELSE
            k = k + 1
            IF ( j==Imid ) j = Nstrms
            IF ( j==1 ) j = Imid
         ENDIF
      ENDDO
   ENDIF
   pstat(Nstns+1) = Rlow
   pstat(Nstns+2) = Xscale
   DO j = 1 , Nstrms
      DO I = 1 , Nstns
         xx(I) = X(j,I)
         pstat(I) = R(j,I)
      ENDDO
   ENDDO
   pstat(Nstrms+1) = Rlow
   pstat(Nstrms+2) = Xscale
   xx(Nstrms+1) = xmin
   xx(Nstrms+2) = Xscale
   DO I = 1 , Nstns
      DO j = 1 , Nstrms
         pstat(j) = R(j,I)
         xx(j) = X(j,I)
      ENDDO
   ENDDO
99999 END SUBROUTINE alg12
