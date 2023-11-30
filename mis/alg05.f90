
SUBROUTINE alg05
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
   INTEGER ii , iii , ik , il , iw , j , k , l1 , l2 , l3 , l4
   REAL x1 , x2 , xn , xx1(21) , xx2(21) , xx3(21) , xx4(21) , xx5(21)
!
!
!
!
   l1 = Ndimen(I) + 1
   IF ( l1==2 ) THEN
      DO j = 1 , Nstrms
         xx5(j) = R(j,I)/R(Nstrms,I)
      ENDDO
   ELSEIF ( l1==3 ) THEN
      DO j = 1 , Nstrms
         xx5(j) = Xl(j,I)
      ENDDO
   ELSEIF ( l1==4 ) THEN
      DO j = 1 , Nstrms
         xx5(j) = Xl(j,I)/Xl(Nstrms,I)
      ENDDO
   ELSE
      DO j = 1 , Nstrms
         xx5(j) = R(j,I)
      ENDDO
   ENDIF
   l2 = Is2(I)
   l3 = Ndata(I)
   l4 = Nterp(I)
   CALL alg01(Datac(l2),Data1(l2),l3,xx5,Work,x1,Nstrms,l4,0)
   CALL alg01(Datac(l2),Data3(l2),l3,xx5,Taneps,x1,Nstrms,l4,0)
   DO j = 1 , Nstrms
      Taneps(j) = tan(Taneps(j)/C1)
   ENDDO
   iw = Nwork(I)
   il = Nloss(I)
   IF ( iw==7 .OR. il<=3 ) CALL alg01(Datac(l2),Data2(l2),l3,xx5,Loss,x1,Nstrms,l4,0)
   IF ( iw>=5 ) CALL alg01(Datac(l2),Data6(l2),l3,xx5,xx1,x1,Nstrms,l4,0)
   IF ( il==4 ) THEN
      DO ii = I , Nstns
         IF ( Nloss(ii)==1 ) EXIT
      ENDDO
      l2 = Is2(ii)
      l3 = Ndata(ii)
      l4 = Nterp(ii)
      l1 = Ndimen(ii) + 1
      IF ( l1==2 ) THEN
         DO j = 1 , Nstrms
            xx5(j) = R(j,ii)/R(Nstrms,ii)
         ENDDO
      ELSEIF ( l1==3 ) THEN
         DO j = 1 , Nstrms
            xx5(j) = Xl(j,ii)
         ENDDO
      ELSEIF ( l1==4 ) THEN
         DO j = 1 , Nstrms
            xx5(j) = Xl(j,ii)/Xl(Nstrms,ii)
         ENDDO
      ELSE
         DO j = 1 , Nstrms
            xx5(j) = R(j,ii)
         ENDDO
      ENDIF
      CALL alg01(Datac(l2),Data2(l2),l3,xx5,Loss,x1,Nstrms,l4,0)
      iii = I + Nl1(I) + 1
      DO j = 1 , Nstrms
         xx2(j) = 0.0
         DO ik = iii , ii
            xx2(j) = xx2(j) + sqrt((X(j,ik)-X(j,ik-1))**2+(R(j,ik)-R(j,ik-1))**2)
            IF ( ik==I ) xx3(j) = xx2(j)
         ENDDO
         xx3(j) = xx3(j)/xx2(j)
      ENDDO
      l1 = Ncurve(I)
      l2 = Nm(l1)
      l3 = Nrad(l1)
      DO j = 1 , Nstrms
         DO k = 1 , l3
            CALL alg01(Dm(1,k,l1),Wfrac(1,k,l1),l2,xx3(j),xx2(k),x1,1,0,0)
         ENDDO
         x2 = (R(j,ii)-R(1,ii))/(R(Nstrms,ii)-R(1,ii))
         CALL alg01(Terad(1,l1),xx2,l3,x2,x1,x1,1,0,0)
         Loss(j) = Loss(j)*x1
      ENDDO
   ENDIF
   IF ( iw>=5 ) THEN
      IF ( iw==5 ) THEN
         DO j = 1 , Nstrms
            Tbeta(j,I) = tan((Work(j)+xx1(j))/C1)
         ENDDO
      ELSEIF ( iw==7 ) THEN
         xn = Speed(I)*Spdfac(Icase)*Pi/(30.0*Sclfac)
         CALL alg01(Datac(l2),Data7(l2),l3,xx5,xx2,x1,Nstrms,l4,0)
         CALL alg01(Datac(l2),Data8(l2),l3,xx5,xx3,x1,Nstrms,l4,0)
         CALL alg01(Datac(l2),Data9(l2),l3,xx5,xx4,x1,Nstrms,l4,0)
         ii = I + Nl1(I)
         DO j = 1 , Nstrms
            x1 = C1*atan((Vw(j,ii)-xn*R(j,ii))/Vm(j,ii))
            x2 = xx3(j)
            IF ( x1<xx1(j) ) x2 = xx4(j)
            Loss(j) = Loss(j)*(1.0+((x1-xx1(j))/(x2-xx1(j)))**2)
            IF ( Loss(j)>0.5 ) Loss(j) = 0.5
            Tbeta(j,I) = tan((Work(j)+(x1-xx1(j))*xx2(j))/C1)
         ENDDO
      ELSE
         DO j = 1 , Nstrms
            xx2(j) = tan((atan((R(j,I+1)-R(j,I))/(X(j,I+1)-X(j,I)))+atan((R(j,I)-R(j,I-1))/(X(j,I)-X(j,I-1))))/2.0)
         ENDDO
         l1 = Is1(I)
         CALL alg01(Rstn(l1),Xstn(l1),Nspec(I),R(1,I),x1,xx3,Nstrms,0,1)
         DO j = 1 , Nstrms
            Tbeta(j,I) = tan(atan((tan(Work(j)/C1)*(1.0-xx3(j)*xx2(j))-xx2(j)*Taneps(j)*sqrt(1.0+xx3(j)**2))/sqrt(1.0+xx2(j)**2))   &
                       & +xx1(j)/C1)
         ENDDO
      ENDIF
   ENDIF
END SUBROUTINE alg05