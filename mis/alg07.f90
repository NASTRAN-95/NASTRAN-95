
SUBROUTINE alg07
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
   REAL alg2 , alg3 , alg4
   INTEGER il , iw , j , l1 , l2
   REAL x1 , x2 , x3 , x4 , xn
!
! End of declarations
!
!
!
!
   l1 = I + Nl1(I)
   l2 = I + Nl2(I)
   iw = Nwork(I)
   il = Nloss(I)
   xn = Speed(I)*Spdfac(Icase)*Pi/(30.0*Sclfac)
   IF ( iw==2 ) THEN
      DO j = 1 , Nstrms
         H(j,I) = Work(j)
         Vw(j,I) = (xn*Rim1(j)*Vw(j,I-1)+(H(j,I)-H(j,I-1))*G*Ej)/(xn*R(j,I))
      ENDDO
      GOTO 200
   ELSEIF ( iw==3 ) THEN
      DO j = 1 , Nstrms
         Vw(j,I) = Work(j)/R(j,I)
      ENDDO
      GOTO 100
   ELSEIF ( iw==4 ) THEN
      DO j = 1 , Nstrms
         Vw(j,I) = Work(j)
      ENDDO
      GOTO 100
   ELSEIF ( iw==5 .OR. iw==6 .OR. iw==7 ) THEN
      DO j = 1 , Nstrms
         Xi(j) = H(j,I-1) - xn*Rim1(j)*Vw(j,I-1)/(G*Ej)
      ENDDO
      IF ( il==2 ) THEN
         IF ( Ipass==1 .AND. Iter==0 ) THEN
            DO j = 1 , Nstrms
               S(j,I) = S(j,l1)
            ENDDO
         ELSE
            DO j = 1 , Nstrms
               IF ( Iter==0 ) Vv(j) = Vm(j,I)
               x1 = H(j,I-1) + xn*(Vv(j)*(Tbeta(j,I)+xn*R(j,I)/Vv(j))*R(j,I)-Rim1(j)*Vw(j,I-1))/(G*Ej)
               IF ( x1<Hmin ) x1 = Hmin
               x2 = alg4(H(j,l1)+(x1-H(j,l1))*Loss(j),S(j,l1))
               S(j,I) = alg3(x2,x1)
            ENDDO
         ENDIF
      ELSEIF ( il==3 ) THEN
         DO j = 1 , Nstrms
            S(j,I) = S(j,l1) + Loss(j)
         ENDDO
      ELSEIF ( l2/=I ) THEN
         DO j = 1 , Nstrms
            x4 = Xi(j) + (xn*R(j,I))**2/(2.0*G*Ej)
            IF ( x4<Hmin ) x4 = Hmin
            x1 = alg4(x4,S(j,l1))
            IF ( Ipass/=1 .OR. l2<=I ) THEN
               x2 = Xi(j) + (xn*R(j,l2))**2/(2.0*G*Ej)
               x3 = H(j,l2) - (Vm(j,l2)**2+Vw(j,l2)**2)/(2.0*G*Ej)
               IF ( x2<Hmin ) x2 = Hmin
               IF ( x3<Hmin ) x3 = Hmin
               x1 = x1 - Loss(j)*(alg4(x2,S(j,l2))-alg4(x3,S(j,l2)))
            ENDIF
            S(j,I) = alg3(x1,x4)
         ENDDO
      ELSE
         DO j = 1 , Nstrms
            x2 = Xi(j) + (xn*R(j,I))**2/(2.0*G*Ej)
            IF ( Ipass==1 .AND. Iter==0 ) THEN
               x3 = 1.0
            ELSE
               IF ( Iter==0 ) Vv(j) = Vm(j,I)
               x1 = x2 - Vv(j)**2*(1.0+Tbeta(j,I)**2)/(2.0*G*Ej)
               IF ( x1<Hmin ) x1 = Hmin
               IF ( x2<Hmin ) x2 = Hmin
               x3 = 1.0/(1.0+Loss(j)*(1.0-alg4(x1,S(j,I))/alg4(x2,S(j,I))))
            ENDIF
            S(j,I) = alg3(x3*alg4(x2,S(j,l1)),x2)
         ENDDO
      ENDIF
   ELSE
      IF ( il==2 ) THEN
         DO j = 1 , Nstrms
            H(j,I) = H(j,l1) + (alg2(S(j,l1),Work(j))-H(j,l1))/Loss(j)
            S(j,I) = alg3(Work(j),H(j,I))
         ENDDO
      ELSEIF ( il==3 ) THEN
         DO j = 1 , Nstrms
            S(j,I) = S(j,l1) + Loss(j)
            H(j,I) = alg2(S(j,I),Work(j))
         ENDDO
      ELSEIF ( l2/=I ) THEN
         DO j = 1 , Nstrms
            IF ( Ipass==1 .AND. l2>I ) THEN
               x4 = 1.0
            ELSE
               x1 = H(j,l1) - (Vw(j,l1)**2-(Vw(j,l1)-xn*R(j,l1))**2)/(2.0*G*Ej) + xn**2*(R(j,I)**2-R(j,l1)**2)/(2.0*G*Ej)
               IF ( x1<Hmin ) x1 = Hmin
               x2 = H(j,l2) - (Vm(j,l2)**2+Vw(j,l2)**2)/(2.0*G*Ej)
               x3 = H(j,l2) - (Vw(j,l2)**2-(Vw(j,l2)-xn*R(j,l2))**2)/(2.0*G*Ej)
               IF ( x2<Hmin ) x2 = Hmin
               IF ( x3<Hmin ) x3 = Hmin
               x4 = 1.0 - Loss(j)/alg4(x1,S(j,l1))*(alg4(x3,S(j,l2))-alg4(x2,S(j,l2)))
            ENDIF
            H(j,I) = alg2(S(j,l1),Work(j)/x4)
            S(j,I) = alg3(Work(j),H(j,I))
         ENDDO
      ELSE
         DO j = 1 , Nstrms
            IF ( Ipass==1 .AND. Iter==0 ) THEN
               x3 = 1.0
            ELSE
               IF ( Iter==0 ) Vv(j) = Vm(j,I)
               x1 = H(j,I) - (Vv(j)**2+Vw(j,I)**2)/(2.0*G*Ej)
               x2 = H(j,I) - (Vw(j,I)**2-(Vw(j,I)-xn*R(j,I))**2)/(2.0*G*Ej)
               IF ( x1<Hmin ) x1 = Hmin
               IF ( x2<Hmin ) x2 = Hmin
               x3 = 1.0/(1.0+Loss(j)*(1.0-alg4(x1,S(j,I))/alg4(x2,S(j,I))))
            ENDIF
            H(j,I) = alg2(S(j,l1),Work(j)/x3)
            S(j,I) = alg3(Work(j),H(j,I))
         ENDDO
      ENDIF
      DO j = 1 , Nstrms
         Vw(j,I) = (xn*Rim1(j)*Vw(j,I-1)+(H(j,I)-H(j,I-1))*G*Ej)/(xn*R(j,I))
      ENDDO
   ENDIF
   GOTO 99999
 100  DO j = 1 , Nstrms
      H(j,I) = H(j,I-1) + xn*(R(j,I)*Vw(j,I)-Rim1(j)*Vw(j,I-1))/(G*Ej)
   ENDDO
 200  IF ( il==2 ) THEN
      DO j = 1 , Nstrms
         S(j,I) = alg3(alg4(H(j,l1)+Loss(j)*(H(j,I)-H(j,l1)),S(j,l1)),H(j,I))
      ENDDO
   ELSEIF ( il==3 ) THEN
      DO j = 1 , Nstrms
         S(j,I) = S(j,l1) + Loss(j)
      ENDDO
   ELSEIF ( l2/=I ) THEN
      DO j = 1 , Nstrms
         IF ( Ipass==1 .AND. l2>I ) THEN
            x4 = 1.0
         ELSE
            x1 = H(j,l1) - (Vw(j,l1)**2-(Vw(j,l1)-xn*R(j,l1))**2)/(2.0*G*Ej) + xn**2*(R(j,I)**2-R(j,l1)**2)/(2.0*G*Ej)
            IF ( x1<Hmin ) x1 = Hmin
            x2 = H(j,l2) - (Vm(j,l2)**2+Vw(j,l2)**2)/(2.0*G*Ej)
            x3 = H(j,l2) - (Vw(j,l2)**2-(Vw(j,l2)-xn*R(j,l2))**2)/(2.0*G*Ej)
            IF ( x2<Hmin ) x2 = Hmin
            IF ( x3<Hmin ) x3 = Hmin
            x4 = 1.0 - Loss(j)/alg4(x1,S(j,l1))*(alg4(x3,S(j,l2))-alg4(x2,S(j,l2)))
         ENDIF
         S(j,I) = alg3(x4*alg4(H(j,I),S(j,l1)),H(j,I))
      ENDDO
   ELSE
      DO j = 1 , Nstrms
         IF ( Ipass==1 .AND. Iter==0 ) THEN
            x3 = 1.0
         ELSE
            IF ( Iter==0 ) Vv(j) = Vm(j,I)
            x1 = H(j,I) - (Vv(j)**2+Vw(j,I)**2)/(2.0*G*Ej)
            x2 = H(j,I) - (Vw(j,I)**2-(Vw(j,I)-xn*R(j,I))**2)/(2.0*G*Ej)
            IF ( x1<Hmin ) x1 = Hmin
            IF ( x2<Hmin ) x2 = Hmin
            x3 = 1.0/(1.0+Loss(j)*(1.0-alg4(x1,S(j,I))/alg4(x2,S(j,I))))
         ENDIF
         S(j,I) = alg3(x3*alg4(H(j,I),S(j,l1)),H(j,I))
      ENDDO
   ENDIF
99999 RETURN
END SUBROUTINE alg07
