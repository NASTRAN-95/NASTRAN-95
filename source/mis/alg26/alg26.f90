!*==alg26.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg26
   IMPLICIT NONE
   USE C_UD300C
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(20) :: afun , bfun , dvmdvm , hs , xm2
   REAL , DIMENSION(21) :: dl , drvwdm , dsdl , dsdm , dvmdm , fx1 , fx2 , tbip1 , teip1 , vvold , xx1
   REAL :: dv , dv2dl , dwdv , vav , vmax , vmin , vold , w , x1 , x11 , x12 , x2 , x3 , x4 , x5 , x6 , x7 , x8 , xn
   INTEGER :: iconf1 , iconf2 , ifaie , itmax , j , jinc , jj , jold , k , l1 , l2 , loop , lpmax
   EXTERNAL alg01 , alg03 , alg07 , alg4 , alg5 , alg7 , alg8 , alg9
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
                  dvmdm(j) = 0.0
                  IF ( Ipass/=1 ) THEN
                     dsdm(j) = ((S(j,I+1)-S(j,I))/x1+(S(j,I)-S(j,I-1))/x2)/2.0*G*Ej
                     drvwdm(j) = ((R(j,I+1)*Vw(j,I+1)-R(j,I)*Vw(j,I))/x1+(R(j,I)*Vw(j,I)-Rim1(j)*Vw(j,I-1))/x2)/(2.0*R(j,I))
                     dvmdm(j) = ((Vm(j,I+1)-Vm(j,I))/x1+(Vm(j,I)-Vm(j,I-1))/x2)*0.5
                  ENDIF
               ENDDO
               IF ( Ipass/=1 .AND. Ndata(I)/=0 .AND. Neqn/=3 .AND. Nwork(I)==0 .AND. Nwork(I+1)/=0 ) THEN
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
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         DO j = 1 , Nstrms
            dvmdm(j) = 0.0
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
         spag_nextblock_1 = 2
      CASE (2)
         CALL alg01(R(1,I),X(1,I),Nstrms,R(1,I),x1,Gama,Nstrms,0,1)
         DO j = 1 , Nstrms
            Gama(j) = atan(Gama(j))
            Sppg(j) = Gama(j) + Phi(j)
            Cppg(j) = cos(Sppg(j))
            Sppg(j) = sin(Sppg(j))
            Vv(j) = Vm(j,I)
         ENDDO
         DO j = 1 , Itub
            dl(j) = Xl(j+1,I) - Xl(j,I)
            dsdl(j) = (S(j+1,I)-S(j,I))*G*Ej/dl(j)
         ENDDO
         IF ( I==1 .OR. Nwork(I)>=5 ) THEN
            DO j = 1 , Itub
               fx1(j) = (Tbeta(j+1,I)+Tbeta(j,I))/(R(j+1,I)+R(j,I))*(R(j+1,I)*Tbeta(j+1,I)-R(j,I)*Tbeta(j,I))/dl(j)
               fx2(j) = (Xi(j+1)-Xi(j))/dl(j)*G*Ej
            ENDDO
         ELSE
            DO j = 1 , Itub
               dvmdvm(j) = 0.0
               fx1(j) = (Vw(j+1,I)+Vw(j,I))/(R(j+1,I)+R(j,I))*(R(j+1,I)*Vw(j+1,I)-R(j,I)*Vw(j,I))/dl(j)
               fx2(j) = (H(j+1,I)-H(j,I))/dl(j)*G*Ej
            ENDDO
         ENDIF
         vmax = 0.0
         vmin = 2500.0
         Iter = 0
         spag_nextblock_1 = 3
      CASE (3)
         Iter = Iter + 1
         Ifail = 0
         iconf1 = 0
         DO j = 1 , Nstrms
            vvold(j) = Vv(j)
         ENDDO
         IF ( I==1 .OR. Nwork(I)>=5 ) THEN
            j = Imid
            jinc = 1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO j = 1 , Itub
               x1 = (H(j,I)+H(j+1,I))/2.0 - (((vvold(j)+vvold(j+1))/2.0)**2+((Vw(j,I)+Vw(j+1,I))/2.0)**2)/(2.0*G*Ej)
               IF ( x1<Hmin ) THEN
                  IF ( Ipass>Nforce ) THEN
                     IF ( Lnct>=Npage ) THEN
                        WRITE (Log2,99012)
                        Lnct = 1
                     ENDIF
                     Lnct = Lnct + 1
                     WRITE (Log2,99001) Ipass , I , Iter , j , x1
99001                FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,                                      &
                            &53H  STATIC ENTHALPY BELOW LIMIT IN MOMENTUM EQUATION AT,E13.5)
                  ENDIF
                  Ifail = 1
                  x1 = Hmin
               ENDIF
               x2 = (S(j,I)+S(j+1,I))/2.0
               x7 = alg7(x1,x2)
               x2 = (Cppg(j)+Cppg(j+1))*0.5
               x3 = (Sppg(j)+Sppg(j+1))*0.5
               afun(j) = -2.0*x3*(dvmdm(j)+dvmdm(j+1))/(vvold(j)+vvold(j+1)) - x2*(Cr(j)+Cr(j+1))
               bfun(j) = 2.0*(fx2(j)-x7*dsdl(j)-fx1(j))
               IF ( Ipass/=1 .AND. I/=Nstns ) THEN
                  IF ( Ndata(I)==0 .OR. Neqn==3 .OR. (Nwork(I)==0 .AND. Nwork(I+1)==0) ) THEN
                     bfun(j) = bfun(j) + x7*(dsdm(j)+dsdm(j+1))*x3
                  ELSE
                     IF ( Nwork(I)==0 ) THEN
                        x4 = (tbip1(j)+tbip1(j+1))*0.5
                        x5 = (teip1(j)+teip1(j+1))*0.5
                     ELSE
                        x4 = (Tbeta(j,I)+Tbeta(j+1,I))*0.5
                        x5 = (Taneps(j)+Taneps(j+1))*0.5
                     ENDIF
                     bfun(j) = bfun(j) + x7*(dsdm(j)+dsdm(j+1))*(x3/(1.0+x4*x4)-x5*x4/(1.0+x4*x4)*0.5) - x5*(drvwdm(j)+drvwdm(j+1)) &
                             & *(vvold(j)+vvold(j+1))*0.5
                  ENDIF
               ENDIF
               Vv(Imid) = vvold(Imid)**2
            ENDDO
            j = Imid
            jinc = 1
         ENDIF
         DO
            jold = j
            j = j + jinc
            jj = jold
            IF ( jinc==-1 ) jj = j
            IF ( abs(afun(j))<=1.0E-5 ) THEN
               Vv(j) = Vv(jold) + bfun(jj)*(Xl(j,I)-Xl(jold,I))
            ELSE
               x1 = -afun(jj)*(Xl(j,I)-Xl(jold,I))
               IF ( abs(x1)<=1.0E-10 ) THEN
                  Vv(j) = Vv(jold) + bfun(jj)*(Xl(j,I)-Xl(jold,I))
               ELSE
                  IF ( x1>88.0 ) THEN
                     IF ( Ipass>Nforce ) THEN
                        IF ( Lnct>=Npage ) THEN
                           WRITE (Log2,99012)
                           Lnct = 1
                        ENDIF
                        Lnct = Lnct + 1
                        WRITE (Log2,99002) Ipass , I , Iter , jj , x1
99002                   FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,                                   &
                               &43H  MOMENTUM EQUATION EXPONENT ABOVE LIMIT AT,E13.5)
                     ENDIF
                     Ifail = 1
                     x1 = 88.0
                  ENDIF
                  x1 = exp(x1)
                  Vv(j) = Vv(jold)*x1 + (1.0-x1)*bfun(jj)/afun(jj)
               ENDIF
            ENDIF
            IF ( j==k ) THEN
               DO j = k , Nstrms
                  IF ( Vv(j)>4.0*vvold(Imid)**2 ) THEN
                     Ifail = 1
                     IF ( Ipass>Nforce ) THEN
                        CALL alg03(Lnct,1)
                        WRITE (Log2,99003) Ipass , I , Iter , j
99003                   FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,                                   &
                               &50H  MERIDIONAL VELOCITY GREATER THAN TWICE MID VALUE)
                     ENDIF
                     Vv(j) = 4.0*vvold(Imid)**2
                  ENDIF
                  IF ( Vv(j)>=1.0 ) THEN
                     Vv(j) = sqrt(Vv(j))
                  ELSE
                     IF ( Ipass>Nforce ) THEN
                        IF ( Lnct>=Npage ) THEN
                           WRITE (Log2,99012)
                           Lnct = 1
                        ENDIF
                        Lnct = Lnct + 1
                        WRITE (Log2,99004) Ipass , I , Iter , j , Vv(j)
99004                   FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,                                   &
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
                           WRITE (Log2,99012)
                           Lnct = 1
                        ENDIF
                        Lnct = Lnct + 1
                        WRITE (Log2,99013) Ipass , I , Iter , j , hs(j)
                     ENDIF
                     Ifail = 1
                     hs(j) = Hmin
                  ENDIF
                  xm2(j) = alg9(hs(j),(S(j,I)+S(j+1,I))/2.0,((Vv(j)+Vv(j+1))/2.0)**2)
               ENDDO
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( j==Nstrms ) THEN
               j = Imid
               jinc = -1
            ENDIF
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
         loop = 1
         jold = j
         j = j + jinc
         jj = jold
         IF ( jinc==-1 ) jj = j
         SPAG_Loop_1_1: DO
            vold = Vv(j)
            vav = (vold+Vv(jold))/2.0
            ifaie = 0
            iconf2 = 0
            x2 = (Tbeta(j,I)+Tbeta(jold,I))/2.0
            x1 = (Xi(j)+Xi(jold))/2.0 + ((xn*(R(j,I)+R(jold,I))/2.0)**2-vav**2*(1.0+x2*x2))/(2.0*G*Ej)
            IF ( x1<Hmin ) THEN
               IF ( Ipass>Nforce ) THEN
                  IF ( Lnct>=Npage ) THEN
                     WRITE (Log2,99012)
                     Lnct = 1
                  ENDIF
                  Lnct = Lnct + 1
                  WRITE (Log2,99005) Ipass , I , Iter , jj , loop , x1
99005             FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,6H  LOOP,I3,                             &
                         &43H  STATIC H IN MOMENTUM EQUN. BELOW LIMIT AT,E13.5)
               ENDIF
               ifaie = 1
               iconf2 = 1
               x1 = Hmin
            ENDIF
            x3 = (S(j,I)+S(jold,I))/2.0
            x7 = alg7(x1,x3)
            x4 = (Sppg(j)+Sppg(jold))*0.5
            x5 = (Cppg(j)+Cppg(jold))*0.5
            x1 = x5*(Cr(j)+Cr(jold))*0.5 - fx1(jj)
            x12 = 1.0/(1.0+x2*x2)
            x8 = (Taneps(j)+Taneps(jold))*0.5
            x11 = fx2(jj) - x7*dsdl(jj)
            x6 = x4*(dvmdm(j)+dvmdm(jold))*0.5 - 2.0*xn*x2*cos((Gama(j)+Gama(jold))*0.5)
            IF ( Ipass/=1 .AND. I/=1 .AND. I/=Nstns ) THEN
               IF ( Neqn==3 ) THEN
                  x11 = x11 + x7*(dsdm(j)+dsdm(jold))*0.5*x4
               ELSE
                  x11 = x11 + x7*(dsdm(j)+dsdm(jold))*0.5*(x4*x12-x8*x2*x12)
                  x6 = x6 - x8*(drvwdm(j)+drvwdm(jold))*0.5
               ENDIF
            ENDIF
            dv2dl = 2.0*x12*(vav*(x6+vav*x1)+x11)
            dvmdvm(jj) = x12*(x1-x11/vav**2)
            x1 = Vv(jold)**2 + dv2dl*(Xl(j,I)-Xl(jold,I))
            IF ( x1>9.0*vvold(Imid)**2 ) THEN
               iconf2 = 1
               ifaie = 1
               IF ( Ipass>Nforce ) THEN
                  CALL alg03(Lnct,1)
                  x1 = sqrt(x1)
                  x2 = 3.0*vvold(Imid)
                  WRITE (Log2,99006) Ipass , I , Iter , j , loop , x1 , x2
99006             FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,6H  LOOP,I3,                             &
                         &33H  MERIDIONAL VELOCITY ABOVE LIMIT,E13.5,9H  LIMIT =,E13.5)
               ENDIF
               x1 = 9.0*vvold(Imid)**2
            ENDIF
            IF ( x1<1.0 ) THEN
               IF ( Ipass>Nforce ) THEN
                  IF ( Lnct>=Npage ) THEN
                     WRITE (Log2,99012)
                     Lnct = 1
                  ENDIF
                  Lnct = Lnct + 1
                  WRITE (Log2,99007) Ipass , I , Iter , j , loop , x1
99007             FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,6H  LOOP,I3,                             &
                         &46H  (MERIDIONAL VELOCITY) SQUARED BELOW LIMIT AT,E13.5)
               ENDIF
               x1 = 1.0
               ifaie = 1
               iconf2 = 1
            ENDIF
            Vv(j) = sqrt(x1)
            IF ( abs(Vv(j)/vold-1.0)>Tolnce/5.0 ) THEN
               IF ( loop>=lpmax ) THEN
                  iconf2 = 1
                  IF ( Ipass>Nforce ) THEN
                     IF ( Lnct>=Npage ) THEN
                        WRITE (Log2,99012)
                        Lnct = 1
                     ENDIF
                     Lnct = Lnct + 1
                     WRITE (Log2,99008) Ipass , I , Iter , j , Vv(j) , vold
99008                FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,                                      &
                            &38H  MERIDIONAL VELOCITY UNCONVERGED  VM=,E13.6,9H VM(OLD)=,E13.6)
                  ENDIF
               ELSE
                  loop = loop + 1
                  CYCLE
               ENDIF
            ENDIF
            IF ( ifaie==1 ) Ifail = 1
            IF ( iconf2==1 ) iconf1 = 1
            IF ( j==Nstrms ) THEN
               j = Imid
               jinc = -1
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( j/=1 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( I/=1 ) THEN
                  IF ( Nloss(I)==2 .OR. (Nloss(I)==1 .AND. Nl2(I)==0) ) CALL alg07
               ENDIF
               DO j = 1 , Itub
                  x1 = ((Vv(j)+Vv(j+1))/2.0)**2*(1.0+((Tbeta(j,I)+Tbeta(j+1,I))/2.0)**2)
                  hs(j) = (Xi(j)+Xi(j+1))/2.0 + ((xn*(R(j,I)+R(j+1,I))/2.0)**2-x1)/(2.0*G*Ej)
                  IF ( hs(j)<Hmin ) THEN
                     IF ( Ipass>Nforce ) THEN
                        IF ( Lnct>=Npage ) THEN
                           WRITE (Log2,99012)
                           Lnct = 1
                        ENDIF
                        Lnct = Lnct + 1
                        WRITE (Log2,99013) Ipass , I , Iter , j , hs(j)
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
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 5
      CASE (5)
         Delw(1) = 0.0
         dwdv = 0.0
         x2 = Bblock(I)*Bdist(I)
         x3 = Bblock(I)*(1.0-Bdist(I))/Xl(Nstrms,I)
         DO j = 1 , Itub
            x1 = dl(j)*(R(j+1,I)+R(j,I))*alg5(hs(j),(S(j,I)+S(j+1,I))/2.0)*(Vv(j)+Vv(j+1))*(Cppg(j)+Cppg(j+1))*Pi/(4.0*Sclfac**2)
            x1 = x1*((Lami(j)+Lami(j+1))/2.0-Wwbl(I)-x2-x3*(Xl(j,I)+Xl(j+1,I)))
            Delw(j+1) = Delw(j) + x1
            x4 = 0.0
            IF ( j>=Imid ) THEN
               l1 = Imid + 1
               SPAG_Loop_2_2: DO
                  x4 = x4 + dvmdvm(l1)
                  IF ( l1>=j ) THEN
                     x4 = x4/float(j-Imid+1)
                     EXIT SPAG_Loop_2_2
                  ELSE
                     l1 = l1 + 1
                  ENDIF
               ENDDO SPAG_Loop_2_2
            ELSE
               l1 = j
               SPAG_Loop_2_3: DO
                  x4 = x4 + dvmdvm(l1)
                  IF ( l1>=Imid-1 ) THEN
                     x4 = x4/float(Imid-j)
                     EXIT SPAG_Loop_2_3
                  ELSE
                     l1 = l1 + 1
                  ENDIF
               ENDDO SPAG_Loop_2_3
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
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( w<Flow(Icase) .AND. iconf1==0 ) vmin = Vv(Imid)
            ENDIF
         ELSEIF ( Nmach(I)==1 ) THEN
            IF ( Vv(Imid)>vmax .AND. iconf1==0 ) vmax = Vv(Imid)
            dv = 0.1*Vv(Imid)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( w<Flow(Icase) .AND. iconf1==0 ) vmax = Vv(Imid)
         ENDIF
         dv = (Flow(Icase)-w)/dwdv
         IF ( dv<-0.1*Vv(Imid) ) dv = -0.1*Vv(Imid)
         IF ( dv>0.1*Vv(Imid) ) dv = 0.1*Vv(Imid)
         spag_nextblock_1 = 6
      CASE (6)
         IF ( .NOT.(Ipass==1 .OR. (I/=1 .AND. Nwork(I)<=4)) ) THEN
            IF ( Vv(Imid)+dv>=vmin ) dv = (vmin-Vv(Imid))*0.5
            IF ( Vv(Imid)+dv<=vmax ) dv = (vmax-Vv(Imid))*0.5
         ENDIF
         DO j = k , Nstrms
            Vv(j) = Vv(j) + dv
            IF ( Vv(j)<1.0 ) THEN
               IF ( Ipass>Nforce ) THEN
                  IF ( Lnct>=Npage ) THEN
                     WRITE (Log2,99012)
                     Lnct = 1
                  ENDIF
                  Lnct = Lnct + 1
                  WRITE (Log2,99009) Ipass , I , Iter , j , Vv(j)
99009             FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,                                         &
                         &50H  MERIDIONAL VELOCITY BELOW LIMIT IN CONTINUITY AT,E13.5)
               ENDIF
               Vv(j) = 1.0
               Ifail = 1
            ENDIF
         ENDDO
         x1 = Tolnce/5.0
         IF ( Neval(I)>0 ) x1 = x1/2.0
         IF ( abs(w/Flow(Icase)-1.0)>x1 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO j = k , Nstrms
            IF ( abs(Vv(j)/vvold(j)-1.0)>x1 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
         Ifail = 1
         IF ( Ipass>Nforce ) THEN
            IF ( Lnct>=Npage ) THEN
               WRITE (Log2,99012)
               Lnct = 1
            ENDIF
            Lnct = Lnct + 1
            WRITE (Log2,99010) Ipass , I , Iter
99010       FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,43H  OTHER CONTINUITY EQUATION BRANCH REQUIRED)
         ENDIF
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
         IF ( Iter<itmax ) THEN
            IF ( I/=1 ) THEN
               IF ( (Nloss(I)==1 .AND. Nl2(I)==0) .OR. (Nwork(I)>=5 .AND. Nloss(I)==2) ) CALL alg07
            ENDIF
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Ipass>Nforce ) THEN
            IF ( Lnct>=Npage ) THEN
               WRITE (Log2,99012)
               Lnct = 1
            ENDIF
            Lnct = Lnct + 1
            x1 = w/Flow(Icase)
            x2 = Vv(k)/vvold(k)
            x3 = Vv(Imid)/vvold(Imid)
            x4 = Vv(Nstrms)/vvold(Nstrms)
            WRITE (Log2,99011) Ipass , I , x1 , x2 , x3 , x4
99011       FORMAT (5X,4HPASS,I3,9H  STATION,I3,49H  MOMENTUM AND/OR CONTINUITY UNCONVERGED W/WSPEC=,F8.5,16H VM/VM(OLD) HUB=,F8.5, &
                   &5H MID=,F8.5,5H TIP=,F8.5)
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         IF ( Ifail/=0 .AND. Ifailo==0 ) Ifailo = I
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
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99012 FORMAT (1H1)
99013 FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,                                                     &
             &55H  STATIC ENTHALPY BELOW LIMIT IN CONTINUITY EQUATION AT,E13.5)
END SUBROUTINE alg26
