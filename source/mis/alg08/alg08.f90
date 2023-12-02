!*==alg08.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg08
   USE c_ud300c
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(20) :: afun , bfun , dvmdvm , hs , xm2
   REAL , DIMENSION(21) :: dl , dladm , dphidl , drvwdm , dsdl , dsdm , fx1 , fx2 , tbip1 , teip1 , vmmax , vvold , xx1
   REAL :: dv , dv2dl , dwdv , vav , vmax , vmin , vold , w , x1 , x10 , x11 , x12 , x2 , x3 , x4 , x5 , x6 , x7 , x8 , x9 , xn , xq
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
         IF ( i==istag ) k = 2
         xn = speed(i)*spdfac(icase)*pi/(30.0*sclfac)
         IF ( i/=1 ) THEN
            DO j = 1 , nstrms
               lamim1(j) = lami(j)
               lami(j) = lamip1(j)
               lamip1(j) = 1.0
            ENDDO
            IF ( i/=nstns ) THEN
               IF ( ndata(i+1)/=0 ) THEN
                  l1 = ndimen(i+1) + 1
                  IF ( l1==2 ) THEN
                     DO j = 1 , nstrms
                        xx1(j) = r(j,i+1)/r(nstrms,i+1)
                     ENDDO
                  ELSEIF ( l1==3 ) THEN
                     DO j = 1 , nstrms
                        xx1(j) = xl(j,i+1)
                     ENDDO
                  ELSEIF ( l1==4 ) THEN
                     DO j = 1 , nstrms
                        xx1(j) = xl(j,i+1)/xl(nstrms,i+1)
                     ENDDO
                  ELSE
                     DO j = 1 , nstrms
                        xx1(j) = r(j,i+1)
                     ENDDO
                  ENDIF
                  l1 = is2(i+1)
                  CALL alg01(datac(l1),data4(l1),ndata(i+1),xx1,xx1,x1,nstrms,nterp(i+1),0)
                  DO j = 1 , nstrms
                     lamip1(j) = 1.0 - xx1(j)
                  ENDDO
               ENDIF
               DO j = 1 , nstrms
                  x1 = sqrt((r(j,i+1)-r(j,i))**2+(x(j,i+1)-x(j,i))**2)
                  x2 = sqrt((r(j,i)-rim1(j))**2+(x(j,i)-xim1(j))**2)
                  x3 = atan2(r(j,i+1)-r(j,i),x(j,i+1)-x(j,i))
                  x4 = atan2(r(j,i)-rim1(j),x(j,i)-xim1(j))
                  phi(j) = (x3+x4)/2.0
                  cr(j) = (x3-x4)/(x1+x2)*2.0
                  dsdm(j) = 0.0
                  drvwdm(j) = 0.0
                  dladm(j) = ((lamip1(j)-lami(j))/x1+(lami(j)-lamim1(j))/x2)/2.0
                  IF ( ipass/=1 ) THEN
                     dsdm(j) = ((s(j,i+1)-s(j,i))/x1+(s(j,i)-s(j,i-1))/x2)/2.0*g*ej
                     drvwdm(j) = ((r(j,i+1)*vw(j,i+1)-r(j,i)*vw(j,i))/x1+(r(j,i)*vw(j,i)-rim1(j)*vw(j,i-1))/x2)/(2.0*r(j,i))
                  ENDIF
               ENDDO
               IF ( ipass/=1 .AND. ndata(i)/=0 .AND. neqn/=1 .AND. nwork(i)==0 .AND. nwork(i+1)/=0 ) THEN
                  l1 = ndimen(i) + 1
                  IF ( l1==2 ) THEN
                     DO j = 1 , nstrms
                        teip1(j) = r(j,i)/r(nstrms,i)
                     ENDDO
                  ELSEIF ( l1==3 ) THEN
                     DO j = 1 , nstrms
                        teip1(j) = xl(j,i)
                     ENDDO
                  ELSEIF ( l1==4 ) THEN
                     DO j = 1 , nstrms
                        teip1(j) = xl(j,i)/xl(nstrms,i)
                     ENDDO
                  ELSE
                     DO j = 1 , nstrms
                        teip1(j) = r(j,i)
                     ENDDO
                  ENDIF
                  l1 = is2(i)
                  CALL alg01(datac(l1),data3(l1),ndata(i),teip1,teip1,x1,nstrms,nterp(i),0)
                  x1 = speed(i+1)*spdfac(icase)*pi/(30.0*sclfac)
                  DO j = 1 , nstrms
                     teip1(j) = tan(teip1(j)/c1)
                     tbip1(j) = (vw(j,i)-x1*r(j,i))/vm(j,i)
                  ENDDO
               ENDIF
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         DO j = 1 , nstrms
            dladm(j) = 0.0
            dsdm(j) = 0.0
            drvwdm(j) = 0.0
            cr(j) = 0.0
         ENDDO
         IF ( i==1 ) THEN
            DO j = 1 , nstrms
               phi(j) = atan2(r(j,2)-r(j,1),x(j,2)-x(j,1))
            ENDDO
            DO j = 1 , nstrms
               xi(j) = h(j,1)
               lami(j) = 1.0
               lamip1(j) = 1.0
            ENDDO
            IF ( ndata(2)/=0 ) THEN
               l2 = ndimen(2) + 1
               IF ( l2==2 ) THEN
                  DO j = 1 , nstrms
                     xx1(j) = r(j,2)/r(nstrms,2)
                  ENDDO
               ELSEIF ( l2==3 ) THEN
                  DO j = 1 , nstrms
                     xx1(j) = xl(j,2)
                  ENDDO
               ELSEIF ( l2==4 ) THEN
                  DO j = 1 , nstrms
                     xx1(j) = xl(j,2)/xl(nstrms,2)
                  ENDDO
               ELSE
                  DO j = 1 , nstrms
                     xx1(j) = r(j,2)
                  ENDDO
               ENDIF
               l1 = is2(2)
               CALL alg01(datac(l1),data4(l1),ndata(2),xx1,xx1,x1,nstrms,nterp(2),0)
               DO j = 1 , nstrms
                  lamip1(j) = 1.0 - xx1(j)
               ENDDO
            ENDIF
         ELSE
            DO j = 1 , nstrms
               phi(j) = atan2(r(j,i)-rim1(j),x(j,i)-xim1(j))
            ENDDO
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL alg01(r(1,i),x(1,i),nstrms,r(1,i),x1,gama,nstrms,0,1)
         DO j = 1 , nstrms
            gama(j) = atan(gama(j))
            sppg(j) = gama(j) + phi(j)
            cppg(j) = cos(sppg(j))
            sppg(j) = sin(sppg(j))
            vv(j) = vm(j,i)
         ENDDO
         DO j = 1 , itub
            dl(j) = xl(j+1,i) - xl(j,i)
            dsdl(j) = (s(j+1,i)-s(j,i))/dl(j)*g*ej
            dphidl(j) = (phi(j+1)-phi(j))/dl(j)
         ENDDO
         IF ( i==1 .OR. nwork(i)>=5 ) THEN
            DO j = 1 , itub
               fx1(j) = (tbeta(j+1,i)+tbeta(j,i))/(r(j+1,i)+r(j,i))*(r(j+1,i)*tbeta(j+1,i)-r(j,i)*tbeta(j,i))/dl(j)
               fx2(j) = (xi(j+1)-xi(j))/dl(j)*g*ej
            ENDDO
            DO j = 1 , nstrms
               x1 = xi(j) + (xn*r(j,i))**2/(2.0*g*ej)
               x1 = 1.0/(alg9(x1,s(j,i),1.0)*(1.0+(alg8(x1,s(j,i))-1.0)*(1.0+tbeta(j,i)**2)/2.0))
               IF ( x1<=1.0 ) THEN
                  IF ( ipass>nforce ) THEN
                     CALL alg03(lnct,1)
                     WRITE (log2,99014) ipass , i , j , x1
                  ENDIF
                  x1 = 6250000.0
                  IF ( ifailo==0 ) ifailo = i
               ENDIF
               vmmax(j) = sqrt(x1)
            ENDDO
         ELSE
            DO j = 1 , itub
               dvmdvm(j) = 0.0
               fx1(j) = (vw(j+1,i)+vw(j,i))/(r(j+1,i)+r(j,i))*(r(j+1,i)*vw(j+1,i)-r(j,i)*vw(j,i))/dl(j)
               fx2(j) = (h(j+1,i)-h(j,i))/dl(j)*g*ej
            ENDDO
            DO j = 1 , nstrms
               x1 = alg8(h(j,i),s(j,i))
               x1 = (2.0/alg9(h(j,i),s(j,i),1.0)-vw(j,i)**2*(x1-1.0))/(x1+1.0)
               IF ( x1<=1.0 ) THEN
                  IF ( ipass>nforce ) THEN
                     CALL alg03(lnct,1)
                     WRITE (log2,99014) ipass , i , j , x1
                  ENDIF
                  x1 = 6250000.0
                  IF ( ifailo==0 ) ifailo = i
               ENDIF
               vmmax(j) = sqrt(x1)
            ENDDO
         ENDIF
         vmax = 0.0
         vmin = 1.05*vmmax(imid)
         iter = 0
         spag_nextblock_1 = 3
      CASE (3)
         iter = iter + 1
         ifail = 0
         iconf1 = 0
         DO j = 1 , nstrms
            vvold(j) = vv(j)
         ENDDO
         IF ( i==1 .OR. nwork(i)>=5 ) THEN
            j = imid
            jinc = 1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO j = 1 , itub
               x1 = (h(j,i)+h(j+1,i))/2.0 - (((vvold(j)+vvold(j+1))/2.0)**2+((vw(j,i)+vw(j+1,i))/2.0)**2)/(2.0*g*ej)
               IF ( x1<hmin ) THEN
                  IF ( ipass>nforce ) THEN
                     IF ( lnct>=npage ) THEN
                        WRITE (log2,99015)
                        lnct = 1
                     ENDIF
                     lnct = lnct + 1
                     WRITE (log2,99001) ipass , i , iter , j , x1
99001                FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,                                      &
                            &53H  STATIC ENTHALPY BELOW LIMIT IN MOMENTUM EQUATION AT,E13.5)
                  ENDIF
                  ifail = 1
                  x1 = hmin
               ENDIF
               x2 = (s(j,i)+s(j+1,i))/2.0
               x6 = alg8(x1,x2)
               x7 = alg7(x1,x2)
               x1 = alg9(x1,x2,((vvold(j)+vvold(j+1))/2.0)**2)
               xq = x1
               IF ( x1>0.9801 ) THEN
                  IF ( ipass>nforce ) THEN
                     IF ( lnct>=npage ) THEN
                        WRITE (log2,99015)
                        lnct = 1
                     ENDIF
                     lnct = lnct + 1
                     x1 = sqrt(x1)
                     WRITE (log2,99002) ipass , i , iter , j , x1
99002                FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,                                      &
                            &39H  MERIDIONAL MACH NUMBER ABOVE LIMIT AT,E13.5)
                  ENDIF
                  ifail = 1
                  x1 = 0.9801
               ENDIF
               x2 = (cppg(j)+cppg(j+1))/2.0
               x3 = (sppg(j)+sppg(j+1))/2.0
               afun(j) = -2.0/(1.0-x1)*((1.0-x2*x2*xq)*(cr(j)+cr(j+1))/(2.0*x2)-x3/x2*dphidl(j)                                     &
                       & -x3*(sin((phi(j)+phi(j+1))/2.0)/(r(j,i)+r(j+1,i))*2.0*(1.0+x1*((vw(j,i)+vw(j+1,i))/(vvold(j)+vvold(j+1)))  &
                       & **2)+(dladm(j)+dladm(j+1))/(lami(j)+lami(j+1))))
               bfun(j) = 2.0*(fx2(j)-x7*dsdl(j)-fx1(j))
               IF ( i/=nstns .AND. ipass/=1 ) THEN
                  IF ( neqn==1 .OR. ndata(i)==0 .OR. (nwork(i)==0 .AND. nwork(i+1)==0) ) THEN
                     bfun(j) = bfun(j) + x7*(dsdm(j)+dsdm(j+1))*x3*(1.0-x1*(x6-1.0))/(1.0-x1)
                  ELSE
                     IF ( nwork(i)==0 ) THEN
                        x4 = (tbip1(j)+tbip1(j+1))*0.5
                        x5 = (teip1(j)+teip1(j+1))*0.5
                     ELSE
                        x4 = (tbeta(j,i)+tbeta(j+1,i))/2.0
                        x5 = (taneps(j)+taneps(j+1))/2.0
                     ENDIF
                     bfun(j) = bfun(j) + 2.0*(x7*(dsdm(j)+dsdm(j+1))/2.0*(x3*(1.0/(1.0+x4*x4)+x6*x1/(1.0-x1))-x5*x4/(1.0+x4*x4))    &
                             & -(vvold(j)+vvold(j+1))*.25*(drvwdm(j)+drvwdm(j+1))*(x5-x3*x1/(1.0-x1)*x4))
                  ENDIF
               ENDIF
            ENDDO
            vv(imid) = vvold(imid)**2
            j = imid
            jinc = 1
         ENDIF
         DO
            jold = j
            j = j + jinc
            jj = jold
            IF ( jinc==-1 ) jj = j
            IF ( abs(afun(jj))<=1.0E-5 ) THEN
               vv(j) = vv(jold) + bfun(jj)*(xl(j,i)-xl(jold,i))
            ELSE
               x1 = -afun(jj)*(xl(j,i)-xl(jold,i))
               IF ( x1>88.0 ) THEN
                  IF ( ipass>nforce ) THEN
                     IF ( lnct>=npage ) THEN
                        WRITE (log2,99015)
                        lnct = 1
                     ENDIF
                     lnct = lnct + 1
                     WRITE (log2,99003) ipass , i , iter , jj , x1
99003                FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,                                      &
                            &43H  MOMENTUM EQUATION EXPONENT ABOVE LIMIT AT,E13.5)
                  ENDIF
                  ifail = 1
                  x1 = 88.0
               ENDIF
               x1 = exp(x1)
               vv(j) = vv(jold)*x1 + (1.0-x1)*bfun(jj)/afun(jj)
            ENDIF
            IF ( j==k ) THEN
               DO j = k , nstrms
                  IF ( vv(j)>4.0*vvold(imid)**2 ) THEN
                     ifail = 1
                     IF ( ipass>nforce ) THEN
                        CALL alg03(lnct,1)
                        WRITE (log2,99004) ipass , i , iter , j
99004                   FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,                                   &
                               &50H  MERIDIONAL VELOCITY GREATER THAN TWICE MID VALUE)
                     ENDIF
                     vv(j) = 4.0*vvold(imid)**2
                  ENDIF
                  IF ( vv(j)>=1.0 ) THEN
                     vv(j) = sqrt(vv(j))
                     IF ( vv(j)>vmmax(j) ) THEN
                        ifail = 1
                        IF ( ipass>nforce ) THEN
                           CALL alg03(lnct,1)
                           WRITE (log2,99016) ipass , i , iter , j , vv(j) , vmmax(j)
                        ENDIF
                        vv(j) = vmmax(j)
                     ENDIF
                  ELSE
                     IF ( ipass>nforce ) THEN
                        IF ( lnct>=npage ) THEN
                           WRITE (log2,99015)
                           lnct = 1
                        ENDIF
                        lnct = lnct + 1
                        WRITE (log2,99005) ipass , i , iter , j , vv(j)
99005                   FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,                                   &
                               &46H  (MERIDIONAL VELOCITY) SQUARED BELOW LIMIT AT,E13.5)
                     ENDIF
                     vv(j) = 1.0
                     ifail = 1
                  ENDIF
               ENDDO
               x1 = 0.0
               DO j = k , itub
                  x1 = x1 + (xl(j+1,i)-xl(j,i))*abs((vv(j+1)+vv(j))/(vvold(j+1)+vvold(j))-1.0)
               ENDDO
               x1 = x1/(xl(nstrms,i)-xl(k,i))
               x2 = 0.1
               IF ( x1<0.2 ) x2 = exp(-11.52*x1)
               DO j = k , nstrms
                  vv(j) = vvold(j) + x2*(vv(j)-vvold(j))
               ENDDO
               IF ( nloss(i)==1 .AND. nl2(i)==0 ) CALL alg07
               DO j = 1 , itub
                  hs(j) = (h(j,i)+h(j+1,i))/2.0 - (((vv(j)+vv(j+1))/2.0)**2+((vw(j,i)+vw(j+1,i))/2.0)**2)/(2.0*g*ej)
                  IF ( hs(j)<hmin ) THEN
                     IF ( ipass>nforce ) THEN
                        IF ( lnct>=npage ) THEN
                           WRITE (log2,99015)
                           lnct = 1
                        ENDIF
                        lnct = lnct + 1
                        WRITE (log2,99017) ipass , i , iter , j , hs(j)
                     ENDIF
                     ifail = 1
                     hs(j) = hmin
                  ENDIF
                  xm2(j) = alg9(hs(j),(s(j,i)+s(j+1,i))/2.0,((vv(j)+vv(j+1))/2.0)**2)
               ENDDO
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( j==nstrms ) THEN
               j = imid
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
            vold = vv(j)
            vav = (vold+vv(jold))/2.0
            ifaie = 0
            iconf2 = 0
            x2 = (tbeta(j,i)+tbeta(jold,i))/2.0
            x1 = (xi(j)+xi(jold))/2.0 + ((xn*(r(j,i)+r(jold,i))/2.0)**2-vav**2*(1.0+x2*x2))/(2.0*g*ej)
            IF ( x1<hmin ) THEN
               IF ( ipass>nforce ) THEN
                  IF ( lnct>=npage ) THEN
                     WRITE (log2,99015)
                     lnct = 1
                  ENDIF
                  lnct = lnct + 1
                  WRITE (log2,99006) ipass , i , iter , jj , loop , x1
99006             FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,6H  LOOP,I3,                             &
                         &43H  STATIC H IN MOMENTUM EQUN. BELOW LIMIT AT,E13.5)
               ENDIF
               ifaie = 1
               iconf2 = 1
               x1 = hmin
            ENDIF
            x3 = (s(j,i)+s(jold,i))/2.0
            x6 = alg8(x1,x3)
            x7 = alg7(x1,x3)
            x1 = alg9(x1,x3,vav*vav)
            IF ( x1>0.9801 ) THEN
               IF ( ipass>nforce ) THEN
                  IF ( lnct>=npage ) THEN
                     WRITE (log2,99015)
                     lnct = 1
                  ENDIF
                  lnct = lnct + 1
                  x1 = sqrt(x1)
                  WRITE (log2,99007) ipass , i , iter , jj , loop , x1
99007             FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,6H  LOOP,I3,                             &
                         &39H  MERIDIONAL MACH NUMBER ABOVE LIMIT AT,E13.5)
               ENDIF
               ifaie = 1
               iconf2 = 1
               x1 = 0.9801
            ENDIF
            x4 = (sppg(j)+sppg(jold))/2.0
            x5 = (cppg(j)+cppg(jold))/2.0
            x9 = (r(j,i)+r(jold,i))*0.5
            x10 = sin((phi(j)+phi(jold))*0.5)
            x11 = (1.0-x5*x5*x1)*(cr(j)+cr(jold))*0.5/x5 - x4/x5*dphidl(jj)                                                         &
                & - x4*(x10/x9*(1.0+x1*(x2+xn*x9/vav)**2)+(dladm(j)+dladm(jold))/(lami(j)+lami(jold)))
            dv2dl = fx2(jj) - x7*dsdl(jj) - 2.0*xn*vav*x2*cos((gama(j)+gama(jold))*0.5) + vav*vav*(x11/(1.0-x1)-fx1(jj))
            x12 = 1.0/(1.0+x2*x2)
            dvmdvm(jj) = x12*((x7*dsdl(jj)-fx2(jj))/vav**2-fx1(jj)+x11/(1.0-x1))
            IF ( i/=1 .AND. i/=nstns .AND. ipass/=1 ) THEN
               IF ( neqn==1 ) THEN
                  x5 = 0.5*(dsdm(j)+dsdm(jold))*x7*x4*(1.0-x1*(x6-1.0))/(1.0-x1)
                  dv2dl = dv2dl + x5
                  dvmdvm(jj) = dvmdvm(jj) - x5*x12/vav**2
               ELSE
                  x8 = (taneps(j)+taneps(jold))*0.5
                  x5 = 0.5*(dsdm(j)+dsdm(jold))*x7*(x4*(x12+x6*x1/(1.0-x1))-x8*x2*x12)
                  dv2dl = dv2dl + x5 - vav*(drvwdm(j)+drvwdm(jold))*0.5*(x8-x4*x1*x2/(1.0-x1))
                  dvmdvm(jj) = dvmdvm(jj) - x5*x12/vav**2
               ENDIF
            ENDIF
            dv2dl = dv2dl*2.0*x12
            x1 = vv(jold)**2 + dv2dl*(xl(j,i)-xl(jold,i))
            IF ( x1>9.0*vvold(imid)**2 ) THEN
               iconf2 = 1
               ifaie = 1
               IF ( ipass>nforce ) THEN
                  CALL alg03(lnct,1)
                  x1 = sqrt(x1)
                  x2 = 3.0*vvold(imid)
                  WRITE (log2,99008) ipass , i , iter , j , loop , x1 , x2
99008             FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,6H  LOOP,I3,                             &
                         &33H  MERIDIONAL VELOCITY ABOVE LIMIT,E13.5,9H  LIMIT =,E13.5)
               ENDIF
               x1 = 9.0*vvold(imid)**2
            ENDIF
            IF ( x1<1.0 ) THEN
               IF ( ipass>nforce ) THEN
                  IF ( lnct>=npage ) THEN
                     WRITE (log2,99015)
                     lnct = 1
                  ENDIF
                  lnct = lnct + 1
                  WRITE (log2,99009) ipass , i , iter , j , loop , x1
99009             FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,6H  LOOP,I3,                             &
                         &46H  (MERIDIONAL VELOCITY) SQUARED BELOW LIMIT AT,E13.5)
               ENDIF
               x1 = 1.0
               ifaie = 1
               iconf2 = 1
            ENDIF
            vv(j) = sqrt(x1)
            IF ( vv(j)>vmmax(j) ) THEN
               ifaie = 1
               iconf2 = 1
               IF ( ipass>nforce ) THEN
                  CALL alg03(lnct,1)
                  WRITE (log2,99016) ipass , i , iter , j , vv(j) , vmmax(j)
               ENDIF
               vv(j) = vmmax(j)
            ENDIF
            IF ( abs(vv(j)/vold-1.0)>tolnce*0.2 ) THEN
               IF ( loop>=lpmax ) THEN
                  iconf2 = 1
                  IF ( ipass>nforce ) THEN
                     IF ( lnct>=npage ) THEN
                        WRITE (log2,99015)
                        lnct = 1
                     ENDIF
                     lnct = lnct + 1
                     WRITE (log2,99010) ipass , i , iter , j , vv(j) , vold
99010                FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,                                      &
                            &38H  MERIDIONAL VELOCITY UNCONVERGED  VM=,E13.6,9H VM(OLD)=,E13.6)
                  ENDIF
               ELSE
                  loop = loop + 1
                  CYCLE
               ENDIF
            ENDIF
            IF ( ifaie==1 ) ifail = 1
            IF ( iconf2==1 ) iconf1 = 1
            IF ( j==nstrms ) THEN
               j = imid
               jinc = -1
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( j/=1 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i/=1 ) THEN
                  IF ( nloss(i)==2 .OR. (nloss(i)==1 .AND. nl2(i)==0) ) CALL alg07
               ENDIF
               DO j = 1 , itub
                  x1 = ((vv(j)+vv(j+1))/2.0)**2*(1.0+((tbeta(j,i)+tbeta(j+1,i))/2.0)**2)
                  hs(j) = (xi(j)+xi(j+1))/2.0 + ((xn*(r(j,i)+r(j+1,i))/2.0)**2-x1)/(2.0*g*ej)
                  IF ( hs(j)<hmin ) THEN
                     IF ( ipass>nforce ) THEN
                        IF ( lnct>=npage ) THEN
                           WRITE (log2,99015)
                           lnct = 1
                        ENDIF
                        lnct = lnct + 1
                        WRITE (log2,99017) ipass , i , iter , j , hs(j)
                     ENDIF
                     ifail = 1
                     hs(j) = hmin
                  ENDIF
                  xm2(j) = alg9(hs(j),(s(j,i)+s(j+1,i))/2.0,x1)
                  IF ( i/=1 .AND. nloss(i)==1 .AND. nl2(i)==0 ) THEN
                     x1 = (s(j,i)+s(j+1,i))/2.0
                     x2 = alg4(hs(j),x1)
                     x4 = alg8(hs(j),x1)
                     x3 = (xi(j)+xi(j))/2.0 + (xn*((r(j,i)+r(j+1,i))/2.0))**2/(2.0*g*ej)
                     x3 = alg4(x3,x1)
                     xm2(j) = xm2(j)*(1.0+x4*(loss(j)+loss(j+1))/2.0*x2/(x3*(1.0+(loss(j)+loss(j+1))/2.0*(1.0-x2/x3))))
                  ENDIF
               ENDDO
            ENDIF
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 5
      CASE (5)
         delw(1) = 0.0
         dwdv = 0.0
         x2 = bblock(i)*bdist(i)
         x3 = bblock(i)*(1.0-bdist(i))*2.0/xl(nstrms,i)
         DO j = 1 , itub
            x1 = dl(j)*(r(j+1,i)+r(j,i))*alg5(hs(j),(s(j,i)+s(j+1,i))/2.0)*(vv(j)+vv(j+1))*(cppg(j)+cppg(j+1))*pi/(4.0*sclfac**2)
            x1 = x1*((lami(j)+lami(j+1))/2.0-wwbl(i)-x2-x3*(xl(j,i)+xl(j+1,i)))
            delw(j+1) = delw(j) + x1
            x4 = 0.0
            IF ( j>=imid ) THEN
               l1 = imid + 1
               SPAG_Loop_2_2: DO
                  x4 = x4 + dvmdvm(l1)
                  IF ( l1>=j ) THEN
                     x4 = x4/float(j-imid+1)
                     EXIT SPAG_Loop_2_2
                  ELSE
                     l1 = l1 + 1
                  ENDIF
               ENDDO SPAG_Loop_2_2
            ELSE
               l1 = j
               SPAG_Loop_2_3: DO
                  x4 = x4 + dvmdvm(l1)
                  IF ( l1>=imid-1 ) THEN
                     x4 = x4/float(imid-j)
                     EXIT SPAG_Loop_2_3
                  ELSE
                     l1 = l1 + 1
                  ENDIF
               ENDDO SPAG_Loop_2_3
            ENDIF
            dwdv = dwdv + x1*(1.0-xm2(j))*2.0/((vv(j)+vv(j+1))*(1.0-((xl(j,i)+xl(j+1,i))*0.5-xl(imid,i))*x4))
         ENDDO
         w = delw(nstrms)
         fm2 = dwdv/w*vv(imid)
         DO j = 2 , nstrms
            delw(j) = delw(j)/w
         ENDDO
         IF ( dwdv<=0.0 ) THEN
            IF ( nmach(i)==0 ) THEN
               IF ( vv(imid)<vmin .AND. iconf1==0 ) vmin = vv(imid)
               dv = -.1*vv(imid)
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( w<flow(icase) .AND. iconf1==0 ) vmin = vv(imid)
            ENDIF
         ELSEIF ( nmach(i)==1 ) THEN
            IF ( vv(imid)>vmax .AND. iconf1==0 ) vmax = vv(imid)
            dv = 0.1*vv(imid)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( w<flow(icase) .AND. iconf1==0 ) vmax = vv(imid)
         ENDIF
         dv = (flow(icase)-w)/dwdv
         IF ( dv<-0.1*vv(imid) ) dv = -0.1*vv(imid)
         IF ( dv>0.1*vv(imid) ) dv = 0.1*vv(imid)
         spag_nextblock_1 = 6
      CASE (6)
         IF ( .NOT.(ipass==1 .OR. (i/=1 .AND. nwork(i)<=4)) ) THEN
            IF ( vv(imid)+dv>=vmin ) dv = (vmin-vv(imid))*0.5
            IF ( vv(imid)+dv<=vmax ) dv = (vmax-vv(imid))*0.5
         ENDIF
         DO j = k , nstrms
            vv(j) = vv(j) + dv
            IF ( vv(j)>vmmax(j) ) THEN
               ifail = 1
               vv(j) = vmmax(j)
            ENDIF
            IF ( vv(j)<1.0 ) THEN
               IF ( ipass>nforce ) THEN
                  IF ( lnct>=npage ) THEN
                     WRITE (log2,99015)
                     lnct = 1
                  ENDIF
                  lnct = lnct + 1
                  WRITE (log2,99011) ipass , i , iter , j , vv(j)
99011             FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,                                         &
                         &50H  MERIDIONAL VELOCITY BELOW LIMIT IN CONTINUITY AT,E13.5)
               ENDIF
               vv(j) = 1.0
               ifail = 1
            ENDIF
         ENDDO
         x1 = tolnce/5.0
         IF ( neval(i)>0 ) x1 = x1/2.0
         IF ( abs(w/flow(icase)-1.0)>x1 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO j = k , nstrms
            IF ( abs(vv(j)/vvold(j)-1.0)>x1 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 9
      CASE (7)
         ifail = 1
         IF ( ipass>nforce ) THEN
            IF ( lnct>=npage ) THEN
               WRITE (log2,99015)
               lnct = 1
            ENDIF
            lnct = lnct + 1
            WRITE (log2,99012) ipass , i , iter
99012       FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,43H  OTHER CONTINUITY EQUATION BRANCH REQUIRED)
         ENDIF
         spag_nextblock_1 = 6
      CASE (8)
         IF ( iter<itmax ) THEN
            IF ( i/=1 ) THEN
               IF ( (nloss(i)==1 .AND. nl2(i)==0) .OR. (nwork(i)>=5 .AND. nloss(i)==2) ) CALL alg07
            ENDIF
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( ipass>nforce ) THEN
            IF ( lnct>=npage ) THEN
               WRITE (log2,99015)
               lnct = 1
            ENDIF
            lnct = lnct + 1
            x1 = w/flow(icase)
            x2 = vv(k)/vvold(k)
            x3 = vv(imid)/vvold(imid)
            x4 = vv(nstrms)/vvold(nstrms)
            WRITE (log2,99013) ipass , i , x1 , x2 , x3 , x4
99013       FORMAT (5X,4HPASS,I3,9H  STATION,I3,49H  MOMENTUM AND/OR CONTINUITY UNCONVERGED W/WSPEC=,F8.5,16H VM/VM(OLD) HUB=,F8.5, &
                   &5H MID=,F8.5,5H TIP=,F8.5)
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         IF ( ifail/=0 .AND. ifailo==0 ) ifailo = i
         DO j = 1 , nstrms
            vm(j,i) = vv(j)
         ENDDO
         IF ( i/=1 ) THEN
            IF ( nmix==1 ) THEN
               DO j = 1 , nstrms
                  s(j,i-1) = skeep(j)
                  h(j,i-1) = hkeep(j)
                  vw(j,i-1) = vwkeep(j)
               ENDDO
            ENDIF
            IF ( nwork(i)>=5 ) THEN
               DO j = 1 , nstrms
                  vw(j,i) = vv(j)*tbeta(j,i) + xn*r(j,i)
                  h(j,i) = xi(j) + xn*r(j,i)*vw(j,i)/(g*ej)
               ENDDO
            ELSE
               tbeta(1,i) = 0.0
               DO j = k , nstrms
                  tbeta(j,i) = (vw(j,i)-xn*r(j,i))/vv(j)
               ENDDO
            ENDIF
         ELSE
            DO j = 1 , nstrms
               vw(j,1) = vv(j)*tbeta(j,1)
            ENDDO
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99014 FORMAT (5X,4HPASS,I3,9H  STATION,I3,12H  STREAMLINE,I3,40H  LIMITING MERIDIONAL VELOCITY SQUARED =,E12.5)
99015 FORMAT (1H1)
99016 FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMLINE,I3,44H  MERIDIONAL VELOCITY ABOVE SOUND SPEED  VM=,F8.2,&
             &3H A=,F8.2)
99017 FORMAT (5X,4HPASS,I3,9H  STATION,I3,11H  ITERATION,I3,12H  STREAMTUBE,I3,                                                     &
             &55H  STATIC ENTHALPY BELOW LIMIT IN CONTINUITY EQUATION AT,E13.5)
END SUBROUTINE alg08
