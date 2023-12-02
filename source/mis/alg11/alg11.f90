!*==alg11.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg11
   USE c_algino
   USE c_system
   USE c_ud300c
   USE c_ud3prt
   USE c_udsign
   USE c_udstr2
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: alpha , bbeta , beta , bspace , coef , delp , dh1 , dhn , dif , dpq , eff1 , effn , fi , ga , h1bar , hbar , hnbar ,     &
         & hri , hs , oeff , opr , p1bar , pbar , pfac , pload , pnbar , power , pr , prd , prl1 , prl2 , psins , psl1 , psl2 ,     &
         & ptins , q , radius , rbar1 , rbarn , s1bar , snbar , tt , u , va , vr , x1 , x2 , x3 , x4 , xblade , xmr , xn , xsign
   REAL , DIMENSION(21) :: deltb , p1 , pn , ppg , ps , pt , solid , ts , v , wt , xm
   REAL , DIMENSION(21,30) :: deltp , tr
   INTEGER :: ible , ielem , ifle , ifte , ii , ilast , ileb , inode , ip , itrleb , j , k , l1 , l1keep , l2 , l3 , l4 , l5 , l6 , &
            & ledgeb , nout3s , nout3t , nstnsx
   INTEGER , DIMENSION(6) :: idata
   INTEGER , DIMENSION(2) , SAVE :: name1 , name2
   REAL , DIMENSION(6) :: rdata
   REAL , DIMENSION(21,6) :: rmdv
   EXTERNAL alg01 , alg03 , alg2 , alg3 , alg4 , alg5 , alg7 , alg9 , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (idata(1),rdata(1))
   DATA name1 , name2/4HPLOA , 4HD2   , 4HTEMP , 4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         opr = 0.0
         oeff = 1.0
         pfac = 550.0
         ilast = nstns
!
!     LOCATE COMPUTING STATION NUMBER AT THE BLADE LEADING EDGE AND
!     AT THE BLADE TRAILING EDGE.
!
         ledgeb = 0
         itrleb = 0
         DO ible = 1 , nstns
            nout3s = nout3(ible)/10
            IF ( nout3(ible)==1 .OR. nout3s==1 ) ledgeb = ible
            IF ( nout3(ible)==2 .OR. nout3s==2 ) itrleb = ible
         ENDDO
         IF ( ifailo/=0 ) ilast = ifailo
         DO i = 1 , ilast
            CALL alg03(lnct,7+nstrms)
            IF ( iprtc==1 ) WRITE (log2,99001) i
99001       FORMAT (//10X,'STATION',I3,'  FLOW-FIELD DESCRIPTION',/10X,34(1H*),//,'  STREAM      -----MESH-POINT COORDS------',3X,  &
                  & 16(1H-),'V E L O C I T I E S,16(1H-)    RADIUS OF  ','STREAMLINE   STATION',/,'  -LINE       RADIUS    X-COORD',&
                   &'    L-COORD   MERIDIONAL TANGENTIAL   AXIAL',6X,'RADIAL',4X,'TOTAL    CURVATURE SLOPE ANGLE LEAN ANGLE',/)
            CALL alg01(r(1,i),x(1,i),nstrms,r(1,i),x1,gama,nstrms,0,1)
            IF ( i/=1 .AND. i/=nstns ) THEN
               DO j = 1 , nstrms
                  x1 = sqrt((r(j,i+1)-r(j,i))**2+(x(j,i+1)-x(j,i))**2)
                  x2 = sqrt((r(j,i)-r(j,i-1))**2+(x(j,i)-x(j,i-1))**2)
                  x3 = atan2(r(j,i+1)-r(j,i),x(j,i+1)-x(j,i))
                  x4 = atan2(r(j,i)-r(j,i-1),x(j,i)-x(j,i-1))
                  cr(j) = (x3-x4)/(x1+x2)*2.0
                  IF ( cr(j)/=0.0 ) cr(j) = 1.0/cr(j)
                  phi(j) = (x3+x4)/2.0
               ENDDO
            ELSE
               l1 = 1
               l2 = 2
               IF ( i/=1 ) THEN
                  l2 = nstns
                  l1 = l2 - 1
               ENDIF
               DO j = 1 , nstrms
                  cr(j) = 0.0
                  phi(j) = atan2(r(j,l2)-r(j,l1),x(j,l2)-x(j,l1))
               ENDDO
            ENDIF
            DO j = 1 , nstrms
               va = vm(j,i)*cos(phi(j))
               vr = vm(j,i)*sin(phi(j))
               fi = phi(j)*c1
               ga = atan(gama(j))*c1
               ppg(j) = fi + ga
               v(j) = sqrt(vm(j,i)**2+vw(j,i)**2)
!
!     STORE RADIUS AT BLADE LEADING AND TRAILING EDGES, ALL STREAMLINES
!
               IF ( icase==1 .AND. i==ledgeb ) rmdv(j,5) = r(j,i)
               IF ( icase==1 .AND. i==itrleb ) rmdv(j,6) = r(j,i)
               IF ( iprtc==1 ) WRITE (log2,99002) j , r(j,i) , x(j,i) , xl(j,i) , vm(j,i) , vw(j,i) , va , vr , v(j) , cr(j) , fi , &
                                    & ga
99002          FORMAT (I6,F14.4,2F11.4,5F11.2,1X,F10.2,2F11.3)
            ENDDO
            CALL alg03(lnct,nstrms+4)
            IF ( iprtc==1 ) WRITE (log2,99003)
99003       FORMAT (/8H  STREAM,7X,4HMACH,6X,4(1H-),9HPRESSURES,4(1H-),5X,17H---TEMPERATURES--,4X,8HSPECIFIC,4X,                    &
                  & 17H---ENTHALPIES----,4X,7HENTROPY,6X,4HFLOW,3X,11H(PHI+GAMMA),/,7H  -LINE,7X,6HNUMBER,5X,5HTOTAL,6X,6HSTATIC,5X,&
                   &5HTOTAL,6X,6HSTATIC,5X,6HWEIGHT,5X,5HTOTAL,6X,6HSTATIC,16X,5HANGLE,/)
            DO j = 1 , nstrms
               deltb(j) = 0.0
               hs = h(j,i) - v(j)**2/(2.0*g*ej)
               IF ( hs<hmin ) hs = hmin
               xm(j) = sqrt(alg9(hs,s(j,i),v(j)**2))
               pt(j) = alg4(h(j,i),s(j,i))
               ptins = pt(j)/sclfac**2
               ps(j) = alg4(hs,s(j,i))
               psins = ps(j)/sclfac**2
               tt = alg7(h(j,i),s(j,i))
               ts(j) = alg7(hs,s(j,i))
               wt(j) = alg5(hs,s(j,i))
               alpha = 0.0
               IF ( i/=istag .OR. j/=1 ) alpha = c1*atan(vw(j,i)/vm(j,i))
!
!     STORE DENSITY AT BLADE LEADING EDGE FOR ALL STREAMLINES
!
               IF ( icase==1 .AND. i==ledgeb ) rmdv(j,2) = wt(j)
               IF ( iprtc==1 ) WRITE (log2,99004) j , xm(j) , ptins , psins , tt , ts(j) , wt(j) , h(j,i) , hs , s(j,i) , alpha ,   &
                                    & ppg(j)
99004          FORMAT (I6,F14.4,2F11.4,2F11.3,F12.6,F10.3,F11.3,F12.6,F10.3,F11.3)
            ENDDO
            IF ( i/=1 ) THEN
               ifle = 0
               ifte = 0
               IF ( nwork(i)/=0 ) THEN
                  ifte = 1
                  IF ( i/=nstns .AND. nwork(i+1)/=0 .AND. speed(i)/=speed(i+1) ) ifle = 1
               ELSEIF ( i/=nstns .AND. nwork(i+1)/=0 ) THEN
                  ifle = 1
               ENDIF
               IF ( ifte/=0 ) THEN
                  CALL alg03(lnct,nstrms+8)
                  xn = speed(i)*spdfac(icase)
                  xblade = 10.0
                  IF ( nblade(i)/=0 ) xblade = abs(float(nblade(i)))
                  l1 = xblade
                  IF ( iprtc==1 ) WRITE (log2,99005) i , xn , l1
99005             FORMAT (/10X,'STATION',I3,' IS WITHIN OR AT THE TRAILING EDGE OF',' A BLADE ROTATING AT',F8.1,                    &
                         &' RPM  NUMBER OF BLADES IN ','ROW =',I3,/10X,109(1H*),//,'  STREAM      BLADE     ',                      &
                         &'RELATIVE    RELATIVE   RELATIVE  DEVIATION    BLADE    ',                                                &
                         &'  LEAN    PRESS DIFF    LOSS     DIFFUSION   DELTA P',/,'  -LINE',7X,                                    &
                         &'SPEED     VELOCITY    MACH NO.  FLOW',' ANGLE   ANGLE      ANGLE      ANGLE   ACROSS BLADE  ',           &
                         &'COEFF      FACTOR     ON Q',/)
                  q = 1.0
                  IF ( speed(i)>=0.0 ) THEN
                     IF ( speed(i)>0.0 ) THEN
                        q = -1.0
                     ELSEIF ( i>=3 ) THEN
                        ii = i - 1
                        DO WHILE ( speed(ii)==0.0 )
                           IF ( ii==2 ) GOTO 2
                           ii = ii - 1
                        ENDDO
                        IF ( speed(ii)<0.0 ) q = -1.0
                     ENDIF
                  ENDIF
 2                l1 = ndimen(i) + 1
                  IF ( l1==2 ) THEN
                     DO j = 1 , nstrms
                        taneps(j) = r(j,i)/r(nstrms,i)
                     ENDDO
                  ELSEIF ( l1==3 ) THEN
                     DO j = 1 , nstrms
                        taneps(j) = xl(j,i)
                     ENDDO
                  ELSEIF ( l1==4 ) THEN
                     DO j = 1 , nstrms
                        taneps(j) = xl(j,i)/xl(nstrms,i)
                     ENDDO
                  ELSE
                     DO j = 1 , nstrms
                        taneps(j) = r(j,i)
                     ENDDO
                  ENDIF
                  l1 = is2(i)
                  IF ( nwork(i)==5 .OR. nwork(i)==6 ) CALL alg01(datac(l1),data6(l1),ndata(i),taneps,deltb,x1,nstrms,nterp(i),0)
                  CALL alg01(datac(l1),data5(l1),ndata(i),taneps,solid,x1,nstrms,nterp(i),0)
                  CALL alg01(datac(l1),data3(l1),ndata(i),taneps,taneps,x1,nstrms,nterp(i),0)
                  l1 = i + nl1(i)
                  l2 = l1
                  IF ( nloss(i)==1 .OR. nloss(i)==4 .OR. nwork(i)==7 ) l2 = i + nl2(i)
                  xn = xn*pi/(30.0*sclfac)
                  DO j = 1 , nstrms
                     u = xn*r(j,i)
                     vr = sqrt(vm(j,i)**2+(vw(j,i)-u)**2)
                     xmr = xm(j)*vr/v(j)
                     beta = atan(tbeta(j,i))*c1
                     bbeta = 0.0
                     IF ( nwork(i)==5 .OR. nwork(i)==6 ) bbeta = beta - deltb(j)
                     deltb(j) = deltb(j)*q
                     delp = 0.0
                     IF ( i/=nstns .AND. nwork(i+1)/=0 .AND. speed(i)==speed(i+1) ) THEN
                        x1 = sqrt((r(j,i+1)-r(j,i))**2+(x(j,i+1)-x(j,i))**2)
                        x2 = sqrt((r(j,i)-r(j,i-1))**2+(x(j,i)-x(j,i-1))**2)
                        x3 = xblade
                        delp = pi*r(j,i)*wt(j)/(sclfac**2*x3*g)                                                                     &
                             & *(tbeta(j,i)/(1.0+tbeta(j,i)**2)*ts(j)*g*ej*((s(j,i+1)-s(j,i))/x1+(s(j,i)-s(j,i-1))/x2)+vm(j,i)      &
                             & /r(j,i)*((r(j,i+1)*vw(j,i+1)-r(j,i)*vw(j,i))/x1+(r(j,i)*vw(j,i)-r(j,i-1)*vw(j,i-1))/x2))
                        deltp(j,i) = delp
                     ENDIF
                     hri = h(j,i) - (v(j)**2-vr**2)/(2.0*g*ej)
                     prd = alg4(hri,s(j,l1))
                     pr = alg4(hri,s(j,i))
                     tr(j,i) = alg7(hri,s(j,i))
                     prl2 = pr
                     psl2 = ps(j)*sclfac**2
                     IF ( l2/=i ) THEN
                        prl2 = h(j,l2) - (vw(j,l2)**2-(vw(j,l2)-xn*r(j,l2))**2)/(2.0*g*ej)
                        prl2 = alg4(prl2,s(j,l2))
                        psl2 = h(j,l2) - (vw(j,l2)**2+vm(j,l2)**2)/(2.0*g*ej)
                        psl2 = alg4(psl2,s(j,l2))
                     ENDIF
                     coef = (prd-pr)/(prl2-psl2)
                     dif = 0.0
                     IF ( solid(j)/=0.0 ) THEN
                        x2 = vw(j,l1) - xn*r(j,l1)
                        x1 = sqrt(vm(j,l1)**2+x2**2)
                        x3 = vw(j,i) - u
                        dif = 1.0 - vr/x1 + (x2-x3)/(2.0*x1*solid(j))*q
                     ENDIF
                     prl1 = prl2
                     psl1 = psl2
                     IF ( l2/=l1 ) THEN
                        psl1 = h(j,l1) - (vw(j,l1)**2+vm(j,l1)**2)/(2.0*g*ej)
                        prl1 = psl1 + (vm(j,l1)**2+(vw(j,l1)-xn*r(j,l1))**2)/(2.0*g*ej)
                        psl1 = alg4(psl1,s(j,l1))
                        prl1 = alg4(prl1,s(j,l1))
                     ENDIF
                     dpq = (ps(j)-psl1)/(prl1-psl1)
                     IF ( iprtc==1 ) WRITE (log2,99006) j , u , vr , xmr , beta , deltb(j) , bbeta , taneps(j) , delp , coef , dif ,&
                        & dpq
99006                FORMAT (I6,F14.2,F11.2,F11.4,4F11.3,F11.4,F11.5,F10.4,F11.4)
                  ENDDO
                  CALL alg03(lnct,nstrms+5)
                  pbar = 0.0
                  hbar = 0.0
                  DO j = 1 , itub
                     x1 = (delf(j+1)-delf(j))/2.0
                     pbar = pbar + x1*(pt(j)+pt(j+1))
                     hbar = hbar + x1*(h(j,i)+h(j+1,i))
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
                     DO j = 1 , nstrms
                        pn(j) = alg4(h(j,l1),s(j,l1))
                     ENDDO
                     DO j = 1 , itub
                        x1 = (delf(j+1)-delf(j))/2.0
                        pnbar = pnbar + x1*(pn(j)+pn(j+1))
                        hnbar = hnbar + x1*(h(j,l1)+h(j+1,l1))
                     ENDDO
                     snbar = alg3(pnbar,hnbar)
                  ENDIF
                  effn = 0.0
                  IF ( hnbar/=hbar ) effn = (alg2(snbar,pbar)-hnbar)/(hbar-hnbar)
                  rbarn = pbar/pnbar
                  dhn = (hbar-hnbar)/hnbar
                  IF ( iprtc==1 ) WRITE (log2,99007) i , l1 , i , i , l1 , i , rbar1 , rbarn , eff1 , effn , dh1 , dhn
99007             FORMAT (/,'  STREAM',7X,'INLET THROUGH STATION',I3,7X,'STATION',I3,' THROUGH STATION',I3,5X,'MEAN VALUES',6X,     &
                         &'INLET TO STA.',I2,'   STA.',I2,' TO STA.',I2,/,'  -LINE',6X,'PRESSURE  ISENTROPIC  DELTA H    PRESSURE  '&
                        & ,'ISENTROPIC  DELTA H     PRESSURE RATIO',F14.4,F19.4,/15X,                                               &
                         &'RATIO   EFFICIENCY  ON H1        RATIO   EFFICIENCY  ON ','H1       ISEN EFFY',2F19.4,/80X,              &
                         &'DELTA H ON H1',F15.4,F19.4)
                  DO j = 1 , nstrms
                     rbar1 = pt(j)/p1(j)
                     eff1 = 0.0
                     IF ( h(j,i)/=h(j,1) ) eff1 = (alg2(s(j,1),pt(j))-h(j,1))/(h(j,i)-h(j,1))
                     dh1 = (h(j,i)-h(j,1))/h(j,1)
                     rbarn = pt(j)/pn(j)
                     effn = 0.0
                     IF ( h(j,i)/=h(j,l1) ) effn = (alg2(s(j,l1),pt(j))-h(j,l1))/(h(j,i)-h(j,l1))
                     dhn = (h(j,i)-h(j,l1))/h(j,l1)
                     IF ( iprtc==1 ) WRITE (log2,99008) j , rbar1 , eff1 , dh1 , rbarn , effn , dhn
99008                FORMAT (I6,F14.4,F10.4,F11.4,F12.4,F10.4,F11.4)
                  ENDDO
               ENDIF
               IF ( ifle/=0 ) THEN
                  CALL alg03(lnct,nstrms+8)
                  xn = speed(i+1)*spdfac(icase)
                  ip = i + 1
                  xblade = 10.0
                  IF ( nblade(ip)/=0 ) xblade = abs(float(nblade(ip)))
                  l1 = xblade
                  IF ( iprtc==1 ) WRITE (log2,99009) i , xn , l1
99009             FORMAT (/10X,'STATION',I3,' IS AT THE LEADING EDGE OF A BLADE ','ROATING AT',F9.1,                                &
                         &' RPM  NUMBER OF BLADES IN ROW =',I3,/10X,99(1H*),//,'  STREAM      BLADE     RELATIVE   ',               &
                         &'RELATIVE   RELATIVE  INCIDENCE    BLADE      LEAN    ','PRESS DIFF',/,                                   &
                         &'  -LINE       SPEED     VELOCITY   MACH',' NO.  FLOW ANGLE   ANGLE      ANGLE      ANGLE   ACROSS',      &
                         &' BLADE',/)
                  xn = xn*pi/(30.0*sclfac)
                  q = 1.0
                  IF ( speed(ip)>=0.0 ) THEN
                     IF ( speed(ip)>0.0 ) THEN
                        q = -1.0
                     ELSEIF ( ip>=3 ) THEN
                        ii = ip - 1
                        DO WHILE ( speed(ii)==0.0 )
                           IF ( ii==2 ) GOTO 4
                           ii = ii - 1
                        ENDDO
                        IF ( speed(ii)<0.0 ) q = -1.0
                     ENDIF
                  ENDIF
 4                DO j = 1 , nstrms
                     cr(j) = 0.0
                     taneps(j) = 0.0
                  ENDDO
                  IF ( nwork(i)==0 .AND. ndata(i)/=0 ) THEN
                     l1 = ndimen(i) + 1
                     IF ( l1==2 ) THEN
                        DO j = 1 , nstrms
                           taneps(j) = r(j,i)/r(nstrms,i)
                        ENDDO
                     ELSEIF ( l1==3 ) THEN
                        DO j = 1 , nstrms
                           taneps(j) = xl(j,i)
                        ENDDO
                     ELSEIF ( l1==4 ) THEN
                        DO j = 1 , nstrms
                           taneps(j) = xl(j,i)/xl(nstrms,i)
                        ENDDO
                     ELSE
                        DO j = 1 , nstrms
                           taneps(j) = r(j,i)
                        ENDDO
                     ENDIF
                     l1 = is2(i)
                     CALL alg01(datac(l1),data1(l1),ndata(i),taneps,cr,x1,nstrms,nterp(i),0)
                     CALL alg01(datac(l1),data3(l1),ndata(i),taneps,taneps,x1,nstrms,nterp(i),0)
                  ENDIF
                  bbeta = 0.0
                  DO j = 1 , nstrms
                     u = xn*r(j,i)
                     vr = sqrt(vm(j,i)**2+(vw(j,i)-u)**2)
                     xmr = xm(j)*vr/v(j)
                     tr(j,i) = alg7(h(j,i)-(v(j)**2-vr**2)/(2.0*g*ej),s(j,i))
                     beta = atan((vw(j,i)-u)/vm(j,i))*c1
!
!     STORE REL. MACH, REL. VEL AND REL. FLOW ANGLE FOR ALL STREAMLINES
!     AT THE BLADE LEADING EDGE
!
                     IF ( icase==1 .AND. i==ledgeb ) THEN
                        rmdv(j,1) = xmr
                        rmdv(j,3) = vr
                        rmdv(j,4) = beta
                     ENDIF
                     deltb(j) = 0.0
                     IF ( nwork(i)==0 .AND. ndata(i)/=0 ) THEN
                        bbeta = atan((tan(cr(j)/c1)*(1.0-gama(j)*tan(phi(j)))-tan(phi(j))*tan(taneps(j)/c1)*sqrt(1.0+gama(j)**2))   &
                              & *cos(phi(j)))*c1
                        deltb(j) = (beta-bbeta)*q
                     ENDIF
                     x1 = sqrt((r(j,i+1)-r(j,i))**2+(x(j,i+1)-x(j,i))**2)
                     delp = pi*r(j,i)*2.0*wt(j)/(sclfac**2*xblade*g)*(sin(beta/c1)*cos(beta/c1)*g*ej*ts(j)*(s(j,i+1)-s(j,i))        &
                          & /x1+vm(j,i)/(r(j,i)*x1)*(r(j,i+1)*vw(j,i+1)-r(j,i)*vw(j,i)))
                     deltp(j,i) = delp
                     IF ( iprtc==1 ) WRITE (log2,99010) j , u , vr , xmr , beta , deltb(j) , bbeta , taneps(j) , delp
99010                FORMAT (I6,F14.2,F11.2,F11.4,4F11.3,F11.4)
                  ENDDO
               ENDIF
            ELSE
               p1bar = 0.0
               h1bar = 0.0
               p1(1) = pt(1)
               pn(1) = pt(1)
               DO j = 1 , itub
                  p1(j+1) = pt(j+1)
                  pn(j+1) = pt(j+1)
                  x1 = (delf(j+1)-delf(j))/2.0
                  p1bar = p1bar + x1*(pt(j)+pt(j+1))
                  h1bar = h1bar + x1*(h(j,1)+h(j+1,1))
               ENDDO
               hbar = h1bar
               s1bar = alg3(p1bar,h1bar)
               pnbar = p1bar
               hnbar = h1bar
               snbar = s1bar
               l1keep = 1
            ENDIF
         ENDDO
         IF ( nbl/=0 ) THEN
            l1 = (ilast-1)/10 + 1
            CALL alg03(lnct,3+5*l1)
            IF ( iprtc==1 ) THEN
               WRITE (log2,99011)
99011          FORMAT (/10X,'ANNULUS WALL BOUNDARY LAYER CALCULATION RESULTS',/10X,47(1H*))
               DO k = 1 , l1
                  l2 = 10*(k-1) + 1
                  l3 = l2 + 9
                  IF ( l3>ilast ) l3 = ilast
                  WRITE (log2,99012) (i,i=l2,l3)
99012             FORMAT (/,' STATION NUMBER',14X,10I10)
                  WRITE (log2,99013) (delh(i),i=l2,l3)
99013             FORMAT (' HUB DISPLACEMENT THICKNESS',4X,10F10.5)
                  WRITE (log2,99014) (delt(i),i=l2,l3)
99014             FORMAT (' CASE DISPLACEMENT THICKNESS',3X,10F10.5)
                  WRITE (log2,99015) (wwbl(i),i=l2,l3)
99015             FORMAT (' BLOCKAGE AREA FRACTION',8X,10F10.5)
               ENDDO
            ENDIF
         ENDIF
         CALL alg03(lnct,4)
         IF ( iprtc==1 .AND. ivfail==0 .AND. iffail==0 ) WRITE (log2,99016) icase , ipass
99016    FORMAT (/10X,'POINT NO',I3,'   PASS',I3,'   THE CALCULATION IS ','CONVERGED',/10X,52(1H*))
         IF ( ifailo/=0 ) WRITE (log2,99017) icase , ipass , ifailo
99017    FORMAT (/10X,'POINT NO',I3,'   PASS',I3,'   THE CALCULATION FAIL','ED AT STATION',I3,/10X,60(1H*))
         IF ( ifailo==0 .AND. (ivfail/=0 .OR. iffail/=0) ) WRITE (log2,99018) icase , ipass , ivfail , iffail
99018    FORMAT (/10X,'POINT NO',I3,'   PASS',I3,'   THE CALCULATION IS ','NOT FULLY CONVERGED  IVFAIL =',I3,'  IFFAIL =',I3,/10X,  &
               & 88(1H*))
         power = flow(icase)*(hbar-h1bar)*ej/pfac
         IF ( iprtc==1 ) WRITE (log2,99019) spdfac(icase) , flow(icase) , opr , oeff , power
99019    FORMAT (10X,'SPEED FACTOR =',F10.3,'  FLOW =',F8.3,'  TOTAL PRES','SURE RATIO =',F7.3,'  ISENTROPIC EFFICIENCY =',F6.4,    &
                &'  POWER =',E11.4)
         IF ( iprtc==0 ) WRITE (log2,99020) icase , ipass , spdfac(icase) , flow(icase) , opr , oeff , power
99020    FORMAT (18H     FOR POINT NO.,I3,5H PASS,I3,15H - SPEED FACTOR,10X,1H=,F10.4/32X,4HFLOW,18X,1H=,F10.4,/32X,                &
                &23HTOTAL PRESSURE RATIO  =,F10.4,/32X,'ISENTROPIC ','EFFICIENCY =',F10.4,/32X,'POWER',17X,1H=,E10.4)
         IF ( ifailo/=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         l1 = 2
         spag_nextblock_1 = 2
      CASE (2)
         DO i = l1 , nstns
            nout3s = nout3(i)/10
            IF ( nout3s==0 ) nout3s = nout3(i)
            IF ( nout3s==1 .OR. nout3s==3 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 4
      CASE (3)
         l2 = i
         l3 = i + 1
         SPAG_Loop_1_1: DO i = l3 , nstns
            nout3s = nout3(i)/10
            nout3t = nout3(i) - nout3s*10
            IF ( nout3s==0 ) nout3t = 1
            IF ( nout3s==0 ) nout3s = nout3(i)
            IF ( nout3s==2 .OR. nout3s==3 ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
         l3 = i
         CALL alg03(lnct,10)
         IF ( iprtc==1 ) WRITE (log2,99021) l2 , l3
99021    FORMAT (/10X,'DATA FOR NASTRAN PROGRAM FOR BLADE BETWEEN STATIONS',I3,' AND',I3,/10X,61(1H*),//)
         IF ( nout3t/=2 ) THEN
            IF ( iprtc==1 ) WRITE (log2,99022)
99022       FORMAT (' NAME   CODE    DELTA P   ELEMENT',7X,'MESHPOINTS -  J   I',9X,'J   I',9X,'J   I',/)
            lnct = lnct - 4
            ielem = 0
            xsign = -float(nsign)
            l4 = l2 + 1
            idata(1) = name1(1)
            idata(2) = name1(2)
            idata(3) = 60
            DO j = 1 , itub
               DO i = l4 , l3
                  CALL alg03(lnct,2)
                  ielem = ielem + 1
                  l5 = i - 1
                  l6 = j + 1
                  IF ( i==l3 ) THEN
                     pload = xsign*((deltp(j,l5)+deltp(l6,l5))/3.0)
                     IF ( nblade(i)<0 ) pload = pload*0.75
                     IF ( iprtc==1 ) WRITE (log2,99028) pload , ielem , j , l5 , l6 , l5 , l6 , i
                     rdata(4) = pload
                     idata(5) = ielem
                     CALL write(iscr,idata,5,1)
                     ielem = ielem + 1
                     IF ( nblade(i)>=0 ) pload = xsign*(deltp(j,l5)/3.0)
                     IF ( iprtc==1 ) WRITE (log2,99028) pload , ielem , j , l5 , l6 , i , j , i
                     rdata(4) = pload
                     idata(5) = ielem
                     CALL write(iscr,idata,5,1)
                  ELSE
                     pload = xsign*((deltp(j,l5)+deltp(l6,l5)+deltp(l6,i))/3.0)
                     IF ( nblade(i)<0 ) pload = xsign*((deltp(j,l5)+deltp(j,i)+deltp(l6,l5)+deltp(l6,i))*0.25)
                     IF ( iprtc==1 ) WRITE (log2,99028) pload , ielem , l6 , l5 , l6 , i , j , l5
                     rdata(4) = pload
                     idata(5) = ielem
                     CALL write(iscr,idata,5,1)
                     ielem = ielem + 1
                     IF ( nblade(i)>=0 ) pload = xsign*((deltp(j,l5)+deltp(l6,i)+deltp(j,i))/3.0)
                     IF ( iprtc==1 ) WRITE (log2,99028) pload , ielem , j , l5 , l6 , i , j , i
                     rdata(4) = pload
                     idata(5) = ielem
                     CALL write(iscr,idata,5,1)
                  ENDIF
               ENDDO
            ENDDO
            l1 = l3
         ENDIF
         IF ( nout3t/=1 ) THEN
!
!     OUTPUT RELATIVE TOTAL TEMPERATURES AT NODES ON *TEMP* CARDS
!
            CALL alg03(lnct,10)
            lnct = lnct - 6
            IF ( iprtc==1 ) WRITE (log2,99023)
99023       FORMAT (//,' NAME   CODE    DELTA T   NODE',10X,'MESHPOINTS -  ','J   I   COORDINATES -   RADIAL       AXIAL',/)
            inode = 1
            idata(1) = name2(1)
            idata(2) = name2(2)
            idata(3) = 70
            DO j = 1 , nstrms
               DO i = l2 , l3
                  CALL alg03(lnct,1)
                  idata(4) = inode
                  rdata(5) = tr(j,i)
                  CALL write(iscr,idata,5,1)
                  IF ( iprtc==1 ) WRITE (log2,99024) tr(j,i) , inode , j , i , r(j,i) , x(j,i)
99024             FORMAT (' TEMP     70',F12.5,I6,21X,2I4,16X,F10.4,2X,F10.4)
                  inode = inode + 1
               ENDDO
            ENDDO
         ENDIF
         spag_nextblock_1 = 2
      CASE (4)
!
!     PUNCH STREAML2 BULK DATA CARDS FOR EACH STREAMLINE
!     CHANGE THE SIGN ON THE STAGGER AND FLOW ANGLES FOR STREAML2 CARDS.
!     THIS CHANGE IS NECESSARY BECAUSE OF THE AERODYNAMIC PROGRAMS IN
!     NASTRAN MODULE AMG THAT USE THESE ANGLES.
!
         IF ( ledgeb*itrleb/=0 ) THEN
            IF ( istrml/=-1 .AND. istrml/=1 ) THEN
               WRITE (log2,99025)
99025          FORMAT (//10X,47HNASTRAN - STREAML2 - COMPRESSOR BLADE BULK DATA,/10X,49(1H*),/,                                     &
                      &'  SLN  NSTNS  STAGGER    CHORD    RADIUS','    BSPACE     MACH       DEN       VEL      FLOWA',/)
               nstnsx = itrleb - ledgeb + 1
               DO ileb = 1 , nstrms
                  radius = (rmdv(ileb,5)+rmdv(ileb,6))/2.0
                  bspace = (6.283185*radius)/float(nbldes)
                  stag(ileb) = -1.0*stag(ileb)
                  rmdv(ileb,4) = -1.0*rmdv(ileb,4)
                  WRITE (lpunch,99026) ileb , nstnsx , stag(ileb) , chordd(ileb) , radius , bspace , rmdv(ileb,1) , rmdv(ileb,2) ,  &
                                     & ileb , ileb , rmdv(ileb,3) , rmdv(ileb,4)
99026             FORMAT (8HSTREAML2,2I8,F8.3,3F8.5,2F8.6,5H+STRL,I2,5H+STRL,I2,F8.1,F8.3)
                  WRITE (log2,99027) ileb , nstnsx , stag(ileb) , chordd(ileb) , radius , bspace , rmdv(ileb,1) , rmdv(ileb,2) ,    &
                                   & rmdv(ileb,3) , rmdv(ileb,4)
99027             FORMAT (I5,I6,2X,F8.3,3(2X,F8.5),2(2X,F8.6),2X,F8.1,2X,F8.3)
               ENDDO
            ENDIF
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99028 FORMAT (' PLOAD2   60',F12.5,I7,14X,3(I10,I4))
END SUBROUTINE alg11
