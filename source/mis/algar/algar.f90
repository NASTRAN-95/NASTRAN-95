!*==algar.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE algar
   USE c_contrl
   USE c_ud300c
   USE c_ud3prt
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(59,30) :: deltar
   INTEGER :: if , ifailk , j , k , kk , l1 , l2 , l3 , l4 , l5
   REAL , DIMENSION(59) :: pass
   REAL :: psmid , vmin , x1 , x2 , x3 , xn , xx
   REAL , DIMENSION(21) :: vmlold , vmold , xx1 , xx2 , xx3 , xx4
   EXTERNAL alg01 , alg02 , alg04 , alg05 , alg06 , alg07 , alg08 , alg09 , alg10 , alg11 , alg12 , alg25 , alg26 , alg3 , alg4 ,   &
          & alg5 , alg6
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
         if = 0
         log1 = loq1
         log2 = loq2
         log3 = loq3
         log5 = loq5
         log6 = loq6
         IF ( iprtc==1 ) WRITE (log2,99001)
99001    FORMAT (1HT)
         pi = 3.141592653589
         c1 = 180.0/pi
         hmin = 50.0
         vmin = 25.0
         IF ( iprtc==1 ) WRITE (log2,99002)
99002    FORMAT (1H1,37X,53HPROGRAM ALG - COMPRESSOR DESIGN - AERODYNAMIC SECTION,/,38X,53(1H*))
         lnct = 2
         CALL alg02
         icase = 1
         spag_nextblock_1 = 2
      CASE (2)
         IF ( iprtc==1 ) WRITE (log2,99003) icase
99003    FORMAT (1H1,9X,20HOUTPUT FOR POINT NO.,I2,/,10X,22(1H*))
         lnct = 2
         DO i = 1 , 30
            DO j = 1 , 59
               deltar(j,i) = 0.0
            ENDDO
         ENDDO
         IF ( .NOT.((icase==1 .AND. nread==1) .OR. (icase>1 .AND. ifailk==0)) ) THEN
            IF ( nsplit/=1 ) THEN
               l1 = nspec(1)
               xx1(1) = 0.0
               DO k = 2 , l1
                  xx1(k) = xx1(k-1) + sqrt((rstn(k)-rstn(k-1))**2+(xstn(k)-xstn(k-1))**2)
               ENDDO
               x1 = 1.0/xx1(l1)
               DO k = 2 , l1
                  xx1(k) = xx1(k)*x1
               ENDDO
               DO k = 1 , 11
                  xx2(k) = float(k-1)*0.1
               ENDDO
               CALL alg01(xx1,xstn,l1,xx2,xx3,x1,11,0,0)
               CALL alg01(xx1,rstn,l1,xx2,xx4,x1,11,0,0)
               DO k = 2 , 11
                  xx1(k) = xx1(k-1) + sqrt((xx3(k)-xx3(k-1))**2+(xx4(k)-xx4(k-1))**2)
                  xx3(k-1) = (xx1(k)+xx1(k-1))*0.5
               ENDDO
               l2 = is1(2)
               xx2(1) = atan2(rstn(l2)-rstn(1),xstn(l2)-xstn(1))
               l2 = l2 + nspec(2) - 1
               xx2(2) = atan2(rstn(l2)-rstn(l1),xstn(l2)-xstn(l1))
               xi(1) = 0.0
               xi(2) = xx1(11)
               CALL alg01(xi,xx2,2,xx3,phi,x1,10,1,0)
               CALL alg01(rstn,xstn,l1,xx3,x1,gama,10,0,1)
               xx3(1) = 0.0
               DO k = 2 , 11
                  xx3(k) = xx3(k-1) + cos(phi(k-1)+atan(gama(k-1)))*(xx4(k)+xx4(k-1))*(xx1(k)-xx1(k-1))
               ENDDO
               x1 = 1.0/xx3(11)
               x2 = 1.0/xx1(11)
               DO k = 2 , 11
                  xx1(k) = xx1(k)*x2
                  xx3(k) = xx3(k)*x1
               ENDDO
               x1 = 1.0/float(itub)
               DO k = 1 , nstrms
                  xx2(k) = float(k-1)*x1
               ENDDO
               CALL alg01(xx1,xx3,11,xx2,delf,x1,nstrms,1,0)
            ENDIF
            DO i = 1 , nstns
               l1 = is1(i)
               l2 = nspec(i)
               xx1(1) = 0.0
               vv(1) = 0.0
               DO k = 2 , l2
                  l3 = l1 + k - 1
                  vv(k) = vv(k-1) + sqrt((rstn(l3)-rstn(l3-1))**2+(xstn(l3)-xstn(l3-1))**2)
               ENDDO
               x1 = 1.0/vv(l2)
               DO k = 2 , l2
                  xx1(k) = vv(k)*x1
               ENDDO
               DO k = 1 , 11
                  xx2(k) = float(k-1)*0.1
               ENDDO
               CALL alg01(xx1,xstn(l1),l2,xx2,xx3,x1,11,0,0)
               CALL alg01(xx1,rstn(l1),l2,xx2,xx4,x1,11,0,0)
               DO k = 2 , 11
                  xx1(k) = xx1(k-1) + sqrt((xx3(k)-xx3(k-1))**2+(xx4(k)-xx4(k-1))**2)
                  gama(k-1) = (xx4(k)+xx4(k-1))*0.5
                  xx3(k-1) = (xx1(k)+xx1(k-1))*0.5
               ENDDO
               IF ( i/=1 .AND. i/=nstns ) THEN
                  l3 = is1(i+1)
                  l4 = is1(i-1)
                  l5 = l1
                  xx2(1) = (atan2(rstn(l3)-rstn(l5),xstn(l3)-xstn(l5))+atan2(rstn(l5)-rstn(l4),xstn(l5)-xstn(l4)))*0.5
                  l3 = l3 + nspec(i+1) - 1
                  l4 = l4 + nspec(i-1) - 1
                  l5 = l5 + l2 - 1
                  xx2(2) = (atan2(rstn(l3)-rstn(l5),xstn(l3)-xstn(l5))+atan2(rstn(l5)-rstn(l4),xstn(l5)-xstn(l4)))*0.5
               ELSEIF ( i==nstns ) THEN
                  l4 = is1(i-1)
                  xx2(1) = atan2(rstn(l1)-rstn(l4),xstn(l1)-xstn(l4))
                  l4 = l4 + nspec(i-1) - 1
                  l3 = l1 + l2 - 1
                  xx2(2) = atan2(rstn(l3)-rstn(l4),xstn(l3)-xstn(l4))
               ELSE
                  l3 = is1(2)
                  xx2(1) = atan2(rstn(l3)-rstn(1),xstn(l3)-xstn(1))
                  l4 = nspec(1)
                  l3 = l3 + nspec(2) - 1
                  xx2(2) = atan2(rstn(l3)-rstn(l4),xstn(l3)-xstn(l4))
               ENDIF
               xi(1) = 0.0
               xi(2) = xx1(11)
               CALL alg01(xi,xx2,2,xx3,phi,x1,10,1,0)
               CALL alg01(rstn(l1),xstn(l1),l2,gama,x1,gama,10,0,1)
               xx3(1) = 0.0
               DO k = 2 , 11
                  xx3(k) = xx3(k-1) + cos(phi(k-1)+atan(gama(k-1)))*(xx4(k)+xx4(k-1))*(xx1(k)-xx1(k-1))
               ENDDO
               x1 = 1.0/xx3(11)
               DO k = 2 , 11
                  xx3(k) = xx3(k)*x1
               ENDDO
               CALL alg01(xx3,xx1,11,delf,xl(1,i),x1,nstrms,1,0)
               x1 = vv(l2)/xx1(11)
               DO j = 2 , nstrms
                  xl(j,i) = xl(j,i)*x1
               ENDDO
               CALL alg01(vv,xstn(l1),l2,xl(1,i),x(1,i),x1,nstrms,0,0)
               CALL alg01(vv,rstn(l1),l2,xl(1,i),r(1,i),x1,nstrms,0,0)
            ENDDO
         ENDIF
         IF ( icase<=1 ) THEN
            x1 = (x(imid,2)-x(imid,1))**2 + (r(imid,2)-r(imid,1))**2
            drdm2(1) = ((r(nstrms,1)-r(1,1))**2+(x(nstrms,1)-x(1,1))**2)/x1
            l1 = nstns - 1
            DO i = 2 , l1
               x2 = (x(imid,i+1)-x(imid,i))**2 + (r(imid,i+1)-r(imid,i))**2
               x3 = x2
               IF ( x1<x3 ) x3 = x1
               drdm2(i) = ((r(nstrms,i)-r(1,i))**2+(x(nstrms,i)-x(1,i))**2)/x3
               x1 = x2
            ENDDO
            drdm2(nstns) = ((r(nstrms,nstns)-r(1,nstns))**2+(x(nstrms,nstns)-x(1,nstns))**2)/x2
         ENDIF
         DO i = 1 , nstns
            wwbl(i) = wblock(i)
         ENDDO
         ipass = 1
         spag_nextblock_1 = 3
      CASE (3)
         i = 1
         IF ( .NOT.((ipass>1 .OR. icase>1) .AND. ndata(1)==1) ) THEN
            l1 = ndimen(1) + 1
            IF ( l1==2 ) THEN
               DO j = 1 , nstrms
                  xx1(j) = r(j,1)/r(nstrms,1)
               ENDDO
            ELSEIF ( l1==3 ) THEN
               DO j = 1 , nstrms
                  xx1(j) = xl(j,1)
               ENDDO
            ELSEIF ( l1==4 ) THEN
               DO j = 1 , nstrms
                  xx1(j) = xl(j,1)/xl(nstrms,1)
               ENDDO
            ELSE
               DO j = 1 , nstrms
                  xx1(j) = r(j,1)
               ENDDO
            ENDIF
            l1 = nterp(1)
            l2 = ndata(1)
            CALL alg01(datac,data1,l2,xx1,s,x1,nstrms,l1,0)
            CALL alg01(datac,data2,l2,xx1,h,x1,nstrms,l1,0)
            CALL alg01(datac,data3,l2,xx1,tbeta,x1,nstrms,l1,0)
            DO j = 1 , nstrms
               h(j,1) = alg6(s(j,1),h(j,1))
               s(j,1) = alg3(s(j,1),h(j,1))
               tbeta(j,1) = tan(tbeta(j,1)/c1)
            ENDDO
         ENDIF
         IF ( ipass<=1 .AND. icase<=1 ) THEN
            x1 = flow(1)/(alg5(h,s)*pi*(r(nstrms,1)+r(1,1))*xl(nstrms,1))*sclfac**2
            DO j = 1 , nstrms
               vm(j,1) = x1
            ENDDO
            IF ( istag==1 ) vm(1,1) = 0.0
         ENDIF
         ifailo = 0
         iffail = 0
         ivfail = 0
         DO j = 1 , nstrms
            vmold(j) = vm(j,1)
         ENDDO
         spag_nextblock_1 = 5
      CASE (4)
         DO j = 1 , nstrms
            vwkeep(j) = vw(j,i-1)
            skeep(j) = s(j,i-1)
            hkeep(j) = h(j,i-1)
         ENDDO
         x1 = h(imid,i-1) - (vm(imid,i-1)**2+vw(imid,i-1)**2)/(2.0*g*ej)
         IF ( x1<hmin ) x1 = hmin
         psmid = alg4(x1,s(imid,i-1))
         IF ( nmix==1 ) CALL alg04(h(1,i-1),s(1,i-1),vw(1,i-1),r(1,i-1),r(1,i),x(1,i-1),x(1,i),vm(1,i-1),conmx,sclfac,g,ej,hmin,    &
                                 & vmin,psmid,nstrms,log2,lnct,if)
         IF ( if/=0 ) THEN
            ifailo = i - 1
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( nwork(i)==0 ) THEN
            DO j = 1 , nstrms
               h(j,i) = h(j,i-1)
               s(j,i) = s(j,i-1)
               vw(j,i) = 0.0
               IF ( i>istag .OR. j/=1 ) vw(j,i) = vw(j,i-1)*rim1(j)/r(j,i)
            ENDDO
         ELSE
            CALL alg05
            IF ( ntrans==1 .AND. ipass>1 ) CALL alg06(r(1,i-1),r(1,i),x(1,i-1),x(1,i),h(1,i),s(1,i),vm(1,i),tbeta(1,i-1),tbeta(1,i),&
               & loss,contr,sclfac,speed(i),spdfac(icase),g,ej,hmin,nstrms,pi)
            iter = 0
            CALL alg07
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         DO j = 1 , nstrms
            vmlold(j) = vm(j,i)
         ENDDO
         IF ( neqn>=2 ) THEN
            CALL alg26
         ELSE
            CALL alg08
         ENDIF
         IF ( neval(i)<=0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         iprint = 0
         CALL alg09
         IF ( ifailo/=0 .AND. ipass>nforce ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO j = 1 , nstrms
            IF ( abs(vm(j,i)/vmlold(j)-1.0)>tolnce/5.0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 8
      CASE (6)
         IF ( iloss<nliter(i) ) THEN
            iloss = iloss + 1
            DO j = 1 , nstrms
               vmlold(j) = vm(j,i)
            ENDDO
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         IF ( ipass>nforce ) THEN
            IF ( lnct+1>npage ) THEN
               IF ( iprtc==1 ) WRITE (log2,99004)
99004          FORMAT (1H1)
               lnct = 1
            ENDIF
            lnct = lnct + 1
            x1 = vm(1,i)/vmlold(1)
            x2 = vm(imid,i)/vmlold(imid)
            x3 = vm(nstrms,i)/vmlold(nstrms)
            IF ( iprtc==1 ) WRITE (log2,99005) ipass , i , x1 , x2 , x3
99005       FORMAT (5X,4HPASS,I3,9H  STATION,I3,66H  VM PROFILE NOT CONVERGED WITH LOSS RECALC   VM NEW/VM PREV  HUB=,F9.6,6H  MID=,&
                  & F9.6,7H  CASE=,F9.6)
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         IF ( nbl==1 .AND. (ifailo==0 .OR. ipass<=nforce) ) CALL alg10
         DO j = 1 , nstrms
            xim1(j) = x(j,i)
            rim1(j) = r(j,i)
            IF ( i/=istag .OR. j/=1 ) THEN
               IF ( abs(vm(j,i)/vmold(j)-1.0)>tolnce ) ivfail = ivfail + 1
               IF ( abs(delw(j)-delf(j))>tolnce ) iffail = iffail + 1
            ENDIF
         ENDDO
         IF ( .NOT.(nmax==1 .OR. (ipass==1 .AND. nread==1)) ) THEN
            x1 = fm2
            IF ( x1<1.0-xmmax ) x1 = 1.0 - xmmax
            x2 = 1.0
            IF ( i==1 .OR. nwork(i)>=5 ) x2 = 1.0 + tbeta(imid,i)**2
            x1 = 1.0/(1.0+x1*drdm2(i)/(rconst*x2))
            l3 = nstrms - 2
            CALL alg01(delw,xl(1,i),nstrms,delf(2),xx1(2),x1,l3,1,0)
            xx = xl(imid,i)
            DO j = 2 , itub
               xl(j,i) = xl(j,i) + x1*(xx1(j)-xl(j,i))
            ENDDO
            l1 = ipass
            IF ( l1>59 ) THEN
               l1 = 59
               DO k = 1 , 58
                  deltar(k,i) = deltar(k+1,i)
               ENDDO
            ENDIF
            deltar(l1,i) = xl(imid,i) - xx
            l1 = is1(i)
            l2 = nspec(i)
            xx1(1) = 0.0
            DO k = 2 , l2
               kk = l1 - 1 + k
               xx1(k) = xx1(k-1) + sqrt((xstn(kk)-xstn(kk-1))**2+(rstn(kk)-rstn(kk-1))**2)
            ENDDO
            CALL alg01(xx1,rstn(l1),l2,xl(2,i),r(2,i),x1,l3,0,0)
            CALL alg01(xx1,xstn(l1),l2,xl(2,i),x(2,i),x1,l3,0,0)
         ENDIF
         IF ( ipass<=nforce .OR. ifailo==0 ) THEN
            IF ( i/=nstns ) THEN
               i = i + 1
               IF ( ipass<=1 ) THEN
                  DO j = 1 , nstrms
                     vm(j,i) = vm(j,i-1)
                  ENDDO
                  IF ( i-1==istag ) vm(1,i) = vm(2,i)
                  IF ( i==istag ) vm(1,i) = 0.0
               ENDIF
               iloss = 1
               DO j = 1 , nstrms
                  vmold(j) = vm(j,i)
               ENDDO
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ipass<nmax ) THEN
               IF ( ifailo/=0 ) THEN
                  ipass = ipass + 1
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( ivfail/=0 .OR. iffail/=0 ) THEN
                  ipass = ipass + 1
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         CALL alg11
         l1 = nstns
         IF ( ifailo/=0 ) l1 = ifailo
         iprint = 1
         DO i = 2 , l1
            IF ( neval(i)/=0 ) CALL alg09
         ENDDO
         IF ( nplot/=0 ) CALL alg12
         IF ( ifailo==0 ) THEN
            IF ( npunch/=0 ) THEN
               WRITE (log3,99006) (delf(j),j=1,nstrms)
99006          FORMAT (6F12.8)
               WRITE (log3,99007) ((r(j,i),x(j,i),xl(j,i),i,j,j=1,nstrms),i=1,nstns)
99007          FORMAT (3F12.8,2I3)
            ENDIF
            DO i = 1 , nstns
               IF ( nout1(i)/=0 ) THEN
                  WRITE (log3,99008) (r(j,i),j,i,j=1,nstrms)
99008             FORMAT (F12.8,60X,2I4)
               ENDIF
            ENDDO
            l1 = log3
            IF ( narbit/=0 ) l1 = log6
            DO i = 1 , nstns
               IF ( nout2(i)/=0 ) THEN
                  l2 = is1(i)
                  l3 = l2 + nspec(i) - 1
                  WRITE (l1,99009) nspec(i) , (xstn(k),rstn(k),k=l2,l3)
99009             FORMAT (I3,/,(2F12.7))
                  xn = speed(i)
                  IF ( i/=nstns ) THEN
                     IF ( speed(i)/=speed(i+1) .AND. nwork(i+1)/=0 ) xn = speed(i+1)
                  ENDIF
                  xn = xn*spdfac(icase)*pi/(30.0*sclfac)
                  DO j = 1 , nstrms
                     xx1(j) = atan((vw(j,i)-xn*r(j,i))/vm(j,i))*c1
                  ENDDO
                  WRITE (l1,99010) (r(j,i),xx1(j),j,i,j=1,nstrms)
99010             FORMAT (2F12.8,48X,2I4)
               ENDIF
            ENDDO
         ENDIF
         IF ( nstplt/=0 ) THEN
            l1 = ipass
            IF ( l1>59 ) l1 = 59
            DO k = 1 , l1
               pass(k) = float(k)
            ENDDO
            DO k = 1 , nstns
               IF ( iprtc==1 ) WRITE (log2,99011) k
99011          FORMAT (1H1,53X,19HDELTA L FOR STATION,I3,/,2X)
               CALL alg25(l1,ipass,log2,pass,deltar(1,k))
            ENDDO
         ENDIF
         IF ( icase>=ncase ) THEN
            IF ( iprtc==1 ) WRITE (log2,99012)
99012       FORMAT (1HS)
         ELSE
            icase = icase + 1
            ifailk = ifailo
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE algar
