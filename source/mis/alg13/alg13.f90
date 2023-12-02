!*==alg13.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg13(Ibl,Ys,Yp,Xs,Xp,Ysemi,Xsemi,Log1,Log2,N,Iprint,Beta1,Beta2,P,Q,Yzero,T,Yone,Xdel,Ydel,Z,Axialc,Lnct,Ifcord,Sq,Sb,  &
               & Isecn,Xsemj,Ysemj,Istak,Xhere,X,Ss,Nstns,R,Dx,Y,Dy,Ss1,Bx,Sigma,Ccord,Isplit,Yzeros,Ts,Yones,Zspmxt,Perspj,Inast,  &
               & Irle,Irte,Tharr)
   USE c_udstr2
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ibl
   REAL , DIMENSION(21,80) :: Ys
   REAL , DIMENSION(21,80) :: Yp
   REAL , DIMENSION(21,80) :: Xs
   REAL , DIMENSION(21,80) :: Xp
   REAL , DIMENSION(21,31) :: Ysemi
   REAL , DIMENSION(21,31) :: Xsemi
   INTEGER :: Log1
   INTEGER :: Log2
   INTEGER :: N
   INTEGER :: Iprint
   REAL :: Beta1
   REAL :: Beta2
   REAL :: P
   REAL :: Q
   REAL :: Yzero
   REAL :: T
   REAL :: Yone
   REAL :: Xdel
   REAL :: Ydel
   REAL :: Z
   REAL :: Axialc
   INTEGER :: Lnct
   INTEGER :: Ifcord
   REAL :: Sq
   REAL :: Sb
   INTEGER :: Isecn
   REAL , DIMENSION(21,31) :: Xsemj
   REAL , DIMENSION(21,31) :: Ysemj
   INTEGER :: Istak
   REAL , DIMENSION(100) :: Xhere
   REAL , DIMENSION(100) :: X
   REAL , DIMENSION(100) :: Ss
   INTEGER :: Nstns
   REAL , DIMENSION(10,21) :: R
   REAL , DIMENSION(100) :: Dx
   REAL , DIMENSION(100) :: Y
   REAL , DIMENSION(100) :: Dy
   REAL , DIMENSION(80,4) :: Ss1
   REAL :: Bx
   REAL , DIMENSION(100) :: Sigma
   REAL , DIMENSION(1) :: Ccord
   INTEGER :: Isplit
   REAL :: Yzeros
   REAL :: Ts
   REAL :: Yones
   REAL :: Zspmxt
   REAL :: Perspj
   INTEGER :: Inast
   INTEGER :: Irle
   INTEGER :: Irte
   REAL , DIMENSION(21,10) :: Tharr
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , a1 , a2 , a4 , aform , ang , area , area2 , areas , at , ax , b , b1 , b2 , beta3 , bform , bta1 , bta2 , c , c1 ,   &
         & c2 , camber , cc1 , cc2 , chord , chords , cosang , ct , d , d2 , dela , delep , delly , delsig , delx , delxx , dt ,    &
         & e1 , eps , eps2 , et , fact , fcslmn , fcslms , ft , fypr , g1 , g2 , g3 , h , hh , ht , ipx , ipy , ix , ixd , ixn ,    &
         & ixy , ixyn , iy , iyd , iyn , oa , oa48 , phi1 , phi2 , phip , phipp , phis , phiss , phix , pi , r1 , r2 , r3 , rab ,   &
         & rdius , rdius1 , rdius2 , rle
   REAL , DIMENSION(80) :: am , s , thick2 , ym
   REAL :: f1 , f2 , f3 , f4 , f5 , f6 , f7 , f8
   INTEGER :: i1 , i2 , i3 , ik , isec1 , isec2 , j , jj , jm1 , k1 , k11 , kj , kl , km , mk , nasnum , nnn , nq
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(11) :: phi
   REAL :: rp , rs , rx1 , rx2 , rxbar , s0 , s2 , s23 , s3 , sbar , sigdum , sigmao , sn , sndum1 , sndum2 , ss2 , ssdum , sspls , &
         & ssurf , stager , t2 , tm , tprim2 , ts1 , x0 , x1 , x11 , x12 , xab , xbar , xbarb , xbarc , xbars , xbc , xc , xd ,     &
         & xdum , xint , xk2 , xml , xmlc , xmlcs , xmm , xn , xnorms , xrnge , xsurf , xtc , xx , y0 , y1 , y11 , y12 , y2 , y21 , &
         & y22 , ybar , ybarb , ybarc , ybars , yc , yint , yle , yml , ymm , yp1 , ypp , yprime , yss , ytc , yzs
   REAL , DIMENSION(10) :: snadum
   REAL , DIMENSION(45) :: sspltm , thick , xspltm , xspltp , xsplts , yspltm , yspltp , ysplts
   REAL , DIMENSION(81) :: xm , xxm
   EXTERNAL alg14 , alg15 , alg16 , alg18 , mesage
!
! End of declarations rewritten by SPAG
!
!
   DATA name/4HALG1 , 4H3   /
!
   f1(a) = a*exp(1.0-a*Sq)*Sq
   f2(a) = (Sq-1.0)*a*exp(1.0+a*(1.0-Sq))
   f3(a,b,c,d) = b/a**3*exp(a*xd)*(a*xd-2.0) + c*(xd+Sq) + d
   f4(a,b) = abs(a-b)/(a-b)
   f5(a,b,c) = b/a**2*exp(a*xd)*(a*xd-1.0) + c
   f6(xab) = sqrt(rdius**2-(xab-x1)**2) + y1
   f7(xab) = -sqrt(rdius**2-(xab-x1)**2) + y1
   f8(xab) = -1./sqrt(rdius**2-(xab-x1)**2)*(xab-x1)
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   INTEGER :: spag_nextblock_4
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         a = 0.
         d = 0.
         bta1 = Beta1
         bta2 = Beta2
         beta3 = 0.0
         pi = 3.1415926535
         c1 = 180.0/pi
         IF ( Iprint<2 ) THEN
            WRITE (Log2,99001) Ibl , P , Q , Beta1 , Beta2 , Yzero , T , Yone , Z , Axialc
99001       FORMAT (1H1,44X,43HSTREAMSURFACE GEOMETRY ON STREAMLINE NUMBER,I3,/45X,46(1H*),//20X,1HP,15X,1H=,F7.4,6X,               &
                   &72H(D2YDX2 OF MEANLINE AT LEADING EDGE AS A FRACTION OF ITS MAXIMUM VALUE.),/20X,1HQ,15X,1H=,F7.4,6X,           &
                   &73H(D2YDX2 OF MEANLINE AT TRAILING EDGE AS A FRACTION OF ITS MAXIMUM VALUE.),/20X,5HBETA1,11X,1H=,F7.3,6X,      &
                   &20H(BLADE INLET ANGLE.),/20X,5HBETA2,11X,1H=,F7.3,6X,21H(BLADE OUTLET ANGLE.),/20X,5HYZERO,11X,1H=,F8.5,5X,     &
                   &51H(BLADE LEADING EDGE RADIUS AS A FRACTION OF CHORD.),/20X,1HT,15X,1H=,F8.5,5X,                                &
                   &49H(BLADE MAXIMUM THICKNESS AS A FRACTION OF CHORD.),/20X,4HYONE,12X,1H=,F8.5,5X,                               &
                   &60H(BLADE TRAILING EDGE HALF-THICKNESS AS A FRACTION OF CHORD.),/20X,1HZ,15X,1H=,F7.4,6X,                       &
                   &59H(LOCATION OF MAXIMUM THICKNESS AS A FRACTION OF MEAN LINE.),/20X,4HCORD,12X,1H=,F7.4,6X,                     &
                   &39H(CHORD OR MERIDIONAL CHORD OF SECTION.))
            IF ( Isecn==1 .OR. Isecn==3 ) WRITE (Log2,99002) Sq , Sb
99002       FORMAT (20X,1HS,15X,1H=,F7.4,6X,53H(INFLECTION POINT AS A FRACTION OF MERIDIONAL CHORD.),/20X,5HBETA3,11X,1H=,F7.3,6X,  &
                   &36H(CHANGE IN ANGLE FROM LEADING EDGE.))
         ENDIF
         IF ( Iprint/=3 ) THEN
            Lnct = Lnct + 2
            IF ( Lnct>60 ) THEN
               Lnct = 3
               WRITE (Log2,99019)
            ENDIF
            WRITE (Log2,99003) Ibl , P , Q , Beta1 , Beta2 , Yzero , T , Yone , Z , Axialc
99003       FORMAT (2X,/5X,4HLINE,I3,4H  P=,F7.4,4H  Q=,F7.4,8H  BETA1=,F7.3,8H  BETA2=,F7.3,8H  YZERO=,F7.5,6H  T/C=,F7.5,         &
                  & 7H  YONE=,F7.5,4H  Z=,F7.4,6H  AXC=,F7.3)
         ENDIF
         IF ( Isecn==1 ) THEN
            nq = 1
            Sb = Beta1 + Sb
            g1 = 1.0/Sq
            r1 = f1(g1)
            g2 = g1 + 5.0
            r2 = f1(g2)
            s2 = f4(r2,P)
            SPAG_Loop_1_1: DO
               g3 = g2 + (P-r2)*(g2-g1)/(r2-r1)
               r3 = f1(g3)
               s3 = f4(r3,P)
               IF ( abs(r3-P)<=0.0001 ) THEN
                  a1 = g3
                  nq = 1
                  g1 = 1.0/(Sq-1.0)
                  r1 = f2(g1)
                  g2 = g1 - 5.0
                  r2 = f2(g2)
                  s2 = f4(r2,Q)
                  DO
                     g3 = g2 + (Q-r2)*(g2-g1)/(r2-r1)
                     r3 = f2(g3)
                     s3 = f4(r3,Q)
                     IF ( abs(r3-Q)<=0.0001 ) THEN
                        a2 = g3
                        b1 = a1**2*(tan(Beta1/c1)-tan(Sb/c1))/(1.0-(a1*Sq+1.0)*exp(-a1*Sq))
                        cc1 = tan(Sb/c1) + b1/a1**2
                        e1 = (a1*Sq+2.0)*b1/a1**3*exp(-a1*Sq)
                        b2 = a2**2*(tan(Beta2/c1)-tan(Sb/c1))/(1.0+(a2*(1.0-Sq)-1.0)*exp(a2*(1.0-Sq)))
                        cc2 = tan(Sb/c1) + b2/a2**2
                        d2 = 2.*(b2/a2**3-b1/a1**3) + Sq*(cc1-cc2) + e1
                        xd = 1.0 - Sq
                        r2 = f3(a2,b2,cc2,d2)
                        xmlc = sqrt(1.0+r2**2)
                        EXIT SPAG_Loop_1_1
                     ELSE
                        IF ( nq>50 ) THEN
                           spag_nextblock_1 = 9
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        nq = nq + 1
                        IF ( abs(s2-s3)<=0.0001 ) THEN
                           g2 = g3
                           r2 = r3
                           s2 = s3
                        ELSE
                           g1 = g3
                           r1 = r3
                        ENDIF
                     ENDIF
                  ENDDO
               ELSE
                  IF ( nq>50 ) THEN
                     spag_nextblock_1 = 9
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  nq = nq + 1
                  IF ( abs(s2-s3)<=0.0001 ) THEN
                     g2 = g3
                     r2 = r3
                     s2 = s3
                  ELSE
                     g1 = g3
                     r1 = r3
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_1
         ELSEIF ( Isecn==3 ) THEN
            i1 = 1
            beta3 = Beta1 + Sb
            s0 = 0.
            x0 = 0.
            y0 = 0.
            y21 = 0.0
            i2 = float(N)*Sq
            IF ( i2<=1 ) Sq = 0.0
            IF ( i2<=1 ) beta3 = Beta1
            IF ( i2>1 ) THEN
               xrnge = Sq
               fact = Sq
               CALL alg18(Beta1,beta3,i1,i2,fact,x0,y0,s0,xrnge,y11,x11,y21,rdius1,s,c1)
               i1 = i2
               x0 = Sq
               y0 = y21
               s0 = s(i1)
            ENDIF
            i2 = N
            fact = 1. - Sq
            xrnge = fact
            CALL alg18(beta3,Beta2,i1,i2,fact,x0,y0,s0,xrnge,y12,x12,y22,rdius2,s,c1)
            xmlc = sqrt(1.0+y22**2)
         ELSEIF ( Isecn==2 ) THEN
            CALL alg18(Beta1,Beta2,1,N,1.0,0.0,0.0,0.0,1.0,y1,x1,y2,rdius,s,c1)
            xmlc = sqrt(1.+y2**2)
            chord = xmlc/(1.-2.*Yzero*(1.-xmlc))
            fcslmn = 1.0 - chord*2.*Yzero
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            h = 1.0/(1.0+sqrt((1.0-Q)/(1.0-P)))
            hh = h*h
            oa = 4.0*(tan(Beta1/c1)-tan(Beta2/c1))/(P/(1.0-P)*hh+h-1.0/3.0)
            oa48 = oa/48.0
            xk2 = -hh/(8.0*(1.0-P))*oa
            b = hh*h/12.0*oa + tan(Beta1/c1)
            c = -hh*hh*oa48
            xmlc = sqrt(1.0+(oa48*(1.0-h)**4+xk2+b+c)**2)
         ENDIF
         chord = xmlc/(1.0-Yzero+xmlc*(Yzero+abs(Yone*sin(Beta2/c1))))
         fcslmn = 1.0 - chord*(Yzero+abs(Yone*sin(Beta2/c1)))
         spag_nextblock_1 = 2
      CASE (2)
         IF ( Ifcord==1 ) Axialc = Axialc/chord
         Yzero = Yzero*chord/fcslmn
         Yone = Yone*chord/fcslmn
         T = T*chord/fcslmn
         s(1) = 0.0
         xx = 0.0
         xn = N
         IF ( Isecn/=2 ) THEN
            at = (Yzero-T/2.0)/(2.0*Z**3)
            ct = (T/2.0-Yzero)*3.0/(2.0*Z)
            dt = Yzero
            et = (Yone-T/2.0)/(1.0-Z)**3 - 1.5*(Yzero-T/2.0)/(Z**2*(1.0-Z))
            ft = 1.5*(Yzero-T/2.0)/Z**2
            ht = T/2.0
            IF ( Isecn/=3 ) THEN
               delx = 1.0/(10.0*(xn-1.0))
               ASSIGN 2 TO isec1
               ASSIGN 6 TO isec2
               IF ( Isecn/=0 ) THEN
                  ASSIGN 4 TO isec1
                  ASSIGN 8 TO isec2
               ENDIF
               DO j = 2 , N
                  DO jj = 1 , 11
                     spag_nextblock_2 = 1
                     SPAG_DispatchLoop_2: DO
                        SELECT CASE (spag_nextblock_2)
                        CASE (1)
                           GOTO isec1
 2                         phi(jj) = sqrt(1.0+(oa/12.0*(xx-h)**3+xk2*2.0*xx+b)**2)
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
 4                         xd = xx - Sq
                           IF ( xd>0.0 ) THEN
                              phi(jj) = sqrt(1.+(f5(a2,b2,cc2))**2)
                           ELSE
                              phi(jj) = sqrt(1.0+(f5(a1,b1,cc1))**2)
                           ENDIF
                           spag_nextblock_2 = 2
                        CASE (2)
                           xx = xx + delx
                           EXIT SPAG_DispatchLoop_2
                        END SELECT
                     ENDDO SPAG_DispatchLoop_2
                  ENDDO
                  xx = xx - delx
                  s(j) = s(j-1) + (phi(1)+phi(11)+4.0*(phi(2)+phi(4)+phi(6)+phi(8)+phi(10))+2.0*(phi(3)+phi(5)+phi(7)+phi(9)))      &
                       & /(30.0*(xn-1.0))
               ENDDO
            ENDIF
         ENDIF
         delx = 1.0/(xn-1.0)
         IF ( Isecn==2 ) THEN
            t2 = T/2.
            tprim2 = t2 - Yzero
            c2 = 2.*c1
            aform = (tprim2+rdius*(1.-cos((Beta1-Beta2)/c2)))/xmlc*2.
            phis = acos((1.-aform**2)/(1.+aform**2))
            rs = Yzero + xmlc/2./sin(phis)
            yss = rdius - rs + t2
            bform = (rdius*(1.-cos((Beta1-Beta2)/c2))-tprim2)/xmlc*2.
            phip = acos((1.-bform**2)/(1.+bform**2))
            phi2 = abs((Beta1-Beta2)/c1)
         ENDIF
         xm(1) = 0.0
         IF ( Isecn==3 ) THEN
            ymm = 0.0
            xmm = 0.0
            i2 = Sq*float(N)
            i3 = i2
            IF ( i2<=1 ) i2 = N + 1
            delx = Sq/float(i2-1)
            IF ( i3/=i2 ) i3 = 1
            delxx = (1.-Sq)/float(N-i3)
            IF ( i2==N+1 ) delx = delxx
         ENDIF
         DO j = 1 , N
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
                  sn = s(j)/s(N)
                  IF ( Isecn==2 ) THEN
                     phix = (sn-0.5)*phi2
                     thick2(j) = yss*cos(phix) + sqrt(rs**2-yss**2*sin(phix)**2) - rdius
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
                  ELSE
                     IF ( sn>Z ) THEN
                        sn = sn - Z
                        thick2(j) = (et*sn+ft)*sn**2 + ht
                     ELSE
                        thick2(j) = (at*sn**2+ct)*sn + dt
                     ENDIF
                     IF ( Isecn/=3 ) THEN
                        GOTO isec2
                     ELSEIF ( xm(j)-Sq>0.0 .OR. xm(j)==0.0 .AND. Sq==0.0 ) THEN
                        IF ( Beta2==beta3 ) THEN
                           spag_nextblock_3 = 3
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
                        rdius = rdius2
                        x1 = x12
                        y1 = y12
                        bta1 = beta3
                        bta2 = Beta2
                        spag_nextblock_3 = 2
                        CYCLE SPAG_DispatchLoop_3
                     ELSE
                        IF ( Beta1==beta3 ) THEN
                           spag_nextblock_3 = 3
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
                        bta1 = Beta1
                        bta2 = beta3
                        rdius = rdius1
                        y1 = y11
                        x1 = x11
                        spag_nextblock_3 = 2
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                  ENDIF
 6                ym(j) = oa48*(xm(j)-h)**4 + xk2*xm(j)**2 + b*xm(j) + c
                  yprime = oa/12.0*(xm(j)-h)**3 + xk2*2.0*xm(j) + b
                  spag_nextblock_3 = 4
                  CYCLE SPAG_DispatchLoop_3
 8                xd = xm(j) - Sq
                  IF ( xd>0.0 ) THEN
                     ym(j) = f3(a2,b2,cc2,d2)
                     yprime = f5(a2,b2,cc2)
                  ELSE
                     ym(j) = f3(a1,b1,cc1,e1)
                     yprime = f5(a1,b1,cc1)
                  ENDIF
                  spag_nextblock_3 = 4
               CASE (2)
                  ym(j) = f6(xm(j))
                  yprime = f8(xm(j))
                  IF ( bta1-bta2<0.0 ) yprime = -yprime
                  IF ( bta1-bta2<0.0 ) ym(j) = f7(xm(j))
                  IF ( Isecn/=2 ) THEN
                     IF ( j==i3 ) delx = delxx
                  ENDIF
                  spag_nextblock_3 = 4
               CASE (3)
                  yprime = tan(beta3/c1)
                  IF ( j/=1 ) xmm = xm(j-1)/fcslmn - Yzero
                  IF ( j/=1 ) ymm = ym(j-1)/fcslmn
                  ym(j) = yprime*(xm(j)-xmm) + ymm
                  IF ( j==i3 ) delx = delxx
                  spag_nextblock_3 = 4
               CASE (4)
                  xm(j+1) = xm(j) + delx
                  fypr = 1.0/sqrt(1.0+yprime**2)
                  Xs(Ibl,j) = (xm(j)-thick2(j)*yprime*fypr+Yzero)*fcslmn
                  Ys(Ibl,j) = (ym(j)+thick2(j)*fypr)*fcslmn
                  Xp(Ibl,j) = (xm(j)+thick2(j)*yprime*fypr+Yzero)*fcslmn
                  Yp(Ibl,j) = (ym(j)-thick2(j)*fypr)*fcslmn
                  am(j) = atan(yprime)*c1
                  xxm(j) = xm(j)
                  IF ( j==N ) stager = atan(ym(N)/xm(N))*c1
                  IF ( j==N ) stag(Ibl) = stager
                  xm(j) = (xm(j)+Yzero)*fcslmn
                  ym(j) = ym(j)*fcslmn
                  thick2(j) = thick2(j)*fcslmn
                  s(j) = s(j)*fcslmn
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO
         IF ( Isplit/=0 ) THEN
            xspltm(1) = 1. - Perspj
            k1 = 25
            xspltm(k1) = 1.
            k11 = k1 - 1
            delxx = Perspj/float(k11)
            DO j = 2 , k11
               xspltm(j) = xspltm(j-1) + delxx
            ENDDO
            CALL alg15(xm,ym,N,xspltm,yspltm,k1,1)
            yle = yspltm(1)
            CALL alg15(xm,s,N,xspltm,sspltm,k1,1)
            CALL alg15(xm,am,N,xspltm,Ss1(1,3),k1,1)
            sspls = sspltm(1)
            DO j = 1 , k1
               sspltm(j) = sspltm(j) - sspls
            ENDDO
            IF ( Isplit==2 ) THEN
               yzs = Yzeros
               ts1 = Ts
               Beta1 = Ss1(1,3)
               y1 = -cos(Beta1/c1)/(sin(Beta1/c1)-sin(Beta2/c1))
               x1 = sin(Beta1/c1)/(sin(Beta1/c1)-sin(Beta2/c1))
               rdius = abs(1./(sin(Beta1/c1)-sin(Beta2/c1)))
               y2 = tan((Beta1+Beta2)/(2.*c1))
               xmlcs = sqrt(1.+y2**2)
               chords = xmlcs/(1.0-2.*Yzeros*(1.0-xmlcs))
               fcslms = 1.0 - chords*2.*Yzeros
               Yzeros = Yzeros*chords/fcslms
               Ts = Ts*chords/fcslms
               Ss1(1,1) = 0.
               delx = 1./(xn-1.)
               t2 = Ts/2.
               tprim2 = t2 - Yzeros
               c2 = 2.*c1
               aform = (tprim2+rdius*(1.-cos((Beta1-Beta2)/c2)))/xmlcs*2.
               phis = acos((1.-aform**2)/(1.+aform**2))
               rs = Yzeros + xmlcs/2./sin(phis)
               yss = rdius - rs + t2
               bform = (rdius*(1.-cos((Beta1-Beta2)/c2))-tprim2)/xmlcs*2.
               phip = acos((1.-bform**2)/(1.+bform**2))
               rp = xmlcs/2./sin(phip) - Yzeros
               ypp = rdius - rp - t2
               xx = 0.
               DO j = 2 , N
                  xx = xx + delx
                  phi1 = atan(-1./sqrt(rdius**2-(xx-x1)**2)*(xx-x1))
                  IF ( Beta1<0. ) phi1 = -phi1
                  phi2 = abs(Beta1/c1-phi1)
                  Ss1(j,1) = rdius*phi2
               ENDDO
               DO j = 1 , N
                  Ss1(j,1) = Ss1(j,1)/Ss1(N,1)
                  phix = (Ss1(j,1)-.5)*phi2
                  Ss1(j,2) = (yss*cos(phix)+sqrt(rs**2-yss**2*sin(phix)**2)-rdius)/t2
               ENDDO
               CALL alg14(xspltm,yspltm,k1,xspltm,xdum,Ss1(1,3),k1,1)
               xnorms = sqrt(Perspj**2+(yspltm(k1)-yspltm(1))**2)
               chords = xnorms/(1.-2.*yzs*(1.-xnorms))
               fcslms = (Perspj-chords*2.*yzs)/Perspj
               Ts = ts1*chords/fcslms
               Yzeros = yzs*chords/fcslms
            ELSE
               xnorms = sqrt((xspltm(k1)-xspltm(1))**2+(yspltm(k1)-yspltm(1))**2)
               chords = xnorms/(1.-Yzeros+xnorms*(Yzeros+abs(Yones*sin(Beta2/c1))))
               fcslms = (Perspj-chords*(Yzeros+abs(Yones*sin(Beta2/c1))))/Perspj
               Yzeros = Yzeros*chords/fcslms
               Yones = Yones*chords/fcslms
               Ts = Ts*chords/fcslms
               at = (Yzeros-Ts/2.)/(2.*Zspmxt**3)
               ct = (Ts/2.-Yzeros)*3./(2.*Zspmxt)
               dt = Yzeros
               et = (Yones-Ts/2.)/(1.-Zspmxt)**3 - 1.5*(Yzeros-Ts/2.)/(Zspmxt**2*(1.-Zspmxt))
               ft = 1.5*(Yzeros-Ts/2.)/Zspmxt**2
               ht = Ts/2.
            ENDIF
            DO j = 1 , k1
               sn = sspltm(j)/sspltm(k1)
               IF ( Isplit>1 ) THEN
                  CALL alg15(Ss1,Ss1(1,2),N,sn,thick(j),1,1)
                  thick(j) = thick(j)*Ts/2.
                  fypr = 1.0/sqrt(1.0+Ss1(j,3)**2)
                  yprime = Ss1(j,3)
               ELSE
                  IF ( sn>Zspmxt ) THEN
                     sn = sn - Zspmxt
                     thick(j) = (et*sn+ft)*sn**2 + ht
                  ELSE
                     thick(j) = (at*sn**2+ct)*sn + dt
                  ENDIF
                  fypr = 1./sqrt(1.+tan(Ss1(j,3)/c1)**2)
                  yprime = tan(Ss1(j,3)/c1)
               ENDIF
               xspltp(j) = (xspltm(j)-(1.-Perspj)+thick(j)*yprime*fypr+Yzeros)*fcslms + (1.-Perspj)
               xsplts(j) = (xspltm(j)-(1.-Perspj)-thick(j)*yprime*fypr+Yzeros)*fcslms + (1.-Perspj)
               yspltp(j) = (yspltm(j)-yle-thick(j)*fypr)*fcslms + yle
               ysplts(j) = (yspltm(j)-yle+thick(j)*fypr)*fcslms + yle
               xspltm(j) = (xspltm(j)-(1.-Perspj)+Yzeros)*fcslms + (1.-Perspj)
               yspltm(j) = (yspltm(j)-yle)*fcslms + yle
               thick(j) = thick(j)*fcslms
               sspltm(j) = sspltm(j)*fcslms
            ENDDO
            IF ( Isplit>1 ) Ss1(1,3) = atan(Ss1(1,3))*c1
            Yzeros = Yzeros*fcslms
            areas = pi/2.*Yzeros**2
            area2 = areas
            yint = -4./(3.*pi)*Yzeros*areas*sin(Ss1(1,3)/c1)
            xint = Yzeros*(1.-cos(Ss1(1,3)/c1)*4./(3.*pi))*areas
            DO j = 2 , k1
               dela = (thick(j)+thick(j-1))*(sspltm(j)-sspltm(j-1))
               areas = areas + dela
               xint = xint + dela*(xspltm(j)+xspltm(j-1))/2.
               yint = yint + dela*(yspltm(j)+yspltm(j-1))/2.
            ENDDO
            IF ( Isplit>=2 ) THEN
               xint = xint + area2*(xspltm(k1)+4.*Yzeros/(3.*pi)*cos(Beta2/c1))
               yint = yint + area2*(yspltm(k1)+4.*Yzeros/(3.*pi)*sin(Beta2/c1))
               areas = areas + area2
            ENDIF
            xbars = xint/areas
            ybars = yint/areas
         ENDIF
         Yzero = Yzero*fcslmn
         IF ( Inast/=0 ) THEN
            nasnum = Irte - Irle + 1
            CALL alg15(X,Ss,100,Xhere(Irle),snadum(Irle),nasnum,1)
            sndum1 = snadum(Irle)
            sndum2 = snadum(Irte)
            DO j = Irle , Irte
               snadum(j) = (snadum(j)-sndum1)/(sndum2-sndum1)
               CALL alg15(xxm,thick2,N,snadum(j),Tharr(Ibl,j),1,1)
               Tharr(Ibl,j) = Tharr(Ibl,j)*2.*Axialc
            ENDDO
         ENDIF
         area = pi/2.0*Yzero**2
         xint = Yzero*(1.0-cos(Beta1/c1)*4.0/(3.0*pi))*area
         yint = -4.0/(3.0*pi)*Yzero*area*sin(Beta1/c1)
         DO j = 2 , N
            dela = (thick2(j)+thick2(j-1))*(s(j)-s(j-1))
            area = area + dela
            xint = xint + dela*(xm(j)+xm(j-1))/2.0
            yint = yint + dela*(ym(j)+ym(j-1))/2.0
         ENDDO
         IF ( Isecn==2 ) THEN
            area2 = pi/2.*Yzero**2
            xint = xint + area2*(xm(N)+4.*Yzero/(3.*pi)*cos(Beta2/c1))
            yint = yint + area2*(ym(N)+4.*Yzero/(3.*pi)*sin(Beta2/c1))
            area = area + area2
         ENDIF
         xbar = xint/area
         ybar = yint/area
         xbarb = xbar
         ybarb = ybar
         ybar = ybar + Ydel/Axialc
         xbar = xbar + Xdel/Axialc
         ax = 1./99.
         Dx(1) = 0.
         DO ik = 2 , 100
            Dx(ik) = Dx(ik-1) + ax
         ENDDO
         ymm = 0.0
         xmm = 0.0
         DO ik = 1 , 100
            spag_nextblock_4 = 1
            SPAG_DispatchLoop_4: DO
               SELECT CASE (spag_nextblock_4)
               CASE (1)
                  xab = Dx(ik)
                  IF ( Isecn/=0 ) THEN
                     IF ( Isecn==1 ) THEN
                        xd = xab - Sq
                        IF ( xd>0. ) THEN
                           Y(ik) = f3(a2,b2,cc2,d2)*fcslmn
                           Ss1(ik,1) = f5(a2,b2,cc2)
                        ELSE
                           Y(ik) = f3(a1,b1,cc1,e1)*fcslmn
                           Ss1(ik,1) = f5(a1,b1,cc1)
                        ENDIF
                        spag_nextblock_4 = 4
                        CYCLE SPAG_DispatchLoop_4
                     ELSE
                        IF ( Isecn==2 ) THEN
                           spag_nextblock_4 = 2
                           CYCLE SPAG_DispatchLoop_4
                        ENDIF
                        IF ( Isecn==3 ) THEN
                           IF ( xab-Sq>0.0 .OR. xab==0.0 .AND. Sq==0.0 ) THEN
                              IF ( Beta2==beta3 ) THEN
                                 spag_nextblock_4 = 3
                                 CYCLE SPAG_DispatchLoop_4
                              ENDIF
                              rdius = rdius2
                              x1 = x12
                              y1 = y12
                              bta1 = beta3
                              bta2 = Beta2
                              spag_nextblock_4 = 2
                           ELSE
                              IF ( Beta1==beta3 ) THEN
                                 spag_nextblock_4 = 3
                                 CYCLE SPAG_DispatchLoop_4
                              ENDIF
                              rdius = rdius1
                              x1 = x11
                              y1 = y11
                              bta1 = Beta1
                              bta2 = beta3
                              spag_nextblock_4 = 2
                           ENDIF
                           CYCLE
                        ENDIF
                     ENDIF
                  ENDIF
                  Y(ik) = (oa48*(xab-h)**4+xab**2*xk2+b*xab+c)*fcslmn
                  Ss1(ik,1) = oa/12.*(xab-h)**3 + xk2*2.*xab + b
                  spag_nextblock_4 = 4
               CASE (2)
                  Y(ik) = f6(xab)*fcslmn
                  Ss1(ik,1) = f8(xab)
                  IF ( bta1-bta2<0.0 ) Ss1(ik,1) = -Ss1(ik,1)
                  IF ( bta1-bta2<0.0 ) Y(ik) = f7(xab)*fcslmn
                  spag_nextblock_4 = 4
               CASE (3)
                  Ss1(ik,1) = tan(beta3/c1)
                  IF ( ik/=1 ) ymm = Y(ik-1)/fcslmn
                  IF ( ik/=1 ) xmm = Dx(ik-1)
                  Y(ik) = (Ss1(ik,1)*(xab-xmm)+ymm)*fcslmn
                  spag_nextblock_4 = 4
               CASE (4)
                  Sigma(ik) = Dx(ik)*fcslmn + Yzero
                  EXIT SPAG_DispatchLoop_4
               END SELECT
            ENDDO SPAG_DispatchLoop_4
         ENDDO
         CALL alg15(Sigma,Y,100,Dx,Dy,100,1)
         CALL alg15(Sigma,Ss1(1,1),100,Dx,Y,100,1)
         CALL alg15(Dx,Dy,100,xbar,xab,1,1)
         CALL alg15(Dx,Y,100,xbar,xbc,1,1)
         xbar = xbarb
         ybar = ybarb
         ix = 0.0
         iy = 0.0
         ixy = 0.0
         DO j = 2 , N
            dela = (thick2(j)+thick2(j-1))*(s(j)-s(j-1))
            ixd = (thick2(j)+thick2(j-1))**3*(s(j)-s(j-1))/12.0
            iyd = (thick2(j)+thick2(j-1))*(s(j)-s(j-1))**3/12.0
            cosang = cos((am(j)+am(j-1))/c1)
            ixn = (ixd+iyd+(ixd-iyd)*cosang)/2.0
            iyn = (ixd+iyd-(ixd-iyd)*cosang)/2.0
            ixyn = 0.0
            IF ( am(j)+am(j-1)/=0.0 ) ixyn = ((ixn-iyn)*cosang-ixd+iyd)/(2.0*sin((am(j)+am(j-1))/c1))
            ix = ix + ixn + dela*((ym(j)+ym(j-1))/2.0-ybar)**2
            iy = iy + iyn + dela*((xm(j)+xm(j-1))/2.0-xbar)**2
            ixy = ixy + ixyn + dela*(ybar-(ym(j)+ym(j-1))/2.0)*(xbar-(xm(j)+xm(j-1))/2.0)
         ENDDO
         ang = atan(2.0*ixy/(iy-ix))
         ipx = (ix+iy)/2.0 + (ix-iy)/2.0*cos(ang) - ixy*sin(ang)
         ipy = (ix+iy)/2.0 - (ix-iy)/2.0*cos(ang) + ixy*sin(ang)
         ang = ang/2.0*c1
         xml = xm(N)
         yml = ym(N)
         camber = Beta1 - Beta2
         IF ( Iprint<2 ) THEN
            Lnct = 47
            IF ( Isecn==1 .OR. Isecn==3 ) Lnct = 49
            WRITE (Log2,99004) chord , stager , camber , area , xbar , ybar , ix , iy , ixy , ang , ipx , ang , ipy , ang
99004       FORMAT (/16X,100HNORMALISED RESULTS - ALL THE FOLLOWING REFER TO ABLADE HAVING A MERIDIONAL CHORD PROJECTION OF UNITY,  &
                  & /16X,100(1H*),//20X,11HBLADE CHORD,4X,1H=,F7.4,//20X,16HSTAGGER ANGLE  =,F7.3,//20X,16HCAMBER ANGLE   =,F7.3,   &
                  & //20X,16HSECTION AREA   =,F7.5,//20X,45HLOCATION OF CENTROID RELATIVE TO LEADING EDGE,//30X,6HXBAR =,F8.5,/30X, &
                   &6HYBAR =,F8.5,//20X,37HSECOND MOMENTS OF AREA ABOUT CENTROID,//30X,6HIX   =,F8.5,/30X,6HIY   =,F8.5,/30X,       &
                   &6HIXY  =,F8.5,//20X,58HANGLE OF INCLINATION OF (ONE) PRINCIPAL AXIS TO  X  AXIS =,F7.3,//20X,                   &
                   &47HPRINCIPAL SECOND MOMENTS OF AREA ABOUT CENTROID,//30X,6HIPX  =,F7.5,6X,3H(AT,F7.3,15H WITH  X  AXIS),/30X,   &
                   &6HIPY  =,F7.5,6X,3H(AT,F7.3,15H WITH  Y  AXIS),//)
            WRITE (Log2,99020)
            DO j = 1 , N
               IF ( Lnct==60 ) THEN
                  WRITE (Log2,99019)
                  WRITE (Log2,99020)
                  Lnct = 4
               ENDIF
               Lnct = Lnct + 1
               tm = thick2(j)*2.0
               WRITE (Log2,99021) j , xm(j) , ym(j) , am(j) , tm , Xs(Ibl,j) , Ys(Ibl,j) , Xp(Ibl,j) , Yp(Ibl,j)
            ENDDO
            IF ( Isplit/=0 ) THEN
               IF ( Lnct>40 ) THEN
                  WRITE (Log2,99019)
                  Lnct = 1
               ENDIF
               WRITE (Log2,99005)
99005          FORMAT (//10X,20HSPLITTER COORDINATES,/10X,21(1H*),//2X)
               WRITE (Log2,99020)
               Lnct = Lnct + 11
               N = k1
               DO j = 1 , N
                  tm = thick(j)*2.
                  Xs(Ibl,j) = xsplts(j)
                  Xp(Ibl,j) = xspltp(j)
                  Yp(Ibl,j) = yspltp(j)
                  Ys(Ibl,j) = ysplts(j)
                  WRITE (Log2,99021) j , xspltm(j) , yspltm(j) , Ss1(j,3) , tm , Xs(Ibl,j) , Ys(Ibl,j) , Xp(Ibl,j) , Yp(Ibl,j)
                  Lnct = Lnct + 1
                  IF ( Lnct>60 ) THEN
                     WRITE (Log2,99019)
                     WRITE (Log2,99020)
                     Lnct = 4
                  ENDIF
               ENDDO
            ENDIF
            DO j = 1 , N
               xm(j) = Xs(Ibl,j)
               ym(j) = Ys(Ibl,j)
               am(j) = Xp(Ibl,j)
               thick2(j) = Yp(Ibl,j)
            ENDDO
            WRITE (Log2,99006) Ibl
99006       FORMAT (1H1,45X,33HNORMALISED PLOT OF SECTION NUMBER,I3,/2X)
            CALL alg16(N,Log2,xm,ym,am,thick2)
         ENDIF
         a2 = Axialc**2
         a4 = a2**2
         ix = ix*a4
         iy = iy*a4
         ixy = ixy*a4
         ipx = ipx*a4
         ipy = ipy*a4
         IF ( Istak<=1 ) THEN
            xbar = Istak
            IF ( Istak==0 ) ybar = 0.
            IF ( Istak==1 ) ybar = yml
         ENDIF
         rle = Yzero*Axialc
         IF ( Isplit/=0 ) THEN
            rle = Yzeros*Axialc
            chord = chords*Axialc
            areas = areas*Axialc**2
            xc = (xspltm(1)-xbar)*Axialc - Xdel
            yc = (yspltm(1)-ybar)*Axialc - Ydel
            xtc = (xspltm(k1)-xbar)*Axialc - Xdel
            ytc = (yspltm(k1)-ybar)*Axialc - Ydel
            xbars = (xbars-xbar)*Axialc - Xdel
            ybars = (ybars-ybar)*Axialc - Ydel
            IF ( Iprint>=2 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Isplit==2 ) THEN
               WRITE (Log2,99007) chord , rle , xc , yc , xtc , ytc , xbars , ybars , areas
99007          FORMAT (1H1,31X,69HDIMENSIONAL RESULTS - ALL RESULTS REFER TO A BLADE OF SPECIFIED CHORD,/32X,69(1H*),//20X,         &
                      &11HBLADE CHORD,4X,1H=,1P,E12.5,//20X,10HEND RADIUS,5X,1H=,1P,E12.5,8X,14HCENTERED AT X=,1P,E12.5,3H Y=,1P,   &
                     & E13.5,/64X,6HAND X=,1P,E12.5,3H Y=,1P,E13.5,/20X,26HLOCATION OF CENTROID AT X=,1P,E12.5,7H AND Y=,1P,E12.5,  &
                     & //20X,16HSECTION AREA   =,1P,E12.5,//2X)
            ELSE
               WRITE (Log2,99008) chord , rle , xc , yc , xbars , ybars , areas
99008          FORMAT (1H1,31X,69HDIMENSIONAL RESULTS - ALL RESULTS REFER TO A BLADE OF SPECIFIED CHORD,/32X,69(1H*),//20X,         &
                      &11HBLADE CHORD,4X,1H=,1P,E12.5,//20X,10HEND RADIUS,5X,1H=,1P,E12.5,8X,14HCENTERED AT X=,1P,E12.5,3H Y=,1P,   &
                     & E13.5,//20X,26HLOCATION OF CENTROID AT X=,1P,E12.5,7H AND Y=,1P,E12.5,//20X,16HSECTION AREA   =,1P,E12.5,    &
                     & //2X)
            ENDIF
         ELSE
            chord = chord*Axialc
            Ccord(Ibl) = chord
            area = area*a2
            xc = rle - xbar*Axialc - Xdel
            yc = -ybar*Axialc - Ydel
            xtc = (xml-xbar)*Axialc - Xdel
            ytc = (yml-ybar)*Axialc - Ydel
            IF ( Iprint>=2 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Isecn==2 ) THEN
               WRITE (Log2,99009) chord , rle , xc , yc , xtc , ytc , area , ix , iy , ixy , ipx , ang , ipy , ang
99009          FORMAT (1H1,31X,69HDIMENSIONAL RESULTS - ALL RESULTS REFER TO A BLADE OF SPECIFIED CHORD,/32X,69(1H*),//20X,         &
                      &11HBLADE CHORD,4X,1H=,1P,E12.5,//20X,9HEND RADII,6X,1H=,1P,E12.5,8X,14HCENTERED AT X=,1P,E13.5,3H Y=,1P,     &
                     & E13.5,/64X,6HAND X=,1P,E13.5,3H Y=,1P,E13.5,/20X,16HSECTION AREA   =,1P,E12.5,//20X,                         &
                      &37HSECOND MOMENTS OF AREA ABOUT CENTROID,//30X,6HIX   =,1P,E12.5,/30X,6HIY   =,1P,E12.5,/30X,6HIXY  =,1P,    &
                     & E12.5,//20X,47HPRINCIPAL SECOND MOMENTS OF AREA ABOUT CENTROID,//30X,6HIPX  =,1P,E12.5,5H  (AT,0P,F7.3,      &
                      &15H WITH  X  AXIS),/30X,6HIPY  =,1P,E12.5,5H  (AT,0P,F7.3,15H WITH  Y  AXIS),//)
            ELSE
               WRITE (Log2,99010) chord , rle , xc , yc , area , ix , iy , ixy , ipx , ang , ipy , ang
99010          FORMAT (1H1,31X,69HDIMENSIONAL RESULTS - ALL RESULTS REFER TO A BLADE OF SPECIFIED CHORD,/32X,69(1H*),//20X,         &
                      &11HBLADE CHORD,4X,1H=,1P,E12.5,//20X,10HL.E.RADIUS,5X,1H=,1P,E12.5,8X,14HCENTERED AT X=,1P,E13.5,3H Y=,1P,   &
                     & E13.5,//20X,16HSECTION AREA   =,1P,E12.5,//20X,37HSECOND MOMENTS OF AREA ABOUT CENTROID,//30X,6HIX   =,1P,   &
                     & E12.5,/30X,6HIY   =,1P,E12.5,/30X,6HIXY  =,1P,E12.5,//20X,47HPRINCIPAL SECOND MOMENTS OF AREA ABOUT CENTROID,&
                     & //30X,6HIPX  =,1P,E12.5,5H  (AT,0P,F7.3,15H WITH  X  AXIS),/30X,6HIPY  =,1P,E12.5,5H  (AT,0P,F7.3,           &
                      &15H WITH  Y  AXIS),//)
            ENDIF
         ENDIF
         WRITE (Log2,99022)
         WRITE (Log2,99023)
         Lnct = 24
         spag_nextblock_1 = 3
      CASE (3)
         DO j = 1 , N
            Xs(Ibl,j) = (Xs(Ibl,j)-xbar)*Axialc - Xdel
            Ys(Ibl,j) = (Ys(Ibl,j)-ybar)*Axialc - Ydel
            Xp(Ibl,j) = (Xp(Ibl,j)-xbar)*Axialc - Xdel
            Yp(Ibl,j) = (Yp(Ibl,j)-ybar)*Axialc - Ydel
            IF ( Iprint<2 ) THEN
               IF ( (j/2)*2==j ) THEN
                  IF ( Lnct==60 ) THEN
                     Lnct = 4
                     WRITE (Log2,99019)
                     WRITE (Log2,99022)
                     WRITE (Log2,99023)
                  ENDIF
                  Lnct = Lnct + 1
                  jm1 = j - 1
                  WRITE (Log2,99011) jm1 , Xs(Ibl,jm1) , Ys(Ibl,jm1) , Xp(Ibl,jm1) , Yp(Ibl,jm1) , j , Xs(Ibl,j) , Ys(Ibl,j) ,      &
                                   & Xp(Ibl,j) , Yp(Ibl,j)
99011             FORMAT (3X,I3,4(2X,1P,E12.5),6X,I3,4(2X,1P,E12.5))
               ENDIF
            ENDIF
         ENDDO
         chordd(Ibl) = chord
         IF ( Isplit>1 ) Isecn = Isplit
         IF ( Iprint<2 ) THEN
            IF ( Lnct>24 ) WRITE (Log2,99012)
99012       FORMAT (1H1)
            IF ( Lnct>24 ) Lnct = 2
            Lnct = Lnct + 5
            IF ( Isecn==2 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            WRITE (Log2,99013)
99013       FORMAT (//48X,37HPOINTS DESCRIBING LEADING EDGE RADIUS,//48X,9HPOINT NO.,6X,1HX,13X,1HY,/2X)
         ENDIF
         eps = Beta1 + 180.0
         IF ( Isecn/=2 ) THEN
            DO j = 1 , 31
               Xsemi(Ibl,j) = xc - rle*sin(eps/c1)
               Ysemi(Ibl,j) = yc + rle*cos(eps/c1)
               eps = eps - 6.0
               IF ( Iprint<2 ) THEN
                  WRITE (Log2,99014) j , Xsemi(Ibl,j) , Ysemi(Ibl,j)
99014             FORMAT (48X,I5,1P,E17.5,1P,E14.5)
                  Lnct = Lnct + 1
               ENDIF
            ENDDO
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         phiss = phis - abs((Beta1-Beta2)/c2)
         phipp = abs((Beta1-Beta2))/c2 - phip
         eps = Beta1 + 180.0
         eps2 = Beta2 + 90.
         delep = (180.-(phiss+phipp)*c1)/28.
         DO j = 1 , 31
            IF ( j==1 ) THEN
               Xsemi(Ibl,j) = Xp(Ibl,1)
               Ysemi(Ibl,j) = Yp(Ibl,1)
               Xsemj(Ibl,j) = Xs(Ibl,N)
               Ysemj(Ibl,j) = Ys(Ibl,N)
               eps = eps - phipp*c1
               eps2 = eps2 - phiss*c1
            ELSEIF ( j/=31 ) THEN
               Xsemi(Ibl,j) = xc - rle*sin(eps/c1)
               Ysemi(Ibl,j) = yc + rle*cos(eps/c1)
               Xsemj(Ibl,j) = xtc + rle*cos(eps2/c1)
               Ysemj(Ibl,j) = ytc + rle*sin(eps2/c1)
               eps = eps - delep
               eps2 = eps2 - delep
            ELSE
               Xsemi(Ibl,j) = Xs(Ibl,1)
               Ysemi(Ibl,j) = Ys(Ibl,1)
               Ysemj(Ibl,j) = Yp(Ibl,N)
               Xsemj(Ibl,j) = Xp(Ibl,N)
            ENDIF
         ENDDO
         IF ( Iprint<2 ) THEN
            WRITE (Log2,99015)
99015       FORMAT (//39X,44HPOINTS DESCRIBING LEADING AND TRAILING EDGES,/25X,12HLEADING EDGE,22X,13HTRAILING EDGE,/2X,9HPOINT NO.,&
                  & 4X,8X,1HX,14X,1HY,12X,8X,1HX,14X,1HY,/2X)
            WRITE (Log2,99016) (j,Xsemi(Ibl,j),Ysemi(Ibl,j),Xsemj(Ibl,j),Ysemj(Ibl,j),j=1,31)
99016       FORMAT (6X,I2,7X,1P,E17.5,1P,E14.5,2X,1P,E17.5,1P,E14.5)
            Lnct = Lnct + 31
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         ssurf = Axialc
         ss2 = Bx - Axialc*xbar - Xdel
         sbar = ss2 + Axialc*xbarb + Xdel
         DO ik = 1 , 100
            Ss(ik) = Ss(ik) - sbar
         ENDDO
         CALL alg15(Ss,X,100,0.0,sbar,1,1)
         CALL alg15(Xhere,R(1,Ibl),Nstns,sbar,rxbar,1,0)
         xbarc = xbar
         ybarc = ybar
         xbar = xbarb + Xdel/Axialc
         ybar = ybarb + Ydel/Axialc
         Ss1(1,1) = Ss(1)
         s23 = Axialc/99.
         Ss(1) = Ss(1) + ss2
         DO ik = 2 , 100
            Ss1(ik,1) = Ss(ik)
            Ss(ik) = Ss(ik-1) + s23
         ENDDO
         sigmao = (xab-ybar)/rxbar*Axialc
         DO ik = 2 , 100
            IF ( xbar==Dx(ik) ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( xbar>Dx(ik-1) .AND. xbar<Dx(ik) ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         WRITE (Log2,99017)
99017    FORMAT (1H1,23H XBAR CANNOT BE LOCATED)
         spag_nextblock_1 = 6
      CASE (6)
         Sigma(ik) = sigmao
         kl = ik + 1
         spag_nextblock_1 = 8
      CASE (7)
         kl = ik
         Sigma(ik-1) = sigmao
         spag_nextblock_1 = 8
      CASE (8)
         ssdum = Ss(kl-1)
         Ss(kl-1) = 0.
         yp1 = xbc
         rx1 = rxbar
         DO ik = kl , 100
            xsurf = ss2 + Dx(ik)*ssurf + Ss1(1,1)
            CALL alg15(Ss1(1,1),X,100,xsurf,xdum,1,1)
            CALL alg15(Xhere,R(1,Ibl),Nstns,xdum,rx2,1,0)
            Sigma(ik) = Sigma(ik-1) + (Y(ik)/rx2+yp1/rx1)/2.*(Ss(ik)-Ss(ik-1))
            yp1 = Y(ik)
            rx1 = rx2
         ENDDO
         Ss(kl-1) = ssdum
         ssdum = Ss(kl)
         sigdum = Sigma(kl)
         Sigma(kl) = sigmao
         Ss(kl) = 0.
         rx1 = rxbar
         yp1 = xbc
         km = kl - 1
         DO ik = 1 , km
            kj = kl - ik
            xsurf = ss2 + Dx(kj)*ssurf + Ss1(1,1)
            CALL alg15(Ss1(1,1),X,100,xsurf,xdum,1,1)
            CALL alg15(Xhere,R(1,Ibl),Nstns,xdum,rx2,1,0)
            Sigma(kj) = Sigma(kj+1) - (Y(kj)/rx2+yp1/rx1)/2.*(Ss(kj+1)-Ss(kj))
            yp1 = Y(kj)
            rx1 = rx2
         ENDDO
         Sigma(kl) = sigdum
         Ss(kl) = ssdum
         DO ik = 1 , 100
            Ss(ik) = Ss1(ik,1)
         ENDDO
         xbar = xbarc
         ybar = ybarc
         DO ik = 1 , N
            Ss1(ik,1) = ss2 + ((Xs(Ibl,ik)+Xdel)/Axialc+xbar)*ssurf + Ss(1)
            Ss1(ik,2) = ss2 + ((Xp(Ibl,ik)+Xdel)/Axialc+xbar)*ssurf + Ss(1)
         ENDDO
         DO ik = 1 , 31
            Ss1(ik,3) = ss2 + ((Xsemi(Ibl,ik)+Xdel)/Axialc+xbar)*ssurf + Ss(1)
         ENDDO
         IF ( Isecn==2 ) THEN
            DO ik = 1 , 31
               Ss1(ik,4) = ss2 + ((Xsemj(Ibl,ik)+Xdel)/Axialc+xbar)*ssurf + Ss(1)
            ENDDO
            CALL alg15(Ss,X,100,Ss1(1,4),Ss1(1,4),31,1)
         ENDIF
         CALL alg15(Ss,X,100,Ss1(1,1),Ss1(1,1),N,1)
         CALL alg15(Ss,X,100,Ss1(1,2),Ss1(1,2),N,1)
         CALL alg15(Ss,X,100,Ss1(1,3),Ss1(1,3),31,1)
         IF ( Istak<=1 ) THEN
            IF ( Istak==1 ) sigmao = Sigma(100)
            IF ( Istak==0 ) sigmao = Sigma(1)
            DO ik = 1 , 100
               Sigma(ik) = Sigma(ik) - sigmao
            ENDDO
         ENDIF
         DO ik = 1 , 100
            Dx(ik) = (Dx(ik)-xbar)*Axialc - Xdel
            Dy(ik) = (Dy(ik)-ybar)*Axialc - Ydel
         ENDDO
         DO mk = 1 , 4
            IF ( Isecn==2 .OR. mk/=4 ) THEN
               IF ( mk==4 .OR. mk==3 ) nnn = 31
               IF ( mk==1 .OR. mk==2 ) nnn = N
               DO ik = 1 , nnn
                  IF ( mk==1 ) yp1 = Ys(Ibl,ik)
                  IF ( mk==2 ) yp1 = Yp(Ibl,ik)
                  IF ( mk==3 ) yp1 = Ysemi(Ibl,ik)
                  IF ( mk==4 ) yp1 = Ysemj(Ibl,ik)
                  IF ( mk==1 ) rx1 = Xs(Ibl,ik)
                  IF ( mk==2 ) rx1 = Xp(Ibl,ik)
                  IF ( mk==3 ) rx1 = Xsemi(Ibl,ik)
                  IF ( mk==4 ) rx1 = Xsemj(Ibl,ik)
                  CALL alg15(Dx,Dy,100,rx1,rxbar,1,1)
                  delly = yp1 - rxbar
                  CALL alg15(Xhere,R(1,Ibl),Nstns,Ss1(ik,mk),rab,1,0)
                  delsig = delly/rab
                  CALL alg15(Dx,Sigma,100,rx1,xab,1,1)
                  Ss1(ik,mk) = xab + delsig
               ENDDO
            ENDIF
         ENDDO
         RETURN
      CASE (9)
!
         WRITE (Log2,99018)
99018    FORMAT (1H1,10X,54HITERATIVE SOLUTION FOR CONSTANT FAILS - CASE ABANDONED)
         CALL mesage(-37,0,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
99019 FORMAT (1H1)
99020 FORMAT (27X,5HPOINT,8X,24HM E A N L I N E  D A T A,13X,23HSURFACE COORDINATE DATA,/27X,6HNUMBER,5X,1HX,7X,1HY,5X,             &
             &15HANGLE THICKNESS,9X,2HX1,6X,2HY1,6X,2HX2,6X,2HY2,//)
99021 FORMAT (27X,I3,F13.5,F8.5,F7.3,F8.5,F16.5,3F8.5)
99022 FORMAT (4X,2HPT,5X,7HSURFACE,10(1H-),3HONE,8X,7HSURFACE,10(1H-),3HTWO,10X,2HPT,5X,7HSURFACE,10(1H-),3HONE,8X,7HSURFACE,10(1H-)&
            & ,3HTWO)
99023 FORMAT (4X,2HNO,8X,1HX,13X,1HY,13X,1HX,13X,1HY,12X,2HNO,8X,1HX,13X,1HY,13X,1HX,13X,1HY,//)
END SUBROUTINE alg13
