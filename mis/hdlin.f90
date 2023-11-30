
SUBROUTINE hdlin(X,Y,Z,Np,Nc,Xcc,Icount,Irct,X21,Y21,Z21,Iia,Xe,Ye,Xu,Yu,Xi,Yi,Zi,Di,Ibeg,Iend,Ict,Icct,Ind,Nind,Xxx,Ccc,In,In1,In2,&
               & Tgm,Tgmt,Tgi,Zm,Zmi,Rv,Rvi,Nno,Noct,Ymin,Zmin,Coord,Sndt,Neh,Keep)
   IMPLICIT NONE
   INTEGER Icore , Jat , Jjj , Jt , L0 , L00 , L01 , L1 , L10 , L11 , L12 , L13 , L2 , L3 , L4 , L5 , L6 , L7 , L8 , L9 , Lz , Me , &
         & Nn
   REAL Pit , Roll , Scx , Vp , Vx , Vx1 , Vx2 , Vx3 , Yaw
   COMMON /go3   / L0 , L1 , L00 , L01 , L2 , L3 , L4 , L5 , L6 , L7 , L8 , L9 , L10 , L11 , L12 , L13
   COMMON /hdsc  / Scx , Yaw , Roll , Pit , Lz , Vp , Jjj , Icore
   COMMON /hedg  / Jat , Me , Jt , Vx , Vx1 , Vx2 , Vx3 , Nn
   INTEGER Nc , Np
   REAL Ccc(1) , Coord(1) , Di(1) , Rv(1) , Rvi(1) , Sndt(1) , Tgi(1) , Tgm(1) , Tgmt(1) , X(1) , X21(1) , Xcc(1) , Xe(1) , Xi(1) , &
      & Xu(1) , Xxx(1) , Y(1) , Y21(1) , Ye(1) , Yi(1) , Ymin(1) , Yu(1) , Z(1) , Z21(1) , Zi(1) , Zm(1) , Zmi(1) , Zmin(1)
   INTEGER Ibeg(1) , Icct(1) , Icount(1) , Ict(1) , Iend(1) , Iia(1) , In(1) , In1(1) , In2(1) , Ind(1) , Irct(1) , Keep(1) , Neh(1)&
         & , Nind(1) , Nno(1) , Noct(1)
   REAL a , a1 , a3 , amaxx , amaxy , aminx , aminy , ava , b , b1 , b3 , c , c3 , c4 , cosx , cosy , cosz , crx , cry , d3 , dy ,  &
      & e , en , h(15) , hh , hold , hold1 , hold2 , hold3 , hx1 , hxx , oj , pi , pk , ppppp , ps , pt , r , r1 , r2 , rrx(20) ,   &
      & rx , s , s1 , sct , sd , sinx , siny , sinz , sw , sw1 , t , t1 , tmax , tmj , tw , ty , tz , u(6) , ux , v(6) , vl , vpx , &
      & w(6) , x1(10) , xj , xmat , xmd , xmit , xx , y1(10) , yg , yj , ymat , ymit , yy , zj , zs , zs1 , zz
   INTEGER i , i1 , i2(2) , i3(2) , ia , iabc , iaug , ib , ibb , ibl , idav , iexp , igx , igy , ii , ij , ik , ikk , ikt , il ,   &
         & im , ir , is , isave , it , iv , ix , ix1 , ixr , ixt , ixu , ixxx , j , jb , jh , jj , jk , jm , jo , js , jtt , jv ,   &
         & jx , jxt , jxx , k , k1 , k2 , ke , kr , ks , ku , kv , l , l14 , l15 , l16 , l17 , l18 , lc , lg , lt , lx , m , mnd ,  &
         & ms , mu , mx , n , nb , ng , ngx(15) , ni , nii , nit , npx , ns , nt , nv
!
!
!     THIS SUBROUTINE IS THE EXECUTIVE.
!
!
!
   IF ( Vp>=0. ) THEN
      hxx = .015
      ava = .0
      hx1 = .001
      lc = 10**6
      ixxx = 0
      IF ( Scx<0. ) ixxx = 1
      Scx = abs(Scx)
!
!     INITIALIZE VARIABLES.
!
      Lz = Lz*5
      sw1 = 0
      sw = 0
      idav = 0
!
!     CALCULATE MAXIMUM ALLOWABLE ELEMENTS.
!
      iabc = Icore/(25+Lz+4*Jjj)
      sct = 1.
      Vp = Vp/sct
      vpx = abs(Vp)
      isave = Nc
      Nc = iabc
      L5 = 0
      L6 = Nc
      L7 = 2*Nc
      L8 = 3*Nc
      L2 = 4*Nc
      L3 = 5*Nc
      L4 = 6*Nc
      L00 = 7*Nc
      L01 = 8*Nc
      L1 = 9*Nc
      L0 = 10*Nc
      L9 = 11*Nc
      L10 = 12*Nc
      L11 = 13*Nc
      l15 = 14*Nc
      l16 = 15*Nc
      l17 = 16*Nc
      l18 = 19*Nc
      L12 = 20*Nc
      L13 = 25*Nc
      l14 = L13 + Lz*Nc
      DO j = 1 , Nc
         Rvi(L8+j) = 10**6
         Tgm(L5+j) = 10**6
         Rv(L7+j) = -Rvi(L8+j)
         Tgi(L6+j) = -Tgm(L5+j)
         Noct(L9+j) = 0
         Zm(L2+j) = Rv(L7+j)
         Zmi(L3+j) = Rvi(L8+j)
         Nind(l16+j) = 0
         Ind(l15+j) = j
         Keep(l18+j) = 0
      ENDDO
      Nc = isave
      ik = 0
      ikt = 0
      kr = Jjj
      pi = 3.1416/180.
      u(6) = Scx
      v(6) = Scx
      Vp = -Vp
!
!     STORE EULERIAN ANGLES.
!
      xx = Yaw*pi
      yy = Roll*pi
      zz = Pit*pi
      cosy = cos(yy)
      siny = sin(yy)
      cosz = cos(zz)
      sinz = sin(zz)
      cosx = cos(xx)
      sinx = sin(xx)
   ENDIF
   nt = Np - 1
   ikk = ik + 1
   ik = ik + 1
!
!     SET ERROR CODES, IF NECESSARY.
!
   IF ( ikk>iabc ) sw = 1
   IF ( Nc/=0 ) THEN
      idav = 1
      Nc = -sw1
      IF ( sw/=0. ) THEN
         Icore = (25+Lz+4*Jjj)*ikk
         Nc = -(sw+sw1)
      ENDIF
   ENDIF
   DO j = 1 , Np
      X21(j) = X(j)
      Y21(j) = Y(j)
      Z21(j) = Z(j)
   ENDDO
!
!     STORE COORDINATES AND SET PEN POSITION WHENEVER ABS(Z)=9999.
!
   DO j = 1 , nt
      Iia(j) = 0
      IF ( Z21(j)==9999. ) THEN
         Iia(j) = 1
         ixu = j - 2
         ibb = j - isign(1,ixu)
         X21(j) = X21(ibb)
         Y21(j) = Y21(ibb)
         Z21(j) = Z21(ibb)
      ENDIF
   ENDDO
   Iia(Np) = 1
   Z21(Np) = Z21(nt)
   Y21(Np) = Y21(nt)
   X21(Np) = X21(nt)
   jxx = ikk
   i = 1
   vl = abs(Vp)
!
!     LOOP THAT DOES THE THREE DIMENSIONAL TRANSFORMATION ON THE
!     COORDINATES.
!
   jv = l14 + (ikk-1)*4*Jjj
   Jt = 1
   DO j = 1 , Np
      xj = X21(j)/sct
      yj = Y21(j)/sct
      zj = Z21(j)/sct
      u(i) = zj*(cosy*sinx) + xj*(cosy*cosx) - yj*siny
      tw = yj*cosy*cosz
      tz = xj*(sinz*sinx+siny*cosz*cosx)
      ty = zj*(-sinz*cosx+siny*cosz*sinx)
      v(i) = tz + tw + ty
      pt = yj*cosy*sinz
      pk = zj*(cosz*cosx+siny*sinz*sinx)
      ps = xj*(-cosz*sinx+siny*sinz*cosx)
      zj = pk + ps + pt
      IF ( zj>=vl ) THEN
         sw1 = 2
         vpx = amax1(zj,vpx)
         vpx = vpx + (.5/sct)
      ENDIF
      t = sw + sw1
      IF ( t==0. ) THEN
!
!     CALCULATES PERSPECTIVE BASED ON VALUE VP(DV) FROM CALLING PROGRAM.
!
         hh = vl/(vl-zj)
         X21(j) = u(i)*hh
         Y21(j) = v(i)*hh
         Z21(j) = zj*hh
!
!     CALCULATES MAX/MIN VALUES OF EACH ELEMENT ON THE X,Y,Z DIMENSION
!
         Rv(L7+jxx) = amax1(Rv(L7+jxx),Y21(j))
         Rvi(L8+jxx) = amin1(Rvi(L8+jxx),Y21(j))
         Tgi(L6+jxx) = amax1(Tgi(L6+jxx),X21(j))
         Tgm(L5+jxx) = amin1(Tgm(L5+jxx),X21(j))
         Zm(L2+jxx) = amax1(Zm(L2+jxx),Z21(j))
         Zmi(L3+jxx) = amin1(Zmi(L3+jxx),Z21(j))
         Coord(Jt+jv) = X21(j)
         Coord(Jt+jv+1) = Y21(j)
         Coord(Jt+jv+2) = Z21(j)
         Coord(Jt+3+jv) = Iia(j)
         Jt = Jt + 4
      ENDIF
   ENDDO
   IF ( idav==1 ) Vp = vpx*sct
   IF ( t==0. ) THEN
      Noct(L9+ikk) = Noct(L9+ikk) + Np
      ns = Np
      ava = ava + (Tgi(L6+jxx)-Tgm(L5+jxx))*(Rv(L7+jxx)-Rvi(L8+jxx))
!
!     CALL SUBROUTINE WHICH CALCULATES BOTH THE EQUATIONS OF THE LINE
!     SEGMENTS AND POLYGONS.
!
      IF ( ixxx/=1 ) CALL hdcoef(X21,Y21,Z21,Xxx,jxx,ns,Ccc,Lz)
!
!     CHECKS TO SEE IF ALL ELEMENTS(SETS) HAVE BEEN PASSED.
!
      IF ( idav==1 ) THEN
         ava = ava/ikk
         DO j = 1 , 100
            Icct(j) = 0
            Ict(j) = 0
            Irct(j) = j - 1
            Ibeg(j) = 1
            Iend(j) = 0
         ENDDO
         iaug = 50 + (ikk/10000)*2
         amaxx = -999999.
         amaxy = -999999.
         aminx = 999999.
         aminy = 999999.
         DO j = 1 , ikk
            amaxx = amax1(amaxx,Tgi(L6+j))
            amaxy = amax1(amaxy,Rv(L7+j))
            aminx = amin1(aminx,Tgm(L5+j))
            aminy = amin1(aminy,Rvi(L8+j))
         ENDDO
         tmax = (amaxx-aminx)*(amaxy-aminy)
         ibl = tmax/ava
         ibl = ibl/4
!
!     DETERMINES THE NUMBER OF GRID POINTS IN THE GRID.
!
!
         en = ikk
         k = (alog(en)/alog(2.)) + .01
         k = k + iaug
         k = min0(k,ibl)
         IF ( k<=1 ) k = 1
         t = k
         r = t**.5
         ks = r + .5
         s = t/ks
         ms = s + .5
         n = ks*ms
         mnd = n + 1
         xmd = mnd
         t = 3./(mnd-1)
         igy = t*ikk
         k = ks
         k1 = ms
         crx = (amaxx-aminx)/k
         cry = (amaxy-aminy)/k1
!
!
!     DETERMINES THE RELEVANT ELEMENTS VIA THE GRID BLOCKS.
!
!
         DO j = 1 , ikk
            ia = 0
            xmat = Tgi(L6+j)
            xmit = Tgm(L5+j)
            ymat = Rv(L7+j)
            ymit = Rvi(L8+j)
            m = 0
            DO i = 1 , k1
               DO l = 1 , k
                  m = m + 1
                  s = xmat - ((l-1)*crx+aminx)
                  s1 = xmat - (l*crx+aminx)
                  r = xmit - ((l-1)*crx+aminx)
                  r1 = xmit - (l*crx+aminx)
                  a = ymat - ((i-1)*cry+aminy)
                  a1 = ymat - (i*cry+aminy)
                  b = ymit - ((i-1)*cry+aminy)
                  b1 = ymit - (i*cry+aminy)
                  IF ( s>0. .AND. r1<0. ) THEN
                     IF ( a>0. .AND. b1<0. ) THEN
                        IF ( s*s1<=0. .AND. r*r1<=0. ) THEN
                           IF ( a*a1<=0. .AND. b*b1<=0. ) THEN
                              Nind(l16+j) = m
                              GOTO 20
                           ENDIF
                        ENDIF
                        ia = ia + 1
                        IF ( ia<=4 ) THEN
                           Nind(l16+j) = Nind(l16+j) + m*(mnd**(ia-1))
                        ELSE
                           Nind(j+l16) = 0
                        ENDIF
                        IF ( Icct(m)>=0 ) THEN
                           Icct(m) = Icct(m) + 1
                           jk = (m-1)*igy + Icct(m) + l17
                           Neh(jk) = j
                           IF ( Icct(m)>=igy ) Icct(m) = -1
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
 20      ENDDO
         CALL hdvs1(Nind(l16+1),ik,Ind(l15+1))
         sw = 0
         l = 1
         DO i = 1 , ikk
            DO WHILE ( Nind(l16+i)/=Irct(l) )
               IF ( sw/=0. ) THEN
                  Ibeg(l) = lt
                  Iend(l) = lt + Ict(l) - 1
                  sw = 0
                  IF ( Nind(l16+i)>=mnd ) GOTO 40
                  l = l + 1
               ELSE
                  l = l + 1
               ENDIF
            ENDDO
            sw = sw + 1
            IF ( sw==1. ) lt = i
            Ict(l) = Ict(l) + 1
         ENDDO
         Ibeg(l) = lt
         Iend(l) = lt + Ict(l) - 1
 40      DO j = 1 , ikk
            Sndt(L4+j) = Ind(l15+j)
         ENDDO
         CALL hdvsr(Sndt(L4+1),ik,Nind(l16+1))
         en = ikk
         igx = (alog(en)/alog(2.)) + 1.
         DO j = 1 , igx
            rrx(j) = 2**(igx-j)
         ENDDO
         u(6) = Scx
         v(6) = Scx
         w(6) = Scx
         ikt = Nc
         t = aminy
         t1 = aminx
         v(5) = t
         u(5) = t1
         ij = 0
         x1(3) = u(5)
         y1(3) = v(5)
         x1(4) = u(6)
         y1(4) = v(6)
         x1(4) = x1(4)/sct
         y1(4) = y1(4)/sct
         DO j = 1 , ikk
            In(L11+j) = j
            In1(L0+j) = j
            In2(L00+j) = j
            Tgmt(L10+j) = Tgm(L5+j)
            Ymin(L1+j) = Rvi(L8+j)
            Zmin(L01+j) = Zm(L2+j)
         ENDDO
!
!     CALL SUBROUTINE WHICH WILL SORT ON X,Y AND Z.
!
         CALL hdvsr(Tgmt(L10+1),ik,In(L11+1))
         CALL hdvsr(Ymin(L1+1),ik,In1(L0+1))
         CALL hdvsr(Zmin(L01+1),ik,In2(L00+1))
         h(8) = 0
         DO j = 1 , ikk
            ks = ikk
            jj = l14 + (j-1)*4*Jjj
            jh = 1
            ii = 0
            ixr = Noct(L9+j)
            nit = 0
            Jt = L12 + 5*(j-1)
            jo = L13 + Lz*(j-1)
            IF ( ixxx/=1 ) THEN
               ns = Xxx(5+Jt)
               ng = ns*5
               a3 = Xxx(1+Jt)
               b3 = Xxx(2+Jt)
               c3 = Xxx(3+Jt)
               d3 = Xxx(4+Jt)
               i = 0
               DO ix = 1 , ng , 5
                  IF ( ixr>3 ) THEN
                     i = i + 1
                     Xe(i) = Ccc(ix+3+jo)
                     IF ( Ccc(ix+jo)/=0. ) THEN
                        Ye(i) = -Ccc(ix+2+jo) - Ccc(ix+1+jo)*Xe(i)
                     ELSE
                        Xe(i) = -Ccc(ix+2+jo)
                        Ye(i) = Ccc(ix+3+jo)
                     ENDIF
                  ENDIF
               ENDDO
!
!     THIS LOOP DETERMINES THE RELEVANT ELEMENTS AS THEY RELATE TO A
!     PARTICULAR ELEMENT.  THAT IS, EACH ELEMENT HAS ASSOCIATED WITH IT
!     THOSE OTHER ELEMENTS WHICH COULD POSSIBLY HIDE SOME PORTION
!     OF THE GIVEN ELEMENT.
!
               k = 2**igx
               k1 = k
               k2 = k
!
!     DO LOGARITHMIC SEARCH TO DETERMINE RELEVANT ELEMENTS.
!
               s = -1
               DO i = 1 , igx
                  k = k + sign(rrx(i),s)
                  IF ( k>ikk ) k = ikk
                  s = Tgi(L6+j) - Tgmt(L10+k)
                  s1 = Tgi(L6+j) - Tgmt(L10+k-1)
                  IF ( s*s1<=0. ) GOTO 45
               ENDDO
               k = ikk
 45            s = -1
               DO i = 1 , igx
                  k1 = k1 + sign(rrx(i),s)
                  IF ( k1>ikk ) k1 = ikk
                  s = Rv(L7+j) - Ymin(L1+k1)
                  s1 = Rv(L7+j) - Ymin(L1+k1-1)
                  IF ( s*s1<=0. ) GOTO 50
               ENDDO
               k1 = ikk
 50            s = -1
               DO i = 1 , igx
                  k2 = k2 + sign(rrx(i),s)
                  IF ( k2<=1 ) k2 = 2
                  IF ( k2>ikk ) k2 = ikk
                  s = Zmi(L3+j) - Zmin(L01+k2)
                  s1 = Zmi(L3+j) - Zmin(L01+k2-1)
                  IF ( s*s1<=0. ) GOTO 55
               ENDDO
               k2 = 1
 55            i1 = ikk - k2 + 1
!
!     RETRIEVE THE RELEVANT ELEMENTS DETERMINED FROM SCHEME 1.
!
               IF ( Nind(l16+j)/=0 ) THEN
                  ir = Nind(l16+j)
                  Vx = Nind(l16+j)
                  t = alog(Vx)
                  IF ( Nind(l16+j)>lc ) THEN
                     e = lc
                     lg = Nind(l16+j)/lc
                     mu = mod(ir,lc)
                     ux = lg + (mu/e)
                     t = alog(ux) + alog(e)
                  ENDIF
                  ixt = 0
                  iexp = (t/alog(xmd)) + 1
                  DO l = 1 , iexp
                     iv = ir/(mnd**(iexp-l))
                     ir = ir - iv*(mnd**(iexp-l))
                     iv = iv + 1
                     IF ( Icct(iv-1)/=0 ) THEN
                        IF ( Icct(iv-1)<=0 ) GOTO 60
                        ke = Icct(iv-1)
                        il = 0
                        jtt = (iv-2)*igy + l17
                        DO i = 1 , ke
                           kv = Neh(i+jtt)
                           IF ( Keep(l18+kv)/=j ) THEN
                              il = il + 1
                              Nno(L4+ixt+il) = kv
                              Keep(l18+kv) = j
                           ENDIF
                        ENDDO
                        ixt = ixt + il
                     ENDIF
                     ix = Ibeg(iv)
                     ix1 = Iend(iv)
                     DO i = ix , ix1
                        Nno(L4+ixt+i-ix+1) = Ind(l15+i)
                     ENDDO
                     ixt = ixt + ix1 - ix + 1
                  ENDDO
                  ks = ixt
               ENDIF
 60            im = min0(i1,k,k1)
!
!     PICK MINIMUM COUNT FROM BOTH SCHEMES.
!
               IF ( ks>=im ) THEN
                  IF ( im/=i1 ) THEN
                     IF ( im==k ) THEN
                        ks = k
                        DO i = 1 , ks
                           Nno(L4+i) = In(L11+i)
                        ENDDO
                        GOTO 65
                     ELSEIF ( im==k1 ) THEN
                        ks = k1
                        DO i = 1 , ks
                           Nno(L4+i) = In1(L0+i)
                        ENDDO
                        GOTO 65
                     ENDIF
                  ENDIF
                  ks = i1
                  DO i = 1 , ks
                     Nno(L4+i) = In2(L00+ikk-i+1)
                  ENDDO
               ENDIF
 65            DO i = 1 , ks
                  it = 0
                  jb = Nno(L4+i)
                  IF ( j/=jb ) THEN
                     jk = L13 + Lz*(jb-1)
                     js = L12 + 5*(jb-1)
                     IF ( Tgm(L5+j)<Tgi(L6+jb) .AND. Tgi(L6+j)>Tgm(L5+jb) ) THEN
                        IF ( Rv(L7+j)>Rvi(L8+jb) .AND. Rvi(L8+j)<Rv(L7+jb) ) THEN
                           IF ( Zmi(L3+j)<Zm(L2+jb) ) THEN
                              nv = Xxx(5+js)
                              IF ( Xxx(js+3)/=0. ) THEN
                                 IF ( Xxx(3+Jt)/=0. ) THEN
                                    nb = 5*nv
!
!
!     TEST TO SEE IF ALL VERTICES LIE EITHER BEHIND OR IN FRONT OF
!     THE GIVEN POLYGON.
!
!
                                    m = 0
                                    DO ix = 1 , nb , 5
                                       m = m + 1
                                       a = Ccc(ix+3+jk)
                                       IF ( Ccc(ix+jk)/=0. ) THEN
                                         b = -Ccc(ix+2+jk) - Ccc(ix+1+jk)*a
                                       ELSE
                                         a = -Ccc(ix+2+jk)
                                         b = Ccc(ix+3+jk)
                                       ENDIF
                                       Xu(m) = a
                                       Yu(m) = b
                                       Vx = Xxx(4+js)
                                       Vx1 = Xxx(2+js)*b
                                       Vx2 = Xxx(1+js)*a
                                       zs = -(Vx+Vx1+Vx2)/Xxx(3+js)
                                       Vx = Xxx(4+Jt)
                                       Vx1 = Xxx(2+Jt)*b
                                       Vx2 = Xxx(1+Jt)*a
                                       zs1 = -(Vx+Vx1+Vx2)/Xxx(3+Jt)
                                       IF ( abs(zs-zs1)>=hxx ) THEN
                                         it = it + 1
                                         Icount(it) = 0
                                         IF ( zs>zs1 ) Icount(it) = 1
                                       ENDIF
                                    ENDDO
!
!
!     TESTS FOR SEMI-RELEVANT PLANES.  THAT IS,NEGATIVE INDEXES
!     INDICATE ELEMENT IS TO BE USED FOR VISIBILITY TEST, BUT NOT FOR
!     INTERSECTION LINE DETERMINATION.
!
!
                                    IF ( it==0 ) CYCLE
                                    l = 0
                                    DO m = 1 , it
                                       l = l + Icount(m)
                                    ENDDO
                                    IF ( l==0 ) CYCLE
                                    IF ( l==it ) jb = -jb
                                    IF ( ii==0 ) THEN
!
!
!     INTERROGATE THE RELATIONSHIP OF THE CANDIDATE POLYGON TO THE
!     GIVEN POLYGON BY DETERMINING IF THE PROJECTION OF ONE POLYGON
!     CAN BE SEPARATED BY AN EDGE FROM THE OTHER'S PROJECTION
!
!
                                       c3 = Xxx(3+Jt)
                                       c4 = Xxx(3+js)
                                       sd = 0
                                       i3(1) = jk
                                       i3(2) = jo
                                       i2(1) = nv*5
                                       i2(2) = ns*5
                                       DO ku = 1 , 2
                                         is = i3(ku)
                                         ib = i2(ku)
                                         DO l = 1 , ib , 5
 66                                      IF ( sd==1. ) THEN
                                         a = Ccc(l+is)
                                         b = Ccc(l+is+1)
                                         c = Ccc(l+is+2)
                                         ELSE
                                         a = Xxx(2+Jt)*c4 - Xxx(2+js)*c3
                                         b = Xxx(1+Jt)*c4 - Xxx(1+js)*c3
                                         c = Xxx(4+Jt)*c4 - Xxx(4+js)*c3
                                         ENDIF
                                         IF ( a/=0. .OR. b/=0. ) THEN
                                         IF ( a/=0. ) THEN
                                         b = b/a
                                         c = c/a
                                         a = 1
                                         ELSE
                                         a = 0
                                         c = c/b
                                         b = 1
                                         ENDIF
                                         m = 0
                                         r1 = 0
                                         DO ix = 1 , nv
                                         m = m + 1
                                         yg = Yu(m)
                                         IF ( a/=0. ) THEN
                                         dy = -c - b*Xu(m)
                                         ELSE
                                         dy = -c/b
                                         yg = Xu(m)
                                         ENDIF
                                         IF ( abs(dy-yg)>=hxx ) THEN
                                         r = yg - dy
                                         IF ( r*r1<0. ) GOTO 68
                                         r1 = r
                                         ENDIF
                                         ENDDO
                                         m = 0
                                         r2 = 0
                                         DO ix = 1 , ns
                                         m = m + 1
                                         yg = Ye(m)
                                         IF ( a/=0. ) THEN
                                         dy = -c - b*Xe(m)
                                         ELSE
                                         dy = -c/b
                                         yg = Xe(m)
                                         ENDIF
                                         IF ( abs(dy-yg)>=hxx ) THEN
                                         r = yg - dy
                                         IF ( r*r2<0. ) GOTO 68
                                         r2 = r
                                         ENDIF
                                         ENDDO
                                         IF ( r1*r2<0. ) GOTO 70
                                         ENDIF
 68                                      IF ( sd==0. ) THEN
                                         sd = 1
                                         GOTO 66
                                         ENDIF
                                         ENDDO
                                       ENDDO
                                    ENDIF
                                 ENDIF
                                 ii = ii + 1
                                 Nno(L4+ii) = jb
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
 70            ENDDO
               js = 1
               Jat = -4
               Jt = L12 + (j-1)*5
               Nn = Xxx(Jt+5)
               Vx = Xxx(Jt+4)
               Vx1 = Xxx(2+Jt)
               Vx2 = Xxx(1+Jt)
               Vx3 = Xxx(3+Jt)
               IF ( ixr>2 ) THEN
!
!     CALL SUBROUTINE WHICH SOLVES FOR THE LINES OF INTERSECTION,IF ANY,
!     OF THE JTH ELEMENT WITH OTHER ELEMENTS.
!
                  IF ( ii/=0 ) CALL hdsolv(ixr,j,Xxx,Ccc,ii,Nno,nit,X21,Y21,Z21,Iia,Nc,Zm,Zmi,Lz)
               ENDIF
            ENDIF
            DO jm = 1 , ixr
               X21(jm) = Coord(jh+jj)
               Y21(jm) = Coord(jh+1+jj)
               Z21(jm) = Coord(jh+2+jj)
               Iia(jm) = Coord(jh+3+jj)
               jh = jh + 4
            ENDDO
            ixr = ixr + 3*nit
            IF ( ii/=0 ) THEN
               IF ( ixxx/=1 ) THEN
                  jx = 1
                  DO
!
!     PLOTS IF IIA(JX+1) IS EQUAL TO 1.
!
                     IF ( Iia(jx)==0 .AND. Iia(jx+1)==0 ) THEN
                        Jat = Jat + 5
                        Me = 0
!
!     CALL SUBROUTINE WHICH DETERMINES THE POINTS OF INTERSECTIONS
!     OF THE LINES OF THE JTH SET WITH THE RELEVANT LINES AND PLANES
!     OF OTHER ELEMENTS.
!
                        CALL hdchk(Xxx,Ccc,Nno,ii,Xi,Yi,ngx,Zm,Zmi,Rv,Rvi,Tgm,Tgi,Zi,Lz,Xcc)
                        IF ( js/=1 ) STOP 'MY GOSH. JS IS NOT 1 /HDLIN'
                        ng = ngx(js) + 2
                        Xi(1) = X21(jx)
                        Yi(1) = Y21(jx)
                        Zi(1) = Z21(jx)
                        Xi(ng) = X21(jx+1)
                        Yi(ng) = Y21(jx+1)
                        Zi(ng) = Z21(jx+1)
                        IF ( ng>3 ) THEN
!
!     THE FOLLOWING CODE SORTS THE INTERSECTION POINTS IN ASCENDING
!     ORDER OF OCCURENCE AND THEN SHRINKS THE LIST IF REDUNDANCY EXIST.
!
                           ni = ng - 2
                           nii = ni
                           DO m = 1 , ng
                              Di(m) = (Xi(m)-Xi(1))**2
                              ppppp = (Yi(m)-Yi(1))**2
                              Di(m) = Di(m) + ppppp
                           ENDDO
                           DO m = 2 , ni
                              DO mx = 2 , nii
                                 IF ( Di(mx)>Di(mx+1) ) THEN
                                    hold = Di(mx)
                                    hold1 = Xi(mx)
                                    hold2 = Yi(mx)
                                    hold3 = Zi(mx)
                                    Xi(mx) = Xi(mx+1)
                                    Yi(mx) = Yi(mx+1)
                                    Zi(mx) = Zi(mx+1)
                                    Di(mx) = Di(mx+1)
                                    Di(mx+1) = hold
                                    Xi(mx+1) = hold1
                                    Yi(mx+1) = hold2
                                    Zi(mx+1) = hold3
                                 ENDIF
                              ENDDO
                              nii = nii - 1
                           ENDDO
                           lx = 1
                           npx = ng
                           DO
                              npx = npx - 1
                              i = lx
                              DO m = i , npx
                                 rx = 0
                                 t = Xi(m) - Xi(m+1)
                                 t1 = Yi(m) - Yi(m+1)
                                 t = (t**2+t1**2)**.5
                                 IF ( t<=hx1 ) THEN
                                    ix = m
                                    ix1 = npx
                                    DO mx = ix , ix1
                                       Xi(mx) = Xi(mx+1)
                                       Yi(mx) = Yi(mx+1)
                                       Zi(mx) = Zi(mx+1)
                                    ENDDO
                                    rx = 1
                                    lx = m
                                    IF ( lx/=npx ) GOTO 72
                                    EXIT
                                 ENDIF
                              ENDDO
                              EXIT
 72                        ENDDO
                           IF ( rx==1. ) npx = npx - 1
                           ng = npx + 1
                        ENDIF
!
!     THIS CODE DETERMINES THE HDSTUS(VISIBILITY) OF EVERY OTHER POINT
!     AS SUGGESTED BY THE THEOREM IN THE TECHNICAL REPORT.
!
                        DO l = 1 , ng , 2
!
                           oj = Xi(l)
                           tmj = Yi(l)
                           zj = Zi(l)
                           CALL hdstus(oj,tmj,Xxx,Tgm,Rv,Rvi,Tgi,Zm,Nno,ii,h,im,jxt,zj,Nc,Zmi,Ccc,Lz)
                           Di(l) = im
                        ENDDO
                        DO l = 1 , ng , 2
                           IF ( l/=ng ) THEN
                              IF ( l/=ng-1 ) THEN
                                 c = Di(l) + Di(l+2)
                                 IF ( c==2. ) THEN
                                    Di(l+1) = Di(l)
                                    CYCLE
                                 ENDIF
                              ENDIF
                              oj = Xi(l+1)
                              tmj = Yi(l+1)
                              zj = Zi(l+1)
                              CALL hdstus(oj,tmj,Xxx,Tgm,Rv,Rvi,Tgi,Zm,Nno,ii,h,im,jxt,zj,Nc,Zmi,Ccc,Lz)
                              Di(l+1) = im
                           ENDIF
                        ENDDO
!
!     THE FOLLOWING CODE ACTUALLY PLOTS THE POINTS ON A GIVEN LINE
!     GOVERNED BY THE VALUE(IM) RETURNED BY HDSTUS SUBROUTINE.
!     1 MEANS INVISIBLE,...0 MEANS VISIBLE.
!
                        DO l = 1 , ng
                           x1(2) = Xi(l)
                           y1(2) = Yi(l)
                           im = Di(l)
                           CALL hdplt(x1,y1,ij,im)
                           IF ( l/=ng ) THEN
                              c = Di(l) + Di(l+1)
                              IF ( c<=0. ) THEN
                                 h(8) = 1
                                 oj = (Xi(l)+Xi(l+1))/2
                                 tmj = (Yi(l)+Yi(l+1))/2
                                 zj = (Zi(l)+Zi(l+1))/2
                                 CALL hdstus(oj,tmj,Xxx,Tgm,Rv,Rvi,Tgi,Zm,Nno,ii,h,im,jxt,zj,Nc,Zmi,Ccc,Lz)
                                 h(8) = 0
                                 x1(2) = oj
                                 y1(2) = tmj
                                 CALL hdplt(x1,y1,ij,im)
                              ENDIF
                           ENDIF
                        ENDDO
                        jx = jx + 1
                     ELSE
                        im = Iia(jx+1)
                        x1(2) = X21(jx+1)
                        y1(2) = Y21(jx+1)
                        CALL hdplt(x1,y1,ij,im)
                        jx = jx + 2
                        IF ( jx>=ixr ) GOTO 80
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
            DO jm = 1 , ixr
               x1(2) = X21(jm)
               y1(2) = Y21(jm)
               im = Iia(jm)
               CALL hdplt(x1,y1,ij,im)
            ENDDO
!
!     DECREMENTS THE COUNT OF THE NUMBER OF LINES IN THE JTH SET
!     SINCE THE LINES OF INTERSECTIONS WERE ADDED TO THIS ELEMENT
!     BY THE SUBROUTINE SOLVE.
!
 80         Xxx(5+Jt) = Xxx(5+Jt) - nit
         ENDDO
      ENDIF
   ENDIF
END SUBROUTINE hdlin