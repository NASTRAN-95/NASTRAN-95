
SUBROUTINE twistd
   IMPLICIT NONE
   REAL Alpha , Costh , Dum(15) , Ecpt(100) , Eltemp , Esp , Fmu , Gp1(3) , Gp2(3) , Gp3(3) , Gp4(3) , Gsp , Gsube , Heat , Nu ,    &
      & Rho , Sigc , Sigs , Sigt , Sinth , Stress , Tempel , Tsp , Tsub0
   INTEGER Elid , Estid , Icsid1 , Icsid2 , Icsid3 , Icsid4 , Idm , Iecpt(2) , Ielid , Iprec , Isilno(4) , Ismb(3) , Ksystm(55) ,   &
         & Ldict , Matflg , Matid , Matidc , Ngrids
   LOGICAL Iheat , Nogo
   COMMON /emgdic/ Idm , Ldict , Ngrids , Elid , Estid
   COMMON /emgest/ Ielid , Isilno , Matid , Tsp , Fmu , Icsid1 , Gp1 , Icsid2 , Gp2 , Icsid3 , Gp3 , Icsid4 , Gp4 , Tempel
   COMMON /emgprm/ Dum , Ismb , Iprec , Nogo , Heat
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ Esp , Gsp , Nu , Rho , Alpha , Tsub0 , Gsube , Sigt , Sigc , Sigs
   COMMON /system/ Ksystm , Iheat
   DOUBLE PRECISION a , a2 , a3 , a4 , a5 , avec(4) , b , b2 , b3 , b4 , b5 , c , c2 , c23 , c3 , c4 , c5 , cep1 , cep2 , cepx ,    &
                  & cepy , con , d , d2 , d3 , d4 , d5 , e , ep , f , g , ke(144) , kout(144) , me(144) , mout(144) , nuc , p(4) ,  &
                  & smallu(4) , smallv(4) , t , temp , term , term1 , term2 , term3 , term4 , term5 , ti(9) , v12(3) , v41(3) ,     &
                  & vd1(3) , vd2(3) , vi(3) , vj(3) , vk(3) , vkn(3) , vleft(6) , vp12(3) , vright(6) , x1 , x2 , x3 , x4 , xl ,    &
                  & xl13 , xl24 , xp , xq , y1 , y2 , y3 , y4 , yp , z
   INTEGER dict(11) , i , ij , ike , iout , ip , ip1 , ipart(4) , ipvt , is , isort , it , ivlbeg , ivrbeg , j , js , jt , jt8 , k ,&
         & korm , l
   REAL dict5 , pa , sa , v12dk , vjl , vkl , vp12l
!
!     THIS SUBROUTINE COMPUTES THE 12 X 12 STIFFNESS MATRIX FOR THE
!     TWIST PANEL ELEMENT, AS WELL AS ITS DIAGONALIZED MASS MATRIX
!
!     DOUBLE PRECISION VERSION
!
!     ECPT FOR THE BOTH TWIST PANEL ELEMENTS
!
!     ECPT( 1)  -  IELID          ELEMENT ID. NO.
!     ECPT( 2)  -  ISILNO(4)      SCALAR INDEX NUMBERS
!     ECPT( 3)  -   ...                   ...
!     ECPT( 4)  -   ...                   ...
!     ECPT( 5)  -   ...                   ...
!     ECPT( 6)  -  MATID          MATERIAL ID.
!     ECPT( 7)  -  T              THICKNESS
!     ECPT( 8)  -  FMU            NON-STRUCTURAL MASS
!     ECPT( 9)  -  ICSID1         COOR. SYS. ID. FOR GRID POINT 1
!     ECPT(10)  -  GP1(3)         BASIC COORDINATES FOR GRID POINT 1
!     ECPT(11)  -   ...                      ...
!     ECPT(12)  -   ...                      ...
!     ECPT(13)  -  ICSID2         COOR. SYS. ID. FOR GRID POINT 2
!     ECPT(14)  -  GP2(3)         BASIC COORDINATES FOR GRID POINT 2
!     ECPT(15)  -   ...                      ...
!     ECPT(16)  -   ...                      ...
!     ECPT(17)  -  ICSID3         COOR. SYS. ID. FOR GRID POINT 3
!     ECPT(18)  -  GP3(3)         BASIC COORDINATES FOR GRID POINT 3
!     ECPT(19)  -   ...                      ...
!     ECPT(20)  -   ...                      ...
!     ECPT(21)  -  ICSID4         COOR. SYS. ID. FOR GRID POINT 4
!     ECPT(22)  -  GP4(3)         BASIC COORDINATES FOR GRID POINT 4
!     ECPT(23)  -   ...                      ...
!     ECPT(24)  -   ...                      ...
!     ECPT(25)  -  TEMPEL         ELEMENT TEMPERATURE
!
!
!     ECPT COMMON BLOCK
!
!
!     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1),Ielid) , (dict(5),dict5) , (me(1),ke(1)) , (kout(1),mout(1))
   DATA ipart/1 , 2 , 3 , 4/
!
!
   dict(1) = Estid
   dict(2) = 1
   dict(3) = 12
   dict(4) = 56
   ip = Iprec
   isort = 0
!
!     IF STIFFNESS MATRIX NOT NEEDED GO TO PERFORM MASS CALCULATIONS
!
   IF ( Ismb(1)==0 ) GOTO 400
!
!
   Matidc = Matid
   Matflg = 1
   Eltemp = Tempel
   CALL mat(Iecpt(1))
   dict5 = Gsube
   e = Esp
   g = Gsp
   t = Tsp
   IF ( t*g==0.D0 ) THEN
!
!     ERROR EXITS
!
      CALL mesage(30,26,Iecpt(1))
      GOTO 600
   ELSE
      c23 = 2.D0/3.D0
      nuc = 1./(1.+Nu)
!
!     COMPUTE DIAGONAL VECTORS.
!
      DO i = 1 , 3
         vd1(i) = Gp3(i) - Gp1(i)
         vd2(i) = Gp4(i) - Gp2(i)
      ENDDO
!
!     COMPUTE THE NORMAL VECTOR VKN, NORMALIZE, AND COMPUTE THE
!     PROJECTED AREA, PA
!
      vkn(1) = vd1(2)*vd2(3) - vd1(3)*vd2(2)
      vkn(2) = vd1(3)*vd2(1) - vd1(1)*vd2(3)
      vkn(3) = vd1(1)*vd2(2) - vd1(2)*vd2(1)
      vkl = dsqrt(vkn(1)**2+vkn(2)**2+vkn(3)**2)
      IF ( vkl==0. ) THEN
         CALL mesage(30,26,Iecpt(1))
         GOTO 600
      ELSE
         vk(1) = vkn(1)/vkl
         vk(2) = vkn(2)/vkl
         vk(3) = vkn(3)/vkl
         pa = vkl/2.
!
!     COMPUTE  SIDES -12- AND -41-
!
         DO i = 1 , 3
            v12(i) = Gp2(i) - Gp1(i)
            v41(i) = Gp1(i) - Gp4(i)
         ENDDO
!
!     COMPUTE DOT PRODUCT, V12DK, OF V12 AND VK, THE VECTORS VP12, VI,
!     VJ
!
         v12dk = v12(1)*vk(1) + v12(2)*vk(2) + v12(3)*vk(3)
         vp12(1) = v12(1) - v12dk*vk(1)
         vp12(2) = v12(2) - v12dk*vk(2)
         vp12(3) = v12(3) - v12dk*vk(3)
         vp12l = dsqrt(vp12(1)**2+vp12(2)**2+vp12(3)**2)
         IF ( vp12l==0. ) THEN
            CALL mesage(30,26,Iecpt(1))
            GOTO 600
         ELSE
            vi(1) = vp12(1)/vp12l
            vi(2) = vp12(2)/vp12l
            vi(3) = vp12(3)/vp12l
            vj(1) = vk(2)*vi(3) - vk(3)*vi(2)
            vj(2) = vk(3)*vi(1) - vk(1)*vi(3)
            vj(3) = vk(1)*vi(2) - vk(2)*vi(1)
!
!     NORMALIZE J FOR GOOD MEASURE
!
            vjl = dsqrt(vj(1)**2+vj(2)**2+vj(3)**2)
            IF ( vjl==0. ) THEN
               CALL mesage(30,26,Iecpt(1))
               GOTO 600
            ELSE
               vj(1) = vj(1)/vjl
               vj(2) = vj(2)/vjl
               vj(3) = vj(3)/vjl
               x1 = 0.
               y1 = 0.
               x2 = vp12l
               y2 = 0.
               x3 = vi(1)*vd1(1) + vi(2)*vd1(2) + vi(3)*vd1(3)
               y3 = vj(1)*vd1(1) + vj(2)*vd1(2) + vj(3)*vd1(3)
               x4 = -vi(1)*v41(1) - vi(2)*v41(2) - vi(3)*v41(3)
               y4 = -vj(1)*v41(1) - vj(2)*v41(2) - vj(3)*v41(3)
!
!     CHECK TO SEE IF INTERIOR ANGLES ARE LESS THAN 180 DEGREES. IF NOT,
!     CALL FATAL ERROR MESSAGE.
!
               IF ( y3<=0. ) THEN
!
                  Iecpt(2) = 2
                  CALL mesage(30,27,Iecpt(1))
                  GOTO 600
               ELSEIF ( y4<=0. ) THEN
                  Iecpt(2) = 1
                  CALL mesage(30,27,Iecpt(1))
                  GOTO 600
               ELSEIF ( x3<=y3*x4/y4 ) THEN
                  Iecpt(2) = 3
                  CALL mesage(30,27,Iecpt(1))
                  GOTO 600
               ELSEIF ( x4>=x2-(x2-x3)*y4/y3 ) THEN
                  Iecpt(i) = 4
                  CALL mesage(30,27,Iecpt(1))
                  GOTO 600
               ELSE
!
!     TEST FOR PARALLEL EFFECTS.
!
                  cep1 = dabs(y3-y4)
                  cepx = dabs(x3-x4)
                  temp = x3 - x2
                  cep2 = dabs(y4*temp-y3*x4)
                  cepy = dabs(x4*temp+y4*y3)
                  ep = 0.01D0
                  IF ( cep1<ep*cepx ) THEN
                     IF ( cep2<ep*cepy ) THEN
!
!     IN THIS CASE THE PANEL APPROXIMATES A PARALLELOGRAM.
!
                        DO i = 1 , 4
                           p(i) = 1.
                        ENDDO
                        d = -.5*(x4/y4+(x3-x2)/y3+(y3-y4)/(x3-x4))
                        z = pa/(2.*g*t)*(1.+2.*d**2*nuc)
                     ELSE
!
!     AT THIS POINT THE LINE CONNECTING POINTS 3 AND 4 IS -PARALLEL- TO
!     THE LINE CONNECTING POINTS 1 AND 2.
!
                        temp = y3*x4 - y4*(x3-x2)
                        yp = x2*y3*y4/temp
                        p(1) = yp - y1
                        p(2) = yp - y2
                        p(3) = yp - y3
                        p(4) = yp - y4
                        xp = x2*y3*x4/temp
                        sa = (x2-xp)/yp
                        c = (x1-xp)/yp
                        z = ((p(1)*p(2)*pa)/(p(3)*p(4)*2.*g*t))*(1.+c23*nuc*(sa**2+sa*c+c**2))
                     ENDIF
                  ELSEIF ( cep2<ep*cepy ) THEN
!
!     AT THIS POINT THE LINE CONNECTING POINTS 1 AND 4 IS -PARALLEL- TO
!     THE LINE CONNECTING POINTS 2 AND 3.
!
                     d = -.5*(x4/y4+(x3-x2)/y3)
                     xq = x4 - y4*(x3-x4)/(y3-y4)
                     temp = 1.D0/dsqrt(1.D0+d**2)
                     p(1) = (xq-x1-d*y1)*temp
                     p(2) = (xq-x2-d*y2)*temp
                     p(3) = (xq-x3-d*y3)*temp
                     p(4) = (xq-x4-d*y4)*temp
                     temp = xq - x4
                     b = (temp*d+y4)/(temp-y4*d)
                     z = ((p(1)*p(2)*pa)/(p(3)*p(4)*2.*g*t))*(1.+c23*nuc*(b**2+b*d+d**2))
                  ELSE
!
!     IN THIS CASE NO PARALLEL EFFECTS EXIST.
!
                     xq = x4 - (x3-x4)/(y3-y4)*y4
                     temp = y3*x4 - y4*(x3-x2)
                     xp = x2*y3*x4/temp
                     yp = x2*y3*y4/temp
                     xl = dsqrt((xp-yp)**2+yp**2)
                     d = (xq-xp)/yp
                     temp = yp/xl
                     p(1) = temp*(xq-x1-d*y1)
                     p(2) = temp*(xq-x2-d*y2)
                     p(3) = temp*(xq-x3-d*y3)
                     p(4) = temp*(xq-x4-d*y4)
                     c = xl/p(1) - d
                     b = xl/p(4) - c
                     a = xl/p(2) - d
                     a2 = a**2
                     b2 = b**2
                     c2 = c**2
                     d2 = d**2
                     a3 = a2*a
                     b3 = b2*b
                     c3 = c2*c
                     d3 = d2*d
                     a4 = a3*a
                     b4 = b3*b
                     c4 = c3*c
                     d4 = d3*d
                     a5 = a4*a
                     b5 = b4*b
                     c5 = c4*c
                     d5 = d4*d
                     temp = .5*p(1)*p(2)*p(3)*p(4)/xl**2
                     term = (a+b+c23*(a3+b3)+.2*(a5+b5))*dlog(dabs(a+b))
                     term1 = (c+d+c23*(c3+d3)+.2*(c5+d5))*dlog(dabs(c+d))
                     term2 = (b+c+c23*(b3+c3)+.2*(b5+c5))*dlog(dabs(b+c))
                     term3 = (d+a+c23*(d3+a3)+.2*(d5+a5))*dlog(dabs(d+a))
                     term4 = .1*((a2-c2)*(b3-d3)+(b2-d2)*(a3-c3))
                     term5 = .2*((a-c)*(b4-d4)+(b-d)*(a4-c4))
                     f = temp*(term+term1-term2-term3+term4-term5)
                     z = p(1)*p(2)/(p(3)*p(4)*2.*g*t)*(pa+4.*nuc*(f-c23*pa))
                  ENDIF
                  xl13 = dsqrt(x3**2+y3**2)
                  xl24 = dsqrt((x4-x2)**2+y4**2)
                  smallu(1) = x3/xl13
                  smallu(2) = (x4-x2)/xl24
                  smallu(3) = smallu(1)
                  smallu(4) = smallu(2)
                  smallv(1) = y3/xl13
                  smallv(2) = y4/xl24
                  smallv(3) = smallv(1)
                  smallv(4) = smallv(2)
                  temp = x4*y3 - x3*y4
                  avec(1) = -.5*x2*y4*xl13/temp
                  avec(2) = .5*x2*y3*xl24/(temp-x2*(y3-y4))
                  avec(3) = -avec(1)
                  avec(4) = -avec(2)
!
!     SINCE WE ARE DEALING WITH A TWIST PANEL STORE -SMALLV IN SMALLU
!     AND SMALLU IN SMALLV.
!
                  DO i = 1 , 4
                     temp = smallu(i)
                     smallu(i) = -smallv(i)
                     smallv(i) = temp
                  ENDDO
!
                  DO i = 1 , 144
                     ke(i) = 0.D0
                  ENDDO
                  DO ipvt = 1 , 4
                     con = avec(ipvt)*t**2/(24.*z)
!
!     COMPUTE THE -VLEFT- VECTOR
!
                     ivlbeg = 1
                     vleft(1) = vi(1)*smallu(ipvt) + vj(1)*smallv(ipvt)
                     vleft(2) = vi(2)*smallu(ipvt) + vj(2)*smallv(ipvt)
                     vleft(3) = vi(3)*smallu(ipvt) + vj(3)*smallv(ipvt)
                     IF ( Iecpt(4*ipvt+5)/=0 ) THEN
                        CALL transd(Iecpt(4*ipvt+5),ti)
                        ivlbeg = 4
                        CALL gmmatd(ti,3,3,1,vleft(1),3,1,0,vleft(4))
                     ENDIF
!
!     COMPUTE THE 6 X 6 -S
!
                     DO j = 1 , 4
                        jt = (ipvt-1)*36 + (j-1)*9 + 1
                        ivrbeg = 1
                        vright(1) = smallu(j)*vi(1) + smallv(j)*vj(1)
                        vright(2) = smallu(j)*vi(2) + smallv(j)*vj(2)
                        vright(3) = smallu(j)*vi(3) + smallv(j)*vj(3)
                        IF ( Iecpt(4*j+5)/=0 ) THEN
                           CALL transd(Iecpt(4*j+5),ti)
                           CALL gmmatd(vright(1),1,3,0,ti,3,3,0,vright(4))
                           ivrbeg = 4
                        ENDIF
                        CALL gmmatd(vleft(ivlbeg),3,1,0,vright(ivrbeg),1,3,0,ke(jt))
                        jt8 = jt + 8
                        DO k = jt , jt8
                           ke(k) = con*ke(k)*avec(j)
                        ENDDO
                     ENDDO
                  ENDDO
!
!     NOW REARRANGE KE BY INCREASING SIL THEN OUTPUT IT VIA EMGOUT
!     FIRST DETERMINE WHAT INCREASING SIL ORDER WILL BE
!
                  ASSIGN 300 TO korm
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
 100  DO
      DO i = 1 , 3
         ip1 = i + 1
         it = ipart(i)
         DO j = ip1 , 4
            jt = ipart(j)
            IF ( Isilno(it)>Isilno(jt) ) THEN
               ipart(i) = jt
               ipart(j) = it
               it = jt
               GOTO 200
            ENDIF
         ENDDO
      ENDDO
      isort = 1
      GOTO korm
 200  ENDDO
!
!     NOW REARRANGE TERMS IN THE STIFFNESS MATRIX KE AND STORE IN KOUT
!
!
!     KE = (K  ,K  ,K  ,K  ,K  ,...,K  ,K  ,...,K  )
!            11  12  13  14  21      24  31      44
!
!     WHERE  K  IS A 3X3 SUBMATRIX AND  SILS ARE IN GRID POINT ORDER
!             IJ
!
!     AND    *****                 ****
!            * K     K     K     K    *
!            *  L1L1  L1L2  L1L3  L1L4*
!            *                        *
!            * K     K     K     K    *
!     KOUT = *  L2L1  L2L2  L2L3  L2L4*
!            *                        *
!            * K     K     K     K    *
!            *  L3L1  L3L2  L3L3  L3L4*
!            *                        *
!            * K     K     K     K    *
!            *  L4L1  L4L2  L4L3  L4L4*
!            ****                  ****
!
!     WHERE  KOUT     IS A   3X3    MATRIX AND SILS ARE IN INCREASING
!                LILJ
!     ORDER
!
 300  DO i = 1 , 4
      is = ipart(i)
      DO j = 1 , 4
         js = ipart(j)
         DO k = 1 , 3
            DO l = 1 , 3
               iout = (i-1)*36 + (j-1)*3 + (k-1)*12 + l
               ike = (is-1)*36 + (js-1)*9 + (k-1)*3 + l
               kout(iout) = ke(ike)
            ENDDO
         ENDDO
      ENDDO
   ENDDO
!
!     OUTPUT THE STIFFNESS MATRIX
!
   CALL emgout(kout,kout,144,1,dict,1,ip)
!
!     HERE WE CALCULATE THE MASS MATRIX VIA SUBROUTINE EMASTQ
!
!
 400  IF ( Ismb(2)==0 ) RETURN
!
   CALL emadtq(6,me)
   IF ( isort/=1 ) THEN
      ASSIGN 500 TO korm
      GOTO 100
   ENDIF
!
!     RETURN WITH A GRID POINT SORT ARRAY IN IPART
!
!
 500  DO i = 1 , 4
      it = 1 + (ipart(i)-1)*3
      ij = (i-1)*3 + 1
      mout(ij) = me(it)
      mout(ij+1) = me(it+1)
      mout(ij+2) = me(it+2)
   ENDDO
!
   dict(1) = Estid
   dict(2) = 2
   dict(3) = 12
   dict(4) = 7
   dict5 = 0.
!
   CALL emgout(kout,kout,12,1,dict,2,ip)
   RETURN
 600  Nogo = .TRUE.
   RETURN
END SUBROUTINE twistd