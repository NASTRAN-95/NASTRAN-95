!*==shears.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE shears
   USE c_emgdic
   USE c_emgest
   USE c_emgprm
   USE c_matin
   USE c_matout
   USE c_system
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , a2 , a3 , a4 , a5 , b , b2 , b3 , b4 , b5 , c , c2 , c23 , c3 , c4 , c5 , cep1 , cep2 , cepx , cepy , con , d , d2 , &
         & d3 , d4 , d5 , dict5 , e , ep , f , g , nuc , pa , sa , t , temp , term , term1 , term2 , term3 , term4 , term5 , v12dk ,&
         & vjl , vkl , vp12l , x1 , x2 , x3 , x4 , xl , xl13 , xl24 , xp , xq , y1 , y2 , y3 , y4 , yp , z
   REAL , DIMENSION(4) :: avec , p , smallu , smallv
   INTEGER , DIMENSION(11) :: dict
   INTEGER :: i , ij , ike , iout , ip , ip1 , ipvt , is , isort , it , ivlbeg , ivrbeg , j , js , jt , jt8 , k , korm , l
   INTEGER , DIMENSION(100) :: iecpt
   INTEGER , DIMENSION(4) , SAVE :: ipart
   REAL , DIMENSION(144) :: ke , kout , me , mout
   REAL , DIMENSION(9) :: ti
   REAL , DIMENSION(3) :: v12 , v41 , vd1 , vd2 , vi , vj , vk , vkn , vp12
   REAL , DIMENSION(6) :: vleft , vright
   EXTERNAL emastq , emgout , gmmats , mat , mesage , transs
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE COMPUTES THE 12 X 12 STIFFNESS MATRIX FOR THE
!     SHEAR PANEL ELEMENT, AS WELL AS ITS DIAGONALIZED MASS MATRIX.
!
!     SINGLE PRECISION VERSION
!
!     ECPT FOR THE SHEAR PANEL ELEMENT
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
!     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
   !>>>>EQUIVALENCE (me(1),ke(1)) , (kout(1),mout(1)) , (Iecpt(1),Ecpt(1),Ielid) , (dict(5),dict5)
   DATA ipart/1 , 2 , 3 , 4/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ngrids = 4
         ldict = 5 + ngrids
!
!     IF STIFFNESS MATRIX NOT NEEDED GO TO PERFORM MASS CALCULATIONS
!
         IF ( ismb(1)==0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         dict(1) = estid
         dict(2) = 1
         dict(3) = 12
         dict(4) = 7
         ip = iprec
         isort = 0
!
!     CALL MAT TO GET MATERIAL PROPERTIES.
!
         matidc = matid
         matflg = 1
         eltemp = tempel
         CALL mat(iecpt(1))
         dict5 = gsube
!
         t = tsp
         g = gsp
         e = esp
         IF ( t*g==0.0 ) THEN
!
!     ERROR EXITS
!
            CALL mesage(30,26,iecpt(1))
            nogo = .TRUE.
            RETURN
         ELSE
            c23 = 2.0/3.0
            nuc = 1.0/(1.0+nu)
!
!     COMPUTE DIAGONAL VECTORS.
!
            DO i = 1 , 3
               vd1(i) = gp3(i) - gp1(i)
               vd2(i) = gp4(i) - gp2(i)
            ENDDO
!
!     COMPUTE THE NORMAL VECTOR VKN, NORMALIZE, AND COMPUTE THE
!     PROJECTED AREA, PA
!
            vkn(1) = vd1(2)*vd2(3) - vd1(3)*vd2(2)
            vkn(2) = vd1(3)*vd2(1) - vd1(1)*vd2(3)
            vkn(3) = vd1(1)*vd2(2) - vd1(2)*vd2(1)
            vkl = sqrt(vkn(1)**2+vkn(2)**2+vkn(3)**2)
            IF ( vkl==0. ) THEN
               CALL mesage(30,26,iecpt(1))
               nogo = .TRUE.
               RETURN
            ELSE
               vk(1) = vkn(1)/vkl
               vk(2) = vkn(2)/vkl
               vk(3) = vkn(3)/vkl
               pa = vkl/2.
!
!     COMPUTE  SIDES -12- AND -41-
!
               DO i = 1 , 3
                  v12(i) = gp2(i) - gp1(i)
                  v41(i) = gp1(i) - gp4(i)
               ENDDO
!
!     COMPUTE DOT PRODUCT, V12DK, OF V12 AND VK, THE VECTORS VP12, VI,
!     VJ
!
               v12dk = v12(1)*vk(1) + v12(2)*vk(2) + v12(3)*vk(3)
               vp12(1) = v12(1) - v12dk*vk(1)
               vp12(2) = v12(2) - v12dk*vk(2)
               vp12(3) = v12(3) - v12dk*vk(3)
               vp12l = sqrt(vp12(1)**2+vp12(2)**2+vp12(3)**2)
               IF ( vp12l==0. ) THEN
                  CALL mesage(30,26,iecpt(1))
                  nogo = .TRUE.
                  RETURN
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
                  vjl = sqrt(vj(1)**2+vj(2)**2+vj(3)**2)
                  IF ( vjl==0. ) THEN
                     CALL mesage(30,26,iecpt(1))
                     nogo = .TRUE.
                     RETURN
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
                        iecpt(2) = 2
                        CALL mesage(30,27,iecpt(1))
                        nogo = .TRUE.
                        RETURN
                     ELSEIF ( y4<=0. ) THEN
                        iecpt(2) = 1
                        CALL mesage(30,27,iecpt(1))
                        nogo = .TRUE.
                        RETURN
                     ELSEIF ( x3<=y3*x4/y4 ) THEN
                        iecpt(2) = 4
                        CALL mesage(30,27,iecpt(1))
                        nogo = .TRUE.
                        RETURN
                     ELSEIF ( x4>=x2-(x2-x3)*y4/y3 ) THEN
                        iecpt(2) = 3
                        CALL mesage(30,27,iecpt(1))
                        nogo = .TRUE.
                        RETURN
                     ELSE
!
!     TEST FOR PARALLEL EFFECTS.
!
                        cep1 = abs(y3-y4)
                        cepx = abs(x3-x4)
                        temp = x3 - x2
                        cep2 = abs(y4*temp-y3*x4)
                        cepy = abs(x4*temp+y4*y3)
                        ep = 0.010
                        IF ( cep1<ep*cepx ) THEN
                           IF ( cep2<ep*cepy ) THEN
!
!     IN THIS CASE THE PANEL APPROXIMATES A PARALLELOGRAM.
!
                              DO i = 1 , 4
                                 p(i) = 1.
                              ENDDO
                              d = -.50*(x4/y4+(x3-x2)/y3+(y3-y4)/(x3-x4))
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
                           temp = 1.0/sqrt(1.0+d**2)
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
                           xl = sqrt((xq-xp)**2+yp**2)
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
                           temp = .50*p(1)*p(2)*p(3)*p(4)/xl**2
                           term = (a+b+c23*(a3+b3)+.20*(a5+b5))*alog(abs(a+b))
                           term1 = (c+d+c23*(c3+d3)+.20*(c5+d5))*alog(abs(c+d))
                           term2 = (b+c+c23*(b3+c3)+.20*(b5+c5))*alog(abs(b+c))
                           term3 = (d+a+c23*(d3+a3)+.20*(d5+a5))*alog(abs(d+a))
                           term4 = .10*((a2-c2)*(b3-d3)+(b2-d2)*(a3-c3))
                           term5 = .20*((a-c)*(b4-d4)+(b-d)*(a4-c4))
                           f = temp*(term+term1-term2-term3+term4-term5)
                           z = p(1)*p(2)/(p(3)*p(4)*2.*g*t)*(pa+4.*nuc*(f-c23*pa))
                        ENDIF
                        xl13 = sqrt(x3**2+y3**2)
                        xl24 = sqrt((x4-x2)**2+y4**2)
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
                        DO i = 1 , 144
                           ke(i) = 0.
                        ENDDO
                        DO ipvt = 1 , 4
                           con = avec(ipvt)/(2.*z)
!
!     COMPUTE THE -VLEFT- VECTOR
!
                           ivlbeg = 1
                           vleft(1) = vi(1)*smallu(ipvt) + vj(1)*smallv(ipvt)
                           vleft(2) = vi(2)*smallu(ipvt) + vj(2)*smallv(ipvt)
                           vleft(3) = vi(3)*smallu(ipvt) + vj(3)*smallv(ipvt)
                           IF ( iecpt(4*ipvt+5)/=0 ) THEN
                              CALL transs(iecpt(4*ipvt+5),ti)
                              ivlbeg = 4
                              CALL gmmats(ti,3,3,1,vleft(1),3,1,0,vleft(4))
                           ENDIF
!
!     COMPUTE THE 6 X 6 -S
!
                           DO j = 1 , 4
                              ivrbeg = 1
                              vright(1) = smallu(j)*vi(1) + smallv(j)*vj(1)
                              vright(2) = smallu(j)*vi(2) + smallv(j)*vj(2)
                              vright(3) = smallu(j)*vi(3) + smallv(j)*vj(3)
                              IF ( iecpt(4*j+5)/=0 ) THEN
                                 CALL transs(iecpt(4*j+5),ti)
                                 CALL gmmats(vright(1),1,3,0,ti,3,3,0,vright(4))
                                 ivrbeg = 4
                              ENDIF
                              jt = (ipvt-1)*36 + (j-1)*9 + 1
                              CALL gmmats(vleft(ivlbeg),3,1,0,vright(ivrbeg),1,3,0,ke(jt))
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
                        ASSIGN 20 TO korm
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            DO i = 1 , 3
               ip1 = i + 1
               it = ipart(i)
               DO j = ip1 , 4
                  jt = ipart(j)
                  IF ( isilno(it)>isilno(jt) ) THEN
                     ipart(i) = jt
                     ipart(j) = it
                     it = jt
                     CYCLE SPAG_Loop_1_1
                  ENDIF
               ENDDO
            ENDDO
            isort = 1
            GOTO korm
         ENDDO SPAG_Loop_1_1
!
!     NOW REARRANGE TERMS IN THE STIFFNESS MATRIX KE AND STORE IN KOUT
!
!     KE = (K  ,K  ,K  ,K  ,K  ,...,K  ,K  ,...,K  )
!            11  12  13  14  21      24  31      44
!
!     WHERE  K  IS A 3X3 SUBMATRIX AND  SILS ARE IN GRID POINT ORDER
!             IJ
!
!     AND    *****                  ****
!            * K     K     K     K     *
!            *  L1L1  L1L2  L1L3  L1L4 *
!            *                         *
!            * K     K     K     K     *
!     KOUT = *  L2L1  L2L2  L2L3  L2L4 *
!            *                         *
!            * K     K     K     K     *
!            *  L3L1  L3L2  L3L3  L3L4 *
!            *                         *
!            * K     K     K     K     *
!            *  L4L1  L4L2  L4L3  L4L4 *
!            ****                   ****
!
!     WHERE  KOUT     IS A   3X3    MATRIX AND SILS ARE IN INCREASING
!                LILJ
!     ORDER
!
 20      DO i = 1 , 4
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
         spag_nextblock_1 = 3
      CASE (3)
!
!     HERE WE CALCULATE THE MASS MATRIX VIA SUBROUTINE EMASTQ
!
         IF ( ismb(2)==0 ) RETURN
!
!WKBR 3/94 CALL EMADTQ (6,ME)
         CALL emastq(6,me)
         IF ( isort/=1 ) THEN
            ASSIGN 40 TO korm
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     RETURN WITH A GRID POINT SORT ARRAY IN IPART
!
 40      DO i = 1 , 4
            it = 1 + (ipart(i)-1)*3
            ij = (i-1)*3 + 1
            mout(ij) = me(it)
            mout(ij+1) = me(it+1)
            mout(ij+2) = me(it+2)
         ENDDO
!
         dict(1) = estid
         dict(2) = 2
         dict(3) = 12
         dict(4) = 7
         dict5 = 0.
!
         CALL emgout(kout,kout,12,1,dict,2,ip)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE shears
