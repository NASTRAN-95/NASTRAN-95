!*==kpanel.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE kpanel(Iarg)
   IMPLICIT NONE
   USE c_matin
   USE c_matout
   USE c_sma1bk
   USE c_sma1cl
   USE c_sma1dp
   USE c_sma1et
   USE c_sma1io
   USE c_system
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iarg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ii , ipvt , ivlbeg , ivrbeg , j , k
   INTEGER , DIMENSION(100) :: iecpt
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!*****
! THIS ROUTINE COMPUTES THE  4  6 X 6 MATRICES K(NPVT,NPVT), K(NPVT,J1)
! K(NPVT,J2), K(NPVT,J3) FOR A SHEAR PANEL (IF IARG = 4) AND FOR A
! TWIST PANEL (IF IARG = 5)
!*****
!
!                 E C P T  F O R  B O T H  P A N E L S
! ECPT( 1)  -  IELID          ELEMENT ID. NO.
! ECPT( 2)  -  ISILNO(4)      SCALAR INDEX NUMBERS
! ECPT( 3)  -   ...                   ...
! ECPT( 4)  -   ...                   ...
! ECPT( 5)  -   ...                   ...
! ECPT( 6)  -  MATID          MATERIAL ID.
! ECPT( 7)  -  T              THICKNESS
! ECPT( 8)  -  FMU            NON-STRUCTURAL MASS
! ECPT( 9)  -  ICSID1         COOR. SYS. ID. FOR GRID POINT 1
! ECPT(10)  -  GP1(3)         BASIC COORDINATES FOR GRID POINT 1
! ECPT(11)  -   ...                      ...
! ECPT(12)  -   ...                      ...
! ECPT(13)  -  ICSID2         COOR. SYS. ID. FOR GRID POINT 2
! ECPT(14)  -  GP2(3)         BASIC COORDINATES FOR GRID POINT 2
! ECPT(15)  -   ...                      ...
! ECPT(16)  -   ...                      ...
! ECPT(17)  -  ICSID3         COOR. SYS. ID. FOR GRID POINT 3
! ECPT(18)  -  GP3(3)         BASIC COORDINATES FOR GRID POINT 3
! ECPT(19)  -   ...                      ...
! ECPT(20)  -   ...                      ...
! ECPT(21)  -  ICSID4         COOR. SYS. ID. FOR GRID POINT 4
! ECPT(22)  -  GP4(3)         BASIC COORDINATES FOR GRID POINT 4
! ECPT(23)  -   ...                      ...
! ECPT(24)  -   ...                      ...
! ECPT(25)  -  TEMPEL         ELEMENT TEMPERATURE
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
! SMA1 I/O PARAMETERS
!
!
! SMA1 VARIABLE CORE BOOKKEEPING PARAMETERS
!
!
! SMA1 PROGRAM CONTROL PARAMETERS
!
!
! ECPT COMMON BLOCK
!
!
! SMA1 LOCAL VARIABLES
!
!
! INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
!
!
!
!
!
!
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1),Ielid)
!
! CALL MAT TO GET MATERIAL PROPERTIES.
!
   matidc = matid
   matflg = 1
   eltemp = tempel
   CALL mat(iecpt(1))
   dampc = gsube
!
! STORE ECPT AND MPT VARIABLES IN DOUBLE PRECISION LOCATIONS
!
   e = esp
   g = gsp
   nu = nusp
   t = tsp
   IF ( t*g/=0.0 ) THEN
      c23 = 2.0D0/3.0D0
      nuc = 1.0D0/(1.0D0+nu)
!
! COMPUTE DIAGONAL VECTORS.
!
      DO i = 1 , 3
         vd1(i) = gp3(i) - gp1(i)
         vd2(i) = gp4(i) - gp2(i)
      ENDDO
!
! COMPUTE THE NORMAL VECTOR VKN, NORMALIZE, AND COMPUTE THE PROJECTED
! AREA, PA
!
      vkn(1) = vd1(2)*vd2(3) - vd1(3)*vd2(2)
      vkn(2) = vd1(3)*vd2(1) - vd1(1)*vd2(3)
      vkn(3) = vd1(1)*vd2(2) - vd1(2)*vd2(1)
      vkl = dsqrt(vkn(1)**2+vkn(2)**2+vkn(3)**2)
      IF ( vkl/=0.0D0 ) THEN
         vk(1) = vkn(1)/vkl
         vk(2) = vkn(2)/vkl
         vk(3) = vkn(3)/vkl
         pa = .5D0*vkl
!
! COMPUTE  SIDES -12- AND -41-
!
         DO i = 1 , 3
            v12(i) = gp2(i) - gp1(i)
            v41(i) = gp1(i) - gp4(i)
         ENDDO
!
! COMPUTE DOT PRODUCT, V12DK, OF V12 AND VK, THE VECTORS VP12, VI, VJ
!
         v12dk = v12(1)*vk(1) + v12(2)*vk(2) + v12(3)*vk(3)
         vp12(1) = v12(1) - v12dk*vk(1)
         vp12(2) = v12(2) - v12dk*vk(2)
         vp12(3) = v12(3) - v12dk*vk(3)
         vp12l = dsqrt(vp12(1)**2+vp12(2)**2+vp12(3)**2)
         IF ( vp12l/=0.0D0 ) THEN
            vi(1) = vp12(1)/vp12l
            vi(2) = vp12(2)/vp12l
            vi(3) = vp12(3)/vp12l
            vj(1) = vk(2)*vi(3) - vk(3)*vi(2)
            vj(2) = vk(3)*vi(1) - vk(1)*vi(3)
            vj(3) = vk(1)*vi(2) - vk(2)*vi(1)
!
! NORMALIZE J FOR GOOD MEASURE
!
            vjl = dsqrt(vj(1)**2+vj(2)**2+vj(3)**2)
            IF ( vjl/=0.0D0 ) THEN
               vj(1) = vj(1)/vjl
               vj(2) = vj(2)/vjl
               vj(3) = vj(3)/vjl
               x1 = 0.0D0
               y1 = 0.0D0
               x2 = vp12l
               y2 = 0.0D0
               x3 = vi(1)*vd1(1) + vi(2)*vd1(2) + vi(3)*vd1(3)
               y3 = vj(1)*vd1(1) + vj(2)*vd1(2) + vj(3)*vd1(3)
               x4 = -vi(1)*v41(1) - vi(2)*v41(2) - vi(3)*v41(3)
               y4 = -vj(1)*v41(1) - vj(2)*v41(2) - vj(3)*v41(3)
!
! CHECK TO SEE IF INTERIOR ANGLES ARE LESS THAN 180 DEGREES.  IF NOT,
! CALL FATAL ERROR MESSAGE.
!
               IF ( y3<=0.0D0 ) THEN
                  iecpt(2) = 2
                  GOTO 100
               ELSEIF ( y4<=0.0D0 ) THEN
                  iecpt(2) = 1
                  GOTO 100
               ELSEIF ( x3<=y3*x4/y4 ) THEN
                  iecpt(2) = 4
                  GOTO 100
               ELSEIF ( x4>=x2-(x2-x3)*y4/y3 ) THEN
                  iecpt(2) = 3
                  GOTO 100
               ELSE
!
! TEST FOR PARALLEL EFFECTS.
!
                  temp = x3 - x2
                  ep = 1.0D-1
                  IF ( dabs(y3-y4)<dabs(x3-x4)*ep ) THEN
                     IF ( dabs(y4*temp-y3*x4)<dabs(x4*temp+y4*y3)*ep ) THEN
!
! IN THIS CASE THE PANEL APPROXIMATES A PARALLELOGRAM.
!
                        DO i = 1 , 4
                           p(i) = 1.0D0
                        ENDDO
                        d = -.5D0*(x4/y4+(x3-x2)/y3+(y3-y4)/(x3-x4))
                        z = pa/(2.0D0*g*t)*(1.0D0+2.0D0*d**2*nuc)
                     ELSE
!
! AT THIS POINT THE LINE CONNECTING POINTS 3 AND 4 IS -PARALLEL- TO THE
! LINE CONNECTING POINTS 1 AND 2.
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
                        z = ((p(1)*p(2)*pa)/(p(3)*p(4)*2.0D0*g*t))*(1.0D0+c23*nuc*(sa**2+sa*c+c**2))
                     ENDIF
                  ELSEIF ( dabs(y4*temp-y3*x4)<dabs(x4*temp+y4*y3)*ep ) THEN
!
! AT THIS POINT THE LINE CONNECTING POINTS 1 AND 4 IS -PARALLEL- TO THE
! LINE CONNECTING POINTS 2 AND 3.
!
                     d = -.5D0*(x4/y4+(x3-x2)/y3)
                     xq = x4 - y4*(x3-x4)/(y3-y4)
                     temp = 1.0D0/dsqrt(1.0D0+d**2)
                     p(1) = (xq-x1-d*y1)*temp
                     p(2) = (xq-x2-d*y2)*temp
                     p(3) = (xq-x3-d*y3)*temp
                     p(4) = (xq-x4-d*y4)*temp
                     temp = xq - x4
                     b = (temp*d+y4)/(temp-y4*d)
                     z = ((p(1)*p(2)*pa)/(p(3)*p(4)*2.0D0*g*t))*(1.0D0+c23*nuc*(b**2+b*d+d**2))
                  ELSE
!
! IN THIS CASE NO PARALLEL EFFECTS EXIST.
!
                     xq = x4 - (x3-x4)/(y3-y4)*y4
                     temp = y3*x4 - y4*(x3-x2)
                     xp = x2*y3*x4/temp
                     yp = x2*y3*y4/temp
                     xl = dsqrt((xq-xp)**2+yp**2)
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
                     temp = .5D0*p(1)*p(2)*p(3)*p(4)/xl**2
                     term = a + b + c23*(a3+b3) + .2D0*(a5+b5)
                     term1 = c + d + c23*(c3+d3) + .2D0*(c5+d5)
                     term2 = b + c + c23*(b3+c3) + .2D0*(b5+c5)
                     term3 = d + a + c23*(d3+a3) + .2D0*(d5+a5)
                     term = term*dlog(dabs(a+b))
                     term1 = term1*dlog(dabs(c+d))
                     term2 = term2*dlog(dabs(b+c))
                     term3 = term3*dlog(dabs(d+a))
                     term4 = .1D0*((a2-c2)*(b3-d3)+(b2-d2)*(a3-c3))
                     term5 = .2D0*((a-c)*(b4-d4)+(b-d)*(a4-c4))
                     f = temp*(term+term1-term2-term3+term4-term5)
                     z = p(1)*p(2)/(p(3)*p(4)*2.0D0*g*t)*(pa+4.0D0*nuc*(f-c23*pa))
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
                  avec(1) = -.5D0*x2*y4*xl13/temp
                  avec(2) = .5D0*x2*y3*xl24/(temp-x2*(y3-y4))
                  avec(3) = -avec(1)
                  avec(4) = -avec(2)
!
! IF IARG = 4, WE HAVE A SHEAR PANEL, AND IF IARG = 5, A TWIST PANEL.
!
                  IF ( Iarg/=4 ) THEN
!
! SINCE WE ARE DEALING WITH A TWIST PANEL STORE -SMALLV IN SMALLU AND
! SMALLU IN SMALLV.
!
                     DO i = 1 , 4
                        temp = smallu(i)
                        smallu(i) = -smallv(i)
                        smallv(i) = temp
                     ENDDO
                  ENDIF
!
! SEARCH THE LIST OF THE 4 SIL NOS. TO DETERMINE WHICH IS THE PIVOT
!
                  DO i = 1 , 4
                     IF ( isilno(i)==npvt ) THEN
                        ipvt = i
                        GOTO 2
                     ENDIF
                  ENDDO
                  CALL mesage(-30,34,iecpt(1))
!
! COMPUTE THE DOUBLE PRECISION CONSTANT DPCON
!
 2                IF ( Iarg==5 ) THEN
                     dpcon = avec(ipvt)*t**2/(24.0D0*z)
                  ELSE
                     dpcon = avec(ipvt)/(2.0D0*z)
                  ENDIF
!
! COMPUTE THE -VLEFT- VECTOR
!
                  ivlbeg = 1
                  vleft(1) = vi(1)*smallu(ipvt) + vj(1)*smallv(ipvt)
                  vleft(2) = vi(2)*smallu(ipvt) + vj(2)*smallv(ipvt)
                  vleft(3) = vi(3)*smallu(ipvt) + vj(3)*smallv(ipvt)
                  IF ( iecpt(4*ipvt+5)/=0 ) THEN
                     CALL transd(iecpt(4*ipvt+5),ti)
                     ivlbeg = 4
                     CALL gmmatd(ti,3,3,1,vleft(1),3,1,0,vleft(4))
                  ENDIF
!
! ZERO OUT THE 6 X 6 MATRIX KE
!
                  DO i = 1 , 36
                     ke(i) = 0.0D0
                  ENDDO
!
! COMPUTE THE 6 X 6 -S
!
                  DO j = 1 , 4
                     ivrbeg = 1
                     vright(1) = smallu(j)*vi(1) + smallv(j)*vj(1)
                     vright(2) = smallu(j)*vi(2) + smallv(j)*vj(2)
                     vright(3) = smallu(j)*vi(3) + smallv(j)*vj(3)
                     IF ( iecpt(4*j+5)/=0 ) THEN
                        CALL transd(iecpt(4*j+5),ti)
                        CALL gmmatd(vright(1),1,3,0,ti,3,3,0,vright(4))
                        ivrbeg = 4
                     ENDIF
                     CALL gmmatd(vleft(ivlbeg),3,1,0,vright(ivrbeg),1,3,0,ke(1))
                     DO k = 1 , 9
                        ke(k) = dpcon*ke(k)*avec(j)
                     ENDDO
                     IF ( Iarg==5 ) THEN
                        ke(22) = ke(1)
                        ke(23) = ke(2)
                        ke(24) = ke(3)
                        ke(28) = ke(4)
                        ke(29) = ke(5)
                        ke(30) = ke(6)
                        ke(34) = ke(7)
                        ke(35) = ke(8)
                        ke(36) = ke(9)
                        DO ii = 1 , 9
                           ke(ii) = 0.0D0
                        ENDDO
                     ELSE
                        ke(13) = ke(7)
                        ke(14) = ke(8)
                        ke(15) = ke(9)
                        ke(7) = ke(4)
                        ke(8) = ke(5)
                        ke(9) = ke(6)
                        ke(4) = 0.0D0
                        ke(5) = 0.0D0
                        ke(6) = 0.0D0
                     ENDIF
                     CALL sma1b(ke,iecpt(j+1),-1,ifkgg,0.0D0)
                     IF ( iopt4/=0 .AND. gsube/=0.0 ) THEN
                        k4ggsw = 1
                        CALL sma1b(ke,iecpt(j+1),-1,if4gg,dampc)
                     ENDIF
                  ENDDO
                  RETURN
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   CALL mesage(30,26,iecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
   nogo = 1
   RETURN
 100  CALL mesage(30,27,iecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
   nogo = 1
END SUBROUTINE kpanel
