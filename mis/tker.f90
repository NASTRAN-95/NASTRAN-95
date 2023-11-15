
SUBROUTINE tker(X0,Y0,Z0,Kr,Br,Sgr,Cgr,Sgs,Cgs,T1,T2,M)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ind
   REAL K10 , K10t1 , K1it1 , K1rt1 , K20 , K20t2p , K2it2p , K2rt2p , Kd1i , Kd1r , Kd2i , Kd2r
   COMMON /dlm   / K10 , K20 , K1rt1 , K1it1 , K2rt2p , K2it2p , K10t1 , K20t2p
   COMMON /kds   / Ind , Kd1r , Kd1i , Kd2r , Kd2i
!
! Dummy argument declarations
!
   REAL Br , Cgr , Cgs , Kr , M , Sgr , Sgs , T1 , T2 , X0 , Y0 , Z0
!
! Local variable declarations
!
   REAL beta2 , bigr , c1 , c10 , c11 , c2 , c3 , c4 , c5 , c6 , c7 , c8 , c9 , car , ck1i , ck1r , ck2i , ck2r , dk1i , dk1r ,     &
      & dk2i , dk2r , e , eps , exarg , i00i , i00r , i0ui , i0ur , i10i , i10r , i1ui , i1ur , i20i3 , i20r3 , i2ui3 , i2ur3 ,     &
      & j00i , j00r , j0ui , j0ur , k1 , k2 , mu , mu1 , q1 , q10 , q11 , q2 , q3 , q4 , q5 , q6 , q7 , q8 , q9 , r1 , r10 , r11 ,  &
      & r1s , r2 , r3 , r4 , r5 , r6 , r7 , r8 , r9 , t2p
   INTEGER ichuz
!
! End of declarations
!
!
!     COMPUTES EITHER THE TOTAL KERNELS (IND=0) USED IN THE CALCULATION
!     OF A FINITE LENGTH DOUBLET LINE,  OR
!     THE INCREMENTAL OSCILLATORY KERNELS (IND=1) USED IN EVALUATING
!     THE INFLUENCE COEFFICIENT MATRIX ELEMENTS
!
!
   eps = 0.00001
   K10 = 0.0
   K20 = 0.0
   K1rt1 = 0.0
   K1it1 = 0.0
   K2rt2p = 0.0
   K2it2p = 0.0
   K10t1 = 0.0
   K20t2p = 0.0
   r1 = sqrt(Y0*Y0+Z0*Z0)
   r1s = r1
   IF ( abs(r1)>eps ) THEN
      c1 = Cgr
      c2 = Sgr
      c3 = Cgs
      c4 = Sgs
      t2p = (Z0*Z0*c1*c3+Y0*Y0*c2*c4-Z0*Y0*(c2*c3+c1*c4))
      T2 = (100.*t2p)/(Br*Br)
      IF ( abs(T2)<eps ) THEN
         ichuz = 1
         T1 = Cgr*Cgs + Sgr*Sgs
         T2 = 0.0
      ELSE
         T1 = Cgr*Cgs + Sgr*Sgs
         IF ( abs(T1)<eps ) THEN
            ichuz = 2
            T1 = 0.
         ELSE
            ichuz = 3
         ENDIF
      ENDIF
      beta2 = (1.-M*M)
      bigr = sqrt(X0*X0+beta2*r1*r1)
      k1 = Kr*r1/Br
      mu1 = (M*bigr-X0)/(beta2*r1)
      mu = abs(mu1)
      k2 = k1*k1
      IF ( mu1<0 ) THEN
         ichuz = ichuz + 3
      ELSEIF ( mu1==0 ) THEN
         ichuz = ichuz + 6
      ENDIF
!
!     (N*C)**2  FOR  N = 1,11  AND C = .372 =
!
!       .138384      .553536     1.245456      2.214144
!      3.4596       4.981824     6.780816      8.856576
!     11.209104    13.8384      16.744464
!
!     (N*C)  FOR  N = 1,12  AND  14,16,18,20,22   =
!
!       .744        1.116        1.488         1.86      2.232
!      2.604        2.976        3.348         3.72      4.092
!      4.464        5.208        5.952         6.696     7.44
!      8.184
!
!     A(N)  FOR N = 1,11  =
!
!      .24186198   -2.7918027    24.991079    -111.59196
!      271.43549   -305.75288    -41.18363     545.98537
!     -644.78155    328.72755    -64.279511
!
      exarg = -0.372*mu
!
!     THE FOLLOWING TEST ON THE SIZE OF THE ARGUMENT TO  EXP  IS
!     NEEDED TO AVOID UNDERFLOW IN  SUBPROGRAM  EXP
!
      IF ( exarg>=-180.0 ) THEN
         e = exp(exarg)
      ELSE
         e = 0.0
      ENDIF
      c1 = 0.138384 + k2
      c2 = 0.553536 + k2
      c3 = 1.245456 + k2
      c4 = 2.214144 + k2
      c5 = 3.4596 + k2
      c6 = 4.981824 + k2
      c7 = 6.780816 + k2
      c8 = 8.856576 + k2
      c9 = 11.209104 + k2
      c10 = 13.8384 + k2
      c11 = 16.744464 + k2
      r1 = .24186198/c1
      r2 = -2.7918027/c2
      r3 = 24.991079/c3
      r4 = -111.59196/c4
      r5 = 271.43549/c5
      r6 = -305.75288/c6
      r7 = -41.18363/c7
      r8 = 545.98537/c8
      r9 = -644.78155/c9
      r10 = 328.72755/c10
      r11 = -64.279511/c11
      IF ( ichuz>=4 ) THEN
         i00r = .372*(r1+2.*r2+3.*r3+4.*r4+5.*r5+6.*r6+7.*r7+8.*r8+9.*r9+10.*r10+11.*r11)
         i00i = -k1*(r1+r2+r3+r4+r5+r6+r7+r8+r9+r10+r11)
      ENDIF
      IF ( ichuz==1 ) GOTO 200
      IF ( ichuz==4 ) THEN
      ELSEIF ( ichuz==7 ) THEN
         i10i = -k1*i00r
      ELSE
         q1 = r1/c1
         q2 = r2/c2
         q3 = r3/c3
         q4 = r4/c4
         q5 = r5/c5
         q6 = r6/c6
         q7 = r7/c7
         q8 = r8/c8
         q9 = r9/c9
         q10 = r10/c10
         q11 = r11/c11
         IF ( ichuz==1 ) GOTO 200
         IF ( ichuz==2 .OR. ichuz==3 ) GOTO 100
         IF ( ichuz==4 ) THEN
         ELSEIF ( ichuz==7 ) THEN
            i10i = -k1*i00r
         ELSE
            j00r = q1*(.138384-k2) + q2*(.553536-k2) + q3*(1.245456-k2) + q4*(2.214144-k2) + q5*(3.4596-k2) + q6*(4.981824-k2)      &
                 & + q7*(6.780816-k2) + q8*(8.856576-k2) + q9*(11.209104-k2) + q10*(13.8384-k2) + q11*(16.744464-k2)
            i20r3 = 2. + k1*i00i + k2*j00r
            IF ( ichuz==1 ) GOTO 200
            IF ( ichuz==2 .OR. ichuz==3 .OR. ichuz==5 ) GOTO 100
            IF ( ichuz==4 .OR. ichuz==6 ) THEN
            ELSEIF ( ichuz==7 ) THEN
               i10i = -k1*i00r
            ELSE
               j00i = -k1*(.744*q1+1.488*q2+2.232*q3+2.976*q4+3.72*q5+4.464*q6+5.208*q7+5.952*q8+6.696*q9+7.44*q10+8.184*q11)
               i20i3 = -k1*i00r + k2*j00i
               IF ( ichuz==8 ) GOTO 400
               i10i = -k1*i00r
            ENDIF
         ENDIF
      ENDIF
      i10r = 1. + k1*i00i
      IF ( ichuz==1 .OR. ichuz==4 ) GOTO 200
      IF ( ichuz==7 .OR. ichuz==8 .OR. ichuz==9 ) GOTO 400
   ELSE
      IF ( X0>=0 ) THEN
         c1 = Kr*X0/Br
         T1 = Cgr*Cgs + Sgr*Sgs
         K10 = 2.0
         K1rt1 = 2.0*T1*cos(c1)
         K1it1 = -2.0*T1*sin(c1)
         K10t1 = 2.0*T1
      ENDIF
      GOTO 800
   ENDIF
 100  j0ur = e*(q1*(0.138384-k2+0.372*mu*c1)                                                                                        &
           & +e*(q2*(0.553536-k2+0.744*mu*c2)+e*(q3*(1.245456-k2+1.116*mu*c3)+e*(q4*(2.214144-k2+1.488*mu*c4)                       &
           & +e*(q5*(3.4596-k2+1.860*mu*c5)                                                                                         &
           & +e*(q6*(4.981824-k2+2.232*mu*c6)+e*(q7*(6.780816-k2+2.604*mu*c7)+e*(q8*(8.856576-k2+2.976*mu*c8)                       &
           & +e*(q9*(11.209104-k2+3.348*mu*c9)+e*(q10*(13.8384-k2+3.72*mu*c10)+e*(q11*(16.744464-k2+4.092*mu*c11))))))))))))
   j0ui = -                                                                                                                         &
        & k1*(e*(q1*(0.744+mu*c1)+e*(q2*(1.488+mu*c2)+e*(q3*(2.232+mu*c3)+e*(q4*(2.976+mu*c4)+e*(q5*(3.720+mu*c5)+e*(q6*(4.464+mu*c6&
        & )+e*(q7*(5.208+mu*c7)+e*(q8*(5.952+mu*c8)+e*(q9*(6.696+mu*c9)+e*(q10*(7.44+mu*c10)+e*(q11*(8.184+mu*c11)))))))))))))
 200  i0ur = .372*e*(r1+e*(2.*r2+e*(3.*r3+e*(4.*r4+e*(5.*r5+e*(6.*r6+e*(7.*r7+e*(8.*r8+e*(9.*r9+e*(10.*r10+e*11.*r11))))))))))
   i0ui = -k1*(e*(r1+e*(r2+e*(r3+e*(r4+e*(r5+e*(r6+e*(r7+e*(r8+e*(r9+e*(r10+e*r11)))))))))))
   r1 = r1s
   c6 = k1*mu
   c1 = sin(c6)
   c2 = cos(c6)
   c3 = sqrt(1.+mu*mu)
   c4 = mu/c3
   c5 = c4/(1.+mu*mu)
   IF ( ichuz==2 .OR. ichuz==5 ) THEN
   ELSEIF ( ichuz==7 .OR. ichuz==8 .OR. ichuz==9 ) THEN
      GOTO 400
   ELSE
      i1ur = c2*(1.-c4+k1*i0ui) - c1*k1*i0ur
      i1ui = -c2*k1*i0ur - c1*(1.-c4+k1*i0ui)
      IF ( ichuz==1 .OR. ichuz==7 .OR. ichuz==8 .OR. ichuz==9 ) GOTO 400
      IF ( ichuz==4 ) GOTO 300
   ENDIF
   i2ur3 = c2*(2.*(1.-c4)-c5+k1*i0ui+k2*j0ur) + c1*(c6*(1.-c4)-k1*i0ur+k2*j0ui)
   i2ui3 = c2*(c6*(1.-c4)-k1*i0ur+k2*j0ui) - c1*(2.*(1.-c4)-c5+k1*i0ui+k2*j0ur)
   IF ( ichuz==1 .OR. ichuz==2 .OR. ichuz==3 .OR. ichuz==7 .OR. ichuz==8 .OR. ichuz==9 ) GOTO 400
   IF ( ichuz/=4 ) THEN
      i2ur3 = 2.0*i20r3 - i2ur3
      IF ( ichuz/=6 ) GOTO 400
   ENDIF
 300  car = 2.*i10r - i1ur
   i1ur = car
 400  dk1r = 0.
   r1 = r1s
   dk1i = 0.
   dk2r = 0.
   dk2i = 0.
   c3 = k1*mu1
   c1 = cos(c3)
   c2 = sin(c3)
   c3 = M*r1/bigr
   c4 = sqrt(1.+mu1*mu1)
   c5 = Kr*X0/Br
   c6 = cos(c5)
   c7 = sin(c5)
   IF ( ichuz==1 .OR. ichuz==3 .OR. ichuz==4 .OR. ichuz==6 ) GOTO 500
   IF ( ichuz==2 .OR. ichuz==5 ) GOTO 600
   IF ( ichuz/=8 ) THEN
      i1ur = i10r
      i1ui = i10i
      IF ( ichuz==7 ) GOTO 500
   ENDIF
   i2ur3 = i20r3
   i2ui3 = i20i3
   IF ( ichuz==8 ) GOTO 600
 500  ck1r = i1ur + c3*c1/c4
   ck1i = i1ui - c3*c2/c4
   K10 = 1.0 + X0/bigr
   dk1r = ck1r*c6 + ck1i*c7
   dk1i = ck1i*c6 - ck1r*c7
   IF ( ichuz==1 .OR. ichuz==4 .OR. ichuz==7 ) GOTO 700
 600  c8 = (beta2*(r1/bigr)**2+(2.+mu1*c3)/(c4*c4))*(-c3/c4)
   c9 = (k1*c3)*(c3/c4)
   ck2r = -i2ur3 + c8*c1 - c9*c2
   ck2i = -i2ui3 - c9*c1 - c8*c2
   K20 = -2.0 - X0*(2.0+beta2*(r1/bigr)**2)/bigr
   dk2r = ck2r*c6 + ck2i*c7
   dk2i = ck2i*c6 - ck2r*c7
 700  K1rt1 = T1*dk1r
   K1it1 = T1*dk1i
   K2rt2p = t2p*dk2r
   K2it2p = t2p*dk2i
   K10t1 = K10*T1
   K20t2p = K20*t2p
 800  Kd1r = K1rt1 - K10t1*float(Ind)
   Kd1i = K1it1
   Kd2r = K2rt2p - K20t2p*float(Ind)
   Kd2i = K2it2p
END SUBROUTINE tker
