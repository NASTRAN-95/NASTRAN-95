
SUBROUTINE dcone
   IMPLICIT NONE
   REAL Alph12 , Alpha1 , Alpha2 , Costh , Dum(10) , Dumcl(34) , Ecpt(100) , Eltemp , G , G11 , G12 , G13 , G22 , G23 , G33 , Rhoy ,&
      & Sinth , Stress
   DOUBLE PRECISION Constd(5) , E11 , E12 , E22 , E33 , Eht(96) , Huq(100) , Kij(36) , Kqd(64) , Pi
   INTEGER Inflag , Matid , Matid1 , Matid2 , Matid3 , Necpt(100) , Nogo , Npvt
   COMMON /condad/ Constd
   COMMON /ds1aaa/ Npvt , Dumcl , Nogo
   COMMON /ds1adp/ Huq , Kqd , Kij , Eht , E11 , E12 , E22 , E33
   COMMON /ds1aet/ Ecpt
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rhoy , Alpha1 , Alpha2 , Alph12 , Dum
   DOUBLE PRECISION a(5,3) , a0 , a1 , a2 , a3 , b(7,3) , b0 , b1 , b2 , b3 , c(3,3) , c0 , c1 , cp , cp2 , d0 , d1 , d11 , d12 ,   &
                  & d22 , d33 , depp , deps , determ , epp , eps , fac(10) , gshear , hyq(10) , int(10,4) , l2 , n , n2 , n2d33 ,   &
                  & n2ov4 , ncp , nov4 , nsp , nspopi , nspov4 , one , opi , oq , piovb , q(8) , ra , rasq , rb , rbsq , sd22pi ,   &
                  & sign , sl , sp , sp2 , sp2d22 , sp2d4 , sum , tdif , te11 , te12 , te22 , temp , temp1 , temp2 , temp3 , temp4 ,&
                  & temp48(48) , temp5 , temp6 , temp7 , tm , ts , twod33 , u(10) , za , zb
   INTEGER i , ifac , inc1 , inc2 , ip1 , ip2 , ip3 , ising , j , jp1 , k , k1 , k2 , kk , kpow , krow , mplus1 , na(10) , nbegin , &
         & ncol , nerror(2) , nint , npivot , npow
!
!     DIFFERENTIAL STIFFNESS FOR THE CONICAL SHELL. FMMS-68
!
!     CALLS FROM DCONE ARE MADE TO
!           MESAGE
!           MAT
!           INVERD
!           GMMATD
!           DS1B
!
!     ECPT( 1) = ELEMENT ID                                INTEGER
!     ECPT( 2) = SIL PT A                                  INTEGER
!     ECPT( 3) = SIL PT B                                  INTEGER
!     ECPT( 4) = MATID 1                                   INTEGER
!     ECPT( 5) = TM  (MEMBRANE THICK)                      REAL
!     ECPT( 6) = MATID 2                                   INTEGER
!     ECPT( 7) = I   (MOM.OF INERTIA)                      REAL
!     ECPT( 8) = MATID 3                                   INTEGER
!     ECPT( 9) = TS  (SHEAR THICKNESS)                     REAL
!     ECPT(10) = NON-STRUCTURAL-MASS                       REAL
!     ECPT(11) = Z1                                        REAL
!     ECPT(12) = Z2                                        REAL
!     ECPT(13) = PHI  1                                    REAL
!     ECPT(14) = PHI  2                                    REAL
!     ECPT(15) = PHI  3                                    REAL
!     ECPT(16) = PHI  4                                    REAL
!     ECPT(17) = PHI  5                                    REAL
!     ECPT(18) = PHI  6                                    REAL
!     ECPT(19) = PHI  7                                    REAL
!     ECPT(20) = PHI  8                                    REAL
!     ECPT(21) = PHI  9                                    REAL
!     ECPT(22) = PHI 10                                    REAL
!     ECPT(23) = PHI 11                                    REAL
!     ECPT(24) = PHI 12                                    REAL
!     ECPT(25) = PHI 13                                    REAL
!     ECPT(26) = PHI 14                                    REAL
!     ECPT(27) = COORD. SYS. ID PT.1                       INTEGER
!     ECPT(28) = RADIUS PT. 1                              REAL
!     ECPT(29) = DISTANCE TO PT.1                          REAL
!     ECPT(30) = NULL                                      REAL
!     ECPT(31) = COORD. SYS. ID PT.2                       INTEGER
!     ECPT(32) = RADIUS PT 2                               REAL
!     ECPT(33) = DISTANCE TO PT. 2                         REAL
!     ECPT(34) = NULL                                      REAL
!     ECPT(35) = ELEMENT TEMPERATURE                       REAL
!     ECPT(36) = ELEMENT DEFORMATION                       REAL
!     ECPT(37) = ELEMENT LOADING TEMPERATURE - GRID PT A   REAL
!     ECPT(38) = ELEMENT LOADING TEMPERATURE - GRID PT B   REAL
!     ECPT(39) = DISPLACEMENT COMPONENTS AT GRID POINT A   REAL
!     ECPT(40) =                  ...                      REAL
!     ECPT(41) =                  ...                      REAL
!     ECPT(42) =                  ...                      REAL
!     ECPT(43) =                  ...                      REAL
!     ECPT(44) =                  ...                      REAL
!     ECPT(45) = DISPLACEMENT COMPONENTS AT GRID POINT B   REAL
!     ECPT(46) =                  ...                      REAL
!     ECPT(47) =                  ...                      REAL
!     ECPT(48) =                  ...                      REAL
!     ECPT(49) =                  ...                      REAL
!     ECPT(50) =                  ...                      REAL
!
   !>>>>EQUIVALENCE (G,G12) , (Ecpt(1),Necpt(1)) , (Ecpt(4),Matid1) , (Ecpt(6),Matid2) , (Ecpt(8),Matid3) , (Constd(1),Pi)
   DATA na/6*1 , 2*2 , 2*4/
   DATA fac/1.0D0 , 1.0D0 , 2.0D0 , 6.0D0 , 24.0D0 , 120.0D0 , 720.0D0 , 5040.0D0 , 40320.0D0 , 362880.0D0/
   DATA one/1.0D0/
!
!
!     CALCULATE SHELL ORIENTATION CONSTANTS
!
   Sinth = 0.0
   Costh = 1.0
   nint = Necpt(1)/1000
   n = Necpt(1) - nint*1000 - 1
   ra = Ecpt(28)
   za = Ecpt(29)
   rb = Ecpt(32)
   zb = Ecpt(33)
   temp1 = rb - ra
   temp2 = zb - za
   l2 = temp1**2 + temp2**2
   sl = dsqrt(l2)
   IF ( sl/=0 ) THEN
!
      sp = temp1/sl
      cp = temp2/sl
!
!     COMPUTE INTEGRALS I     FOR M = 0,9
!                        MN       N = 0,3
!
!     FOR EVALUATION OF INTEGRALS  A = RA,  B = SP
!
      IF ( sp/=0 ) THEN
!
!
!     COMPUTE INTEGRALS FOR (B .NE. 0)
!
!     FIRST M = 0 CASE
!
!                             2-N     2-N
!                      PI ( RB    - RA   )
!               I     =--------------------   (N NOT EQUAL TO 2)
!                0,N       (2-N)  B
!
!
!     FOR N=2   I     = PI * (LOG RB  -  LOG RA) / B
!                0,2             E          E
!
!
         rasq = ra*ra
         rbsq = rb*rb
         piovb = Pi/sp
!
         int(1,1) = 0.5D0*piovb*(rbsq-rasq)
         int(1,2) = piovb*(rb-ra)
         int(1,3) = piovb*dlog(rb/ra)
         int(1,4) = -piovb*(one/rb-one/ra)
!
!
!     M = I        WHERE I IS THE DO LOOP INDEX
!     N = J - 1    WHERE J IS THE DO LOOP INDEX
!
!     WE ARE GETTING INTEGRAL(M,N)
!     M = POWER OF S
!     N = POWER OF R
!
!
!     EVALUATING AT R = RB  THEN AT R = RA
!
!                                  K   NPOW
!                  M FAC.      M   (-A) (R)
!     I  = (PI)(-----------)( SUM ------------------------) + (TERM-X)
!      MN               (M+1)   K=0  (M-K)FAC.(K)FAC.(NPOW)
!                    B        (K.NE.M-N+2)                  (K.EQ.M-N+2)
!
!
!     WHERE NPOW = M - N - K + 2
!
!
!                    M-N+2
!                (-A)     LOG(R)
!       TERM-X = --------------------
!               (M-N+2)FAC.(N-2)FAC.
!
!
!     NOTE IN DATA STATEMENT THAT 0 FACTORIAL = FAC(1)
!                                 1 FACTORIAL = FAC(2)
!                                 2 FACTORIAL = FAC(3)    ETC.
!
         DO i = 1 , 9
            mplus1 = i + 1
            nbegin = na(mplus1)
            DO j = nbegin , 4
               sum = 0.0D0
               sign = -1.0D0
               npow = i - j + 3
               DO kk = 1 , mplus1
                  sign = -sign
                  k = kk - 1
                  IF ( k==npow ) THEN
                     sum = sum + sign*ra**npow*dlog(rb/ra)/(fac(npow+1)*fac(j-2))
                  ELSE
                     kpow = npow - k
                     ifac = mplus1 - k
                     temp = kpow
                     sum = sum + sign*ra**k*(rb**kpow-ra**kpow)/(fac(ifac)*fac(kk)*temp)
                  ENDIF
               ENDDO
!
               int(mplus1,j) = sum*Pi*fac(mplus1)/sp**mplus1
            ENDDO
         ENDDO
      ELSE
!
!     COMPUTE INTEGRAL FOR B = 0
!
!                            1-N
!                      PI  RA     M+1
!               I   = --------- SL    (FOR ALL M,N .GE. 0)
!                M,N    M + 1
!
!
!     M = I - 1    WHERE I IS THE DO LOOP INDEX
!     N = J - 1    WHERE J IS THE DO LOOP INDEX
!     MPLUS1 THUS EQUALS I
!
         DO i = 1 , 10
            nbegin = na(i)
            DO j = nbegin , 4
               int(i,j) = (Pi*sl**i)/(dble(float(i))*ra**(j-2))
            ENDDO
!
         ENDDO
      ENDIF
!
!     CRANK OUT HUQ MATRIX FOR ZERO HARMONIC
!     FOR EXPLICIT FORMULATION OF HUQ, SEE MS-28, PP.15,16 AND PP.24,25.
!
      DO i = 1 , 100
         Huq(i) = 0.0D0
      ENDDO
      Huq(1) = one
      Huq(13) = one
      Huq(25) = one
      Huq(36) = one
      Huq(41) = cp/ra
      Huq(49) = one
      Huq(51) = one
      Huq(52) = sl
      Huq(63) = one
      Huq(64) = sl
      Huq(75) = one
      Huq(76) = sl
      Huq(77) = l2
      Huq(78) = Huq(77)*sl
      Huq(86) = one
      Huq(87) = 2.0D0*sl
      Huq(88) = 3.0D0*Huq(77)
      Huq(91) = cp/rb
      Huq(92) = Huq(91)*sl
      Huq(99) = one
      Huq(100) = sl
!
!     IF TRANSVERSE SHEAR IS ZERO
!
!     OR INERTIA           = 0.0
!     OR SHEAR MODULUS(G)  = 0.0
!     OR MATID2            = 0
!     OR MATID3            = 0
!
!     THEN (HYQ)  = (0).  THEREFORE, USE HUQ MATRIX AS IS
!
      IF ( Matid2/=0 .AND. Matid3/=0 ) THEN
         IF ( Ecpt(9)/=0.0 .AND. Ecpt(7)/=0.0 ) THEN
            Inflag = 1
            Matid = Matid3
            Eltemp = Ecpt(35)
            CALL mat(Ecpt(1))
            gshear = G
            IF ( G/=0.0 ) THEN
               Inflag = 2
               Matid = Matid2
               Eltemp = Ecpt(35)
               CALL mat(Ecpt(1))
!
!     FORM
!     (D) = I*(G)
!
               d11 = Ecpt(7)*G11
               d12 = Ecpt(7)*G12
               d22 = Ecpt(7)*G22
               d33 = Ecpt(7)*G33
!
               ts = Ecpt(9)
!
               DO i = 1 , 10
                  hyq(i) = 0.0D0
               ENDDO
               cp2 = cp*cp
               sp2 = sp*sp
               n2 = n*n
               opi = one/Pi
               sd22pi = sp2*d22*opi
               oq = sl*ts*gshear*(ra+rb)*0.5D0 + sd22pi*int(1,3)
               oq = one/oq
!
               hyq(6) = oq*int(1,3)*sd22pi
               hyq(7) = oq*2.0D0*(d11*(ra-rb)+int(2,3)*sd22pi)
               hyq(8) = oq*(-d11*6.0D0*sl*rb+3.0D0*int(3,3)*sd22pi)
!
               DO i = 6 , 8
                  Huq(i+30) = Huq(i+30) - hyq(i)
                  Huq(i+80) = Huq(i+80) - hyq(i)
               ENDDO
            ENDIF
         ENDIF
      ENDIF
!
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
      ising = -1
      CALL inverd(10,Huq(1),10,Dum,0,determ,ising,temp48(1))
!
!     CHECK SINGULARITY
!
      IF ( ising/=2 ) THEN
!
!
!     CALCULATE GENERALIZED DISPLACEMENT VECTOR(Q)
!
!                       ***     ***
!                       *  T      *
!                       *(E )(U ) *
!                       *      A  *
!        (Q)  =  (HUQ)  *---------*
!                       *  T      *
!                       *(E )(U ) *
!                       *      B  *
!                       ***     ***
!
!           WHERE
!                                0    1    0    0    0    0
!                          T     SP   0    CP   0    0    0
!                MATRIX  (E )  = CP   0   -SP   0    0    0
!                                0    0    0    0    1    0
!                                0    0    0    SP   0    CP
!
!
         k1 = 0
         k2 = 0
         DO
            u(k2+1) = dble(Ecpt(k1+40))
            u(k2+2) = dble(Ecpt(k1+39))*sp + dble(Ecpt(k1+41))*cp
            u(k2+3) = dble(Ecpt(k1+39))*cp - dble(Ecpt(k1+41))*sp
            u(k2+4) = dble(Ecpt(k1+43))
            u(k2+5) = dble(Ecpt(k1+42))*sp + dble(Ecpt(k1+44))*cp
!
            IF ( k1/=0 ) THEN
!
               CALL gmmatd(Huq(1),8,10,0,u(1),10,1,0,q(1))
!
!     CALCULATE STRAIN COEFFICIENTS AND OBTAIN MATERIAL PROPERTY MATRIX
!     (E)
!
               Matid = Matid1
               Inflag = 2
               Eltemp = Ecpt(35)
               CALL mat(Ecpt(1))
               E11 = G11
               E12 = G12
               E22 = G22
               E33 = G33
               tdif = (dble(Ecpt(38))-dble(Ecpt(37)))/sl
               deps = dble(Alpha1)*tdif
               depp = dble(Alpha2)*tdif
               eps = dble(Alpha1)*dble(Ecpt(37))
               epp = dble(Alpha2)*dble(Ecpt(37))
!
!     COMPUTE COEFFICIENTS FOR POWER SERIES OF DIFFERENTIAL STIFF. COEFF
!
               tm = Ecpt(5)
               temp1 = sp*q(3) + cp*q(5)
               temp2 = sp*q(4) + cp*q(6)
               temp3 = q(4) - eps
               te11 = tm*E11
               te12 = tm*E12
               te22 = tm*E22
!
               a0 = te12*temp1
               a1 = te12*temp2
               a2 = te12*cp*q(7)
               a3 = te12*cp*q(8)
               b0 = te22*temp1
               b1 = te22*temp2
               b2 = te22*cp*q(7)
               b3 = te22*cp*q(8)
               c0 = te11*temp3 - te12*epp
               c1 = -te11*deps - te12*depp
               d0 = te12*temp3 - te22*epp
               d1 = -te12*deps - te22*depp
!
!     COMPUTE DIFFERENTIAL STIFFNESS COEFFICIENTS
!
               DO i = 1 , 3
                  ip1 = i + 1
                  ip2 = i + 2
                  ip3 = i + 3
                  DO j = i , 3
                     jp1 = j + 1
                     a(i,j) = a0*int(i,jp1) + a1*int(ip1,jp1) + a2*int(ip2,jp1) + a3*int(ip3,jp1) + c0*int(i,j) + c1*int(ip1,j)
                     b(i,j) = b0*int(i,jp1) + b1*int(ip1,jp1) + b2*int(ip2,jp1) + b3*int(ip3,jp1) + d0*int(i,j) + d1*int(ip1,j)
                     c(i,j) = a(i,j) + b(i,j)
                  ENDDO
               ENDDO
!
               j = 1
               jp1 = 2
               DO i = 2 , 5
                  ip1 = i + 1
                  ip2 = i + 2
                  ip3 = i + 3
                  a(i,j) = a0*int(i,jp1) + a1*int(ip1,jp1) + a2*int(ip2,jp1) + a3*int(ip3,jp1) + c0*int(i,j) + c1*int(ip1,j)
               ENDDO
!
               j = 3
               jp1 = 4
               DO i = 4 , 7
                  ip1 = i + 1
                  ip2 = i + 2
                  ip3 = i + 3
                  b(i,j) = b0*int(i,jp1) + b1*int(ip1,jp1) + b2*int(ip2,jp1) + b3*int(ip3,jp1) + d0*int(i,j) + d1*int(ip1,j)
               ENDDO
!
!     COMPUTE KQD
!     FOR EXPLICIT FORMULATION OF KQD, SEE MS-31, PP. 8-11
!     CASE ONE.. HARMONIC NUMBER = ZERO
!
               DO i = 1 , 64
                  Kqd(i) = 0.0D0
               ENDDO
               sp2d4 = sp2*0.25D0
               Kqd(1) = cp2*b(1,3) + sp2d4*c(1,3)
               Kqd(2) = cp2*b(2,3) + 0.25D0*sp*c(1,2) + sp2d4*c(2,3)
               Kqd(9) = Kqd(2)
               Kqd(10) = cp2*b(3,3) + (c(1,1)+2.0D0*sp*c(2,2)+sp2*c(3,3))*0.25D0
               Kqd(46) = a(1,1)
               Kqd(47) = a(2,1)*2.0D0
               Kqd(48) = a(3,1)*3.0D0
               Kqd(54) = Kqd(47)
               Kqd(55) = a(3,1)*4.0D0
               Kqd(56) = a(4,1)*6.0D0
               Kqd(62) = Kqd(48)
               Kqd(63) = Kqd(56)
               Kqd(64) = a(5,1)*9.0D0
!
!     CHECK HARMONIC NUMBER
!
               IF ( n/=0.0D0 ) THEN
!
!     CASE TWO.. HARMONIC NUMBER .NE. ZERO
!
                  nov4 = n*0.25D0
                  nspov4 = nov4*sp
                  ncp = n*cp
                  n2ov4 = nov4*n
                  Kqd(3) = nspov4*c(1,3)
                  Kqd(4) = nspov4*c(2,3)
                  Kqd(5) = ncp*b(1,3)
                  Kqd(6) = ncp*b(2,3)
                  Kqd(7) = ncp*b(3,3)
                  Kqd(8) = ncp*b(4,3)
                  Kqd(11) = nov4*(c(1,2)+sp*c(2,3))
                  Kqd(12) = nov4*(c(2,2)+sp*c(3,3))
                  Kqd(13) = ncp*b(2,3)
                  Kqd(14) = ncp*b(3,3)
                  Kqd(15) = ncp*b(4,3)
                  Kqd(16) = ncp*b(5,3)
                  Kqd(17) = Kqd(3)
                  Kqd(18) = Kqd(11)
                  Kqd(19) = n2ov4*c(1,3)
                  Kqd(20) = n2ov4*c(2,3)
                  Kqd(25) = Kqd(4)
                  Kqd(26) = Kqd(12)
                  Kqd(27) = Kqd(20)
                  Kqd(28) = n2ov4*c(3,3)
                  Kqd(33) = Kqd(5)
                  Kqd(34) = Kqd(13)
                  Kqd(37) = n2*b(1,3)
                  Kqd(38) = n2*b(2,3)
                  Kqd(39) = n2*b(3,3)
                  Kqd(40) = n2*b(4,3)
                  Kqd(41) = Kqd(6)
                  Kqd(42) = Kqd(14)
                  Kqd(45) = Kqd(38)
                  Kqd(46) = Kqd(46) + n2*b(3,3)
                  Kqd(47) = Kqd(47) + n2*b(4,3)
                  Kqd(48) = Kqd(48) + n2*b(5,3)
                  Kqd(49) = Kqd(7)
                  Kqd(50) = Kqd(15)
                  Kqd(53) = Kqd(39)
                  Kqd(54) = Kqd(47)
                  Kqd(55) = Kqd(55) + n2*b(5,3)
                  Kqd(56) = Kqd(56) + n2*b(6,3)
                  Kqd(57) = Kqd(8)
                  Kqd(58) = Kqd(16)
                  Kqd(61) = Kqd(40)
                  Kqd(62) = Kqd(48)
                  Kqd(63) = Kqd(56)
                  Kqd(64) = Kqd(64) + n2*b(7,3)
!
!     COMPUTE HUQ FOR NTH HARMONIC
!
                  DO i = 1 , 100
                     Huq(i) = 0.0D0
                  ENDDO
                  Huq(1) = one
                  Huq(13) = one
                  Huq(25) = one
                  Huq(36) = one
                  Huq(41) = cp/ra
                  Huq(45) = n/ra
                  Huq(49) = one
                  Huq(51) = one
                  Huq(52) = sl
                  Huq(63) = one
                  Huq(64) = sl
                  Huq(75) = one
                  Huq(76) = sl
                  Huq(77) = l2
                  Huq(78) = Huq(77)*sl
                  Huq(86) = one
                  Huq(87) = 2.0D0*sl
                  Huq(88) = 3.0D0*Huq(77)
                  Huq(91) = cp/rb
                  Huq(92) = Huq(91)*sl
                  Huq(95) = n/rb
                  Huq(96) = Huq(95)*sl
                  Huq(97) = Huq(95)*l2
                  Huq(98) = Huq(96)*l2
                  Huq(99) = one
                  Huq(100) = sl
!
!     COMPUTE HYQ
!
                  IF ( Matid2/=0 .AND. Matid3/=0 ) THEN
                     IF ( Ecpt(9)/=0.0 .AND. Ecpt(7)/=0.0 ) THEN
                        IF ( gshear/=0.0D0 ) THEN
!
                           n2d33 = n2*d33
                           sp2d22 = sp2*d22
                           oq = sl*ts*gshear*(ra+rb)*0.5D0 + int(1,3)*(n2d33+sp2d22)*opi
                           oq = one/oq
                           nsp = n*sp
                           ncp = n*cp
                           nspopi = nsp*opi
                           twod33 = 2.0D0*d33
                           temp1 = d12*(one/rb-one/ra)
                           temp2 = nspopi*(d22+d33)
                           temp3 = n*nspopi*(twod33+d22)
                           temp4 = oq*0.5D0*n2d33*cp*opi
                           temp5 = opi*(n2*twod33+sp2d22)
                           temp6 = d12*n2*l2/rb
                           temp7 = nspopi*cp*0.50D0
!
                           hyq(1) = oq*(temp1*ncp-temp7*int(1,4)*(d33+2.0D0*d22))
                           hyq(2) = oq*(ncp*sl/rb*d12-temp7*int(2,4)*(3.0D0*d33+d22)+1.5D0*ncp*opi*int(1,3)*d33)
                           hyq(3) = temp4*int(1,4)
                           hyq(4) = temp4*int(2,4)
                           hyq(5) = oq*(temp1*n2-temp3*int(1,4))
                           hyq(6) = oq*(d12*n2*sl/rb-temp3*int(2,4)+temp5*int(1,3))
                           hyq(7) = oq*(2.0D0*d11*(ra-rb)+temp6+2.0D0*int(2,3)*temp5-temp3*int(3,4))
                           hyq(8) = oq*(-d11*6.0D0*sl*rb+temp6*sl+3.0D0*int(3,3)*temp5-temp3*int(4,4))
                           hyq(9) = -oq*temp2*int(1,3)
                           hyq(10) = oq*(n*sl*(d12+d33)-temp2*int(2,3))
!
                           DO i = 1 , 10
                              Huq(i+30) = Huq(i+30) - hyq(i)
                              Huq(i+80) = Huq(i+80) - hyq(i)
                           ENDDO
                        ENDIF
                     ENDIF
                  ENDIF
!
!
!     AGAIN SET ISING TO -1
!
                  ising = -1
                  CALL inverd(10,Huq(1),10,Dum,0,determ,ising,temp48(1))
                  IF ( ising==2 ) THEN
                     CALL mesage(30,40,Necpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
                     Nogo = 1
                     RETURN
                  ENDIF
               ENDIF
!
!
!     COMPLETE SOLUTION BY TRANSFORMING KQD TO GLOBAL COORDINATES
!
!                     T            T
!       (K  ) = (E)(H  )(KQD)(H)(E )     FOR I = PIVOT POINT
!         IJ         I         J             J = A,B
!
!     FIRST OBTAIN PRODUCTS
!                       T
!        EHAT  =  (E)(H  )      AND STORE AT EHT(1) . . . EHT(48)
!                      A
!
!                       T
!        EHBT  =  (E)(H  )      AND STORE AT EHT(49). . . EHT(96)
!                      B
!
!              0    CP   SP   0    0
!              1    0    0    0    0
!
!              0    CP  -SP   0    0
!
!        MATRIX E =
!              0    0    0    0    SP
!
!              0    0    0    1    0
!
!              0    0    0    0    CP
!
               inc1 = 0
               inc2 = 0
               DO
                  DO i = 1 , 8
                     krow = i + inc1
                     ncol = (i-1)*10 + inc2
                     Eht(krow) = sp*Huq(ncol+2) + cp*Huq(ncol+3)
                     Eht(krow+8) = Huq(ncol+1)
                     Eht(krow+16) = cp*Huq(ncol+2) - sp*Huq(ncol+3)
                     Eht(krow+24) = sp*Huq(ncol+5)
                     Eht(krow+32) = Huq(ncol+4)
                     Eht(krow+40) = cp*Huq(ncol+5)
                  ENDDO
                  IF ( inc1>0 ) THEN
!
!     CHECK FOR PIVOT POINT NUMBER
!
                     DO i = 1 , 2
                        IF ( Npvt==Necpt(i+1) ) GOTO 2
                     ENDDO
!
!     FALL THRU LOOP IMPLIES NO PIVOT POINT NUMBER
!
                     CALL mesage(-30,34,Ecpt(1))
!
 2                   npivot = i
                     CALL gmmatd(Eht(48*npivot-47),6,8,0,Kqd(1),8,8,0,temp48(1))
!
!     IF N = 0 DOUBLE RESULT
!
                     IF ( n==0.0D0 ) THEN
                        DO i = 1 , 48
                           temp48(i) = 2.0D0*temp48(i)
                        ENDDO
                     ENDIF
!
                     DO j = 1 , 2
                        CALL gmmatd(temp48(1),6,8,0,Eht(48*j-47),6,8,1,Kij(1))
                        CALL ds1b(Kij(1),Necpt(j+1))
                     ENDDO
                     GOTO 99999
                  ELSE
                     inc1 = 48
                     inc2 = 5
                  ENDIF
               ENDDO
            ELSE
               k1 = 6
               k2 = 5
            ENDIF
         ENDDO
      ELSE
         CALL mesage(30,40,Necpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
         Nogo = 1
         RETURN
      ENDIF
   ELSE
      nerror(1) = Necpt(1)/1000
      nerror(2) = n + .3D0
      CALL mesage(30,39,nerror(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
      Nogo = 1
      RETURN
   ENDIF
!
99999 RETURN
END SUBROUTINE dcone