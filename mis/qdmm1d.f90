
SUBROUTINE qdmm1d
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alp12 , Alpha1 , Alpha2 , Angle , Consts(4) , Costh , Degra , Dmmm(2) , Dum(75) , Dum2(16) , Dum3 , Dummy1 , Dummy2 ,       &
      & Dummy3 , Dummy4 , Ecpt(26) , Eltemp , Fmu , G11 , G12 , G13 , G22 , G23 , G2x211 , G2x212 , G2x222 , G33 , Gsube , Rho ,    &
      & Sigcom , Sigshe , Sigten , Sinth , Stress , Thick , Tsub0 , X1 , X2 , X3 , X4 , Y1 , Y2 , Y3 , Y4 , Z1 , Z2 , Z3 , Z4
   DOUBLE PRECISION Aq(24) , B(144) , Bq(24) , Btxk(96) , C(6) , Cq(30) , E(9) , Etj(9,4) , Kij(3,3) , Tempar(144) , Ti(9) ,        &
                  & Tie(9,4) , U(64)
   INTEGER Elid , Estid , Inflag , Iprec , Ksystm , Mass , Matid , Matid1 , Necpt(1) , Ngrid(4) , Nlocs , Outpt
   LOGICAL Heat , Nogo
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /condas/ Consts , Degra
   COMMON /emgdic/ Dmmm , Nlocs , Elid , Estid
   COMMON /emgest/ Necpt , Ngrid , Angle , Matid1 , Thick , Fmu , Dummy1 , X1 , Y1 , Z1 , Dummy2 , X2 , Y2 , Z2 , Dummy3 , X3 , Y3 ,&
                 & Z3 , Dummy4 , X4 , Y4 , Z4 , Dum
   COMMON /emgprm/ Dum2 , Mass , Dum3 , Iprec , Nogo , Heat
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alpha1 , Alpha2 , Alp12 , Tsub0 , Gsube , Sigten , Sigcom , Sigshe ,   &
                 & G2x211 , G2x212 , G2x222
   COMMON /sma1dp/ Tie , Kij , B , E , Etj
   COMMON /sma2dp/ U , C , Aq , Bq , Cq , Ti , Btxk
   COMMON /system/ Ksystm , Outpt
   COMMON /xmssg / Ufm , Uwm , Uim
!
! Local variable declarations
!
   DOUBLE PRECISION at1 , at2 , at3 , at4 , cth1 , cth2 , cth31 , cth32 , cth41 , cth42 , d , dlt1 , dlt2 , eta , eta01(2) , f ,    &
                  & fact , h , h1 , hh , la , lb , lbd1 , lc , lcd1 , lcd2 , ld , ldd2 , magi , magj , magk , mgg(4) , p , pi1 ,    &
                  & pi2 , pi3 , pj1 , pj2 , pj3 , pk1 , pk2 , pk3 , q , sth1 , sth2 , sth31 , sth32 , sth41 , sth42 , tea , temp ,  &
                  & v , x12 , x13 , x14 , x21 , x23 , x24 , x31 , x34 , x41 , x42 , y21 , y31 , y34 , y3a , y41 , y42 , y4a ,       &
                  & ysub4 , z21 , z31 , z41 , z42
   INTEGER dict(9) , i , i2 , ia01 , ieoe , ifile , ii , ij , ij1 , im1 , incl , inr , iocl , ior , j , ja01 , jm1 , k , k1 , k2 ,  &
         & k3 , ka , kl , km1 , l , l1 , l2 , l3 , ldata , lm1 , map(2,4) , nn
   REAL dict5 , theta
   DOUBLE PRECISION o
   LOGICAL planar
!
! End of declarations
!
!
!     THIS SUBROUTINE COMPUTES THE STIFFNESS AND MASS MATRIX FOR THE
!     FIRST QUADRILATERAL MEMBRANE ELEMENT.
!
!     DOUBLE PRECISION VERSION
!
!     ECPT LIST
!                                                   IN THIS
!        ECPT       DESCRIPTION                     ROUTINE    TYPE
!     ========   ===============================    ========  =======
!     ECPT( 1) = ELEMENT ID                         NECPT(1)  INTEGER
!     ECPT( 2)   GRID POINT A                       NGRID(1)  INTEGER
!     ECPT( 3)   GRID POINT B                       NGRID(2)  INTEGER
!     ECPT( 4)   GRID POINT C                       NGRID(3)  INTEGER
!     ECPT( 5)   GRID POINT D                       NGRID(4)  INTEGER
!     ECPT( 6) = THETA = ANGLE OF MATERIAL          ANGLE     REAL
!     ECPT( 7)   MATERIAL ID                        MATID     INTEGER
!     ECPT( 8) = THICKNESS                          T         REAL
!     ECPT( 9) = NON-STRUCTURAL MASS                FMU       REAL
!     ECPT(10)   COORD. SYSTEM ID 1                 NECPT(10) INTEGER
!     ECPT(11) = X1                                 X1        REAL
!     ECPT(12) = Y1                                 Y1        REAL
!     ECPT(13) = Z1                                 Z1        REAL
!     ECPT(14)   COORD. SYSTEM ID 2                 NECPT(14) INTEGER
!     ECPT(15) = X2                                 X2        REAL
!     ECPT(16) = Y2                                 Y2        REAL
!     ECPT(17) = Z2                                 Z2        REAL
!     ECPT(18)   COORD. SYSTEM ID 3                 NECPT(18) INTEGER
!     ECPT(19) = X3                                 X3        REAL
!     ECPT(20) = Y3                                 Y3        REAL
!     ECPT(21) = Z3                                 Z3        REAL
!     ECPT(22)   COORD. SYSTEM ID 4                 NECPT(22) INTEGER
!     ECPT(23) = X4                                 X4        REAL
!     ECPT(24) = Y4                                 Y4        REAL
!     ECPT(25)   Z4                                 Z4        REAL
!     ECPT(26) = ELEMENT TEMPERATURE                ELTEMP    REAL
!
   EQUIVALENCE (dict5,dict(5)) , (Ecpt(1),Necpt(1)) , (U(1),Tempar(1))
!
   o(d,v,f,h,p,q,y4a,x12,y34,y3a,x23,x14,eta,tea) = (d+(v*tea)+(f*eta)+(h*tea*eta)+(p*tea*tea)+(q*eta*eta))                         &
    & /((-y4a*x12)+(-y34*x12*eta)+((-y4a*x23)+(y3a*x14))*tea)
!
   eta = 1.D0
   tea = 1.D0
   IF ( Heat ) THEN
!
      WRITE (Outpt,99001) Uwm , Necpt(1)
99001 FORMAT (A25,' 3115, QDMM1D FINDS ELEMENT NO.',I9,' PRESENT IN A',' HEAT FORMULATION AND IS IGNORING SAME.')
      GOTO 99999
   ELSE
      eta01(1) = 0.211324865D0
      eta01(2) = 0.788675135D0
!
!     COMPUTE DIFFERENCES OF COORDINATES OF ACTUAL GRID POINTS
!
      x21 = X2 - X1
      y21 = Y2 - Y1
      z21 = Z2 - Z1
      x31 = X3 - X1
      y31 = Y3 - Y1
      z31 = Z3 - Z1
      x41 = X4 - X1
      y41 = Y4 - Y1
      z41 = Z4 - Z1
      x42 = X4 - X2
      y42 = Y4 - Y2
      z42 = Z4 - Z2
!
!     COMPUTE ELEMENTS OF THE E MATRIX
!
      pk1 = y31*z42 - z31*y42
      pk2 = z31*x42 - x31*z42
      pk3 = x31*y42 - y31*x42
      magk = dsqrt(pk1**2+pk2**2+pk3**2)
      IF ( magk<=1.D-6 ) THEN
!
!     ERROR EXITS
!
         j = 32
      ELSE
         pk1 = pk1/magk
         pk2 = pk2/magk
         pk3 = pk3/magk
!
!     HH IS THE MEASURE OF NON-PLANARITY OF THE ELEMENT
!
         hh = x21*pk1 + y21*pk2 + z21*pk3
         pi1 = x21 - hh*pk1
         pi2 = y21 - hh*pk2
         pi3 = z21 - hh*pk3
         magi = dsqrt(pi1**2+pi2**2+pi3**2)
         IF ( magi<=1.D-6 ) THEN
            j = 31
         ELSE
            pi1 = pi1/magi
            pi2 = pi2/magi
            pi3 = pi3/magi
            hh = -hh/2.D0
!
!     THIS SIGN CHANGE MADE BECAUSE SIGN OF H AS DEFINED ON
!     PAGE 4.87-105 OF PROGRAMMERS MANUAL IS WRONG
!
            temp = dsqrt(x31**2+y31**2+z31**2)
            ysub4 = dsqrt(x42**2+y42**2+z42**2)
            h1 = (2.0*hh)/(temp+ysub4)
            planar = .TRUE.
            IF ( h1>1.0D-6 ) planar = .FALSE.
            IF ( h1>=1.0D-2 ) WRITE (Outpt,99002) Uim , h1 , Necpt(1)
99002       FORMAT (A29,' 3061, THE MEASURE OF NON-PLANARITY IS ',D13.5,' FOR ELEMENT NUMBER',I9)
            pj1 = pk2*pi3 - pk3*pi2
            pj2 = pk3*pi1 - pk1*pi3
            pj3 = pk1*pi2 - pk2*pi1
            magj = dsqrt(pj1**2+pj2**2+pj3**2)
            IF ( magj<=1.D-6 ) THEN
               j = 26
            ELSE
               pj1 = pj1/magj
               pj2 = pj2/magj
               pj3 = pj3/magj
!
!  *  SET UP E MATRIX (3X3) FOR QUAD-MEMBRANE PROJECTION ONTO
!     MEAN PLANE
!     E IS TRANSPOSE OF E MATRIX IN THEORETICAL MANUAL
!
!     E(1),E(4),E(7) IS I-VECTOR
!     E(2),E(5),E(8) IS J-VECTOR
!     E(3),E(6),E(9) IS K-VECTOR
!
               E(1) = pi1
               E(2) = pj1
               E(3) = pk1
               E(4) = pi2
               E(5) = pj2
               E(6) = pk2
               E(7) = pi3
               E(8) = pj3
               E(9) = pk3
!
!     COMPUTE DIFFERENCES OF COORDINATES OF GRID POINTS IN THE MEAN PLAN
!
               x12 = -(x21*E(1)+y21*E(4)+z21*E(7))
               x13 = -(x31*E(1)+y31*E(4)+z31*E(7))
               x24 = -(x42*E(1)+y42*E(4)+z42*E(7))
               x14 = x12 + x24
               y3a = x31*E(2) + y31*E(5) + z31*E(8)
               y4a = x42*E(2) + y42*E(5) + z42*E(8)
               x34 = x14 - x13
               y34 = y3a - y4a
               x23 = x13 - x12
               IF ( y3a<=0.0D0 .OR. y4a<=0.0D0 ) THEN
                  j = 26
               ELSE
                  temp = x12 + x23*(y4a/y3a)
                  ysub4 = (y3a/y4a)*x14
!
!                                              0
!     CHECK FOR INTERNAL ANGLE GREATER THAN 180
!
                  IF ( x13>=ysub4 .OR. x14<=temp ) THEN
                     j = 26
                  ELSE
!
!     GET MASS MATRIX DIAGONALS
!
                     IF ( Mass/=0 ) THEN
                        Inflag = 4
                        Matid = Matid1
                        CALL mat(Ecpt(1))
!
!     GET TRIANGULAR AREA TIMES TWO
!
                        at1 = -x12*y4a
                        at2 = -x12*y3a
                        at3 = -x23*y4a + x24*y3a
                        at4 = -x13*y4a + x14*y3a
!
                        fact = (Fmu+G11*Thick)/12.0D0
                        mgg(1) = (at4+at1+at2)*fact
                        mgg(2) = (at1+at2+at3)*fact
                        mgg(3) = (at2+at3+at4)*fact
                        mgg(4) = (at3+at4+at1)*fact
                     ENDIF
!
!     COMPUTE LENGTHS OF SIDES OF ELEMENT IN THE MEAN PLANE
!
                     la = dabs(x12)
                     lb = dsqrt(x23**2+y3a**2)
                     lc = dsqrt(x34**2+y34**2)
                     ld = dsqrt(x14**2+y4a**2)
                     IF ( la==0.D0 .OR. lb==0.D0 .OR. lc==0.D0 .OR. ld==0.D0 ) THEN
                        j = 26
                     ELSE
!
!     COMPUTE THE CHARACTERISTIC ANGLES OF ELEMENT IN THE MEAN PLANE
!
                        IF ( .NOT.(planar) ) THEN
                           cth1 = -x14/ld
                           sth1 = y4a/ld
                           cth2 = x23/lb
                           sth2 = y3a/lb
                           cth31 = x34/lc
                           sth31 = -y34/lc
                           cth41 = cth1
                           sth41 = sth1
                           cth32 = sth2
                           sth32 = cth2
                           cth42 = sth31
                           sth42 = cth31
                           dlt1 = cth31*cth32 - sth31*sth32
                           dlt2 = cth42*cth41 + sth41*sth42
                           ldd2 = ld*dlt2
                           lbd1 = lb*dlt1
                           lcd1 = lc*dlt1
                           lcd2 = lc*dlt2
!
!     SET UP THE (12X8)  TRANSFORMATION MATRIX B BETWEEN THE MEAN PLANE
!                        AND ACTUAL GRID POINTS
!
                           DO i = 2 , 92
                              B(i) = 0.0
                           ENDDO
!
                           B(1) = 1.0
                           B(10) = 1.0
                           B(17) = -hh/la
                           B(18) = -hh/(ld*sth1) + ((hh*cth1)/(la*sth1))
                           B(19) = hh/la
                           B(20) = (hh*cth2)/(la*sth2)
                           B(23) = (hh*cth42)/ldd2
                           B(24) = (hh*sth42)/ldd2
                           B(27) = 1.0
                           B(36) = 1.
                           B(41) = -B(17)
                           B(42) = (-hh*cth1)/(la*sth1)
                           B(43) = B(17)
                           B(44) = ((-hh*cth2)/(la*sth2)) + (hh/(lb*sth2))
                           B(45) = (-hh*sth31)/lbd1
                           B(46) = (-hh*cth31)/lbd1
                           B(53) = 1.
                           B(62) = 1.
                           B(68) = -hh/(lb*sth2)
                           B(69) = hh*((sth31/lbd1)+(cth32/lcd1))
                           B(70) = hh*((cth31/lbd1)+(sth32/lcd1))
                           B(71) = (-hh*sth41)/lcd2
                           B(72) = (hh*cth41)/lcd2
                           B(79) = 1.0
                           B(88) = 1.0
                           B(90) = hh/(ld*sth1)
                           B(93) = (-hh*cth32)/lcd1
                           B(94) = (-hh*sth32)/lcd1
                           B(95) = hh*((-cth42/ldd2)+(sth41/lcd2))
                           B(96) = hh*((-sth42/ldd2)-(cth41/lcd2))
                        ENDIF
!
                        theta = Angle*Degra
                        Sinth = sin(theta)
                        Costh = cos(theta)
                        IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0E0
                        Eltemp = Ecpt(26)
                        Inflag = 2
                        Matid = Matid1
!
!                                                     T
!     COMPUTE TRANSFORMED MATRIX OF STIFFNESSES  C = P  * G * P
!
                        CALL mat(Ecpt(1))
!
!     STORE INTO G MATRIX
!
                        C(1) = G11
                        C(2) = G12
                        C(3) = G22
                        C(4) = G13
                        C(5) = G23
                        C(6) = 0.D0
                        fact = G33*dble(Thick)/(x24*y3a-x13*y4a)*2.0D0
!
!     COMPUTE COEFFICIENTS OF THE GENERAL INTEGRAL
!
!                                            2         2
!     D + E*ETA + F*ZETA + H*ETA*ZETA + P*ETA  + Q*ZETA
!     --------------------------------------------------
!     Y *X   +Y  *X  *ZETA + (Y *X   - Y *X  ) * ETA
!      4  21   34  21          4  32    3  41
!
                        Aq(1) = -y4a
                        Aq(3) = -x24
                        Aq(5) = -x24
                        Aq(6) = -y4a
                        Aq(7) = y4a
                        Aq(9) = x14
                        Aq(11) = x14
                        Aq(12) = y4a
                        Aq(13) = 0.0
                        Aq(15) = 0.0
                        Aq(17) = 0.0
                        Aq(18) = 0.0
                        Aq(19) = 0.0
                        Aq(21) = -x12
                        Aq(23) = -x12
                        Aq(24) = 0.0
!
                        Bq(1) = y3a
                        Bq(3) = x23
                        Bq(5) = x23
                        Bq(6) = y3a
                        Bq(7) = -y4a
                        Bq(9) = -x14
                        Bq(11) = -x14
                        Bq(12) = -y4a
                        Bq(13) = y4a
                        Bq(15) = x14
                        Bq(17) = x14
                        Bq(18) = y4a
                        Bq(19) = -y3a
                        Bq(21) = -x23
                        Bq(23) = -x23
                        Bq(24) = -y3a
!
                        Cq(1) = -y34
                        Cq(3) = x34
                        Cq(5) = x34
                        Cq(6) = -y34
                        Cq(7) = y34
                        Cq(9) = -x34
                        Cq(11) = -x34
                        Cq(12) = y34
                        Cq(13) = 0.0
                        Cq(15) = -x12
                        Cq(17) = -x12
                        Cq(18) = 0.0
                        Cq(19) = 0.0
                        Cq(21) = x12
                        Cq(23) = x12
                        Cq(24) = 0.0
!
                        nn = 0
                        DO i = 1 , 4
                           DO k = 1 , 2
                              DO j = 1 , 4
                                 DO l = 1 , 2
                                    nn = nn + 1
                                    im1 = i - 1
                                    jm1 = j - 1
                                    km1 = k - 1
                                    lm1 = l - 1
                                    k1 = 6*im1 + 4*km1 + 1
                                    k2 = 6*im1 + 3*km1 + 3
                                    l1 = 6*jm1 + 4*lm1 + 1
                                    l2 = 6*jm1 + 3*lm1 + 3
                                    kl = k + l - 1
                                    k3 = k + 3
                                    l3 = l + 3
                                    d = C(kl)*Aq(k1)*Aq(l1) + C(k3)*Aq(k1)*Aq(l2) + C(l3)*Aq(k2)*Aq(l1)
!
                                    v = C(kl)*((Aq(k1)*Bq(l1))+(Bq(k1)*Aq(l1))) + C(k3)*((Aq(k1)*Bq(l2))+(Bq(k1)*Aq(l2))) + C(l3)   &
                                      & *((Aq(k2)*Bq(l1))+(Bq(k2)*Aq(l1)))
!
                                    f = C(kl)*((Aq(k1)*Cq(l1))+(Cq(k1)*Aq(l1))) + C(k3)*((Aq(k1)*Cq(l2))+(Cq(k1)*Aq(l2))) + C(l3)   &
                                      & *((Aq(k2)*Cq(l1))+(Cq(k2)*Aq(l1)))
!
                                    h = C(kl)*((Bq(k1)*Cq(l1))+(Cq(k1)*Bq(l1))) + C(k3)*((Bq(k1)*Cq(l2))+(Cq(k1)*Bq(l2))) + C(l3)   &
                                      & *((Bq(k2)*Cq(l1))+(Cq(k2)*Bq(l1)))
!
                                    p = C(kl)*Bq(k1)*Bq(l1) + C(k3)*Bq(k1)*Bq(l2) + C(l3)*Bq(k2)*Bq(l1)
!
                                    q = C(kl)*Cq(k1)*Cq(l1) + C(k3)*Cq(k1)*Cq(l2) + C(l3)*Cq(k2)*Cq(l1)
!
!     USE GAUSSIAN INTEGRATION TO FIND THE PARTITIONS OF
!     THE STIFFNESS MATRIX FOR THE MEAN PLANE ELEMENT
!
                                    U(nn) = 0.0D0
                                    DO ia01 = 1 , 2
                                       DO ja01 = 1 , 2
                                         U(nn) = U(nn) + o(d,v,f,h,p,q,y4a,x12,y34,y3a,x23,x14,eta01(ia01),eta01(ja01))
                                       ENDDO
                                    ENDDO
                                    U(nn) = U(nn)/4.0D0*dble(Thick)
!
!     ADD SHEAR TERMS HERE
!
                                    U(nn) = U(nn) + fact*(Aq(k2)+0.5*(Bq(k2)+Cq(k2)))*(Aq(l2)+0.5*(Bq(l2)+Cq(l2)))
                                 ENDDO
                              ENDDO
                           ENDDO
                        ENDDO
!
!     TRANSFORM FROM MEAN PLANE TO ACTUAL GRID POINTS
!
!                   T
!      K = B * K * B
!
!     EXPAND MATRIX TO INCLUDE Z COORDINATES
!     IF NON-PLANAR,
!
                        IF ( planar ) THEN
!
!  *  IF PLANAR, TEMPAR(12X12) .EQ. U(8X8)
!
                           ij1 = -12
                           i2 = 144
                           DO i = 1 , 64
                              Tempar(i2+i) = U(i)
                           ENDDO
                           DO i = 1 , 12
                              ij1 = ij1 + 12
                              IF ( mod(i,3)/=0 ) THEN
                                 DO j = 1 , 12
                                    ij = ij1 + j
                                    IF ( mod(j,3)/=0 ) THEN
                                       i2 = i2 + 1
                                       Tempar(ij) = Tempar(i2)
                                    ELSE
                                       Tempar(ij) = 0.0D0
                                    ENDIF
                                 ENDDO
                              ELSE
                                 DO j = 1 , 12
                                    ij = ij1 + j
                                    Tempar(ij) = 0.0D0
                                 ENDDO
                              ENDIF
                           ENDDO
                        ELSE
                           CALL gmmatd(B(1),12,8,0,U(1),8,8,0,Btxk(1))
                           CALL gmmatd(Btxk(1),12,8,0,B(1),12,8,1,Tempar(1))
                        ENDIF
!
!                T            T
!  *  GENERATE (T  * E) AND (E  * T )
!                I                 J
!
                        DO i = 1 , 4
                           ka = 4*i + 6
                           IF ( Necpt(ka)==0 ) THEN
                              DO ii = 1 , 9
                                 Tie(ii,i) = E(ii)
                              ENDDO
                              Etj(1,i) = E(1)
                              Etj(2,i) = E(4)
                              Etj(3,i) = E(7)
                              Etj(4,i) = E(2)
                              Etj(5,i) = E(5)
                              Etj(6,i) = E(8)
                              Etj(7,i) = E(3)
                              Etj(8,i) = E(6)
                              Etj(9,i) = E(9)
                           ELSE
                              CALL transd(Necpt(ka),Ti)
                              CALL gmmatd(Ti,3,3,1,E,3,3,0,Tie(1,i))
                              CALL gmmatd(E,3,3,1,Ti,3,3,0,Etj(1,i))
                           ENDIF
                        ENDDO
!                                      T              T
!     COMPUTE STIFFNESS MATRIX  K   = T  * E * S   * E  * T
!                                IJ    I        IJ         J
!
!     EXTRACT 3 BY 3 PARTITIONS, TRANSFORM TO GLOBAL, AND INSERT
!     BY ORDER OF SILS INTO A 12 BY 12 MATRIX
!
                        DO i = 1 , 4
                           j = Ngrid(i)
                           DO k = 2 , 5
                              IF ( Necpt(k)==j ) GOTO 2
                           ENDDO
                           CALL mesage(-30,34,Ecpt(1))
 2                         map(1,i) = j
                           map(2,i) = i
                        ENDDO
                        CALL sort(0,0,2,1,map(1,1),8)
!
!     REPLACE SILS WITH INDICES
!     RESORT FOR ORIGINAL ORDER - WORD 1 WILL CONTAIN NEW LOCATION
!
                        DO i = 1 , 4
                           map(1,i) = i
                        ENDDO
                        CALL sort(0,0,2,2,map(1,1),8)
!
!     MOVE AND TRANSFORM HERE
!     ROW LOOP
!
                        DO i = 1 , 4
                           ior = 36*(i-1)
                           inr = 36*(map(1,i)-1)
!
!     COLUMN LOOP
!
                           DO j = 1 , 4
                              iocl = ior + 3*(j-1)
                              incl = inr + 3*(map(1,j)-1)
!
!     INNER LOOPS
!
                              DO k = 1 , 3
                                 kl = iocl + 12*(k-1)
                                 DO l = 1 , 3
                                    Kij(l,k) = Tempar(kl+l)
                                 ENDDO
                              ENDDO
!
!     TRANSFORM 3 BY 3
!
                              CALL gmmatd(Kij,3,3,0,Etj(1,j),3,3,0,E)
                              CALL gmmatd(Tie(1,i),3,3,0,E,3,3,0,Kij)
!
!     INSERT
!
                              DO k = 1 , 3
                                 kl = incl + 12*(k-1)
                                 DO l = 1 , 3
                                    B(kl+l) = Kij(l,k)
                                 ENDDO
                              ENDDO
                           ENDDO
                        ENDDO
!
!     INSERT WHOLE 12 BY 12 USING EMGOUT
!
                        dict(1) = Estid
                        dict(2) = 1
                        dict(3) = 12
                        dict(4) = 7
                        dict5 = Gsube
                        ldata = 144
                        ieoe = 1
                        ifile = 1
                        CALL emgout(B,B,ldata,ieoe,dict,ifile,Iprec)
!
!     DO MASS IF NECESSARY
!
                        IF ( Mass==0 ) RETURN
                        DO i = 1 , 4
                           kl = 3*(map(1,i)-1)
                           DO j = 1 , 3
                              B(kl+j) = mgg(i)
                           ENDDO
                        ENDDO
                        dict(2) = 2
                        dict(5) = 0
                        ldata = 12
                        ifile = 2
                        CALL emgout(B,B,ldata,ieoe,dict,ifile,Iprec)
                        RETURN
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   CALL mesage(30,j,Ecpt(1))
   Nogo = .TRUE.
   RETURN
99999 RETURN
END SUBROUTINE qdmm1d
