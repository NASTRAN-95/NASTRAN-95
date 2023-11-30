
SUBROUTINE qdmm1(Tbar,Pg)
   IMPLICIT NONE
   REAL Alphas(3) , Angle , B(96) , C(24) , Consts(5) , Costh , Degra , Dummy1 , Dummy2 , Dummy3 , Dummy4 , Ecpt(26) , Ee(144) ,    &
      & Eltemp , Fmu , G11 , G12 , G13 , G22 , G23 , G2x211 , G2x212 , G2x222 , G33 , Gsube , Rho , Sigcom , Sigshe , Sigten ,      &
      & Sinth , Stress , T , Tempar(24) , Ti(9) , Tsub0 , X1 , X2 , X3 , X4 , Y1 , Y2 , Y3 , Y4 , Z1 , Z2 , Z3 , Z4
   INTEGER Inflag , Matid , Matid1 , Necpt(1) , Ngrid(4)
   COMMON /condas/ Consts
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alphas , Tsub0 , Gsube , Sigten , Sigcom , Sigshe , G2x211 , G2x212 ,  &
                 & G2x222
   COMMON /ssgwrk/ Ee , B , Tempar , C , Ti
   COMMON /trimex/ Necpt , Ngrid , Angle , Matid1 , T , Fmu , Dummy1 , X1 , Y1 , Z1 , Dummy2 , X2 , Y2 , Z2 , Dummy3 , X3 , Y3 ,    &
                 & Z3 , Dummy4 , X4 , Y4 , Z4
   REAL Tbar
   REAL Pg(1)
   REAL cth1 , cth2 , cth31 , cth32 , cth41 , cth42 , dlt1 , dlt2 , e(9) , g(9) , h , hh , la , lb , lbd1 , lc , lcd1 , lcd2 , ld , &
      & ldd2 , magi , magj , magk , pi1 , pi2 , pi3 , pj1 , pj2 , pj3 , pk1 , pk2 , pk3 , sth1 , sth2 , sth31 , sth32 , sth41 ,     &
      & sth42 , temp , theta , x12 , x13 , x14 , x21 , x23 , x24 , x31 , x34 , x41 , x42 , y21 , y31 , y34 , y3a , y41 , y42 , y4a ,&
      & z21 , z31 , z41 , z42
   INTEGER i , i1 , iict , isw , jj , jjct , jjk , k , k19 , ka , kkct , ktot , l , llct , mmct , nnct
!
!     QUADRILATERAL MEMBRANE ELEMENT
!
!     CALLS FROM THIS ROUTINE ARE MADE TO
!           MAT    - MATERIAL DATA ROUTINE
!           MESAGE - ERROR MESSAGE WRITER
!           BASGLB - TRANSFER COORDINATES FROM BASIC TO GLOBAL
!           GMMATS - SINGLE PRECISION MATRIX MULTIPLY AND TRANSPOSE
!           TRANSS - SINGLE PRECISION TRANSFORMATION SUPPLIER
!
!     ECPT LIST
!                                                    IN THIS
!      ECPT       DESCRIPTION                        ROUTINE   TYPE
!     ========   =================================   ========  =======
!     ECPT( 1) = ELEMENT ID                          NECPT(1)  INTEGER
!     ECPT( 2)   GRID POINT A                        NGRID(1)  INTEGER
!     ECPT( 3)   GRID POINT B                        NGRID(2)  INTEGER
!     ECPT( 4)   GRID POINT C                        NGRID(3)  INTEGER
!     ECPT( 5)   GRID POINT D                        NGRID(4)  INTEGER
!     ECPT( 6) = THETA = ANGLE OF MATERIAL           ANGLE      REAL
!     ECPT( 7)   MATERIAL ID                         MATID1    INTEGER
!     ECPT( 8) = THICKNESS                           T          REAL
!     ECPT( 9) = NON-STRUCTURAL MASS                 FMU        REAL
!     ECPT(10)   COORD. SYSTEM ID 1                  NECPT(10) INTEGER
!     ECPT(11) = X1                                   X1        REAL
!     ECPT(12) = Y1                                   Y1        REAL
!     ECPT(13) = Z1                                   Z1        REAL
!     ECPT(14)   COORD. SYSTEM ID 2                  NECPT(14) INTEGER
!     ECPT(15) = X2                                   X2        REAL
!     ECPT(16) = Y2                                   Y2        REAL
!     ECPT(17) = Z2                                   Z2        REAL
!     ECPT(18)   COORD. SYSTEM ID 3                  NECPT(18) INTEGER
!     ECPT(19) = X3                                   X3        REAL
!     ECPT(20) = Y3                                   Y3        REAL
!     ECPT(21) = Z3                                   Z3        REAL
!     ECPT(22)   COORD. SYSTEM ID 4                  NECPT(22) INTEGER
!     ECPT(23) = X4                                   X4        REAL
!     ECPT(24) = Y4                                   Y4        REAL
!     ECPT(25)   Z4                                   Z4        REAL
!
   EQUIVALENCE (Consts(4),Degra) , (Ecpt(1),Necpt(1))
!
!     SET UP THE E MATRIX WHICH IS (12X12) FOR THE QUAD-MEMBRANE PROJECT
!                         ONTO THE MEAN PLANE
!
   DO i = 1 , 144
      Ee(i) = 0.
   ENDDO
!
!     E(1), E(4), E(7) WILL BE THE I-VECTOR
!     E(2), E(5), E(8) WILL BE THE J-VECTOR
!     E(3), E(6), E(9) WILL BE THE K-VECTOR
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
!     COMPUTE THE ELEMENTS OF THE (3X3) E MATRIX
!
   pk1 = y31*z42 - z31*y42
   pk2 = z31*x42 - x31*z42
   pk3 = x31*y42 - y31*x42
   magk = sqrt(pk1**2+pk2**2+pk3**2)
   IF ( magk<=1.0E-06 ) CALL mesage(-30,32,Ecpt(1))
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
   magi = sqrt(pi1**2+pi2**2+pi3**2)
   IF ( magi<=1.0E-06 ) CALL mesage(-30,31,Ecpt(1))
   pi1 = pi1/magi
   pi2 = pi2/magi
   pi3 = pi3/magi
   hh = -hh/2.
!
!     THIS SIGN CHANGE MADE BECAUSE SIGN OF H AS DEFINED ON
!     PAGE 4.87-105 OF PROGRAMMERS MANUAL IS WRONG
!
   pj1 = pk2*pi3 - pk3*pi2
   pj2 = pk3*pi1 - pk1*pi3
   pj3 = pk1*pi2 - pk2*pi1
   magj = sqrt(pj1**2+pj2**2+pj3**2)
   pj1 = pj1/magj
   pj2 = pj2/magj
   pj3 = pj3/magj
!
!     INSERT ELEMENTS INTO THE (3X3) E MATRIX
!
   e(1) = pi1
   e(2) = pj1
   e(3) = pk1
   e(4) = pi2
   e(5) = pj2
   e(6) = pk2
   e(7) = pi3
   e(8) = pj3
   e(9) = pk3
!
!     STORE FOUR (3X3) E MATRICES INTO (12X12) E MATRIX
!
   llct = -39
   DO iict = 1 , 12 , 3
      llct = llct + 39
      nnct = 0
      mmct = -12
      DO jjct = 1 , 3
         mmct = mmct + 12
         DO kkct = 1 , 3
            nnct = nnct + 1
            ktot = kkct + llct + mmct
            Ee(ktot) = e(nnct)
         ENDDO
      ENDDO
   ENDDO
!
!     COMPUTE DIFFERENCES OF COORDINATES OF GRID POINTS IN THE MEAN PLAN
!
   x12 = -x21*e(1) - y21*e(4) - z21*e(7)
   x13 = -x31*e(1) - y31*e(4) - z31*e(7)
   x14 = -x41*e(1) - y41*e(4) - z41*e(7)
   y3a = x31*e(2) + y31*e(5) + z31*e(8)
   y4a = x42*e(2) + y42*e(5) + z42*e(8)
   x24 = x14 - x12
   x23 = x13 - x12
   x34 = x14 - x13
   y34 = y3a - y4a
!
!     COMPUTE LENGTHS OF SIDES OF ELEMENT IN THE MEAN PLANE
!
   la = abs(x12)
   lb = sqrt(x23**2+y3a**2)
   lc = sqrt(x34**2+y34**2)
   ld = sqrt(x14**2+y4a**2)
!
!     COMPUTE THE CHARACTERISTIC ANGLES OF ELEMENT IN THE MEAN PLANE
!
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
!     SET UP THE (12X12) TRANSFORMATION MATRIX B BETWEEN THE MEAN PLANE
!                        AND ACTUAL GRID POINTS
!
   DO i = 1 , 96
      B(i) = 0.
   ENDDO
   B(1) = 1.
   B(10) = 1.
   B(17) = -hh/la
   B(18) = -hh/(ld*sth1) + ((hh*cth1)/(la*sth1))
   B(19) = hh/la
   B(20) = (hh*cth2)/(la*sth2)
   B(23) = (hh*cth42)/ldd2
   B(24) = (hh*sth42)/ldd2
   B(27) = 1.
   B(36) = 1.
   B(41) = -B(17)
   B(42) = -(hh*cth1)/(la*sth1)
   B(43) = B(17)
   B(44) = ((-hh*cth2)/(la*sth2)) + (hh/(lb*sth2))
   B(45) = -(hh*sth31)/lbd1
   B(46) = -(hh*cth31)/lbd1
   B(53) = 1.
   B(62) = 1.
   B(68) = -hh/(lb*sth2)
   B(69) = hh*((sth31/lbd1)+(cth32/lcd1))
   B(70) = hh*((cth31/lbd1)+(sth32/lcd1))
   B(71) = -(hh*sth41)/lcd2
   B(72) = (hh*cth41)/lcd2
   B(79) = 1.
   B(88) = 1.
   B(90) = hh/(ld*sth1)
   B(93) = -(hh*cth32)/lcd1
   B(94) = -(hh*sth32)/lcd1
   B(95) = hh*((-cth42/ldd2)+(sth41/lcd2))
   B(96) = hh*((-sth42/ldd2)-(cth41/lcd2))
   h = Ecpt(8)
   Eltemp = Ecpt(26)
!
!     SET UP (3X8) C MATRIX (SEE FMMS)
!
   C(1) = -(h*y4a)/2.
   C(2) = 0.
   C(3) = -(h*x24)/2.
   C(4) = 0.
   C(5) = -(h*x24)/2.
   C(6) = -(h*y4a)/2.
   C(7) = (h*y3a)/2.
   C(8) = 0.
   C(9) = (h*x13)/2.
   C(10) = 0.
   C(11) = (h*x13)/2.
   C(12) = (h*y3a)/2.
   C(13) = (h*y4a)/2.
   C(14) = 0.
   C(15) = (h*x24)/2.
   C(16) = 0.
   C(17) = (h*x24)/2.
   C(18) = (h*y4a)/2.
   C(19) = -(h*y3a)/2.
   C(20) = 0.
   C(21) = -(h*x13)/2.
   C(22) = 0.
   C(23) = -(h*x13)/2.
   C(24) = -(h*y3a)/2.
   theta = Angle*Degra
   Sinth = sin(theta)
   Costh = cos(theta)
   IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0
   Matid = Matid1
   Inflag = 2
!                                                     T
!     COMPUTE TRANSFORMED MATRIX OF STIFFNESSES  G = P  * G * P
!
   CALL mat(Ecpt(1))
!
!     STORE INTO G MATRIX
!
   g(1) = G11
   g(2) = G12
   g(3) = G13
   g(4) = G12
   g(5) = G22
   g(6) = G23
   g(7) = G13
   g(8) = G23
   g(9) = G33
!
!                   T                            -
!     COMPUTE PG = T  * E * B * C * G * ALPHA * (T - T )
!                                                     0
!
   temp = Tbar - Tsub0
   Tempar(1) = Alphas(1)*temp
   Tempar(2) = Alphas(2)*temp
   Tempar(3) = Alphas(3)*temp
   CALL gmmats(g(1),3,3,0,Tempar(1),3,1,0,Tempar(13))
   CALL gmmats(C(1),8,3,0,Tempar(13),3,1,0,Tempar(1))
   CALL gmmats(B(1),12,8,0,Tempar(1),8,1,0,Tempar(13))
   CALL gmmats(Ee(1),12,12,0,Tempar(13),12,1,0,Tempar(1))
   DO i = 1 , 4
!
!     T-SUB-I WILL BE USED BELOW ONLY IF THE PIVOT COORDINATE SYSTEM ID
!     IS NOT ZERO, OTHERWISE IT IS ASSUMED TO BE THE IDENTITY MATRIX.
!
      ka = 4*i + 6
!
!     DO WE NEED TRANSFORMATION TI
!
      isw = 0
      jj = 3*i - 2
      IF ( Necpt(ka)/=0 ) THEN
         isw = 1
         CALL basglb(Tempar(jj),Tempar(20),Necpt(ka+1),Necpt(ka))
      ENDIF
!
!     COMPUTE PG VECTOR
!
      DO k = 1 , 3
         jjk = jj + k - 1
         k19 = k + 19
         IF ( isw==0 ) Tempar(k19) = Tempar(jjk)
         i1 = i + 1
         l = Necpt(i1) + k - 1
         Pg(l) = Pg(l) + Tempar(k19)
      ENDDO
   ENDDO
END SUBROUTINE qdmm1
