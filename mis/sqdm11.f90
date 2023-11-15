
SUBROUTINE sqdm11
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(24) , Alphas(3) , Angle , B(96) , Consts(5) , Costh , Degra , Dumb(75) , Dummy(39) , Dummy1 , Dummy2 , Dummy3 , Dummy4 ,  &
      & E(9) , Ecpt(26) , Eltemp , Fmu , Forvec(25) , G(9) , G11 , G12 , G13 , G22 , G23 , G2x211 , G2x212 , G2x222 , G33 , Gsube , &
      & Ph1out(100) , Rho , Sigcom , Sigshe , Sigten , Sinth , Stress , T , Tempar(150) , Theta , Ti(9) , Tsub0 , X1 , X2 , X3 ,    &
      & X4 , Y1 , Y2 , Y3 , Y4 , Z1 , Z2 , Z3 , Z4
   INTEGER Inflag , Matid , Matid1 , Nbpw , Necpt(1) , Ngrid(4)
   COMMON /condas/ Consts
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alphas , Tsub0 , Gsube , Sigten , Sigcom , Sigshe , G2x211 , G2x212 ,  &
                 & G2x222
   COMMON /sdr2x5/ Necpt , Ngrid , Angle , Matid1 , T , Fmu , Dummy1 , X1 , Y1 , Z1 , Dummy2 , X2 , Y2 , Z2 , Dummy3 , X3 , Y3 ,    &
                 & Z3 , Dummy4 , X4 , Y4 , Z4 , Dumb , Ph1out , Forvec
   COMMON /sdr2x6/ E , Ti , Theta , Tempar , A , G , B
   COMMON /system/ Dummy , Nbpw
!
! Local variable declarations
!
   REAL a2 , a3 , aj , b2 , b3 , c1 , cth1 , cth2 , cth31 , cth32 , cth41 , cth42 , dlt1 , dlt2 , ee(144) , etas , hh , la , lb ,   &
      & lbd1 , lc , lcd1 , lcd2 , ld , ldd2 , magi , magj , magk , pi1 , pi2 , pi3 , pj1 , pj2 , pj3 , pk1 , pk2 , pk3 , sth1 ,     &
      & sth2 , sth31 , sth32 , sth41 , sth42 , temp1 , temp2 , tol , tol2 , x12 , x13 , x14 , x21 , x23 , x24 , x31 , x34 , x41 ,   &
      & x42 , xis , xstar , y21 , y31 , y34 , y3a , y41 , y42 , y4a , ystar , z21 , z31 , z41 , z42
   INTEGER i , ii , iict , il , il59 , irowct , jj , jjct , ka , kk , kkct , ktot , l , lcnt , llct , mmct , n , nn , nn49 , nnct
!
! End of declarations
!
!
!     PHASE I OF STRESS DATA RECOVERY FOR THE  QUADRILATERAL MEMBRANE
!     ELEMENT
!
!     CALLS FROM THIS ROUTINE ARE MADE TO
!
!            MAT    - MATERIAL DATA ROUTINE
!            MESAGE - ERROR MESSAGE WRITER
!            GMMATS - SINGLE MATRIX MULTIPLY AND TRANSPOSE
!            TRANSS - SINGLE PRECISION TRANSFORMATION SUPPLIER
!
!
   EQUIVALENCE (Consts(4),Degra) , (Ecpt(1),Necpt(1))
!
!
!      ECPT LIST
!                                                      IN
!                                                      THIS
!        ECPT       DESCRIPTION                        ROUTINE    TYPE
!     *******    *********************************     ********  ******
!     ECPT( 1) = ELEMENT ID                            NECPT(1)  INTEGER
!     ECPT( 2)   GRID POINT A                          NGRID(1)  INTEGER
!     ECPT( 3)   GRID POINT B                          NGRID(2)  INTEGER
!     ECPT( 4)   GRID POINT C                          NGRID(3)  INTEGER
!     ECPT( 5)   GRID POINT D                          NGRID(4)  INTEGER
!     ECPT( 6) = THETA = ANGLE OF MATERIAL             ANGLE      REAL
!     ECPT( 7)   MATERIAL ID                           MATID     INTEGER
!     ECPT( 8) = THICKNESS                             T          REAL
!     ECPT( 9) = NON-STRUCTURAL MASS                   FMU        REAL
!     ECPT(10)   COORD. SYSTEM ID 1                    NECPT(10) INTEGER
!     ECPT(11) = X1                                     X1        REAL
!     ECPT(12) = Y1                                     Y1        REAL
!     ECPT(13) = Z1                                     Z1        REAL
!     ECPT(14)   COORD. SYSTEM ID 2                    NECPT(14) INTEGER
!     ECPT(15) = X2                                     X2        REAL
!     ECPT(16) = Y2                                     Y2        REAL
!     ECPT(17) = Z2                                     Z2        REAL
!     ECPT(18)   COORD. SYSTEM ID 3                    NECPT(18) INTEGER
!     ECPT(19) = X3                                     X3        REAL
!     ECPT(20) = Y3                                     Y3        REAL
!     ECPT(21) = Z3                                     Z3        REAL
!     ECPT(22)   COORD. SYSTEM ID 4                    NECPT(22) INTEGER
!     ECPT(23) = X4                                     X4        REAL
!     ECPT(24) = Y4                                     Y4        REAL
!     ECPT(25)   Z4                                     Z4        REAL
!     ECPT(26) = ELEMENT TEMPERATURE                    ECPT(26)  REAL
!
!
!     SET UP THE E MATRIX WHICH IS (12X12) FOR THE QUAD-MEMBRANE PROJECT
!                         ONTO THE MEAN PLANE
!
   DO i = 1 , 144
      ee(i) = 0.
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
!     COMPUTE ELEMENTS OF THE E MATRIX (3X3)
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
            ee(ktot) = E(nnct)
         ENDDO
      ENDDO
   ENDDO
!
!     COMPUTE DIFFERENCES OF COORDINATES OF GRID POINTS IN THE MEAN PLAN
!
   x12 = -(x21*E(1)+y21*E(4)+z21*E(7))
   x13 = -(x31*E(1)+y31*E(4)+z31*E(7))
   x14 = -(x41*E(1)+y41*E(4)+z41*E(7))
   y3a = (x31*E(2)+y31*E(5)+z31*E(8))
   y4a = (x42*E(2)+y42*E(5)+z42*E(8))
   x24 = x14 - x12
   x23 = x13 - x12
   x34 = x14 - x13
   y34 = y3a - y4a
!
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
!
   dlt1 = cth31*cth32 - sth31*sth32
   dlt2 = cth42*cth41 + sth41*sth42
   ldd2 = ld*dlt2
   lbd1 = lb*dlt1
   lcd1 = lc*dlt1
   lcd2 = lc*dlt2
!
!                                                  *       *
!     COMPUTE THE INTERSECTION OF THE DIAGONALS(ETA  AND XI ) OF
!     THE ELEMENTS IN THE MEAN PLANE
!
   tol = 1.0E-3*(-x12)
   IF ( Nbpw>=60 ) tol = 1.0E-5*(-x12)
   tol2 = 1.0E-3*x12*x12
   IF ( Nbpw>=60 ) tol2 = 1.0E-5*x12*x12
   IF ( abs(x34+x12)>tol .OR. abs(y34)>tol ) THEN
      IF ( abs(x24)>=tol .AND. abs(x13)>=tol ) THEN
         xstar = (y4a*x13*x12)/((y3a*x24)-(y4a*x13))
         ystar = (-y4a/x24)*(xstar+x12)
      ELSEIF ( abs(x13)>tol ) THEN
         xstar = -x12
         ystar = (y3a/x13)*x12
      ELSE
         xstar = -x13
         ystar = (-y4a/x24)*x12
      ENDIF
      IF ( abs(x34+x12)<tol ) THEN
         a3 = -x14*y34
         b3 = x12*y4a - y34*xstar
         IF ( abs(a3)<=tol2 ) THEN
            etas = (x12*ystar)/b3
         ELSE
            temp2 = sqrt(b3**2+(4.*a3*x12*ystar))/(2.*a3)
            temp1 = -b3/(2.*a3)
            etas = temp1 - temp2
            IF ( etas<=0. .OR. etas>=1. ) etas = temp1 + temp2
         ENDIF
         xis = (ystar-(y4a*etas))/(y34*etas)
      ELSE
         c1 = y34*xstar - ystar*(x34+x12)
         a2 = -y4a*x23 + y3a*x14
         b2 = -y4a*x12 + c1
         IF ( abs(a2)<=tol2 ) THEN
            etas = (-x12*ystar)/b2
         ELSE
            temp2 = sqrt(b2**2-(4.*a2*x12*ystar))/(2.*a2)
            temp1 = -b2/(2.*a2)
            etas = temp1 - temp2
            IF ( etas<=0. .OR. etas>=1. ) etas = temp1 + temp2
         ENDIF
         IF ( abs(y34)<tol ) THEN
            xis = (xstar+(x14*etas))/((etas*(x34+x12))-x12)
         ELSE
            xis = (-c1+((y4a*x23)-(y3a*x14))*etas)/(y34*x12)
         ENDIF
      ENDIF
   ELSE
      etas = .5
      xis = .5
   ENDIF
!
!     SET UP THE (12X12) TRANSFORMATION MATRIX B BETWEEN THE MEAN PLANE
!                        AND ACTUAL GRID POINTS
!
   DO i = 2 , 92
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
   B(79) = 1.
   B(88) = 1.
   B(90) = hh/(ld*sth1)
   B(93) = (-hh*cth32)/lcd1
   B(94) = (-hh*sth32)/lcd1
   B(95) = hh*((-cth42/ldd2)+(sth41/lcd2))
   B(96) = hh*((-sth42/ldd2)-(cth41/lcd2))
   DO i = 1 , 24
      A(i) = 0.
   ENDDO
!                                                     T
!     COMPUTE TRANSFORMED MATRIX OF STIFFNESSES  G = P  * G * P
!
   Theta = Angle*Degra
   Sinth = sin(Theta)
   Costh = cos(Theta)
   IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0E0
   Matid = Matid1
   Inflag = 2
   Eltemp = Ecpt(26)
   CALL mat(Ecpt(1))
!
!     STORE INTO G MATRIX
!
   G(1) = G11
   G(2) = G12
   G(3) = G13
   G(4) = G12
   G(5) = G22
   G(6) = G23
   G(7) = G13
   G(8) = G23
   G(9) = G33
!
!     COMPUTE MATRIX A TO RELATE DISPLACEMENTS TO STRAINS
!
   aj = (-y4a*x12) + (-y34*x12*xis) + etas*((-y4a*x23)+(y3a*x14))
   A(1) = (-y4a+(y3a*etas)-(y34*xis))/aj
   A(3) = (y4a-(y4a*etas)+(y34*xis))/aj
   A(5) = (y4a*etas)/aj
   A(7) = (-y3a*etas)/aj
   A(10) = (-x24+(x23*etas)+(x34*xis))/aj
   A(12) = (x14-(x14*etas)-(x34*xis))/aj
   A(14) = ((x14*etas)-(x12*xis))/aj
   A(16) = (-x12-(x23*etas)+(x12*xis))/aj
   A(17) = (-x24+(x23*etas)+(x34*xis))/aj
   A(18) = (-y4a+(y3a*etas)-(y34*xis))/aj
   A(19) = (x14-(x14*etas)-(x34*xis))/aj
   A(20) = (y4a-(y4a*etas)+(y34*xis))/aj
   A(21) = ((x14*etas)-(x12*xis))/aj
   A(22) = (y4a*etas)/aj
   A(23) = (-x12-(x23*etas)+(x12*xis))/aj
   A(24) = (-y3a*etas)/aj
!
!                          T    T
!     COMPUTE S = G * A * B  * E
!
   CALL gmmats(B(1),12,8,1,ee(1),12,12,1,Tempar(1))
   CALL gmmats(A(1),3,8,0,Tempar(1),8,12,0,Tempar(100))
   CALL gmmats(G(1),3,3,0,Tempar(100),3,12,0,Tempar(1))
   DO l = 1 , 4
      DO n = 2 , 5
         IF ( Necpt(n)==Ngrid(l) ) THEN
            ka = 4*n + 2
            GOTO 50
         ENDIF
      ENDDO
      CALL mesage(-30,34,Ecpt(1))
 50   IF ( Necpt(ka)==0 ) THEN
         DO ii = 1 , 9
            Ti(ii) = 0.
         ENDDO
         Ti(1) = 1.
         Ti(5) = 1.
         Ti(9) = 1.
      ELSE
         CALL transs(Necpt(ka),Ti)
      ENDIF
      lcnt = 3*(l-1)
      irowct = -12
      nn = 0
      DO jj = 1 , 3
         irowct = irowct + 12
         DO kk = 1 , 3
            nn = nn + 1
            ktot = kk + irowct + lcnt
            nn49 = nn + 49
            Tempar(nn49) = Tempar(ktot)
         ENDDO
      ENDDO
      CALL gmmats(Tempar(50),3,3,0,Ti,3,3,0,Tempar(60))
!
!                                                          TH
!     MATRICES S  RELATE DISPLACEMENTS TO STRESSES AT THE I   GRIDPOINT
!               I
!
      DO il = 1 , 9
         ktot = il + 9*l
         il59 = il + 59
         Ph1out(ktot) = Tempar(il59)
      ENDDO
   ENDDO
   CALL gmmats(G(1),3,3,0,Alphas(1),3,1,0,Ph1out(7))
   Ph1out(1) = Ecpt(1)
   Ph1out(2) = Ecpt(2)
   Ph1out(3) = Ecpt(3)
   Ph1out(4) = Ecpt(4)
   Ph1out(5) = Ecpt(5)
   Ph1out(6) = Tsub0
END SUBROUTINE sqdm11
