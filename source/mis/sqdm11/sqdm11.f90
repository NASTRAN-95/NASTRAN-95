!*==sqdm11.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sqdm11
   USE c_condas
   USE c_matin
   USE c_matout
   USE c_sdr2x5
   USE c_sdr2x6
   USE c_system
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a2 , a3 , aj , b2 , b3 , c1 , cth1 , cth2 , cth31 , cth32 , cth41 , cth42 , degra , dlt1 , dlt2 , etas , hh , la , lb ,  &
         & lbd1 , lc , lcd1 , lcd2 , ld , ldd2 , magi , magj , magk , pi1 , pi2 , pi3 , pj1 , pj2 , pj3 , pk1 , pk2 , pk3 , sth1 ,  &
         & sth2 , sth31 , sth32 , sth41 , sth42 , temp1 , temp2 , tol , tol2 , x12 , x13 , x14 , x21 , x23 , x24 , x31 , x34 , x41 ,&
         & x42 , xis , xstar , y21 , y31 , y34 , y3a , y41 , y42 , y4a , ystar , z21 , z31 , z41 , z42
   REAL , DIMENSION(26) :: ecpt
   REAL , DIMENSION(144) :: ee
   INTEGER :: i , ii , iict , il , il59 , irowct , jj , jjct , ka , kk , kkct , ktot , l , lcnt , llct , mmct , n , nn , nn49 , nnct
   EXTERNAL gmmats , mat , mesage , transs
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Consts(4),Degra) , (Ecpt(1),Necpt(1))
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
   x21 = x2 - x1
   y21 = y2 - y1
   z21 = z2 - z1
   x31 = x3 - x1
   y31 = y3 - y1
   z31 = z3 - z1
   x41 = x4 - x1
   y41 = y4 - y1
   z41 = z4 - z1
   x42 = x4 - x2
   y42 = y4 - y2
   z42 = z4 - z2
!
!     COMPUTE ELEMENTS OF THE E MATRIX (3X3)
!
   pk1 = y31*z42 - z31*y42
   pk2 = z31*x42 - x31*z42
   pk3 = x31*y42 - y31*x42
   magk = sqrt(pk1**2+pk2**2+pk3**2)
   IF ( magk<=1.0E-06 ) CALL mesage(-30,32,ecpt(1))
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
   IF ( magi<=1.0E-06 ) CALL mesage(-30,31,ecpt(1))
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
            ee(ktot) = e(nnct)
         ENDDO
      ENDDO
   ENDDO
!
!     COMPUTE DIFFERENCES OF COORDINATES OF GRID POINTS IN THE MEAN PLAN
!
   x12 = -(x21*e(1)+y21*e(4)+z21*e(7))
   x13 = -(x31*e(1)+y31*e(4)+z31*e(7))
   x14 = -(x41*e(1)+y41*e(4)+z41*e(7))
   y3a = (x31*e(2)+y31*e(5)+z31*e(8))
   y4a = (x42*e(2)+y42*e(5)+z42*e(8))
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
   IF ( nbpw>=60 ) tol = 1.0E-5*(-x12)
   tol2 = 1.0E-3*x12*x12
   IF ( nbpw>=60 ) tol2 = 1.0E-5*x12*x12
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
      b(i) = 0.
   ENDDO
   b(1) = 1.
   b(10) = 1.
   b(17) = -hh/la
   b(18) = -hh/(ld*sth1) + ((hh*cth1)/(la*sth1))
   b(19) = hh/la
   b(20) = (hh*cth2)/(la*sth2)
   b(23) = (hh*cth42)/ldd2
   b(24) = (hh*sth42)/ldd2
   b(27) = 1.
   b(36) = 1.
   b(41) = -b(17)
   b(42) = (-hh*cth1)/(la*sth1)
   b(43) = b(17)
   b(44) = ((-hh*cth2)/(la*sth2)) + (hh/(lb*sth2))
   b(45) = (-hh*sth31)/lbd1
   b(46) = (-hh*cth31)/lbd1
   b(53) = 1.
   b(62) = 1.
   b(68) = -hh/(lb*sth2)
   b(69) = hh*((sth31/lbd1)+(cth32/lcd1))
   b(70) = hh*((cth31/lbd1)+(sth32/lcd1))
   b(71) = (-hh*sth41)/lcd2
   b(72) = (hh*cth41)/lcd2
   b(79) = 1.
   b(88) = 1.
   b(90) = hh/(ld*sth1)
   b(93) = (-hh*cth32)/lcd1
   b(94) = (-hh*sth32)/lcd1
   b(95) = hh*((-cth42/ldd2)+(sth41/lcd2))
   b(96) = hh*((-sth42/ldd2)-(cth41/lcd2))
   DO i = 1 , 24
      a(i) = 0.
   ENDDO
!                                                     T
!     COMPUTE TRANSFORMED MATRIX OF STIFFNESSES  G = P  * G * P
!
   theta = angle*degra
   sinth = sin(theta)
   costh = cos(theta)
   IF ( abs(sinth)<1.0E-06 ) sinth = 0.0E0
   matid = matid1
   inflag = 2
   eltemp = ecpt(26)
   CALL mat(ecpt(1))
!
!     STORE INTO G MATRIX
!
   g(1) = g11
   g(2) = g12
   g(3) = g13
   g(4) = g12
   g(5) = g22
   g(6) = g23
   g(7) = g13
   g(8) = g23
   g(9) = g33
!
!     COMPUTE MATRIX A TO RELATE DISPLACEMENTS TO STRAINS
!
   aj = (-y4a*x12) + (-y34*x12*xis) + etas*((-y4a*x23)+(y3a*x14))
   a(1) = (-y4a+(y3a*etas)-(y34*xis))/aj
   a(3) = (y4a-(y4a*etas)+(y34*xis))/aj
   a(5) = (y4a*etas)/aj
   a(7) = (-y3a*etas)/aj
   a(10) = (-x24+(x23*etas)+(x34*xis))/aj
   a(12) = (x14-(x14*etas)-(x34*xis))/aj
   a(14) = ((x14*etas)-(x12*xis))/aj
   a(16) = (-x12-(x23*etas)+(x12*xis))/aj
   a(17) = (-x24+(x23*etas)+(x34*xis))/aj
   a(18) = (-y4a+(y3a*etas)-(y34*xis))/aj
   a(19) = (x14-(x14*etas)-(x34*xis))/aj
   a(20) = (y4a-(y4a*etas)+(y34*xis))/aj
   a(21) = ((x14*etas)-(x12*xis))/aj
   a(22) = (y4a*etas)/aj
   a(23) = (-x12-(x23*etas)+(x12*xis))/aj
   a(24) = (-y3a*etas)/aj
!
!                          T    T
!     COMPUTE S = G * A * B  * E
!
   CALL gmmats(b(1),12,8,1,ee(1),12,12,1,tempar(1))
   CALL gmmats(a(1),3,8,0,tempar(1),8,12,0,tempar(100))
   CALL gmmats(g(1),3,3,0,tempar(100),3,12,0,tempar(1))
   DO l = 1 , 4
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            DO n = 2 , 5
               IF ( necpt(n)==ngrid(l) ) THEN
                  ka = 4*n + 2
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            CALL mesage(-30,34,ecpt(1))
            spag_nextblock_1 = 2
         CASE (2)
            IF ( necpt(ka)==0 ) THEN
               DO ii = 1 , 9
                  ti(ii) = 0.
               ENDDO
               ti(1) = 1.
               ti(5) = 1.
               ti(9) = 1.
            ELSE
               CALL transs(necpt(ka),ti)
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
                  tempar(nn49) = tempar(ktot)
               ENDDO
            ENDDO
            CALL gmmats(tempar(50),3,3,0,ti,3,3,0,tempar(60))
!
!                                                          TH
!     MATRICES S  RELATE DISPLACEMENTS TO STRESSES AT THE I   GRIDPOINT
!               I
!
            DO il = 1 , 9
               ktot = il + 9*l
               il59 = il + 59
               ph1out(ktot) = tempar(il59)
            ENDDO
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
   CALL gmmats(g(1),3,3,0,alphas(1),3,1,0,ph1out(7))
   ph1out(1) = ecpt(1)
   ph1out(2) = ecpt(2)
   ph1out(3) = ecpt(3)
   ph1out(4) = ecpt(4)
   ph1out(5) = ecpt(5)
   ph1out(6) = tsub0
END SUBROUTINE sqdm11
