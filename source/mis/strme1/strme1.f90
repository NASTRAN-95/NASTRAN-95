!*==strme1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE strme1(Ntype)
   USE c_blank
   USE c_condas
   USE c_matin
   USE c_matout
   USE c_sdr2x5
   USE c_sdr2x6
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ntype
!
! Local variable declarations rewritten by SPAG
!
   REAL :: degra
   REAL , DIMENSION(21) :: ecpt
   REAL , DIMENSION(9) :: g
   INTEGER :: i , j , npt1
   EXTERNAL gmmats , mat , mesage , transs
!
! End of declarations rewritten by SPAG
!
!
!     ******** PHASE I OF STRESS DATA RECOVERY *************************
!     ******** TRIANGULAR MEMBRANE ELEMENT *****************************
!
!     CALLS FROM THIS ROUTINE ARE MADE TO. . .
!
!     MAT    - MATERIAL DATA ROUTINE
!     TRANSS - SINGLE PRECISION TRANSFORMATION SUPPLIER
!     GMMATS - SINGLE PRECISION MATRIX MULTIPLY AND TRANSPOSE
!     MESAGE - ERROR MESSAGE WRITER
!
!     IF NTYPE = 0  COMPLETE MEMBRANE COMPUTATION IS PERFORMED
!
!     IF NTYPE = 1 RETURN 3 TRANSFORMED 3X3 MATRICES ONLY
!
!
!
!
!
!
   !>>>>EQUIVALENCE (Consts(4),Degra)
   !>>>>EQUIVALENCE (G(1),Tempar(19)) , (Ecpt(1),Necpt(1))
!
!     ECPT LIST
!                                                      IN
!                                                      THIS
!       ECPT       DESCRIPTION                         ROUTINE   TYPE
!     ******************************************************************
!       ECPT( 1) = ELEMENT ID                          NECPT(1)  INTEGER
!       ECPT( 2) = GRID POINT A                        NGRID(1)  INTEGER
!       ECPT( 3) = GRID POINT B                        NGRID(2)  INTEGER
!       ECPT( 4) = GRID POINT C                        NGRID(3)  INTEGER
!       ECPT( 5) = THETA = ANGLE OF MATERIAL           ANGLE     REAL
!       ECPT( 6) = MATERIAL ID                         MATID     INTEGER
!       ECPT( 7) = T                                   T         REAL
!       ECPT( 8) = NON-STRUCTURAL MASS                 FMU       REAL
!       ECPT( 9) = COORD. SYSTEM ID 1                  NECPT(9)  INTEGER
!       ECPT(10) = X1                                  X1        REAL
!       ECPT(11) = Y1                                  Y1        REAL
!       ECPT(12) = Z1                                  Z1        REAL
!       ECPT(13) = COORD. SYSTEM ID 2                  NECPT(13) INTEGER
!       ECPT(14) = X2                                  X2        REAL
!       ECPT(15) = Y2                                  Y2        REAL
!       ECPT(16) = Z2                                  Z2        REAL
!       ECPT(17) = COORD. SYSTEM ID 3                  NECPT(17) INTEGER
!       ECPT(18) = X3                                  X3        REAL
!       ECPT(19) = Y3                                  Y3        REAL
!       ECPT(20) = Z3                                  Z3        REAL
!       ECPT(21) = ELEMENT TEMPERATURE                 ELTEMP    REAL
!
!     ******************************************************************
   eltemp = ecpt(21)
!
!     SET UP THE E MATRIX WHICH IS (3X2) FOR THE TRI-MEMBRANE
!
!     E(1), E(3), E(5) WILL BE THE I-VECTOR
!     E(2), E(4), E(6) WILL BE THE J-VECTOR
!     E(7), E(8), E(9) WILL BE THE K-VECTOR NOT USED IN E FOR MEMBRANE
!
!     FIRST FIND I-VECTOR = RSUBB - RSUBA  (NON-NORMALIZED)
   e(1) = x2 - x1
   e(3) = y2 - y1
   e(5) = z2 - z1
!
!     NOW FIND LENGTH = X-SUB-B   COORD. IN ELEMENT SYSTEM
   xsubb = sqrt(e(1)**2+e(3)**2+e(5)**2)
   IF ( xsubb<=1.0E-06 ) CALL mesage(-30,31,ecpt(1))
!
!  20 NOW NORMALIZE I-VECTOR WITH X-SUB-B
   e(1) = e(1)/xsubb
   e(3) = e(3)/xsubb
   e(5) = e(5)/xsubb
!
!     HERE WE NOW TAKE RSUBC - RSUBA AND STORE TEMPORARILY IN
!     E(2), E(4), E(6) WHICH IS WHERE THE J-VECTOR WILL FIT LATER
!
   e(2) = x3 - x1
   e(4) = y3 - y1
   e(6) = z3 - z1
!
!     X-SUB-C  =  I . (RSUBC - RSUBA) ,  THUS
   xsubc = e(1)*e(2) + e(3)*e(4) + e(5)*e(6)
!
!     AND CROSSING THE I-VECTOR TO (RSUBC-RSUBA) GIVES THE K-VECTOR
!     (NON-NORMALIZED)
!
   e(7) = e(3)*e(6) - e(5)*e(4)
   e(8) = e(5)*e(2) - e(1)*e(6)
   e(9) = e(1)*e(4) - e(3)*e(2)
!
!
!     THE LENGTH OF THE K-VECTOR IS NOW FOUND AND EQUALS Y-SUB-C
!     COORD. IN ELEMENT SYSTEM
   ysubc = sqrt(e(7)**2+e(8)**2+e(9)**2)
   IF ( ysubc<=1.0E-06 ) CALL mesage(-30,32,ecpt(1))
!
!  25 NOW NORMALIZE K-VECTOR WITH YSUBC JUST FOUND
!
   e(7) = e(7)/ysubc
   e(8) = e(8)/ysubc
   e(9) = e(9)/ysubc
!
!     NOW HAVING I AND K VECTORS.GET J = I CROSS K AND
!     STORE IN THE SPOT FOR J
!
   e(2) = e(5)*e(8) - e(3)*e(9)
   e(4) = e(1)*e(9) - e(5)*e(7)
   e(6) = e(3)*e(7) - e(1)*e(8)
!
!     AND JUST FOR COMPUTER EXACTNESS NORMALIZE J-VECTOR TO MAKE SURE.
   temp = sqrt(e(2)**2+e(4)**2+e(6)**2)
   e(2) = e(2)/temp
   e(4) = e(4)/temp
   e(6) = e(6)/temp
!
!     VOLUME OF ELEMENT, THETA, MU, LAMDA, AND DELTA
!
   reelmu = 1.0D0/xsubb
   flamda = 1.0D0/ysubc
   delta = xsubc/xsubb - 1.0E0
!
!     ******************************************************************
!
!     NOW FORM THE  C MATRIX   (3X6) PARTITIONED AS FOLLOWS HERE.
!                 CSUBA = (3X2) STORED IN C(1) . . .C(6)  BY ROWS
!                 CSUBB = (3X2) STORED IN C(7) . . .C(12) BY ROWS
!                 CSUBC = (3X2) STORED IN C(13). . .C(18) BY ROWS
!
   c(1) = -reelmu
   c(2) = 0.0E0
   c(3) = 0.0E0
   c(4) = flamda*delta
   c(5) = c(4)
   c(6) = -reelmu
   c(7) = reelmu
   c(8) = 0.0E0
   c(9) = 0.0E0
   c(10) = -flamda*reelmu*xsubc
   c(11) = c(10)
   c(12) = reelmu
   c(13) = 0.0E0
   c(14) = 0.0E0
   c(15) = 0.0E0
   c(16) = flamda
   c(17) = flamda
   c(18) = 0.0E0
!
   IF ( Ntype/=1 ) THEN
      theta = angle*degra
      sinth = sin(theta)
      costh = cos(theta)
   ENDIF
   IF ( abs(sinth)<1.0E-06 ) sinth = 0.0E0
   eltemp = ecpt(21)
   matid = matid1
   inflag = 2
   CALL mat(ecpt(1))
!
!     FILL G-MATRIX WITH OUTPUT FROM MAT ROUTINE
!
   IF ( strain ) THEN
      g(1) = 1.0
      g(2) = 0.0
      g(3) = 0.0
      g(4) = 0.0
      g(5) = 1.0
      g(6) = 0.0
      g(7) = 0.0
      g(8) = 0.0
      g(9) = 0.5
   ELSE
      g(1) = g11
      g(2) = g12
      g(3) = g13
      g(4) = g12
      g(5) = g22
      g(6) = g23
      g(7) = g13
      g(8) = g23
      g(9) = g33
   ENDIF
!
!     ******************************************************************
!
!     G, E, AND C MATRICES ARE COMPLETE
!
!
!
!                           T
!     COMPUTE  S  = G  C   E   T   , I = 1,2,3.
!               I       I       I
!
   DO i = 1 , 3
!
!     POINTER TO C   = 6*I - 5
!                 I
!
      CALL gmmats(g,3,3,0,c(6*i-5),3,2,0,tempar(1))
      CALL gmmats(tempar(1),3,2,0,e,3,2,1,tempar(10))
!
!     DO WE NEED TRANSFORMATION TI
!
      IF ( necpt(4*i+5)==0 ) THEN
         npt1 = 9*i
         DO j = 10 , 18
            npt1 = npt1 + 1
            ph1out(npt1) = tempar(j)
         ENDDO
      ELSE
         CALL transs(necpt(4*i+5),ti)
         CALL gmmats(tempar(10),3,3,0,ti,3,3,0,ph1out(9*i+1))
      ENDIF
   ENDDO
!
!     COMPUTE S    = G  ALPHAS
!               T
   CALL gmmats(g,3,3,0,alphas,3,1,0,ph1out(7))
!
!     SAVE  T SUB 0  FOR PHASE II
!
   ph1out(6) = tsub0
   ph1out(1) = ecpt(1)
   ph1out(2) = ecpt(2)
   ph1out(3) = ecpt(3)
   ph1out(4) = ecpt(4)
!
!     THIS CONCLUDES PHASE 1 FOR TRIANGULAR MEMBRANE OR SUB CALCULATION
!     TO ANOTHER ROUTINE...
!
END SUBROUTINE strme1