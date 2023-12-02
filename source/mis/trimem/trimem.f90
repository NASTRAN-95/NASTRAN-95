!*==trimem.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trimem(Ntype,Tbar,Pg)
   IMPLICIT NONE
   USE C_CONDAS
   USE C_MATIN
   USE C_MATOUT
   USE C_SSGWRK
   USE C_TRIMEX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ntype
   REAL :: Tbar
   REAL , DIMENSION(1) :: Pg
!
! Local variable declarations rewritten by SPAG
!
   REAL :: degra , delta , flamda , reelmu , temp , theta , vol , xsubb , xsubc , ysubc
   REAL , DIMENSION(21) :: ecpt
   INTEGER :: i , k , l
   EXTERNAL basglb , mat , mesage , mpyl , mpylt
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
   !>>>>EQUIVALENCE (Consts(4),Degra)
   !>>>>EQUIVALENCE (Ecpt(1),Necpt(1))
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
   Eltemp = ecpt(21)
!
!     SET UP THE E MATRIX WHICH IS (3X2) FOR THE TRI-MEMBRANE
!
!     E(1), E(3), E(5) WILL BE THE I-VECTOR
!     E(2), E(4), E(6) WILL BE THE J-VECTOR
!     E(7), E(8), E(9) WILL BE THE K-VECTOR NOT USED IN E FOR MEMBRANE
!
!     FIRST FIND I-VECTOR = RSUBB - RSUBA  (NON-NORMALIZED)
   E(1) = X2 - X1
   E(3) = Y2 - Y1
   E(5) = Z2 - Z1
!
!     NOW FIND LENGTH = X-SUB-B   COORD. IN ELEMENT SYSTEM
   xsubb = sqrt(E(1)**2+E(3)**2+E(5)**2)
   IF ( xsubb<=1.0E-06 ) CALL mesage(-30,31,ecpt(1))
!
!  20 NOW NORMALIZE I-VECTOR WITH X-SUB-B
   E(1) = E(1)/xsubb
   E(3) = E(3)/xsubb
   E(5) = E(5)/xsubb
!
!     HERE WE NOW TAKE RSUBC - RSUBA AND STORE TEMPORARILY IN
!     E(2), E(4), E(6) WHICH IS WHERE THE J-VECTOR WILL FIT LATER
!
   E(2) = X3 - X1
   E(4) = Y3 - Y1
   E(6) = Z3 - Z1
!
!     X-SUB-C  =  I . (RSUBC - RSUBA) ,  THUS
   xsubc = E(1)*E(2) + E(3)*E(4) + E(5)*E(6)
!
!     AND CROSSING THE I-VECTOR TO (RSUBC-RSUBA) GIVES THE K-VECTOR
!     (NON-NORMALIZED)
!
   E(7) = E(3)*E(6) - E(5)*E(4)
   E(8) = E(5)*E(2) - E(1)*E(6)
   E(9) = E(1)*E(4) - E(3)*E(2)
!
!
!     THE LENGTH OF THE K-VECTOR IS NOW FOUND AND EQUALS Y-SUB-C
!     COORD. IN ELEMENT SYSTEM
   ysubc = sqrt(E(7)**2+E(8)**2+E(9)**2)
   IF ( ysubc<=1.0E-06 ) CALL mesage(-30,32,ecpt(1))
!
!  25 NOW NORMALIZE K-VECTOR WITH YSUBC JUST FOUND
!
   E(7) = E(7)/ysubc
   E(8) = E(8)/ysubc
   E(9) = E(9)/ysubc
!
!     NOW HAVING I AND K VECTORS.GET J = I CROSS K AND
!     STORE IN THE SPOT FOR J
!
   E(2) = E(5)*E(8) - E(3)*E(9)
   E(4) = E(1)*E(9) - E(5)*E(7)
   E(6) = E(3)*E(7) - E(1)*E(8)
!
!     AND JUST FOR COMPUTER EXACTNESS NORMALIZE J-VECTOR TO MAKE SURE.
   temp = sqrt(E(2)**2+E(4)**2+E(6)**2)
   E(2) = E(2)/temp
   E(4) = E(4)/temp
   E(6) = E(6)/temp
!
!     VOLUME OF ELEMENT, THETA, MU, LAMDA, AND DELTA
   vol = xsubb*ysubc*T/2.0
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
   C(1) = -reelmu
   C(2) = 0.0E0
   C(3) = 0.0E0
   C(4) = flamda*delta
   C(5) = C(4)
   C(6) = -reelmu
   C(7) = reelmu
   C(8) = 0.0E0
   C(9) = 0.0E0
   C(10) = -flamda*reelmu*xsubc
   C(11) = C(10)
   C(12) = reelmu
   C(13) = 0.0E0
   C(14) = 0.0E0
   C(15) = 0.0E0
   C(16) = flamda
   C(17) = flamda
   C(18) = 0.0E0
!
   IF ( Ntype/=1 ) THEN
      theta = Angle*degra
      Sinth = sin(theta)
      Costh = cos(theta)
   ENDIF
   IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0E0
   Eltemp = ecpt(21)
   Matid = Matid1
   Inflag = 2
   CALL mat(ecpt(1))
!
!     FILL G-MATRIX WITH OUTPUT FROM MAT ROUTINE
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
!     ******************************************************************
!
!     G, E, AND C MATRICES ARE COMPLETE
!
!
!
   temp = (Tbar-Tsub0)*vol
   DO i = 1 , 3
      Tempar(i) = Alphas(i)*temp
   ENDDO
   CALL mpyl(G(1),Tempar(1),3,3,1,Tempar(7))
   DO i = 1 , 3
      k = 6*i - 5
      CALL mpylt(C(k),Tempar(7),3,2,1,Tempar(1))
      CALL mpyl(E,Tempar(1),2,3,1,Tempar(4))
      k = 4*i + 5
      IF ( Necpt(k)/=0 ) CALL basglb(Tempar(4),Tempar(4),Necpt(k+1),Necpt(k))
      DO k = 1 , 3
         l = Necpt(i+1) + k - 1
         Pg(l) = Pg(l) + Tempar(k+3)
      ENDDO
   ENDDO
!
!     THIS CONCLUDES PHASE 1 FOR TRIANGULAR MEMBRANE OR SUB CALCULATION
!     TO ANOTHER ROUTINE...
END SUBROUTINE trimem
