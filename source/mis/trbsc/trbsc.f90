!*==trbsc.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trbsc(Iopt,Ti)
   IMPLICIT NONE
   USE C_CONDAS
   USE C_MATIN
   USE C_MATOUT
   USE C_SSGTRI
   USE C_SSGWRK
   USE C_TRIMEX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iopt
   REAL , DIMENSION(6) :: Ti
!
! Local variable declarations rewritten by SPAG
!
   REAL :: degra
   REAL , DIMENSION(25) :: ecpt
   REAL , DIMENSION(9) :: g , t
   REAL , DIMENSION(4) :: g2x2 , j2x2
   REAL , DIMENSION(18) :: hib , hic , s , tite
   REAL , DIMENSION(36) :: hinv
   INTEGER :: i , j , k
   EXTERNAL gbtran , gmmats , invers , mat , mesage , ssgkhi
!
! End of declarations rewritten by SPAG
!
!
!     ELEMENT THERMAL LOADING ROUTINE FOR THE BASIC BENDING TRIANGLE.
!
!       IOPT = 0    (BASIC BENDING TRIANGLE)
!       IOPT = 1    (SUB-CALCULATIONS FOR SQDPL1)
!       IOPT = 2    (SUB-CALCULATIONS FOR STRPL1)
!
!
!     ECPT LIST FOR BASIC BENDING TRIANGLE           NAME IN
!                                                    THIS
!     ECPT                                           ROUTINE    TYPE
!     --------   ---------------------------------   --------  -------
!     ECPT( 1) = ELEMENT ID                          NECPT(1)  INTEGER
!     ECPT( 2) = GRID POINT A                        NGRID(1)  INTEGER
!     ECPT( 3) = GRID POINT B                        NGRID(2)  INTEGER
!     ECPT( 4) = GRID POINT C                        NGRID(3)  INTEGER
!     ECPT( 5) = THETA = ANGLE OF MATERIAL           ANGLE     REAL
!     ECPT( 6) = MATERIAL ID 1                       MATID1    INTEGER
!     ECPT( 7) = I = MOMENT OF INERTIA               EYE       REAL
!     ECPT( 8) = MATERIAL ID 2                       MATID2    INTEGER
!     ECPT( 9) = T2                                  T2        REAL
!     ECPT(10) = NON-STRUCTURAL-MASS                 FMU       REAL
!     ECPT(11) = Z1                                  Z11       REAL
!     ECPT(12) = Z2                                  Z22       REAL
!     ECPT(13) = COORD. SYSTEM ID 1                  NECPT(13) INTEGER
!     ECPT(14) = X1                                  X1        REAL
!     ECPT(15) = Y1                                  Y1        REAL
!     ECPT(16) = Z1                                  Z1        REAL
!     ECPT(17) = COORD. SYSTEM ID 2                  NECPT(17) INTEGER
!     ECPT(18) = X2                                  X2        REAL
!     ECPT(19) = Y2                                  Y2        REAL
!     ECPT(20) = Z2                                  Z2        REAL
!     ECPT(21) = COORD. SYSTEM ID 3                  NECPT(21) INTEGER
!     ECPT(22) = X3                                  X3        REAL
!     ECPT(23) = Y3                                  Y3        REAL
!     ECPT(24) = Z3                                  Z3        REAL
!     ECPT(25) = ELEMENT TEMPERATURE                 ELTEMP    REAL
!
   !>>>>EQUIVALENCE (Consts(4),Degra) , (G(1),A(79)) , (Ecpt(1),Necpt(1)) , (G2x2(1),A(88)) , (S(1),A(55)) , (Tite(1),A(127)) ,          &
!>>>>    & (J2x2(1),A(92)) , (T(1),A(118)) , (Hib(1),A(109)) , (Hic(1),A(127)) , (Hinv(1),A(73))
!
   IF ( Iopt<=0 ) THEN
      Eltemp = ecpt(25)
!
!     SET UP  I, J, K VECTORS STORING AS FOLLOWS AND ALSO CALCULATE
!     X-SUB-B, X-SUB-C, AND Y-SUB-C.
!
!     E(11), E(14), E(17) WILL BE THE I-VECTOR.
!     E(12), E(15), E(18) WILL BE THE J-VECTOR.
!     E( 1), E( 4), E( 7) WILL BE THE K-VECTOR.
!
!     FIND I-VECTOR = RSUBB - RUBA (NON-NORMALIZED)
      E(11) = X2 - X1
      E(14) = Y2 - Y1
      E(17) = Z2 - Z1
!
!     FIND LENGTH = X-SUB-B COOR. IN ELEMENT SYSTEM
!
      Xsubb = sqrt(E(11)**2+E(14)**2+E(17)**2)
      IF ( Xsubb<=1.0E-06 ) CALL mesage(-30,37,ecpt(1))
!
!     NORMALIZE I-VECTOR WITH X-SUB-B
!
      E(11) = E(11)/Xsubb
      E(14) = E(14)/Xsubb
      E(17) = E(17)/Xsubb
!
!     TAKE RSUBC - RSUBA AND STORE TEMPORARILY IN E(2), E(5), E(8)
!
      E(2) = X3 - X1
      E(5) = Y3 - Y1
      E(8) = Z3 - Z1
!
!     X-SUB-C = I . (RSUBC - RSUBA), THUS
!
      Xsubc = E(11)*E(2) + E(14)*E(5) + E(17)*E(8)
!
!     CROSSING I-VECTOR TO (RSUBC - RSUBA) GIVES THE K-VECTOR
!     (NON-NORMALIZED)
!
      E(1) = E(14)*E(8) - E(5)*E(17)
      E(4) = E(2)*E(17) - E(11)*E(8)
      E(7) = E(11)*E(5) - E(2)*E(14)
!
!     FIND LENGTH = Y-SUB-C COOR. IN ELEMENT SYSTEM
!
      Ysubc = sqrt(E(1)**2+E(4)**2+E(7)**2)
      IF ( Ysubc<=1.0E-06 ) CALL mesage(-30,37,ecpt(1))
!
!     NORMALIZE K-VECTOR WITH Y-SUB-C
!
      E(1) = E(1)/Ysubc
      E(4) = E(4)/Ysubc
      E(7) = E(7)/Ysubc
!
!     NOW HAVING I AND K VECTORS GET -- J = K CROSS I
!
      E(12) = E(4)*E(17) - E(14)*E(7)
      E(15) = E(11)*E(7) - E(1)*E(17)
      E(18) = E(1)*E(14) - E(11)*E(4)
!
!     NORMALIZE J-VECTOR FOR COMPUTER EXACTNESS JUST TO MAKE SURE
!
      Temp = sqrt(E(12)**2+E(15)**2+E(18)**2)
      E(12) = E(12)/Temp
      E(15) = E(15)/Temp
      E(18) = E(18)/Temp
      E(2) = 0.0
      E(3) = 0.0
      E(5) = 0.0
      E(6) = 0.0
      E(8) = 0.0
      E(9) = 0.0
      E(10) = 0.0
      E(13) = 0.0
      E(16) = 0.0
!
!     CONVERT ANGLE FROM DEGREES TO RADIANS STORING IN THETA.
!
      Theta = Angle*degra
      Sinth = sin(Theta)
      Costh = cos(Theta)
      IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0
   ENDIF
!
!     SETTING UP G MATRIX
!
   Matid = Matid1
   Inflag = 2
   CALL mat(ecpt(1))
!
!     FILL G-MATRIX WITH OUTPUT FROM MAT ROUTINE
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
!     COMPUTATION OF D = I.G-MATRIX (EYE IS INPUT FROM THE ECPT)
!
   DO i = 1 , 9
      D(i) = g(i)*Eye
   ENDDO
!
   Xbar = (Xsubb+Xsubc)/3.0
   Ybar = Ysubc/3.0
   Xc = Xbar
   Yc = Ybar
!
!     FORMING K  5X6
!              S
!
   Xc3 = 3.0*Xc
   Yc3 = 3.0*Yc
   Yc2 = 2.0*Yc
   Ks(1) = D(1)
   Ks(2) = D(3)
   Ks(3) = D(2)
   Ks(4) = D(1)*Xc3
   Ks(5) = D(2)*Xc + D(3)*Yc2
   Ks(6) = D(2)*Yc3
   Ks(7) = D(2)
   Ks(8) = D(6)
   Ks(9) = D(5)
   Ks(10) = D(2)*Xc3
   Ks(11) = D(5)*Xc + D(6)*Yc2
   Ks(12) = D(5)*Yc3
   Ks(13) = D(3)
   Ks(14) = D(9)
   Ks(15) = D(6)
   Ks(16) = D(3)*Xc3
   Ks(17) = D(6)*Xc + D(9)*Yc2
   Ks(18) = D(6)*Yc3
!
!     ROWS 4 AND 5
!
   Ks(19) = 0.0
   Ks(20) = 0.0
   Ks(21) = 0.0
   Ks(22) = -D(1)*6.0
   Ks(23) = -D(2)*2.0 - D(9)*4.0
   Ks(24) = -D(6)*6.0
   Ks(25) = 0.0
   Ks(26) = 0.0
   Ks(27) = 0.0
   Ks(28) = -D(3)*6.0
   Ks(29) = -D(6)*6.0
   Ks(30) = -D(5)*6.0
!
!     MULTIPLY FIRST 3 ROWS BY 2.0
!
   DO i = 1 , 18
      Ks(i) = Ks(i)*2.0
   ENDDO
!
!     MULTIPLY KS BY THE AREA
!
   Area = Xsubb*Ysubc/2.0
   DO i = 1 , 30
      Ks(i) = Ks(i)*Area
   ENDDO
!
   Xcsq = Xsubc**2
   Ycsq = Ysubc**2
   Xbsq = Xsubb**2
   Xcyc = Xsubc*Ysubc
!
!     F1LL  (HBAR) MATRIX STORING AT A(37) THRU A(72)
!
   DO i = 37 , 72
      A(i) = 0.0
   ENDDO
!
   A(37) = Xbsq
   A(40) = Xbsq*Xsubb
   A(44) = Xsubb
   A(49) = -2.0*Xsubb
   A(52) = -3.0*Xbsq
   A(55) = Xcsq
   A(56) = Xcyc
   A(57) = Ycsq
   A(58) = Xcsq*Xsubc
   A(59) = Ycsq*Xsubc
   A(60) = Ycsq*Ysubc
   A(62) = Xsubc
   A(63) = Ysubc*2.0
   A(65) = Xcyc*2.0
   A(66) = Ycsq*3.0
   A(67) = -2.0*Xsubc
   A(68) = -Ysubc
   A(70) = -3.0*Xcsq
   A(71) = -Ycsq
!
   IF ( T2/=0.0 ) THEN
!
!     ALL OF THE FOLLOWING OPERATIONS THROUGH STATEMENT LABEL 500
!     ARE NECESSARY IF T2 IS NON-ZERO.
!
!     GET THE G2X2 MATRIX
!
      Matid = Matid2
      Inflag = 3
      CALL mat(ecpt(1))
      IF ( G2x211/=0.0 .OR. G2x212/=0.0 .OR. G2x222/=0.0 ) THEN
         g2x2(1) = G2x211*T2
         g2x2(2) = G2x212*T2
         g2x2(3) = G2x212*T2
         g2x2(4) = G2x222*T2
!
         Determ = g2x2(1)*g2x2(4) - g2x2(3)*g2x2(2)
         j2x2(1) = g2x2(4)/Determ
         j2x2(2) = -g2x2(2)/Determ
         j2x2(3) = -g2x2(3)/Determ
         j2x2(4) = g2x2(1)/Determ
!
!     (H  ) IS PARTITIONED INTO A LEFT AND RIGHT PORTION AND ONLY THE
!       YQ  RIGHT PORTION IS COMPUTED AND USED AS A  (2X3). THE LEFT
!           2X3 PORTION IS NULL.  THE RIGHT PORTION WILL BE STORED AT
!           A(73) THRU A(78) UNTIL NOT NEEDED ANY FURTHER.
!
         Temp = 2.0*D(2) + 4.0*D(9)
         A(73) = -6.0*(j2x2(1)*D(1)+j2x2(2)*D(3))
         A(74) = -j2x2(1)*Temp + 6.0*j2x2(2)*D(6)
         A(75) = -6.0*(j2x2(1)*D(6)+j2x2(2)*D(5))
         A(76) = -6.0*(j2x2(2)*D(1)+j2x2(4)*D(3))
         A(77) = -j2x2(2)*Temp + 6.0*j2x2(4)*D(6)
         A(78) = -6.0*(j2x2(2)*D(6)+j2x2(4)*D(5))
!
!     THE ABOVE 6 ELEMENTS NOW REPRESENT THE (H  ) MATRIX (2X3)
!                                              YQ
!
!     ADD TO 6 OF THE (HBAR) ELEMENTS THE RESULT OF(H  )(H  )
!                                                    UY   YQ
!     THE PRODUCT IS FORMED DIRECTLY IN THE ADDITION PROCESS BELOW.
!     NO (H  ) MATRIX IS ACTUALLY COMPUTED DIRECTLY.
!          UY
!
!     THE FOLLOWING IS THEN PER STEPS 6 AND 7 PAGE -16- MS-17.
!
         DO i = 1 , 3
            A(i+39) = A(i+39) + Xsubb*A(i+72)
            A(i+57) = A(i+57) + Xsubc*A(i+72) + Ysubc*A(i+75)
         ENDDO
      ENDIF
   ENDIF
!
!     THIS ENDS ADDED COMPUTATION FOR CASE OF T2 NOT ZERO
!
!
!     AT THIS POINT INVERT  (H) WHICH IS STORED AT A(37) THRU A(72)
!     STORE INVERSE BACK IN A(37) THRU A(72)
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
   Ising = -1
   CALL invers(6,A(37),6,A(73),0,Determ,Ising,A(79))
!
!     CHECK TO SEE IF H WAS SINGULAR
!
!
!     ISING = 2 IMPLIES SINGULAR MATRIX THUS ERROR CONDITION.
!
   IF ( Ising==2 ) CALL mesage(-30,38,ecpt(1))
!
!     SAVE H-INVERSE IF TRI-PLATE IS CALLING
!
   DO i = 1 , 36
      hinv(i) = A(i+36)
   ENDDO
!
!     FILL  S-MATRIX, EQUIVALENCED TO A(55).  (6X3)
!
   s(1) = 1.0
   s(2) = 0.0
   s(3) = -Xsubb
   s(4) = 0.0
   s(5) = 1.0
   s(6) = 0.0
   s(7) = 0.0
   s(8) = 0.0
   s(9) = 1.0
   s(10) = 1.0
   s(11) = Ysubc
   s(12) = -Xsubc
   s(13) = 0.0
   s(14) = 1.0
   s(15) = 0.0
   s(16) = 0.0
   s(17) = 0.0
   s(18) = 1.0
!
!     COMPUTE  S , S ,  AND S    NO TRANSFORMATIONS
!               A   B        C
!
!                -1
!     S  = - K  H  S ,   S  = K  H   ,   S  = K  H
!      A      S           B    S  IB      C    S  IC
!
!     S   COMPUTATION.
!      A
!
   CALL gmmats(hinv(1),6,6,0,s(1),6,3,0,A(16))
!
!     DIVIDE  H-INVERSE INTO A LEFT 6X3 AND RIGHT 6X3 PARTITION.
!
   i = 0
   j = -6
   SPAG_Loop_1_1: DO
      j = j + 6
      k = 0
      SPAG_Loop_2_2: DO
         k = k + 1
         i = i + 1
         Isub = j + k
         hib(i) = hinv(Isub)
         hic(i) = hinv(Isub+3)
         IF ( k>=3 ) THEN
            IF ( j<30 ) CYCLE SPAG_Loop_1_1
!
            CALL gmmats(Ks(1),5,6,0,A(16),6,3,0,A(1))
!
!     MULTIPLY S SUB A BY (-1)
!
            DO i = 1 , 15
               A(i) = -A(i)
            ENDDO
!
!     S  COMPUTATION
!      B
!
            CALL gmmats(Ks,5,6,0,hib,6,3,0,A(16))
!
!     S  COMPUTATION
!      C
!
            CALL gmmats(Ks,5,6,0,hic,6,3,0,A(31))
!
!     RETURN IF TRI OR QUAD PLATE ROUTINE IS CALLING.
!
            IF ( Iopt>0 ) RETURN
!
!     FILL KHI   (5 X 1)
!             E
!
!     THE N FACTOR = 1.0 FOR THE BASIC BENDING TRIANGLE.
!
            CALL ssgkhi(Ti(1),Ti(1),1.0)
!
!                                   T
!     TRANSFORM  S , S , S  WITH   E  T  , I = A,B,C
!                 A   B   C            I
!
!                              T         T
!     COMPUTING TRANSPOSE OF  E  T  =  T  E
!                                 I     I
!
            DO i = 1 , 3
!
!     POINTER TO S MATRIX = 15 * I - 14
!                 I
!
!     CHECK TO SEE IF T IS NEEDED.
!
               IF ( Necpt(4*i+9)/=0 ) THEN
                  CALL gbtran(Necpt(4*i+9),Necpt(4*i+10),t(1))
                  CALL gmmats(t,3,3,1,E(1),3,3,0,tite(1))
                  CALL gmmats(t,3,3,1,E(10),3,3,0,tite(10))
                  CALL gmmats(A(15*i-14),5,3,0,tite,6,3,1,Ks(1))
               ELSE
                  CALL gmmats(A(15*i-14),5,3,0,E,6,3,1,Ks(1))
               ENDIF
!
!     COMPUTE THE LOAD VECTOR AND INSERT IT INTO OPEN CORE
!
               CALL gmmats(Ks(1),5,6,1,Khi(1),5,1,0,P(1))
               k = Ngrid(i) - 1
               DO j = 1 , 6
                  k = k + 1
                  Z(k) = Z(k) + P(j)
               ENDDO
            ENDDO
            EXIT SPAG_Loop_2_2
         ENDIF
      ENDDO SPAG_Loop_2_2
      EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
END SUBROUTINE trbsc
