!*==pstrb1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pstrb1(Iopt)
   IMPLICIT NONE
   USE C_MATIN
   USE C_MATOUT
   USE C_PLA32S
   USE C_PLA3ES
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iopt
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(9) :: d , g
   REAL , DIMENSION(1) :: ecpt
   REAL , DIMENSION(4) :: g2x2 , j2x2
   REAL , DIMENSION(18) :: hib , hic , s
   REAL , DIMENSION(36) :: hinv
   INTEGER :: i , j , k
   REAL , DIMENSION(30) :: ks
   EXTERNAL gmmats , invers , mat , mesage , plamat
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE DOES SUB-CALCULATIONS FOR PLATE ELEMENTS IN PLA3
!
!     THIS ROUTINE IS SIMILAR TO STRBS1, BUT SINCE THE BASIC BENDING
!     TRIANGLE (IOPT = 0) IS NOT USED IN PLA, THE CORRESPONDING
!     EXECUTIABLE CODE FOR THAT CASE IS NOT USED.
!
!     PHASE ONE FOR STRESS RECOVERY
!
!              IOPT   = 0  (BASIC BENDING TRIANGLE)
!              IOPT   = 1  (SUB-CALCULATIONS FOR SQDPL1)
!              IOPT   = 2  (SUB-CALCULATIONS FOR STRPL1)
!
!     CALLS FROM THIS ROUTINE ARE MADE TO
!
!     PLAMAT - ROTATES AND RETURNS GP MATRIX
!              MAT    - MATERIAL DATA ROUTINE
!              TRANSS - SINGLE PRECISION TRANSFORMATION SUPPLIER
!              INVERS - SINGLE PRECISION INVERSE ROUTINE
!              GMMATS - SINGLE PRECISION MATRIX MULTIPLY AND TRANSPOSE
!              MESAGE - ERROR MESSAGE WRITER
!
   !>>>>EQUIVALENCE (D(1),G(1),A(79)) , (Ecpt(1),Necpt(1)) , (Ks(1),Ph1out(1)) , (G2x2(1),A(88)) , (S(1),A(55)) , (Tite(1),A(127)) ,     &
!>>>>    & (J2x2(1),A(92)) , (T(1),A(118)) , (Hib(1),A(109)) , (Hic(1),A(127)) , (Hinv(1),A(73))
!
!     ECPT LIST FOR BASIC BENDING TRIANGLE           NAME IN
!                                                    THIS
!     ECPT                                           ROUTINE   TYPE
!     ==========================================     ========  =======
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
   Matid = Matid1
   Inflag = -1
!
   CALL plamat
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
      d(i) = g(i)*Eye
   ENDDO
!
   Xbar = (Xsubb+Xsubc)/3.0
   Ybar = Ysubc/3.0
!
!     FORMING K  5X6 AND STORING TEMPORARILY IN PH1OUT OUTPUT SPACE.
!              S                             (EQUIVALENCED)
!
   Xc3 = 3.0*Xc
   Yc3 = 3.0*Yc
   Yc2 = 2.0*Yc
   ks(1) = d(1)
   ks(2) = d(3)
   ks(3) = d(2)
   ks(4) = d(1)*Xc3
   ks(5) = d(2)*Xc + d(3)*Yc2
   ks(6) = d(2)*Yc3
   ks(7) = d(2)
   ks(8) = d(6)
   ks(9) = d(5)
   ks(10) = d(2)*Xc3
   ks(11) = d(5)*Xc + d(6)*Yc2
   ks(12) = d(5)*Yc3
   ks(13) = d(3)
   ks(14) = d(9)
   ks(15) = d(6)
   ks(16) = d(3)*Xc3
   ks(17) = d(6)*Xc + d(9)*Yc2
   ks(18) = d(6)*Yc3
!
!     ROWS 4 AND 5
!
   ks(19) = 0.0
   ks(20) = 0.0
   ks(21) = 0.0
   ks(22) = -d(1)*6.0
   ks(23) = -d(2)*2.0 - d(9)*4.0
   ks(24) = -d(6)*6.0
   ks(25) = 0.0
   ks(26) = 0.0
   ks(27) = 0.0
   ks(28) = -d(3)*6.0
   ks(29) = -d(6)*6.0
   ks(30) = -d(5)*6.0
!
!     MULTIPLY FIRST 3 ROWS BY 2.0
!
   DO i = 1 , 18
      ks(i) = ks(i)*2.0
   ENDDO
!
   Xcsq = Xsubc**2
   Ycsq = Ysubc**2
   Xbsq = Xsubb**2
   Xcyc = Xsubc*Ysubc
!
!     F1LL (HBAR) MATRIX STORING AT A(37) THRU A(72)
!
   DO i = 37 , 72
      A(i) = 0.0
   ENDDO
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
!     ALL OF THE FOLLOWING OPERATIONS THROUGH STATEMENT LABEL 100
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
         Temp = 2.0*d(2) + 4.0*d(9)
         A(73) = -6.0*(j2x2(1)*d(1)+j2x2(2)*d(3))
         A(74) = -j2x2(1)*Temp + 6.0*j2x2(2)*d(6)
         A(75) = -6.0*(j2x2(1)*d(6)+j2x2(2)*d(5))
         A(76) = -6.0*(j2x2(2)*d(1)+j2x2(4)*d(3))
         A(77) = -j2x2(2)*Temp + 6.0*j2x2(4)*d(6)
         A(78) = -6.0*(j2x2(2)*d(6)+j2x2(4)*d(5))
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
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUNTLY.
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
            CALL gmmats(ks(1),5,6,0,A(16),6,3,0,A(1))
!
!     MULTIPLY S SUB A BY -1
!
            DO i = 1 , 15
               A(i) = -A(i)
            ENDDO
!
!     S  COMPUTATION
!      B
!
            CALL gmmats(ks,5,6,0,hib,6,3,0,A(16))
!
!     S  COMPUTATION
!      C
!
            CALL gmmats(ks,5,6,0,hic,6,3,0,A(31))
            EXIT SPAG_Loop_2_2
         ENDIF
      ENDDO SPAG_Loop_2_2
      EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
!
END SUBROUTINE pstrb1
