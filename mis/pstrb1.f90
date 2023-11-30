
SUBROUTINE pstrb1(Iopt)
   IMPLICIT NONE
   REAL A(225) , Alp12 , Alpha1 , Alpha2 , Angle , Area , Costh , D(9) , Determ , Dum55(26) , Dumb(76) , Dumdum(4) , Dummy1 ,       &
      & Dummy2 , Dummy3 , E(18) , Ecpt(1) , Eltemp , Eye , Fmu , G(9) , G11 , G12 , G13 , G22 , G23 , G2x2(4) , G2x211 , G2x212 ,   &
      & G2x222 , G33 , Gsube , Hib(18) , Hic(18) , Hinv(36) , J2x2(4) , Ks(30) , Ph1out(200) , Prod9(9) , Px2 , Pxy2 , Py2 , Rho ,  &
      & S(18) , Sigcom , Sigshe , Sigten , Sinth , Stress , T(9) , T2 , Temp , Temp9(9) , Theta , Tite(18) , Tsub0 , X1 , X2 , X3 , &
      & Xbar , Xbar3 , Xbsq , Xc , Xc3 , Xcsq , Xcyc , Xsubb , Xsubc , Y1 , Y2 , Y3 , Ybar , Ybar2 , Ybar3 , Yc , Yc2 , Yc3 , Ycsq ,&
      & Ysubc , Z1
   INTEGER Inflag , Ising , Isub , Matid , Matid1 , Matid2 , Nbegin , Necpt(1) , Nerror , Ngrid(3) , Npivot , Nsized , Nsubc ,      &
         & Ntyped , Subsca , Subscb
   REAL Z11 , Z2 , Z22 , Z3
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alpha1 , Alpha2 , Alp12 , Tsub0 , Gsube , Sigten , Sigcom , Sigshe ,   &
                 & G2x211 , G2x212 , G2x222
   COMMON /pla32s/ A , Xsubb , Xsubc , Ysubc , E , Temp , Xbar , Area , Xcsq , Ybar2 , Ycsq , Ybar , Xbsq , Px2 , Xcyc , Py2 ,      &
                 & Pxy2 , Xbar3 , Ybar3 , Determ , Prod9 , Temp9 , Nsized , Dumdum , Npivot , Theta , Nsubc , Ising , Subsca ,      &
                 & Subscb , Nerror , Nbegin , Ntyped , Xc , Yc , Yc2 , Yc3 , Isub , Xc3 , Dum55
   COMMON /pla3es/ Necpt , Ngrid , Angle , Matid1 , Eye , Matid2 , T2 , Fmu , Z11 , Z22 , Dummy1 , X1 , Y1 , Z1 , Dummy2 , X2 , Y2 ,&
                 & Z2 , Dummy3 , X3 , Y3 , Z3 , Dumb , Ph1out
   INTEGER Iopt
   INTEGER i , j , k
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
   EQUIVALENCE (D(1),G(1),A(79)) , (Ecpt(1),Necpt(1)) , (Ks(1),Ph1out(1)) , (G2x2(1),A(88)) , (S(1),A(55)) , (Tite(1),A(127)) ,     &
    & (J2x2(1),A(92)) , (T(1),A(118)) , (Hib(1),A(109)) , (Hic(1),A(127)) , (Hinv(1),A(73))
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
!     COMPUTATION OF D = I.G-MATRIX (EYE IS INPUT FROM THE ECPT)
!
   DO i = 1 , 9
      D(i) = G(i)*Eye
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
      CALL mat(Ecpt(1))
      IF ( G2x211/=0.0 .OR. G2x212/=0.0 .OR. G2x222/=0.0 ) THEN
         G2x2(1) = G2x211*T2
         G2x2(2) = G2x212*T2
         G2x2(3) = G2x212*T2
         G2x2(4) = G2x222*T2
!
         Determ = G2x2(1)*G2x2(4) - G2x2(3)*G2x2(2)
         J2x2(1) = G2x2(4)/Determ
         J2x2(2) = -G2x2(2)/Determ
         J2x2(3) = -G2x2(3)/Determ
         J2x2(4) = G2x2(1)/Determ
!
!     (H  ) IS PARTITIONED INTO A LEFT AND RIGHT PORTION AND ONLY THE
!       YQ  RIGHT PORTION IS COMPUTED AND USED AS A  (2X3). THE LEFT
!           2X3 PORTION IS NULL.  THE RIGHT PORTION WILL BE STORED AT
!           A(73) THRU A(78) UNTIL NOT NEEDED ANY FURTHER.
!
         Temp = 2.0*D(2) + 4.0*D(9)
         A(73) = -6.0*(J2x2(1)*D(1)+J2x2(2)*D(3))
         A(74) = -J2x2(1)*Temp + 6.0*J2x2(2)*D(6)
         A(75) = -6.0*(J2x2(1)*D(6)+J2x2(2)*D(5))
         A(76) = -6.0*(J2x2(2)*D(1)+J2x2(4)*D(3))
         A(77) = -J2x2(2)*Temp + 6.0*J2x2(4)*D(6)
         A(78) = -6.0*(J2x2(2)*D(6)+J2x2(4)*D(5))
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
   IF ( Ising==2 ) CALL mesage(-30,38,Ecpt(1))
!
!     SAVE H-INVERSE IF TRI-PLATE IS CALLING
!
   DO i = 1 , 36
      Hinv(i) = A(i+36)
   ENDDO
!
!     FILL  S-MATRIX, EQUIVALENCED TO A(55).  (6X3)
!
   S(1) = 1.0
   S(2) = 0.0
   S(3) = -Xsubb
   S(4) = 0.0
   S(5) = 1.0
   S(6) = 0.0
   S(7) = 0.0
   S(8) = 0.0
   S(9) = 1.0
   S(10) = 1.0
   S(11) = Ysubc
   S(12) = -Xsubc
   S(13) = 0.0
   S(14) = 1.0
   S(15) = 0.0
   S(16) = 0.0
   S(17) = 0.0
   S(18) = 1.0
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
   CALL gmmats(Hinv(1),6,6,0,S(1),6,3,0,A(16))
!
!     DIVIDE  H-INVERSE INTO A LEFT 6X3 AND RIGHT 6X3 PARTITION.
!
   i = 0
   j = -6
 100  j = j + 6
   k = 0
   DO
      k = k + 1
      i = i + 1
      Isub = j + k
      Hib(i) = Hinv(Isub)
      Hic(i) = Hinv(Isub+3)
      IF ( k>=3 ) THEN
         IF ( j<30 ) GOTO 100
!
         CALL gmmats(Ks(1),5,6,0,A(16),6,3,0,A(1))
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
         CALL gmmats(Ks,5,6,0,Hib,6,3,0,A(16))
!
!     S  COMPUTATION
!      C
!
         CALL gmmats(Ks,5,6,0,Hic,6,3,0,A(31))
         EXIT
      ENDIF
   ENDDO
!
END SUBROUTINE pstrb1
