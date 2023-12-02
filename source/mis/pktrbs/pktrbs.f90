!*==pktrbs.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pktrbs(Iopt)
USE C_MATIN
USE C_MATOUT
USE C_PLA42C
USE C_PLA42D
USE C_PLA4ES
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iopt
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(9) :: d , g
   REAL , DIMENSION(1) :: ecpt
   REAL(REAL64) , DIMENSION(4) :: g2x2 , j2x2
   INTEGER :: i
   REAL(REAL64) , DIMENSION(18) :: s
   EXTERNAL gmmatd , inverd , mat , mesage , plamat
!
! End of declarations rewritten by SPAG
!
!
!     BASIC BENDING TRIANGLE ELEMENT ROUTINE
!
!     THIS ROUTINE DOES SUB-CALCULATIONS FOR TRI OR QUAD PLATES IN PLA4
!
!     CALLS FROM THIS ROUTINE ARE MADE TO
!          MAT    - MATERIAL DATA ROUTINE
!
!     PLAMAT - ROTATES AND RETURNS GP MATRIX
!          TRANSD - DOUBLE PRECISION TRANSFORMATION SUPPLIER
!          INVERD - DOUBLE PRECISION INVERSE ROUTINE
!          GMMATD - DOUBLE PRECISION MATRIX MULTIPLY AND TRANSPOSE
!          MESAGE - ERROR MESSAGE WRITER
!
!     IOPT = 1   IMPLIES COMPUTE ONLY THE NINE (3X3)MATRICES
!                WHICH FORM THE 9X9 K SUPER U - MATRIX.
!     IOPT = 2   SAME AS IOPT = 1,BUT SAVE H-INVERSE AND S
!
!     ECPT LIST FOR BASIC BENDING TRIANGLE          NAME IN
!                                                   THIS
!     ECPT                                          ROUTINE   TYPE
!     ------------------------------------        -------------------
!     ECPT( 1) = ELEMENT ID                         NECPT(1)  INTEGER
!     ECPT( 2) = GRID POINT A                       NGRID(1)  INTEGER
!     ECPT( 3) = GRID POINT B                       NGRID(2)  INTEGER
!     ECPT( 4) = GRID POINT C                       NGRID(3)  INTEGER
!     ECPT( 5) = THETA = ANGLE OF MATERIAL          ANGLE     REAL
!     ECPT( 6) = MATERIAL ID 1                      MATID1    INTEGER
!     ECPT( 7) = I = MOMENT OF INERTIA              EYE       REAL
!     ECPT( 8) = MATERIAL ID 2                      MATID2    INTEGER
!     ECPT( 9) = T2                                 T2        REAL
!     ECPT(10) = NON-STRUCTURAL-MASS                FMU       REAL
!     ECPT(11) = Z1                                 Z11       REAL
!     ECPT(12) = Z2                                 Z22       REAL
!     ECPT(13) = COORD. SYSTEM ID 1                 NECPT(13) INTEGER
!     ECPT(14) = X1                                 X1        REAL
!     ECPT(15) = Y1                                 Y1        REAL
!     ECPT(16) = Z1                                 Z1        REAL
!     ECPT(17) = COORD. SYSTEM ID 2                 NECPT(17) INTEGER
!     ECPT(18) = X2                                 X2        REAL
!     ECPT(19) = Y2                                 Y2        REAL
!     ECPT(20) = Z2                                 Z2        REAL
!     ECPT(21) = COORD. SYSTEM ID 3                 NECPT(21) INTEGER
!     ECPT(22) = X3                                 X3        REAL
!     ECPT(23) = Y3                                 Y3        REAL
!     ECPT(24) = Z3                                 Z3        REAL
!     ECPT(25) = ELEMENT TEMPERATURE                ELTEMP    REAL
!
!
!     ECPT BLOCK
   !>>>>EQUIVALENCE (D(1),G(1),A(79)) , (Ecpt(1),Necpt(1)) , (G2x2(1),A(88)) , (Tjte(1),A(100)) , (Tite(1),S(1),A(82)) , (J2x2(1),A(92)) &
!>>>>    & , (Ti(1),A(118))
!
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
      d(i) = g(i)*dble(Eye)
   ENDDO
!
   Area = Xsubb*Ysubc/2.0D0
   Xbar = (Xsubb+Xsubc)/3.0D0
   Ybar = Ysubc/3.0D0
!
   Xcsq = Xsubc**2
   Ycsq = Ysubc**2
   Xbsq = Xsubb**2
   Xcyc = Xsubc*Ysubc
   Px2 = (Xbsq+Xsubb*Xsubc+Xcsq)/6.0D0
   Py2 = Ycsq/6.0D0
   Pxy2 = Ysubc*(Xsubb+2.0D0*Xsubc)/12.0D0
   Xbar3 = 3.0D0*Xbar
   Ybar3 = 3.0D0*Ybar
   Ybar2 = 2.0D0*Ybar
!
!                 X
!     FILL THE  (K ) MATRIX STORING IN  A(1) THRU A(36)
!
   A(1) = d(1)
   A(2) = d(3)
   A(3) = d(2)
   A(4) = d(1)*Xbar3
   A(5) = d(2)*Xbar + Ybar2*d(3)
   A(6) = d(2)*Ybar3
   A(7) = A(2)
   A(8) = d(9)
   A(9) = d(6)
   A(10) = d(3)*Xbar3
   A(11) = d(6)*Xbar + Ybar2*d(9)
   A(12) = d(6)*Ybar3
   A(13) = A(3)
   A(14) = A(9)
   A(15) = d(5)
   A(16) = d(2)*Xbar3
   A(17) = d(5)*Xbar + Ybar2*d(6)
   A(18) = d(5)*Ybar3
   A(19) = A(4)
   A(20) = A(10)
   A(21) = A(16)
   A(22) = d(1)*9.0D0*Px2
   A(23) = d(2)*3.0D0*Px2 + 6.0D0*Pxy2*d(3)
   A(24) = d(2)*9.0D0*Pxy2
   A(25) = A(5)
   A(26) = A(11)
   A(27) = A(17)
   A(28) = A(23)
   A(29) = d(5)*Px2 + 4.0D0*Pxy2*d(6) + 4.0D0*Py2*d(9)
   A(30) = d(5)*3.0D0*Pxy2 + 6.0D0*Py2*d(6)
   A(31) = A(6)
   A(32) = A(12)
   A(33) = A(18)
   A(34) = A(24)
   A(35) = A(30)
   A(36) = d(5)*9.0D0*Py2
   Temp = 4.0D0*Area
   DO i = 1 , 36
      A(i) = A(i)*Temp
   ENDDO
!
!     F1LL  (HBAR) MATRIX STORING AT A(37) THRU A(72)
!
   DO i = 37 , 72
      A(i) = 0.0D0
   ENDDO
!
   A(37) = Xbsq
   A(40) = Xbsq*Xsubb
   A(44) = Xsubb
   A(49) = -2.0D0*Xsubb
   A(52) = -3.0D0*Xbsq
   A(55) = Xcsq
   A(56) = Xcyc
   A(57) = Ycsq
   A(58) = Xcsq*Xsubc
   A(59) = Ycsq*Xsubc
   A(60) = Ycsq*Ysubc
   A(62) = Xsubc
   A(63) = Ysubc*2.0D0
   A(65) = Xcyc*2.0D0
   A(66) = Ycsq*3.0D0
   A(67) = -2.0D0*Xsubc
   A(68) = -Ysubc
   A(70) = -3.0D0*Xcsq
   A(71) = -Ycsq
!
   IF ( T2/=0.0E0 ) THEN
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
!           A(73)...A(78) UNTIL NOT NEEDED ANY FURTHER.
!
         Temp = 2.0D0*d(2) + 4.0D0*d(9)
         A(73) = -6.0D0*(j2x2(1)*d(1)+j2x2(2)*d(3))
         A(74) = -j2x2(1)*Temp + 6.0D0*j2x2(2)*d(6)
         A(75) = -6.0D0*(j2x2(1)*d(6)+j2x2(2)*d(5))
         A(76) = -6.0D0*(j2x2(2)*d(1)+j2x2(4)*d(3))
         A(77) = -j2x2(2)*Temp + 6.0D0*j2x2(4)*d(6)
         A(78) = -6.0D0*(j2x2(2)*d(6)+j2x2(4)*d(5))
!
!     THE ABOVE 6 ELEMENTS NOW REPRESENT THE (H  ) MATRIX (2X3)
!                                              YQ
!
!     NOW FORMING  PRODUCT (G2X2)(H  ) AND STORING AS AN INTERMEDIATE
!     STEP.                        YQ
!
         CALL gmmatd(g2x2(1),2,2,0,A(73),2,3,0,A(79))
!
!                                                               Y
!     WITH LAST PRODUCT  FORM  LOWER RIGHT 3 X 3 PARTITION OF (K )
!
!              Y                   T
!     THUS   (K ) PARTITION = (H  ) (LAST PRODUCT)   STORE AT A(85)
!                               YQ
!
         CALL gmmatd(A(73),2,3,1,A(79),2,3,0,A(85))
!
!                                                     X
!     NOW ADD THE 9 ELEMENTS OF THIS 3X3 PORTION TO (K )
!     PER STEP 5 PAGE -16- MS-17                            Y
!     MULTIPLY IN AREA AT SAME TIME WHICH WAS LEFT OUT OF (K ) ABOVE.
!
         DO i = 1 , 3
            A(i+21) = A(i+21) + A(i+84)*Area
            A(i+27) = A(i+27) + A(i+87)*Area
            A(i+33) = A(i+33) + A(i+90)*Area
         ENDDO
!
!     ADD TO 6 OF THE (HBAR) ELEMENTS THE RESULT OF (H  )(H  )
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
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
   Ising = -1
   CALL inverd(6,A(37),6,A(73),0,Determ,Ising,A(79))
!
!     CHECK TO SEE IF H WAS SINGULAR
!
   IF ( Ising/=2 ) THEN
!
!              Q   -1
! 440 FORM   (K )(H  )  AND STORE AT  A(73) THRU A(108)
!
!                 X                     Q
!     NOTE THAT (K ) AT THIS POINT IS (K )
!
      CALL gmmatd(A(1),6,6,0,A(37),6,6,0,A(73))
!
!                    -1 T
!     FORM(K  ) = (H  ) (LAST PRODUCT) STORE AT A(109) THRU A(144)
!            II
!
      CALL gmmatd(A(37),6,6,1,A(73),6,6,0,A(109))
!
!     FILL S-MATRIX EQUIVALENCED TO A(82)  (S IS 6X3)
!
      IF ( Iopt==2 ) THEN
!
!     SAVE H-INVERSE TO BE USED BY TRIANGULAR PLATE ROUTINE.
!
         DO i = 37 , 72
            A(i+108) = A(i)
         ENDDO
      ENDIF
!
      s(1) = 1.0D0
      s(2) = 0.0D0
      s(3) = -Xsubb
      s(4) = 0.0D0
      s(5) = 1.0D0
      s(6) = 0.0D0
      s(7) = 0.0D0
      s(8) = 0.0D0
      s(9) = 1.0D0
      s(10) = 1.0D0
      s(11) = Ysubc
      s(12) = -Xsubc
      s(13) = 0.0D0
      s(14) = 1.0D0
      s(15) = 0.0D0
      s(16) = 0.0D0
      s(17) = 0.0D0
      s(18) = 1.0D0
!
!                     T
!     FORM   K   = K   = -K   S  STORING AT A(46)   (K   IS 6X3)
!             IA    AI     II                         IA
!
      CALL gmmatd(A(109),6,6,0,s(1),6,3,0,A(46))
!
!     THIS PRODUCT IS MULTIPLIED BY SCALER -1 BELOW.
!
!                    T
!          (K  ) = (S )(-K  )
!            AA           IA
!
!     NOTE K    HAS NOT BEEN MULTIPLIED ABOVE BY -1, THUS IGNORE MINUS
!           IA                                                   HERE.
!
      CALL gmmatd(s(1),6,3,1,A(46),6,3,0,A(1))
!
!     NOW MULTIPLY  K   BY SCALER (-1)
!                    IA
!
      DO i = 46 , 63
         A(i) = -A(i)
      ENDDO
!
!     AT THIS POINT, STORED BY ROWS ARE
!
!        K     (6X6) AT A(109) THRU A(144)
!         II
!
!        K     (6,3) AT  A(46) THRU A(63)
!         IA
!
!        K     (3X3) AT A(1) THRU A(9)
!         AA
!
!     ARRANGE NINE 3X3 MATRICES OF K SUPER U
!
      A(i) = A(i+18)
      A(10) = A(46)
      A(11) = A(49)
      A(12) = A(52)
      A(13) = A(47)
      A(14) = A(50)
      A(15) = A(53)
      A(16) = A(48)
      A(17) = A(51)
      A(18) = A(54)
      A(19) = A(55)
      A(20) = A(58)
      A(21) = A(61)
      A(22) = A(56)
      A(23) = A(59)
      A(24) = A(62)
      A(25) = A(57)
      A(26) = A(60)
      A(27) = A(63)
      A(37) = A(109)
      A(38) = A(110)
      A(39) = A(111)
      A(40) = A(115)
      A(41) = A(116)
      A(42) = A(117)
      A(43) = A(121)
      A(44) = A(122)
      A(45) = A(123)
      A(46) = A(112)
      A(47) = A(113)
      A(48) = A(114)
      A(49) = A(118)
      A(50) = A(119)
      A(51) = A(120)
      A(52) = A(124)
      A(53) = A(125)
      A(54) = A(126)
      A(64) = A(127)
      A(65) = A(128)
      A(66) = A(129)
      A(67) = A(133)
      A(68) = A(134)
      A(69) = A(135)
      A(70) = A(139)
      A(71) = A(140)
      A(72) = A(141)
      A(73) = A(130)
      A(74) = A(131)
      A(75) = A(132)
      A(76) = A(136)
      A(77) = A(137)
      A(78) = A(138)
      A(79) = A(142)
      A(80) = A(143)
      A(81) = A(144)
   ELSE
!
!     ISING = 2 IMPLIES SINGULAR MATRIX THUS ERROR CONDITION.
!
      CALL mesage(30,33,ecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
      Nogo = 1
      RETURN
   ENDIF
END SUBROUTINE pktrbs
