!*==etrbkd.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE etrbkd(Iopt)
   IMPLICIT NONE
   USE C_CONDAD
   USE C_EMGEST
   USE C_EMGPRM
   USE C_EMGTRX
   USE C_MATIN
   USE C_MATOUT
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iopt
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 :: area , degra , determ , px2 , pxy2 , py2 , temp , theta , xbar , xbar3 , xbsq , xcsq , xcyc , ybar , ybar2 , ybar3 ,   &
           & ycsq
   REAL*8 , DIMENSION(9) :: d , g , ti
   REAL , DIMENSION(25) :: ecpt
   REAL*8 , DIMENSION(4) :: g2x2 , j2x2
   INTEGER :: i , ia , ii , iout , ising , j , jj , loopnd , npivot , npt1 , npt2 , ntype
   INTEGER , DIMENSION(26) :: necpt
   INTEGER , DIMENSION(2) , SAVE :: no
   REAL*8 , DIMENSION(18) :: s , tite , tjte
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE CALCULATES THE STIFFNESS MATRIX FOR THE BASIC
!     BENDING TRIANGLE.  IT IS USED BY SUBROUTINES TRBSCD,QDPLTD,
!     TRPLTD, QUAD1D, TRIA1D, TRIA2D
!     DOUBLE PRECISION VERSION
!     IOPT MAY BE VARIED AS FOLLOWS TO PRODUCE APPROPRIATE RESULTS
!     ******************************************************************
!
!
!
!     IOPT = 0   IMPLIES DO COMPLETE BASIC BENDING TRIANGLE.
!     IOPT = 1   IMPLIES COMPUTE ONLY THE NINE (3X3)MATRICES
!                WHICH FORM THE 9X9 K SUPER U - MATRIX.
!     IOPT = 2   SAME AS IOPT = 1,BUT SAVE H-INVERSE AND S...
!
!     ******************************************************************
!     ECPT LIST FOR BASIC BENDING TRIANGLE             NAME IN
!                                                      THIS
!     ECPT                                             ROUTINE   TYPE
!     ******************************************************************
!     ECPT( 1) = ELEMENT ID                            NECPT(1)  INTEGER
!     ECPT( 2) = GRID POINT A                          NGRID(1)  INTEGER
!     ECPT( 3) = GRID POINT B                          NGRID(2)  INTEGER
!     ECPT( 4) = GRID POINT C                          NGRID(3)  INTEGER
!     ECPT( 5) = THETA = ANGLE OF MATERIAL             ANGLE     REAL
!     ECPT( 6) = MATERIAL ID 1                         MATID1    INTEGER
!     ECPT( 7) = I = MOMENT OF INERTIA                 EYE       REAL
!     ECPT( 8) = MATERIAL ID 2                         MATID2    INTEGER
!     ECPT( 9) = T2                                    T2        REAL
!     ECPT(10) = NON-STRUCTURAL-MASS                   FMU       REAL
!     ECPT(11) = Z1                                    Z11       REAL
!     ECPT(12) = Z2                                    Z22       REAL
!     ECPT(13) = COORD. SYSTEM ID 1                    NECPT(13) INTEGER
!     ECPT(14) = X1                                    X1        REAL
!     ECPT(15) = Y1                                    Y1        REAL
!     ECPT(16) = Z1                                    Z1        REAL
!     ECPT(17) = COORD. SYSTEM ID 2                    NECPT(17) INTEGER
!     ECPT(18) = X2                                    X2        REAL
!     ECPT(19) = Y2                                    Y2        REAL
!     ECPT(20) = Z2                                    Z2        REAL
!     ECPT(21) = COORD. SYSTEM ID 3                    NECPT(21) INTEGER
!     ECPT(22) = X3                                    X3        REAL
!     ECPT(23) = Y3                                    Y3        REAL
!     ECPT(24) = Z3                                    Z3        REAL
!     ECPT(25) = ELEMENT TEMPERATURE                   ELTEMP    REAL
!     ******************************************************************
!
!
!
!
   !>>>>EQUIVALENCE (Consts(4),Degra)
   !>>>>EQUIVALENCE (Ielid,Ecpt(1),Necpt(1)) , (D(1),G(1),A(79)) , (G2x2(1),A(88)) , (Tjte(1),A(100)) , (Tite(1),S(1),A(82)) ,           &
!>>>>    & (J2x2(1),A(92)) , (Ti(1),A(118))
!
   DATA no/81 , 190/
   ntype = 0
   IF ( Iopt>0 ) ntype = 1
   IF ( ntype/=1 ) THEN
      Eltemp = ecpt(25)
!     SET UP  I, J, K VECTORS STORING AS FOLLOWS AND ALSO CALCULATE
!     X-SUB-B, X-SUB-C, AND Y-SUB-C.
!
!     E(11), E(14), E(17) WILL BE THE I-VECTOR.
!     E(12), E(15), E(18) WILL BE THE J-VECTOR.
!     E( 1), E( 4), E( 7) WILL BE THE K-VECTOR.
!
!     FIND I-VECTOR = RSUBB - RUBA (NON-NORMALIZED)
      E(11) = dble(X2) - dble(X1)
      E(14) = dble(Y2) - dble(Y1)
      E(17) = dble(Z2) - dble(Z1)
!
!     FIND LENGTH = X-SUB-B COOR. IN ELEMENT SYSTEM
      Xsubb = dsqrt(E(11)**2+E(14)**2+E(17)**2)
      IF ( Xsubb<=1.D-6 ) THEN
!
!     ERROR RETURNS
!
         CALL mesage(30,31,ecpt(1))
         GOTO 100
      ELSE
!
         E(11) = E(11)/Xsubb
         E(14) = E(14)/Xsubb
         E(17) = E(17)/Xsubb
!
!     TAKE RSUBC - RSUBA AND STORE TEMPORARILY IN E(2), E(5), E(8)
!
         E(2) = dble(X3) - dble(X1)
         E(5) = dble(Y3) - dble(Y1)
         E(8) = dble(Z3) - dble(Z1)
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
         Ysubc = dsqrt(E(1)**2+E(4)**2+E(7)**2)
         IF ( Ysubc<=1.0-6 ) THEN
!
            CALL mesage(30,32,Ielid)
            GOTO 100
         ELSE
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
            temp = dsqrt(E(12)**2+E(15)**2+E(18)**2)
            E(12) = E(12)/temp
            E(15) = E(15)/temp
            E(18) = E(18)/temp
            E(2) = 0.
            E(3) = 0.
            E(5) = 0.
            E(6) = 0.
            E(8) = 0.
            E(9) = 0.
            E(10) = 0.
            E(13) = 0.
            E(16) = 0.
!
!     CONVERT ANGLE FROM DEGREES TO RADIANS STORING IN THETA.
!
            theta = dble(Angle)*degra
            Sinth = dsin(theta)
            Costh = dcos(theta)
            IF ( dabs(Sinth)<1.D-6 ) Sinth = 0.D0
         ENDIF
      ENDIF
   ENDIF
!
!     ******************************************************************
!
!     SETTING UP G MATRIX
   Inflag = 2
   Matid = Matid1
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
!     ******************************************************************
!
!  50 COMPUTATION OF D = I.G-MATRIX (EYE IS INPUT FROM THE ECPT)
!
   DO i = 1 , 9
      d(i) = g(i)*dble(Eye)
   ENDDO
!
!     ******************************************************************
!
   area = Xsubb*Ysubc/2.D0
   xbar = (Xsubb+Xsubc)/3.D0
   ybar = Ysubc/3.D0
!
   xcsq = Xsubc**2
   ycsq = Ysubc**2
   xbsq = Xsubb**2
   xcyc = Xsubc*Ysubc
   px2 = (xbsq+Xsubb*Xsubc+xcsq)/6.D0
   py2 = ycsq/6.D0
   pxy2 = Ysubc*(Xsubb+2.D0*Xsubc)/12.D0
   xbar3 = 3.D0*xbar
   ybar3 = 3.D0*ybar
   ybar2 = 2.D0*ybar
!
!     ******************************************************************
!                 X
!     FILL THE  (K ) MATRIX STORING IN  A(1). . .A(36)
!
   A(1) = d(1)
   A(2) = d(3)
   A(3) = d(2)
   A(4) = d(1)*xbar3
   A(5) = d(2)*xbar + ybar2*d(3)
   A(6) = d(2)*ybar3
   A(7) = A(2)
   A(8) = d(9)
   A(9) = d(6)
   A(10) = d(3)*xbar3
   A(11) = d(6)*xbar + ybar2*d(9)
   A(12) = d(6)*ybar3
   A(13) = A(3)
   A(14) = A(9)
   A(15) = d(5)
   A(16) = d(2)*xbar3
   A(17) = d(5)*xbar + ybar2*d(6)
   A(18) = d(5)*ybar3
   A(19) = A(4)
   A(20) = A(10)
   A(21) = A(16)
   A(22) = d(1)*9.*px2
   A(23) = d(2)*3.*px2 + 6.*pxy2*d(3)
   A(24) = d(2)*9.*pxy2
   A(25) = A(5)
   A(26) = A(11)
   A(27) = A(17)
   A(28) = A(23)
   A(29) = d(5)*px2 + 4.*pxy2*d(6) + 4.*py2*d(9)
   A(30) = d(5)*3.*pxy2 + 6.*py2*d(6)
   A(31) = A(6)
   A(32) = A(12)
   A(33) = A(18)
   A(34) = A(24)
   A(35) = A(30)
   A(36) = d(5)*9.*py2
   temp = 4.*area
   DO i = 1 , 36
      A(i) = A(i)*temp
   ENDDO
!
!     ******************************************************************
!
!     F1LL  (HBAR) MATRIX STORING AT A(37). . .A(72)
!
   DO i = 37 , 72
      A(i) = 0.
   ENDDO
!
   A(37) = xbsq
   A(40) = xbsq*Xsubb
   A(44) = Xsubb
   A(49) = -2.*Xsubb
   A(52) = -3.*xbsq
   A(55) = xcsq
   A(56) = xcyc
   A(57) = ycsq
   A(58) = xcsq*Xsubc
   A(59) = ycsq*Xsubc
   A(60) = ycsq*Ysubc
   A(62) = Xsubc
   A(63) = Ysubc*2.
   A(65) = xcyc*2.
   A(66) = ycsq*3.
   A(67) = -2.*Xsubc
   A(68) = -Ysubc
   A(70) = -3.*xcsq
   A(71) = -ycsq
!
!     ******************************************************************
!
   IF ( T2/=0. ) THEN
!
!     ALL OF OPERATIONS THRU STMT 220
!     ARE NECESSARY IF T2 IS NON-ZERO.
!
!     ******************************************************************
!
!
!     GET THE G2X2 MATRIX
!
      Matid = Matid2
      Inflag = 3
      CALL mat(ecpt(1))
      IF ( G2x211/=0.0E0 .OR. G2x212/=0.0E0 .OR. G2x222/=0.0E0 ) THEN
         g2x2(1) = dble(G2x211)*dble(T2)
         g2x2(2) = dble(G2x212)*dble(T2)
         g2x2(3) = dble(G2x212)*dble(T2)
         g2x2(4) = dble(G2x222)*dble(T2)
!
         determ = g2x2(1)*g2x2(4) - g2x2(3)*g2x2(2)
         j2x2(1) = g2x2(4)/determ
         j2x2(2) = -g2x2(2)/determ
         j2x2(3) = -g2x2(3)/determ
         j2x2(4) = g2x2(1)/determ
!
!     ******************************************************************
!
!     (H  ) IS PARTITIONED INTO A LEFT AND RIGHT PORTION AND ONLY THE
!       YQ  RIGHT PORTION IS COMPUTED AND USED AS A  (2X3). THE LEFT
!           2X3 PORTION IS NULL.  THE RIGHT PORTION WILL BE STORED AT
!           A(73)...A(78) UNTIL NOT NEEDED ANY FURTHER.
!
!
!
         temp = 2.*d(2) + 4.*d(9)
         A(73) = -6.*(j2x2(1)*d(1)+j2x2(2)*d(3))
         A(74) = -j2x2(1)*temp - 6.*j2x2(2)*d(6)
         A(75) = -6.*(j2x2(1)*d(6)+j2x2(2)*d(5))
         A(76) = -6.*(j2x2(2)*d(1)+j2x2(4)*d(3))
         A(77) = -j2x2(2)*temp - 6.*j2x2(4)*d(6)
         A(78) = -6.*(j2x2(2)*d(6)+j2x2(4)*d(5))
!
!     THE ABOVE 6 ELEMENTS NOW REPRESENT THE (H  ) MATRIX (2X3)
!                                              YQ
!
!     NOW FORMING  PRODUCT (G2X2)(H  ) AND STORING AS AN INTERMEDIATE
!     STEP.                        YQ
!
!
         CALL gmmatd(g2x2,2,2,0,A(73),2,3,0,A(79))
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
            A(i+21) = A(i+21) + A(i+84)*area
            A(i+27) = A(i+27) + A(i+87)*area
            A(i+33) = A(i+33) + A(i+90)*area
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
!     ******************************************************************
!
!
!     AT THIS POINT INVERT  (H) WHICH IS STORED AT A(37). . .A(72)
!     STORE INVERSE BACK IN A(37) . . . A(72)
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
   ising = -1
   CALL inverd(6,A(37),6,A(73),0,determ,ising,A(79))
!
!     CHECK TO SEE IF H WAS SINGULAR
   IF ( ising==2 ) THEN
!
      CALL mesage(30,33,Ielid)
   ELSE
!
!
!     ******************************************************************
!              Q   -1
!     FORM   (K )(H  )  AND STORE AT  A(73). . .A(108)
!                 X                     Q
!     NOTE THAT (K ) AT THIS POINT IS (K )...
!
      CALL gmmatd(A(1),6,6,0,A(37),6,6,0,A(73))
!
!                    -1 T
!     FORM(K  ) = (H  ) (LAST PRODUCT) STORE AT A(109). . .A(144)
!            II
!
      CALL gmmatd(A(37),6,6,1,A(73),6,6,0,A(109))
!
!     ******************************************************************
!
!     FILL S-MATRIX EQUIVALENCED TO A(82)  (S IS  6X3 )
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
      s(1) = 1.0
      s(2) = 0.
      s(3) = -Xsubb
      s(4) = 0.
      s(5) = 1.
      s(6) = 0.
      s(7) = 0.
      s(8) = 0.
      s(9) = 1.
      s(10) = 1.
      s(11) = Ysubc
      s(12) = -Xsubc
      s(13) = 0.
      s(14) = 1.
      s(15) = 0.
      s(16) = 0.
      s(17) = 0.
      s(18) = 1.
!
!     ******************************************************************
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
!                  K     (6X6) AT A(109). . .A(144)
!                   II
!
!                  K     (6,3) AT  A(46). . .A(63)
!                   IA
!
!                  K     (3X3) AT A(  1). . .A(  9)
!                   AA
!
!     ARRANGE NINE 3X3 MATRICES OF K SUPER U
      DO i = 28 , 36
         A(i) = A(i+18)
      ENDDO
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
      Dict5 = Gsube
      IF ( ntype/=1 ) THEN
!
!     ******************************************************************
!
!
         DO npivot = 1 , 3
!
!
!     AT THIS POINT START ASSEMBLY OF 3   6X6 MATRICES FOR I = PIVOT,
!     AND J =1,2,3  IN THE FOLLOWING EQUATION.
!
!                   T         U    T
!        (K  ) = (T  ) (E) (K  ) (E ) (T )
!          IJ      I         IJ         J
!
!     ******************************************************************
!
!     FIRST GET THE PRODUCT APPLICABLE TO ALL 3 K  .
!                                                IJ
!                   T
!              = (T  ) (E)    A 6X3 MATRIX.
!                  I
!
!
!     CHECK TO SEE IF TI-MATRIX IS NEEDED
!     IF THE CSID IS ZERO FOR THE PIVOT POINT SKIP TRANSFORMATION.
!
            IF ( necpt(4*npivot+9)==0 ) THEN
!
! 250 COMING HERE IMPLIES TI NOT USED.
!     JUST SET TITE = E MATRIX
               DO i = 1 , 18
                  tite(i) = E(i)
               ENDDO
            ELSE
!
!     GET  TI AND MULTIPLY WITH E TO FILL TITE (THE COMMON PRODUCT)
!
               CALL transd(necpt(4*npivot+9),ti)
!
!     TI IS EQUIVALENCED TO A(118) AND IS 3X3.
!
!     FORM TITE (UPPER AND LOWER) OK OK OK....
!
               CALL gmmatd(ti(1),3,3,1,E(1),3,3,0,tite(1))
!
               CALL gmmatd(ti(1),3,3,1,E(10),3,3,0,tite(10))
            ENDIF
!
!     ******************************************************************
!                                                  T
! 280 AT THIS POINT COMMON PRODUCT IS COMPLETE =(T  )(E)  STORED IN TITE
!                                                 I
!
!     THE PIVOT I IS NPIVOT
            npt1 = 189
!
!     THE ABOVE SETS A POINTER, NPT1, TO POINT TO 18 FREE DOUBLE PREC.
!     CORE LOCATIONS IN THE A-ARRAY FOR STORAGE OF THE FOLLOWING
!     SUB-PRODUCT.
!                     U   T
!                  (K  )(E )(T )
!                    IJ       J
!
!     ******************************************************************
!
!     LOOP THRU FOR THE 3 - 6X6 K   ARRAYS.
!                                IJ
            DO j = 1 , 3
!                          T
!     TAKE SUB PRODUCT = (E )(T )..     STORE IN TJTE MATRIX
!                              J
!
!     NOTE.. THE TRANSPOSE OF THE ABOVE IS BEING FOUND AND USED,
!                           T
!                      = (T  )(E),  AND STORED IN TJTE-MATRIX
!                          J        EQUIVALENCED TO A(100)
!
!
!     CHECK TO SEE IF TRANSFORMATION IS NEEDED.
!     IF NOT SKIP TO  480
!
               IF ( necpt(4*j+9)==0 ) THEN
!
! 480 COMING HERE IF TRANSFORMATION NOT USED
!
! 480 SET TJTE = E
                  DO i = 1 , 18
                     tjte(i) = E(i)
                  ENDDO
               ELSE
!
                  CALL transd(necpt(4*j+9),ti)
!
                  CALL gmmatd(ti(1),3,3,1,E(1),3,3,0,tjte(1))
                  CALL gmmatd(ti(1),3,3,1,E(10),3,3,0,tjte(10))
               ENDIF
!
!     ******************************************************************
!           T       T
! 880   ( (E )(T ) )  IS COMPLETE AND STORED BY ROWS IN TJTE-MATRIX.
!               J
!                     U   T
!     NOW FORM,    (K  )(E )(T ), STORING AT A(NPT1)
!                    IJ       J
!
!                                  U
!     TO COMPUTE ABOVE USE 3X3   K
!                                 (NPIVOT,J)
!     COMPUTE POINTER TO THIS 3X3.
!
               npt2 = 27*npivot + 9*j - 35
!
               CALL gmmatd(A(npt2),3,3,0,tjte,6,3,1,A(npt1))
!
!     ******************************************************************
!
! 950 AT THIS POINT,
!                       U   T
!                    (K  )(E )(T )  IS STORED AT A(NPT1), (3X6).
!                      IJ       J
!
!     AND,              T
!                    (T  )(E)      IS STORED AT TITE(1) = A(82)  (6X3)
!                      I
!     ******************************************************************
!
!     FORMING FINAL PRODUCT, AND STORING AT A(100) THE 6X6.
!
               CALL gmmatd(tite(1),6,3,0,A(npt1),3,6,0,A(100))
!
!     ******************************************************************
!
!
!    NOW STORE  THE 6X6 MATRIX IN AOUT
!
               iout = (npivot-1)*27 + (j-1)*9 + 1
               i = 113
               DO ii = 1 , 3
                  DO jj = 1 , 3
                     ia = i + (ii-1)*6 + jj
                     Aout(iout) = A(ia)
                     iout = iout + 1
                  ENDDO
               ENDDO
!
            ENDDO
!
         ENDDO
!
         RETURN
      ELSE
         loopnd = no(Iopt)
         DO i = 1 , loopnd
            Aout(i) = A(i)
         ENDDO
         RETURN
      ENDIF
   ENDIF
 100  Nogo = .TRUE.
!     ******************************************************************
END SUBROUTINE etrbkd
