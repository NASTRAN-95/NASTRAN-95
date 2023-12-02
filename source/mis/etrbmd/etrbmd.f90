!*==etrbmd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE etrbmd
USE C_EMGEST
USE C_EMGPRM
USE C_EMGTRX
USE C_MATIN
USE C_MATOUT
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: aij , bij , determ , fi , fij , fj , fj2 , sizero , temp , xbsq , xcsq , xcyc , xprodi , ycsq , yprodj
   REAL(REAL64) , DIMENSION(9) :: d , g , mbaraa
   REAL , DIMENSION(26) :: ecpt
   REAL(REAL64) , DIMENSION(4) :: g2x2 , j2x2
   REAL(REAL64) , DIMENSION(6) :: hyq
   INTEGER :: i , ising , j , k , npt
   REAL(REAL64) , DIMENSION(18) :: mar , s
   REAL(REAL64) , DIMENSION(36) :: mrr
   REAL(REAL64) , DIMENSION(7,7) :: siij
   EXTERNAL gmmatd , inverd , mat , mesage
!
! End of declarations rewritten by SPAG
!
!
!     BASIC BENDING TRIANGLE ELEMENT ROUTINE
!     DOUBLE PRECISION VERSION
!
!     THIS SUBROUTINE CALCULATES THE COUPLED MASS MATRIX FOR THE BASIC
!     BENDING TRIANGLE.
!
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
!
   !>>>>EQUIVALENCE (Ielid,Ecpt(1),Necpt(1)) , (J2x2(1),A(14)) , (D(1),G(1),A(1),Siij(1,1)) , (G2x2(1),A(10)) , (Hyq(1),A(50)) ,         &
!>>>>    & (Mbaraa(1),A(136)) , (Mar(1),A(145)) , (Mrr(1),A(163)) , (S(1),A(82))
!
!     SETTING UP G MATRIX
!     BEFORE THIS SUBROUTINE CAN FUNCTION SEVERAL TERMS MUST BE DEFINED
!     SEE ETRBKD.
!
!     POSSIBLE ERROR SOURCE FIX.  MAY REQUIRE LOADER CHANGE.
!     IF (ISMB(1) .EQ. 0)  CALL ETRBKD (1)
!
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
   DO i = 1 , 9
      d(i) = g(i)*dble(Eye)
   ENDDO
!
!     F1LL  (HBAR) MATRIX STORING AT A(100). . .A(135)
!
   xcsq = Xsubc**2
   ycsq = Ysubc**2
   xbsq = Xsubb**2
   xcyc = Xsubc*Ysubc
!
   DO i = 100 , 135
      A(i) = 0.
   ENDDO
!
   A(100) = xbsq
   A(103) = xbsq*Xsubb
   A(107) = Xsubb
   A(112) = -2.*Xsubb
   A(115) = -3.*xbsq
   A(118) = xcsq
   A(119) = xcyc
   A(120) = ycsq
   A(121) = xcsq*Xsubc
   A(122) = ycsq*Xsubc
   A(123) = ycsq*Ysubc
   A(125) = Xsubc
   A(126) = Ysubc*2.0D0
   A(128) = xcyc*2.0D0
   A(129) = ycsq*3.0D0
   A(130) = -2.0D0*Xsubc
   A(131) = -Ysubc
   A(133) = -3.0D0*xcsq
   A(134) = -ycsq
!
   IF ( T2/=0. ) THEN
!
!     ALL OF THE FOLLOWING OPERATIONS THROUGH STATEMENT LABEL 110
!     ARE NECESSARY IF T2 IS NON-ZERO.
!
!     GET THE G2X2 MATRIX
!
      Matid = Matid2
      Inflag = 3
      CALL mat(ecpt(1))
      IF ( G2x211/=0.0 .OR. G2x212/=0.0 .OR. G2x222/=0.0 ) THEN
!
         g2x2(1) = dble(G2x211)*dble(T2)
         g2x2(2) = dble(G2x212)*dble(T2)
         g2x2(4) = dble(G2x222)*dble(T2)
!
         determ = g2x2(1)*g2x2(4) - g2x2(3)*g2x2(2)
         j2x2(1) = g2x2(4)/determ
         j2x2(2) = -g2x2(2)/determ
         j2x2(3) = j2x2(2)
         j2x2(4) = g2x2(1)/determ
!
!     (H  ) IS PARTITIONED INTO A LEFT AND RIGHT PORTION AND ONLY THE
!       YQ  RIGHT PORTION IS COMPUTED AND USED AS A  (2X3). THE LEFT
!           2X3 PORTION IS NULL.  THE RIGHT PORTION WILL BE STORED AT
!           A(50)...A(55) UNTIL NOT NEEDED ANY FURTHER.
!
         temp = 2.*d(2) + 4.*d(9)
         hyq(1) = -6.*(j2x2(1)*d(1)+j2x2(2)*d(3))
         hyq(2) = -j2x2(1)*temp - 6.*j2x2(2)*d(6)
         hyq(3) = -6.*(j2x2(1)*d(6)+j2x2(2)*d(5))
         hyq(4) = -6.*(j2x2(2)*d(1)+j2x2(4)*d(3))
         hyq(5) = -j2x2(2)*temp - 6.*j2x2(4)*d(6)
         hyq(6) = -6.*(j2x2(2)*d(6)+j2x2(4)*d(5))
!
!     ADD TO 6 OF THE (HBAR) ELEMENTS THE RESULT OF (H  )(H  )
!                                                    UY   YQ
!     THE PRODUCT IS FORMED DIRECTLY IN THE ADDITION PROCESS BELOW.
!     NO (H  ) MATRIX IS ACTUALLY COMPUTED DIRECTLY.
!          UY
!
!     THE FOLLOWING IS THEN STEP 6 PAGE 8, FMMS-66
!
         DO i = 1 , 3
            A(i+102) = A(i+102) + Xsubb*hyq(i)
            A(i+120) = A(i+120) + Xsubc*hyq(i) + Ysubc*hyq(i+3)
         ENDDO
      ENDIF
   ENDIF
!
!     THIS ENDS ADDED COMPUTATION FOR CASE OF T2 NOT ZERO
!
!
!     AT THIS POINT INVERT  (H) WHICH IS STORED AT A(100). . .A(135)
!     STORE INVERSE BACK IN A(100). . A(135)
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
   ising = -1
   CALL inverd(6,A(100),6,A(136),0,determ,ising,A(142))
!
!     CHECK TO SEE IF H WAS SINGULAR
!
   IF ( ising==2 ) THEN
!
!     ERROR EXITS
!
      CALL mesage(30,33,ecpt(1))
      Nogo = .TRUE.
      RETURN
   ELSE
!
!     ISING = 2 IMPLIES SINGULAR MATRIX THUS ERROR CONDITION.
!
!     CHUNK OUT INTEGRAL VALUES I   USED IN REFERENCED M MATRICES
!                                IJ                    SEE P.9, FMMS-66
!
!     THE CALCULATION FOR  (I  ) ARE AS FOLLOWS
!                            IJ
!                                                           ***
!         A1  = XSUBB * YSUBC**(J+1) / ((J+1)*(J+2))           *
!           0J                                                 *
!                                                              *
!         B   = XSUBC * YSUBC**(J+1) / (J+2)                   *
!           0J                                                 ** J=0,6
!                                                              *
!         A   = A1   + B                                       *
!           0J    0J    0J                                     *
!                                                              *
!         I   = MU * A1                                        *
!           0J         0J                                   ***
!
!                                                           ***
!         A1  = I * XSUBB * A      /(I+J+2)                    *
!           IJ               I-1,J                             *
!                                                              *
!         B   = XSUBC**(I+1) * YSUBC**(J+1) /((I+1)*(I+J+2))   *  I=1,6
!           IJ                                                 ** J=0,6
!                                                              *
!         A   = A1   + B                                       *
!           IJ    IJ    IJ                                     *
!                                                              *
!         I     MU * A1                                        *
!           IJ=        IJ                                      *
!                                                           ***
!      NOTE.. LOOPS FOR PROGRAM BEGIN AT 1 INSTEAD OF 0
!                                      I.E.  I = 1,7
!                                            J = 1,7
!
      DO j = 1 , 7
         yprodj = Ysubc**j
         fj = j
         fj2 = j + 1
         aij = Xsubb*yprodj/(fj*fj2)
         bij = Xsubc*yprodj/fj2
         siij(1,j) = Fmu*aij
         aij = aij + bij
         IF ( j/=7 ) THEN
            k = 8 - j
            DO i = 2 , k
               xprodi = Xsubc**i
               fi = i
               fij = i + j
               aij = (fi-1.)*Xsubb*aij/fij
               bij = xprodi*yprodj/(fi*fij)
               siij(i,j) = Fmu*aij
               aij = aij + bij
            ENDDO
         ENDIF
!
      ENDDO
      sizero = siij(1,1)/3.
!
!     CHUNK IN NUMBERS FOR (M-BAR-AA) 3X3 MATRIX AS PER MS-48, PP. 6-10
!
!                    (M  )         3X6 MATRIX
!                      AR
!
!                    (M  )         6X6 MATRIX
!                      RR
!
!     (M-BAR-AA) MATRIX
!
      mbaraa(1) = siij(1,1)
      mbaraa(2) = siij(1,2)
      mbaraa(3) = -siij(2,1)
      mbaraa(4) = siij(1,2)
      mbaraa(5) = siij(1,3)
      mbaraa(6) = -siij(2,2)
      mbaraa(7) = -siij(2,1)
      mbaraa(8) = -siij(2,2)
      mbaraa(9) = siij(3,1)
!
!     (M  ) MATRIX
!       AR
!
      mar(1) = siij(3,1)
      mar(2) = siij(2,2)
      mar(3) = siij(1,3)
      mar(4) = siij(4,1)
      mar(5) = siij(2,3)
      mar(6) = siij(1,4)
      mar(7) = siij(3,2)
      mar(8) = siij(2,3)
      mar(9) = siij(1,4)
      mar(10) = siij(4,2)
      mar(11) = siij(2,4)
      mar(12) = siij(1,5)
      mar(13) = -siij(4,1)
      mar(14) = -siij(3,2)
      mar(15) = -siij(2,3)
      mar(16) = -siij(5,1)
      mar(17) = -siij(3,3)
      mar(18) = -siij(2,4)
!
!     (M  ) MATRIX  A 6X6 SYMMETRIC MATRIX
!       RR
!
      mrr(1) = siij(5,1)
      mrr(2) = siij(4,2)
      mrr(3) = siij(3,3)
      mrr(4) = siij(6,1)
      mrr(5) = siij(4,3)
      mrr(6) = siij(3,4)
      mrr(7) = mrr(2)
      mrr(8) = siij(3,3)
      mrr(9) = siij(2,4)
      mrr(10) = siij(5,2)
      mrr(11) = siij(3,4)
      mrr(12) = siij(2,5)
      mrr(13) = mrr(3)
      mrr(14) = mrr(9)
      mrr(15) = siij(1,5)
      mrr(16) = siij(4,3)
      mrr(17) = siij(2,5)
      mrr(18) = siij(1,6)
      mrr(19) = mrr(4)
      mrr(20) = mrr(10)
      mrr(21) = mrr(16)
      mrr(22) = siij(7,1)
      mrr(23) = siij(5,3)
      mrr(24) = siij(4,4)
      mrr(25) = mrr(5)
      mrr(26) = mrr(11)
      mrr(27) = mrr(17)
      mrr(28) = mrr(23)
      mrr(29) = siij(3,5)
      mrr(30) = siij(2,6)
      mrr(31) = mrr(6)
      mrr(32) = mrr(12)
      mrr(33) = mrr(18)
      mrr(34) = mrr(24)
      mrr(35) = mrr(30)
      mrr(36) = siij(1,7)
!
      IF ( T2/=0. ) THEN
!
         mar(4) = mar(4) + hyq(1)*siij(2,1) + hyq(4)*siij(1,2)
         mar(5) = mar(5) + hyq(2)*siij(2,1) + hyq(5)*siij(1,2)
         mar(6) = mar(6) + hyq(3)*siij(2,1) + hyq(6)*siij(1,2)
         mar(10) = mar(10) + hyq(1)*siij(2,2) + hyq(4)*siij(1,3)
         mar(11) = mar(11) + hyq(2)*siij(2,2) + hyq(5)*siij(1,3)
         mar(12) = mar(12) + hyq(3)*siij(2,2) + hyq(6)*siij(1,3)
         mar(16) = mar(16) - hyq(1)*siij(3,1) - hyq(4)*siij(2,2)
         mar(17) = mar(17) - hyq(2)*siij(3,1) - hyq(5)*siij(2,2)
         mar(18) = mar(18) - hyq(3)*siij(3,1) - hyq(6)*siij(2,2)
         mrr(4) = mrr(4) + hyq(1)*siij(4,1) + hyq(4)*siij(3,2)
         mrr(5) = mrr(5) + hyq(2)*siij(4,1) + hyq(5)*siij(3,2)
         mrr(6) = mrr(6) + hyq(3)*siij(4,1) + hyq(6)*siij(3,2)
         mrr(10) = mrr(10) + hyq(1)*siij(3,2) + hyq(4)*siij(2,3)
         mrr(11) = mrr(11) + hyq(2)*siij(3,2) + hyq(5)*siij(2,3)
         mrr(12) = mrr(12) + hyq(3)*siij(3,2) + hyq(6)*siij(2,3)
         mrr(16) = mrr(16) + hyq(1)*siij(2,3) + hyq(4)*siij(1,4)
         mrr(17) = mrr(17) + hyq(2)*siij(2,3) + hyq(5)*siij(1,4)
         mrr(18) = mrr(18) + hyq(3)*siij(2,3) + hyq(6)*siij(1,4)
         mrr(19) = mrr(4)
         mrr(20) = mrr(10)
         mrr(21) = mrr(16)
         mrr(22) = mrr(22) + hyq(1)*(hyq(1)*siij(3,1)+2.0D0*(siij(5,1)+hyq(4)*siij(2,2))) + hyq(4)                                  &
                 & *(2.0D0*siij(4,2)+hyq(4)*siij(1,3))
         mrr(23) = mrr(23) + hyq(2)*siij(5,1) + hyq(5)*siij(4,2) + hyq(1)*(siij(3,3)+hyq(2)*siij(3,1)+hyq(5)*siij(2,2)) + hyq(4)    &
                 & *(siij(2,4)+hyq(2)*siij(2,2)+hyq(5)*siij(1,3))
         mrr(24) = mrr(24) + hyq(3)*siij(5,1) + hyq(6)*siij(4,2) + hyq(1)*(siij(2,4)+hyq(3)*siij(3,1)+hyq(6)*siij(2,2)) + hyq(4)    &
                 & *(siij(1,5)+hyq(3)*siij(2,2)+hyq(6)*siij(1,3))
         mrr(25) = mrr(5)
         mrr(26) = mrr(11)
         mrr(27) = mrr(17)
         mrr(28) = mrr(23)
         mrr(29) = mrr(29) + hyq(2)*(hyq(2)*siij(3,1)+2.0D0*(siij(3,3)+hyq(5)*siij(2,2))) + hyq(5)                                  &
                 & *(2.0D0*siij(2,4)+hyq(5)*siij(1,3))
         mrr(30) = mrr(30) + hyq(3)*siij(3,3) + hyq(6)*siij(2,4) + hyq(2)*(siij(2,4)+hyq(3)*siij(3,1)+hyq(6)*siij(2,2)) + hyq(5)    &
                 & *(siij(1,5)+hyq(3)*siij(2,2)+hyq(6)*siij(1,3))
         mrr(31) = mrr(6)
         mrr(32) = mrr(12)
         mrr(33) = mrr(18)
         mrr(34) = mrr(24)
         mrr(35) = mrr(30)
         mrr(36) = mrr(36) + hyq(3)*(hyq(3)*siij(3,1)+2.0D0*(siij(2,4)+hyq(6)*siij(2,2))) + hyq(6)                                  &
                 & *(2.0D0*siij(1,5)+hyq(6)*siij(1,3))
      ENDIF
   ENDIF
!
!     FILL S-MATRIX EQUIVALENCED TO A(82)  (S IS  6X3 )
!
   s(1) = 1.
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
!     CAN NOW COMPUTE 9 (3X3) MASS MATRICES (FMMS-66, PAGES 10-11)
!
!                -1 T           -1
!     ( M ) = ( H  )  ( M  ) ( H  )
!                        RR
!
!              PARTITION (M)
!                                           ///       ///
!                                           /     *     /
!                                           / MBB * MBC /
!                                           /     *     /
!                                ( M )  =   / ********* /
!                                           /     *     /
!                                           / MCB * MCC /
!                                           /     *     /
!                                           ///       ///
!                                                       4 (3X3) MATRICES
!                        -1
!     ( M  ) = ( M  ) ( H  )
!        AI       AR
!
!              PARTITION (M  )              ///                 ///
!                          AI               /          *          /
!                               ( M  )  =   / M-BAR-AB * M-BAR-AC /
!                                  AI       /          *          /
!                                           ///                 ///
!                                                       2 (3X3) MATRICES
!                               T            T
!     ( MAB )  = (M-BAR-AB) - (S ) (MBB) - (S ) (MCB)
!                               B            C
!
!                               T            T
!     ( MAC )  = (M-BAR-AC) - (S ) (MBC) - (S ) (MCC)
!                               B            C
!
!                               T     T      T      T
!     ( MAA )  = (M-BAR-AA) - (S ) (M  ) - (S ) (MAC )
!                               B    AB      C
!
!                           - (M-BAR-AB) (S ) - (M-BAR-AC) (S )
!                                          B                 C
!
!                    T
!     ( MBA )  = (MAB )
!
!                    T
!     ( MCA )  = (MAC )
!
!     CHOOSE APPROPRIATE BLOCK OF A-ARRAY FOR STORAGE
!
!     (3X3)    STORED IN      (3X3)     STORED IN     (3X3)    STORED IN
!     (MAA)   A( 1... 9)      (MAB)   A(10)...8)      (MAC)   A(19...27)
!     (MBA)   A(28...36)      (MBB)   A(37)...45)     (MBC)   A(46...54)
!     (MCA)   A(55...63)      (MCB)   A(64...72)      (MCC)   A(73...81)
!
!       -1
!     (H  ) IS STORED AT A(100...135)
!     (S)   EQUIVALENCED A( 81... 99)
!     WORKING STORAGE IS A(181...216)
!     (M-BAR-AB) STORED UNTIL NO LONGER NEEDED IN A(163...171)
!     (M-BAR-AC) STORED UNTIL NO LONGER NEEDED IN A(172...180)
!
!                     -1 T          -1
!     COMPUTE (M) = (H  )  ((M  ) (H  ))
!                       RR
!
   CALL gmmatd(mrr(1),6,6,0,A(100),6,6,0,A(37))
   CALL gmmatd(A(100),6,6,1,A(37),6,6,0,A(1))
!
!     CREATE PARTITION OF 4 (3X3)
!
   DO i = 1 , 3
      A(i+36) = A(i)
      A(i+39) = A(i+6)
      A(i+42) = A(i+12)
!
      A(i+45) = A(i+3)
      A(i+48) = A(i+9)
      A(i+51) = A(i+15)
!
      A(i+63) = A(i+18)
      A(i+66) = A(i+24)
      A(i+69) = A(i+30)
!
      A(i+72) = A(i+21)
      A(i+75) = A(i+27)
      A(i+78) = A(i+33)
   ENDDO
!
!     COMPUTE             -1
!         (M  ) = (M  ) (H  )  AND  PARTITION INTO 2 (3X3)  (M-BAR-AB)
!           AI      AR                                  AND (M-BAR-AC)
!
   CALL gmmatd(mar(1),3,6,0,A(100),6,6,0,A(181))
   DO i = 1 , 3
      A(i+162) = A(i+180)
      A(i+165) = A(i+186)
      A(i+168) = A(i+192)
!
      A(i+171) = A(i+183)
      A(i+174) = A(i+189)
      A(i+177) = A(i+195)
   ENDDO
!
!     COMPUTE (MAB)
!
   CALL gmmatd(s(1),3,3,1,A(37),3,3,0,A(181))
   CALL gmmatd(s(10),3,3,1,A(64),3,3,0,A(190))
   DO i = 1 , 9
      A(i+9) = A(i+162) - A(i+180) - A(i+189)
   ENDDO
!
!     COMPUTE (MAC)
!
   CALL gmmatd(s(1),3,3,1,A(46),3,3,0,A(181))
   CALL gmmatd(s(10),3,3,1,A(73),3,3,0,A(190))
   DO i = 1 , 9
      A(i+18) = A(i+171) - A(i+180) - A(i+189)
   ENDDO
!
!     COMPUTE (MAA)
!
   CALL gmmatd(s(1),3,3,1,A(10),3,3,1,A(181))
   CALL gmmatd(s(10),3,3,1,A(19),3,3,1,A(190))
   CALL gmmatd(A(163),3,3,0,s(1),3,3,0,A(199))
   CALL gmmatd(A(172),3,3,0,s(10),3,3,0,A(208))
   DO i = 1 , 9
      A(i) = mbaraa(i) - A(i+180) - A(i+189) - A(i+198) - A(i+207)
   ENDDO
!
!     COMPUTE (MBA) AND (MCA)
!
   DO i = 1 , 3
      npt = 3*i + 7
      A(i+27) = A(npt)
      A(i+30) = A(npt+1)
      A(i+33) = A(npt+2)
!
      A(i+54) = A(npt+9)
      A(i+57) = A(npt+10)
      A(i+60) = A(npt+11)
   ENDDO
!
   DO i = 1 , 136
      Aout(i) = A(i)
   ENDDO
   RETURN
END SUBROUTINE etrbmd
