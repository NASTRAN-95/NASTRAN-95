
SUBROUTINE mtrbsc
   IMPLICIT NONE
   DOUBLE PRECISION A(225) , Aij , Bij , D(9) , Determ , E(9) , Fi , Fij , Fj , Fj2 , G(9) , G2x2(4) , Hyq(6) , J2x2(4) , Mar(18) , &
                  & Mbaraa(9) , Mrr(36) , Prod9(9) , S(18) , Siij(7,7) , Sizero , Temp , Temp9(9) , Xbsq , Xcsq , Xcyc , Xprodi ,   &
                  & Xsubb , Xsubc , Ycsq , Yprodj , Ysubc
   REAL Alp12 , Alpha1 , Alpha2 , Angle , Costh , Dum1(10) , Dum2(25) , Dum3(2) , Dumb(76) , Dumcl(7) , Dummy(59) , Dummy1 ,        &
      & Dummy2 , Dummy3 , Ecpt(1) , Eltemp , Eye , Fmu , G11 , G12 , G13 , G22 , G23 , G2x211 , G2x212 , G2x222 , G33 , Gsube ,     &
      & Rho , Sigcom , Sigshe , Sigten , Sinth , Space(2) , Stress , T2 , Tsub0 , X1 , X2 , X3 , Y1 , Y2 , Y3 , Z1 , Z11 , Z2 ,     &
      & Z22 , Z3
   INTEGER Ifmgg , Inflag , Ising , Link(10) , Matid , Matid1 , Matid2 , Necpt(1) , Ngrid(3) , Nogo , Npvt
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alpha1 , Alpha2 , Alp12 , Tsub0 , Gsube , Sigten , Sigcom , Sigshe ,   &
                 & G2x211 , G2x212 , G2x222 , Space
   COMMON /sma2cl/ Dum3 , Npvt , Dumcl , Link , Nogo
   COMMON /sma2dp/ A , Prod9 , Temp9 , Xsubb , Xsubc , Ysubc , E , Temp , Xcsq , Xbsq , Ycsq , Xcyc , Aij , Determ , Bij , Sizero , &
                 & Fj , Fj2 , Fi , Fij , Yprodj , Xprodi , Ising , Dummy
   COMMON /sma2et/ Necpt , Ngrid , Angle , Matid1 , Eye , Matid2 , T2 , Fmu , Z11 , Z22 , Dummy1 , X1 , Y1 , Z1 , Dummy2 , X2 , Y2 ,&
                 & Z2 , Dummy3 , X3 , Y3 , Z3 , Dumb
   COMMON /sma2io/ Dum1 , Ifmgg , Dum2
   INTEGER i , j , k , npt
!
!OMMENT.  ALL WRITE STATEMENTS WHICH HAVE BEEN COMMENTED OUT, HAVE BEEN
!         LEFT IN THE PROGRAMMING FOR ANY FUTURE DEBUGGING USE.
!
!
!      ************* BASIC BENDING TRIANGLE   ELEMENT ROUTINE **********
!
!     CALLS FROM THIS ROUTINE ARE MADE TO. . .
!
!          MAT    - MATERIAL DATA ROUTINE
!          SMA2B  - INSERTION ROUTINE
!          TRANSD - DOUBLE PRECISION TRANSFORMATION SUPPLIER
!          INVERD - DOUBLE PRECISION INVERSE ROUTINE
!          GMMATD - DOUBLE PRECISION MATRIX MULTIPLY AND TRANSPOSE
!          MESAGE - ERROR MESSAGE WRITER
!
!
!     ******************************************************************
!
!
!
!     DIMENSION MNAME(9)
!     DIMENSION NASTER(130)
!     DATA (MNAME(I), I = 1,9) /6H1(MAA),6H (MAB),6H (MAC),6H (MBA),
!    $6H (MBB),6H (MBC),6H (MCA),6H (MCB),6H (MCC) /
!     DATA NASTER /130*1H*/
!
!
!     ECPT BLOCK
!
!
   !>>>>EQUIVALENCE (D(1),G(1),Siij(1,1),A(1)) , (Ecpt(1),Necpt(1)) , (G2x2(1),A(10)) , (J2x2(1),A(14)) , (Hyq(1),A(50)) ,               &
!>>>>    & (Mbaraa(1),A(136)) , (Mar(1),A(145)) , (Mrr(1),A(163)) , (S(1),A(82))
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
!     ******************************************************************
!
!     SETTING UP G MATRIX
!
   Inflag = 2
   Matid = Matid1
   CALL mat(Ecpt(1))
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
      D(i) = G(i)*dble(Eye)
   ENDDO
!
!     F1LL  (HBAR) MATRIX STORING AT A(100). . .A(135)
   Xcsq = Xsubc**2
   Ycsq = Ysubc**2
   Xbsq = Xsubb**2
   Xcyc = Xsubc*Ysubc
!
   DO i = 100 , 135
      A(i) = 0.0D0
   ENDDO
!
   A(100) = Xbsq
   A(103) = Xbsq*Xsubb
   A(107) = Xsubb
   A(112) = -2.0D0*Xsubb
   A(115) = -3.0D0*Xbsq
   A(118) = Xcsq
   A(119) = Xcyc
   A(120) = Ycsq
   A(121) = Xcsq*Xsubc
   A(122) = Ycsq*Xsubc
   A(123) = Ycsq*Ysubc
   A(125) = Xsubc
   A(126) = Ysubc*2.0D0
   A(128) = Xcyc*2.0D0
   A(129) = Ycsq*3.0D0
   A(130) = -2.0D0*Xsubc
   A(131) = -Ysubc
   A(133) = -3.0D0*Xcsq
   A(134) = -Ycsq
!
!
!     ******************************************************************
!
   IF ( T2/=0.0E0 ) THEN
!
!     ALL OF THE FOLLOWING OPERATIONS THROUGH STATEMENT LABEL 110
!     ARE NECESSARY IF T2 IS NON-ZERO.
!
!
!     GET THE G2X2 MATRIX
!
      Matid = Matid2
      Inflag = 3
      CALL mat(Ecpt(1))
      IF ( G2x211/=0.0E0 .OR. G2x212/=0.0E0 .OR. G2x222/=0.0E0 ) THEN
         G2x2(1) = G2x211*T2
         G2x2(2) = G2x212*T2
         G2x2(3) = G2x2(2)
         G2x2(4) = G2x222*T2
!
         Determ = G2x2(1)*G2x2(4) - G2x2(3)*G2x2(2)
         J2x2(1) = G2x2(4)/Determ
         J2x2(2) = -G2x2(2)/Determ
         J2x2(3) = J2x2(2)
         J2x2(4) = G2x2(1)/Determ
!
!
!     (H  ) IS PARTITIONED INTO A LEFT AND RIGHT PORTION AND ONLY THE
!       YQ  RIGHT PORTION IS COMPUTED AND USED AS A  (2X3). THE LEFT
!           2X3 PORTION IS NULL.  THE RIGHT PORTION WILL BE STORED AT
!           A(50)...A(55) UNTIL NOT NEEDED ANY FURTHER.
!
!
!
         Temp = 2.0D0*D(2) + 4.0D0*D(9)
         Hyq(1) = -6.0D0*(J2x2(1)*D(1)+J2x2(2)*D(3))
         Hyq(2) = -J2x2(1)*Temp - 6.0D0*J2x2(2)*D(6)
         Hyq(3) = -6.0D0*(J2x2(1)*D(6)+J2x2(2)*D(5))
         Hyq(4) = -6.0D0*(J2x2(2)*D(1)+J2x2(4)*D(3))
         Hyq(5) = -J2x2(2)*Temp - 6.0D0*J2x2(4)*D(6)
         Hyq(6) = -6.0D0*(J2x2(2)*D(6)+J2x2(4)*D(5))
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
            A(i+102) = A(i+102) + Xsubb*Hyq(i)
            A(i+120) = A(i+120) + Xsubc*Hyq(i) + Ysubc*Hyq(i+3)
         ENDDO
      ENDIF
   ENDIF
!
!     THIS ENDS ADDED COMPUTATION FOR CASE OF T2 NOT ZERO
!
!     ******************************************************************
!
!
!     AT THIS POINT INVERT  (H) WHICH IS STORED AT A(100). . .A(135)
!     STORE INVERSE BACK IN A(100). . A(135)
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
   Ising = -1
   CALL inverd(6,A(100),6,A(136),0,Determ,Ising,A(142))
!
!     CHECK TO SEE IF H WAS SINGULAR
   IF ( Ising/=2 ) THEN
!
!
!HURN OUT INTEGRAL VALUES I   USED IN REFERENCED M MATRICES
!                          IJ                           SEE P.9, FMMS-66
!
!     THE CALCULATION FOR  (I  ) ARE AS FOLLOWS
!                            IJ
!                                                      ***
!         A1  = XSUBB * YSUBC**(J+1) / ((J+1)*(J+2))      *
!           0J                                            *
!                                                         *
!         B   = XSUBC * YSUBC**(J+1) / (J+2)              *
!           0J                                            ** J=0,6
!                                                         *
!         A   = A1   + B                                  *
!           0J    0J    0J                                *
!                                                         *
!         I   = MU * A1                                   *
!           0J         0J                              ***
!
!                                                            ***
!         A1  = I * XSUBB * A      /(I+J+2)                     *
!           IJ               I-1,J                              *
!                                                               *
!         B   = XSUBC**(I+1) * YSUBC**(J+1) /((I+1)*(I+J+2))    *  I=1,6
!           IJ                                                  ** J=0,6
!                                                               *
!         A   = A1   + B                                        *
!           IJ    IJ    IJ                                      *
!                                                               *
!         I     MU * A1                                         *
!           IJ=        IJ                                       *
!                                                            ***
!      NOTE.. LOOPS FOR PROGRAM BEGIN AT 1 INSTEAD OF 0
!                                      I.E.  I = 1,7
!                                            J = 1,7
!
      DO j = 1 , 7
         Yprodj = Ysubc**j
         Fj = j
         Fj2 = j + 1
         Aij = Xsubb*Yprodj/(Fj*Fj2)
         Bij = Xsubc*Yprodj/Fj2
         Siij(1,j) = Fmu*Aij
         Aij = Aij + Bij
         IF ( j/=7 ) THEN
            k = 8 - j
            DO i = 2 , k
               Xprodi = Xsubc**i
               Fi = i
               Fij = i + j
               Aij = (Fi-1.0D0)*Xsubb*Aij/Fij
               Bij = Xprodi*Yprodj/(Fi*Fij)
               Siij(i,j) = Fmu*Aij
               Aij = Aij + Bij
            ENDDO
         ENDIF
!
      ENDDO
      Sizero = Siij(1,1)/3.0D0
!
!HUNK IN NUMBERS FOR (M-BAR-AA)    3X3 MATRIX AS PER MS-48, PAGES 6-10
!
!                    (M  )         3X6 MATRIX
!                      AR
!
!                    (M  )         6X6 MATRIX
!                      RR
!
!     (M-BAR-AA) MATRIX
!
      Mbaraa(1) = Siij(1,1)
      Mbaraa(2) = Siij(1,2)
      Mbaraa(3) = -Siij(2,1)
      Mbaraa(4) = Siij(1,2)
      Mbaraa(5) = Siij(1,3)
      Mbaraa(6) = -Siij(2,2)
      Mbaraa(7) = -Siij(2,1)
      Mbaraa(8) = -Siij(2,2)
      Mbaraa(9) = Siij(3,1)
!
!     (M  ) MATRIX
!       AR
      Mar(1) = Siij(3,1)
      Mar(2) = Siij(2,2)
      Mar(3) = Siij(1,3)
      Mar(4) = Siij(4,1)
      Mar(5) = Siij(2,3)
      Mar(6) = Siij(1,4)
      Mar(7) = Siij(3,2)
      Mar(8) = Siij(2,3)
      Mar(9) = Siij(1,4)
      Mar(10) = Siij(4,2)
      Mar(11) = Siij(2,4)
      Mar(12) = Siij(1,5)
      Mar(13) = -Siij(4,1)
      Mar(14) = -Siij(3,2)
      Mar(15) = -Siij(2,3)
      Mar(16) = -Siij(5,1)
      Mar(17) = -Siij(3,3)
      Mar(18) = -Siij(2,4)
!
!     (M  ) MATRIX  A 6X6 SYMMETRIC MATRIX
!       RR
      Mrr(1) = Siij(5,1)
      Mrr(2) = Siij(4,2)
      Mrr(3) = Siij(3,3)
      Mrr(4) = Siij(6,1)
      Mrr(5) = Siij(4,3)
      Mrr(6) = Siij(3,4)
      Mrr(7) = Mrr(2)
      Mrr(8) = Siij(3,3)
      Mrr(9) = Siij(2,4)
      Mrr(10) = Siij(5,2)
      Mrr(11) = Siij(3,4)
      Mrr(12) = Siij(2,5)
      Mrr(13) = Mrr(3)
      Mrr(14) = Mrr(9)
      Mrr(15) = Siij(1,5)
      Mrr(16) = Siij(4,3)
      Mrr(17) = Siij(2,5)
      Mrr(18) = Siij(1,6)
      Mrr(19) = Mrr(4)
      Mrr(20) = Mrr(10)
      Mrr(21) = Mrr(16)
      Mrr(22) = Siij(7,1)
      Mrr(23) = Siij(5,3)
      Mrr(24) = Siij(4,4)
      Mrr(25) = Mrr(5)
      Mrr(26) = Mrr(11)
      Mrr(27) = Mrr(17)
      Mrr(28) = Mrr(23)
      Mrr(29) = Siij(3,5)
      Mrr(30) = Siij(2,6)
      Mrr(31) = Mrr(6)
      Mrr(32) = Mrr(12)
      Mrr(33) = Mrr(18)
      Mrr(34) = Mrr(24)
      Mrr(35) = Mrr(30)
      Mrr(36) = Siij(1,7)
!
      IF ( T2/=0.0 ) THEN
         IF ( G2x211/=0.0E0 .OR. G2x212/=0.0E0 .OR. G2x222/=0.0E0 ) THEN
!
            Mar(4) = Mar(4) + Hyq(1)*Siij(2,1) + Hyq(4)*Siij(1,2)
            Mar(5) = Mar(5) + Hyq(2)*Siij(2,1) + Hyq(5)*Siij(1,2)
            Mar(6) = Mar(6) + Hyq(3)*Siij(2,1) + Hyq(6)*Siij(1,2)
            Mar(10) = Mar(10) + Hyq(1)*Siij(2,2) + Hyq(4)*Siij(1,3)
            Mar(11) = Mar(11) + Hyq(2)*Siij(2,2) + Hyq(5)*Siij(1,3)
            Mar(12) = Mar(12) + Hyq(3)*Siij(2,2) + Hyq(6)*Siij(1,3)
            Mar(16) = Mar(16) - Hyq(1)*Siij(3,1) - Hyq(4)*Siij(2,2)
            Mar(17) = Mar(17) - Hyq(2)*Siij(3,1) - Hyq(5)*Siij(2,2)
            Mar(18) = Mar(18) - Hyq(3)*Siij(3,1) - Hyq(6)*Siij(2,2)
            Mrr(4) = Mrr(4) + Hyq(1)*Siij(4,1) + Hyq(4)*Siij(3,2)
            Mrr(5) = Mrr(5) + Hyq(2)*Siij(4,1) + Hyq(5)*Siij(3,2)
            Mrr(6) = Mrr(6) + Hyq(3)*Siij(4,1) + Hyq(6)*Siij(3,2)
            Mrr(10) = Mrr(10) + Hyq(1)*Siij(3,2) + Hyq(4)*Siij(2,3)
            Mrr(11) = Mrr(11) + Hyq(2)*Siij(3,2) + Hyq(5)*Siij(2,3)
            Mrr(12) = Mrr(12) + Hyq(3)*Siij(3,2) + Hyq(6)*Siij(2,3)
            Mrr(16) = Mrr(16) + Hyq(1)*Siij(2,3) + Hyq(4)*Siij(1,4)
            Mrr(17) = Mrr(17) + Hyq(2)*Siij(2,3) + Hyq(5)*Siij(1,4)
            Mrr(18) = Mrr(18) + Hyq(3)*Siij(2,3) + Hyq(6)*Siij(1,4)
            Mrr(19) = Mrr(4)
            Mrr(20) = Mrr(10)
            Mrr(21) = Mrr(16)
            Mrr(22) = Mrr(22) + Hyq(1)*(Hyq(1)*Siij(3,1)+2.0D0*(Siij(5,1)+Hyq(4)*Siij(2,2))) + Hyq(4)                               &
                    & *(2.0D0*Siij(4,2)+Hyq(4)*Siij(1,3))
            Mrr(23) = Mrr(23) + Hyq(2)*Siij(5,1) + Hyq(5)*Siij(4,2) + Hyq(1)*(Siij(3,3)+Hyq(2)*Siij(3,1)+Hyq(5)*Siij(2,2)) + Hyq(4) &
                    & *(Siij(2,4)+Hyq(2)*Siij(2,2)+Hyq(5)*Siij(1,3))
            Mrr(24) = Mrr(24) + Hyq(3)*Siij(5,1) + Hyq(6)*Siij(4,2) + Hyq(1)*(Siij(2,4)+Hyq(3)*Siij(3,1)+Hyq(6)*Siij(2,2)) + Hyq(4) &
                    & *(Siij(1,5)+Hyq(3)*Siij(2,2)+Hyq(6)*Siij(1,3))
            Mrr(25) = Mrr(5)
            Mrr(26) = Mrr(11)
            Mrr(27) = Mrr(17)
            Mrr(28) = Mrr(23)
            Mrr(29) = Mrr(29) + Hyq(2)*(Hyq(2)*Siij(3,1)+2.0D0*(Siij(3,3)+Hyq(5)*Siij(2,2))) + Hyq(5)                               &
                    & *(2.0D0*Siij(2,4)+Hyq(5)*Siij(1,3))
            Mrr(30) = Mrr(30) + Hyq(3)*Siij(3,3) + Hyq(6)*Siij(2,4) + Hyq(2)*(Siij(2,4)+Hyq(3)*Siij(3,1)+Hyq(6)*Siij(2,2)) + Hyq(5) &
                    & *(Siij(1,5)+Hyq(3)*Siij(2,2)+Hyq(6)*Siij(1,3))
            Mrr(31) = Mrr(6)
            Mrr(32) = Mrr(12)
            Mrr(33) = Mrr(18)
            Mrr(34) = Mrr(24)
            Mrr(35) = Mrr(30)
            Mrr(36) = Mrr(36) + Hyq(3)*(Hyq(3)*Siij(3,1)+2.0D0*(Siij(2,4)+Hyq(6)*Siij(2,2))) + Hyq(6)                               &
                    & *(2.0D0*Siij(1,5)+Hyq(6)*Siij(1,3))
         ENDIF
      ENDIF
!
!
!
!     FILL S-MATRIX EQUIVALENCED TO A(82)  (S IS  6X3 )
!
      S(1) = 1.0D0
      S(2) = 0.0D0
      S(3) = -Xsubb
      S(4) = 0.0D0
      S(5) = 1.0D0
      S(6) = 0.0D0
      S(7) = 0.0D0
      S(8) = 0.0D0
      S(9) = 1.0D0
      S(10) = 1.0D0
      S(11) = Ysubc
      S(12) = -Xsubc
      S(13) = 0.0D0
      S(14) = 1.0D0
      S(15) = 0.0D0
      S(16) = 0.0D0
      S(17) = 0.0D0
      S(18) = 1.0D0
!
!AN NOW COMPUTE 9 (3X3) MASS MATRICES (FMMS-66, PAGES 10-11)
!
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
!HOOSE APPROPRIATE BLOCK OF A-ARRAY FOR STORAGE
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
!               -1 T          -1
!OMPUTE (M) = (H  )  ((M  ) (H  ))
!                       RR
!
      CALL gmmatd(Mrr(1),6,6,0,A(100),6,6,0,A(37))
      CALL gmmatd(A(100),6,6,1,A(37),6,6,0,A(1))
!
!REATE PARTITION OF 4 (3X3)
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
!OMPUTE                 -1
!       (M  ) = (M  ) (H  )    AND  PARTITION INTO 2 (3X3)  (M-BAR-AB)
!         AI      AR                                    AND (M-BAR-AC)
!
      CALL gmmatd(Mar(1),3,6,0,A(100),6,6,0,A(181))
      DO i = 1 , 3
         A(i+162) = A(i+180)
         A(i+165) = A(i+186)
         A(i+168) = A(i+192)
!
         A(i+171) = A(i+183)
         A(i+174) = A(i+189)
         A(i+177) = A(i+195)
      ENDDO
!OMPUTE (MAB)
      CALL gmmatd(S(1),3,3,1,A(37),3,3,0,A(181))
      CALL gmmatd(S(10),3,3,1,A(64),3,3,0,A(190))
      DO i = 1 , 9
         A(i+9) = A(i+162) - A(i+180) - A(i+189)
      ENDDO
!OMPUTE (MAC)
      CALL gmmatd(S(1),3,3,1,A(46),3,3,0,A(181))
      CALL gmmatd(S(10),3,3,1,A(73),3,3,0,A(190))
      DO i = 1 , 9
         A(i+18) = A(i+171) - A(i+180) - A(i+189)
      ENDDO
!OMPUTE (MAA)
      CALL gmmatd(S(1),3,3,1,A(10),3,3,1,A(181))
      CALL gmmatd(S(10),3,3,1,A(19),3,3,1,A(190))
      CALL gmmatd(A(163),3,3,0,S(1),3,3,0,A(199))
      CALL gmmatd(A(172),3,3,0,S(10),3,3,0,A(208))
      DO i = 1 , 9
         A(i) = Mbaraa(i) - A(i+180) - A(i+189) - A(i+198) - A(i+207)
      ENDDO
!OMPUTE (MBA) AND (MCA)
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
   ELSE
!
!
!     ISING = 2 IMPLIES SINGULAR MATRIX THUS ERROR CONDITION.
      CALL mesage(30,33,Ecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
      Nogo = 1
      RETURN
   ENDIF
!
!
END SUBROUTINE mtrbsc