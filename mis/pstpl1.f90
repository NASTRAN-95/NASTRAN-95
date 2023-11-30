
SUBROUTINE pstpl1
   IMPLICIT NONE
   REAL A(45) , C1 , C2 , Consts(5) , Cosang , Costh , D1(3) , D2(3) , Degra , Determ , Dum10 , Dum11 , Dum12(29) , Dum8 , Dum9 ,   &
      & E(18) , Ecpt(100) , Eltemp , Habc(18) , Hinv(36) , Hq(12) , Ivect(3) , Jvect(3) , Kvect(3) , L1 , L2 , Ph1out(200) ,        &
      & Prod12(12) , Prod15(15) , Prod9(9) , R(2,4) , Requiv(9) , S(18) , S1 , S2 , Sinang , Sinth , Ssum(60) , Stress , T(9) ,     &
      & Temp , Temp1 , Temp2 , Temp9(9) , Theta , Tite(10) , U1 , U2 , V(25) , V1(3) , V2(3) , V3(3) , Vv1(2) , Vv2(2) , X1 , X2 ,  &
      & Xc , Xsubb , Xsubc , Y1 , Y2 , Yc , Ysubc
   INTEGER Inflag , Ising , Km , Matid , Necpt(25) , Npoint , Nsubc , Subsca , Subscb , Subscc
   COMMON /condas/ Consts
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /pla32s/ A , T , S , Hinv , Prod12 , D1 , D2 , Habc , Ssum , R , Ivect , Jvect , Kvect , Vv1 , Vv2 , Xsubb , Xsubc ,      &
                 & Ysubc , E , Temp , L1 , L2 , C1 , C2 , S1 , S2 , X1 , X2 , Y1 , Y2 , Npoint , Dum9 , Temp1 , Temp2 , Prod9 ,     &
                 & Temp9 , Dum8 , Km , Subsca , Subscb , Subscc , Dum11 , Theta , Nsubc , Ising , U1 , U2 , Sinang , Cosang ,       &
                 & Dum10 , Xc , Yc , Determ , Dum12
   COMMON /pla3es/ Ecpt , Ph1out
   REAL g(36) , temp15(15)
   INTEGER i , j , k , m(9)
!
!     THIS ROUTINE CALCULATES PHASE I OUTPUT FOR PLA3
!     FOR THE TRI-PLATE PART OF COMBINATION ELEMENTS
!
!     PHASE I OF STRESS DATA RECOVERY FOR TRI-PLATE
!
!     OUTPUTS FROM THIS PHASE FOR USE IN PHASE II ARE THE FOLLOWING.
!
!     1) ELEMENT ID
!     2) 3 SILS AND A DUMMY
!     3) I
!     4) Z1 AND Z2
!     5) 3  5X6 S-SUB-I ARRAYS
!     THUS, 98 WORDS FOR THE TRI-PLATE
!
!
!     ECPT LISTS AS OF AUGUST 4, 1967
!
!                 DEFINITION
!       ECPT      BSC.BEND.TRI. AND THE TRI-PLATE
!     ========   =================  =======
!     ECPT( 1) = ELEMENT ID         INTEGER
!     ECPT( 2) = GRID PT. A         INTEGER
!     ECPT( 3) = GRID PT. B         INTEGER
!     ECPT( 4) = GRID PT. C         INTEGER
!     ECPT( 5) = THETA              REAL
!     ECPT( 6) = MAT ID 1           INTEGER
!     ECPT( 7) = I  MOM. OF INERT.  REAL
!     ECPT( 8) = MAT ID 2           INTEGER
!     ECPT( 9) = T2                 REAL
!     ECPT(10) = NON-STRUCT. MASS   REAL
!     ECPT(11) = Z1                 REAL
!     ECPT(12) = Z2                 REAL
!     ECPT(13) = COORD. SYS. ID 1   INTEGER
!     ECPT(14) = X1                 REAL
!     ECPT(15) = Y1                 REAL
!     ECPT(16) = Z1                 REAL
!     ECPT(17) = COORD. SYS. ID 2   INTEGER
!     ECPT(18) = X2                 REAL
!     ECPT(19) = Y2                 REAL
!     ECPT(20) = Z2                 REAL
!     ECPT(21) = COORD. SYS. ID 3   INTEGER
!     ECPT(22) = X3                 REAL
!     ECPT(23) = Y3                 REAL
!     ECPT(24) = Z3                 REAL
!     ECPT(25) = ELEMENT TEMP       REAL
!
   !>>>>EQUIVALENCE (Consts(4),Degra) , (Prod15(1),Prod9(1)) , (Requiv(1),R(1,1)) , (Necpt(1),Ecpt(1)) , (Ecpt(14),V1(1)) ,              &
!>>>>    & (V2(1),Ecpt(18)) , (Ecpt(22),V3(1)) , (Tite(1),A(1)) , (Prod12(1),V(1)) , (Hq(1),A(1))
   DATA m/1 , 2 , 4 , 2 , 3 , 4 , 3 , 1 , 4/
!
   Theta = Ecpt(5)*Degra
   Sinang = sin(Theta)
   Cosang = cos(Theta)
!
!     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
!     SUB TRIANGLES. (2X4) FOR THE TRIANGULAR PLATE.
!     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
!
!     ZERO OUT R-MATRIX
!
   DO i = 1 , 8
      Requiv(i) = 0.0
   ENDDO
!
   DO i = 1 , 3
      D2(i) = V2(i) - V1(i)
      D1(i) = V3(i) - V1(i)
   ENDDO
!
!     X2  GOES IN R(1,2)
!
   R(1,2) = sqrt(D2(1)**2+D2(2)**2+D2(3)**2)
   DO i = 1 , 3
      Ivect(i) = D2(i)/R(1,2)
   ENDDO
!
!     NON-NORMALIZED K-VECTOR
!
   Kvect(1) = Ivect(2)*D1(3) - D1(2)*Ivect(3)
   Kvect(2) = Ivect(3)*D1(1) - D1(3)*Ivect(1)
   Kvect(3) = Ivect(1)*D1(2) - D1(1)*Ivect(2)
!
!     Y3 GOES INTO R(2,3)
!
   R(2,3) = sqrt(Kvect(1)**2+Kvect(2)**2+Kvect(3)**2)
   DO i = 1 , 3
      Kvect(i) = Kvect(i)/R(2,3)
   ENDDO
!
!     J-VECTOR = K X I  VECTORS
!
   Jvect(1) = Kvect(2)*Ivect(3) - Ivect(2)*Kvect(3)
   Jvect(2) = Kvect(3)*Ivect(1) - Ivect(3)*Kvect(1)
   Jvect(3) = Kvect(1)*Ivect(2) - Ivect(1)*Kvect(2)
!
!     NORMALIZE J VECTOR TO MAKE SURE
!
   Temp = sqrt(Jvect(1)**2+Jvect(2)**2+Jvect(3)**2)
   DO i = 1 , 3
      Jvect(i) = Jvect(i)/Temp
   ENDDO
!
!     X3 GOES INTO R(1,3) = D1 DOT IVECT
!
   R(1,3) = D1(1)*Ivect(1) + D1(2)*Ivect(2) + D1(3)*Ivect(3)
!
!     CENTROID POINT GOES INTO R(1,4) AND R(2,4)
!
   R(1,4) = (R(1,2)+R(1,3))/3.0
   R(2,4) = R(2,3)/3.0
!
!     COMPUTE SUB-TRIANGLE COORDINATES
!     CALL BASIC BENDING ROUTINE FOR ALL SUB-TRIANGLES.
!
   DO i = 1 , 60
      Ssum(i) = 0.0
   ENDDO
   DO i = 1 , 36
      g(i) = 0.0
   ENDDO
!
   DO j = 1 , 3
      Km = 3*j - 3
      Subsca = m(Km+1)
      Subscb = m(Km+2)
      Subscc = m(Km+3)
!
      DO i = 1 , 2
         Vv1(i) = R(i,Subscb) - R(i,Subsca)
         Vv2(i) = R(i,Subscc) - R(i,Subsca)
      ENDDO
      Xsubb = sqrt(Vv1(1)**2+Vv1(2)**2)
      U1 = Vv1(1)/Xsubb
      U2 = Vv1(2)/Xsubb
      Xsubc = U1*Vv2(1) + Vv2(2)*U2
      Ysubc = U1*Vv2(2) - Vv2(1)*U2
!
      Xc = Xsubc
      Yc = Ysubc
!
      Sinth = Sinang*U1 - Cosang*U2
      Costh = Cosang*U1 + Sinang*U2
      IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0
!
!     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR
!     TRIANGLE -J-
!
      CALL pstrb1(2)
!
!     RETURNING FROM PSTRB1 THE FOLLOWING QUANTITIES ARE AT HAND.
!
!       S , S , S , EACH 5X3.   45 WORDS STORED IN A( 1) THRU A(45)
!        A   B   C
!
!     AND ALSO H-INVERSE IS AT A(73) THRU A(108) AND S IS AT A(55) THRU
!     A(72)
!
!     SET UP OF T-MATRIX
!
      T(1) = 1.0
      T(2) = 0.0
      T(3) = 0.0
      T(4) = 0.0
      T(5) = U1
      T(6) = U2
      T(7) = 0.0
      T(8) = -U2
      T(9) = U1
!
!     SET UP V-MATRIX PER FMMS 51-A
!
      V(1) = U1*U1/3.0
      V(2) = U2*U2/3.0
      V(11) = U1*U2/3.0
      V(3) = -V(11)*2.0
      V(4) = 0.0
      V(5) = 0.0
      V(6) = V(2)
      V(7) = V(1)
      V(8) = -V(3)
      V(9) = 0.0
      V(10) = 0.0
      V(12) = -V(11)
      V(13) = V(1) - V(2)
      V(14) = 0.0
      V(15) = 0.0
      V(16) = 0.0
      V(17) = 0.0
      V(18) = 0.0
      V(19) = U1/3.0
      V(20) = -U2/3.0
      V(21) = 0.0
      V(22) = 0.0
      V(23) = 0.0
      V(24) = -V(20)
      V(25) = V(19)
!
!     ADD IN S , S , S   TO THE 4 5X3 SSUM MATRICES
!             A   B   C
!
      DO i = 1 , 3
         CALL gmmats(V(1),5,5,0,A(15*i-14),5,3,0,temp15(1))
         CALL gmmats(temp15(1),5,3,0,T(1),3,3,0,Prod15(1))
!
!     POINTER TO SSUM MATRIX
!
         Npoint = Km + i
         Npoint = 15*m(Npoint) - 15
         DO k = 1 , 15
            Nsubc = Npoint + k
            Ssum(Nsubc) = Ssum(Nsubc) + Prod15(k)
         ENDDO
      ENDDO
!
!     FORM HQ (2X6)
!
      Temp1 = Xsubb - Xsubc
      Temp2 = Ysubc**2
      L1 = sqrt(Xsubc**2+Temp2)
      L2 = sqrt(Temp1**2+Temp2)
      S1 = Xsubc/L1
      S2 = Temp1/L2
      C1 = Ysubc/L1
      C2 = Ysubc/L2
      X1 = Xsubc/2.0
      Y1 = Ysubc/2.0
      X2 = (Xsubb+Xsubc)/2.0
      Y2 = Y1
      Hq(1) = -Xsubc*C1
      Hq(2) = X1*S1 - Y1*C1
      Hq(3) = 2.0*Y1*S1
      Hq(4) = -3.0*X1*X1*C1
      Hq(5) = Y1*(2.0*X1*S1-Y1*C1)
      Hq(6) = 3.0*Y1*Y1*S1
      Hq(7) = 2.0*X2*C2
      Hq(8) = X2*S2 + Y2*C2
      Hq(9) = 2.0*Y2*S2
      Hq(10) = 3.0*X2*X2*C2
      Hq(11) = Y2*(2.0*X2*S2+Y2*C2)
      Hq(12) = 3.0*Y2*Y2*S2
!
!                      I                    -1
!     COMPUTE (H       I  H     )  = (HQ)(H)    STORE IN PROD12
!               PSI,B  I   PSI,C
!                      I
!
      CALL gmmats(Hq(1),2,6,0,Hinv(1),6,6,0,Prod12(1))
!
!     COMPUTE (H     ) = -(PROD12)(S)
!               PSI,A
!
      CALL gmmats(Prod12(1),2,6,0,S(1),6,3,0,Habc(1))
      Habc(1) = -Habc(1)
      Habc(2) = -Habc(2) + S1
      Habc(3) = -Habc(3) + C1
      Habc(4) = -Habc(4)
      Habc(5) = -Habc(5) + S2
      Habc(6) = -Habc(6) - C2
!
!     SPLIT(H     ) AND (H     )  PARTITION
!            PSI,B        PSI,C
!
      Habc(7) = Prod12(1)
      Habc(8) = Prod12(2)
      Habc(9) = Prod12(3)
      Habc(10) = Prod12(7)
      Habc(11) = Prod12(8)
      Habc(12) = Prod12(9)
      Habc(13) = Prod12(4)
      Habc(14) = Prod12(5)
      Habc(15) = Prod12(6)
      Habc(16) = Prod12(10)
      Habc(17) = Prod12(11)
      Habc(18) = Prod12(12)
!
!     MAP  H , H , AND H  INTO THE G-MATRICES.
!           A   B       C
!
      DO i = 1 , 3
!
!     POINTER TO H  = 6*I-6
!                 I
!
!     TRANSFORM H SUB I
!
         CALL gmmats(Habc(6*i-5),2,3,0,T(1),3,3,0,Temp9(1))
!
         Npoint = Km + i
         Npoint = 9*m(Npoint) - 9
!
!     J = 1  ROW 1 OF H INTO ROW 1 OF G.
!            ROW 2 OF H INTO ROW 2 OF G.
!     J = 2  ROW 1 OF H INTO ROW 2 OF G.
!            ROW 2 OF H INTO ROW 3 OF G.
!     J = 3  ROW 1 OF H INTO ROW 3 OF G.
!            ROW 2 OF H INTO ROW 1 OF G.
!
         IF ( j<2 ) THEN
         ELSEIF ( j==2 ) THEN
!
            Npoint = Npoint + 3
         ELSE
            g(Npoint+7) = g(Npoint+7) + Temp9(1)
            g(Npoint+8) = g(Npoint+8) + Temp9(2)
            g(Npoint+9) = g(Npoint+9) + Temp9(3)
            g(Npoint+1) = g(Npoint+1) + Temp9(4)
            g(Npoint+2) = g(Npoint+2) + Temp9(5)
            g(Npoint+3) = g(Npoint+3) + Temp9(6)
            CYCLE
         ENDIF
         DO k = 1 , 6
            Npoint = Npoint + 1
            g(Npoint) = g(Npoint) + Temp9(k)
         ENDDO
!
      ENDDO
   ENDDO
!
!     FILL E-MATRIX
!
   DO i = 1 , 18
      E(i) = 0.0
   ENDDO
   E(1) = Kvect(1)
   E(4) = Kvect(2)
   E(7) = Kvect(3)
   E(11) = Ivect(1)
   E(14) = Ivect(2)
   E(17) = Ivect(3)
   E(12) = Jvect(1)
   E(15) = Jvect(2)
   E(18) = Jvect(3)
!
!               *         *     -1
!     (S ) = (S  )  -  (S  )(G )  (G )           I=A,B,C
!       I      I         4    4     I
!
!        E            T                  T
!     (S  ) = (S ) (E) (C ) = (S ) (TITE)    I=A,B,C
!       I       I        I      I
!
!                                 *     -1
!     FIRST GET COMMON PRODUCT (S  )(G )
!                                4    4
!
!     INVERT  (G )  STORE INVERSE BACK INTO  (G )
!               4                              4
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
   Ising = -1
   CALL invers(3,g(28),3,Prod9(1),0,Determ,Ising,Temp9(1))
!
!     CHECK FOR SINGULARITY.  ISING = 2 IMPLIES SINGULARITY
!
   IF ( Ising/=1 ) CALL mesage(-30,36,Ecpt(1))
!
   CALL gmmats(Ssum(46),5,3,0,g(28),3,3,0,Prod15(1))
!
   DO i = 1 , 3
!
!    (PROD15)(G )
!              I
!
      CALL gmmats(Prod15(1),5,3,0,g(9*i-8),3,3,0,temp15(1))
!
!     SUBTRACT TEMP15 FROM S
!                          I
!
      Npoint = 15*i - 15
      DO k = 1 , 15
         Npoint = Npoint + 1
         Ssum(Npoint) = Ssum(Npoint) - temp15(k)
      ENDDO
!
!     DO WE NEED TRANSFORMATION T
!                                I
      Nsubc = 4*i + 9
      IF ( Necpt(Nsubc)==0 ) THEN
!
         DO k = 1 , 18
            Tite(k) = E(k)
         ENDDO
      ELSE
         CALL transs(Necpt(Nsubc),T(1))
         CALL gmmats(T(1),3,3,1,E(1),3,3,0,Tite(1))
         CALL gmmats(T(1),3,3,1,E(10),3,3,0,Tite(10))
      ENDIF
!
      CALL gmmats(Ssum(15*i-14),5,3,0,Tite(1),6,3,1,Ph1out(30*i-21))
!
   ENDDO
!
!     I, Z1, Z2, ELEM ID, 3 SILS FOR PHASE 2.  PH1OUT(5) IS A DUMMY
!
   Ph1out(1) = Ecpt(1)
   Ph1out(2) = Ecpt(2)
   Ph1out(3) = Ecpt(3)
   Ph1out(4) = Ecpt(4)
   Ph1out(6) = Ecpt(7)
   Ph1out(7) = Ecpt(11)
   Ph1out(8) = Ecpt(12)
!
!     ALL PHASE ONE COMPLETE
!
END SUBROUTINE pstpl1