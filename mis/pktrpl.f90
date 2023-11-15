
SUBROUTINE pktrpl
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION A(81) , Arr9(9) , Array9(9) , C1 , C2 , D1(3) , D2(3) , Determ , E(18) , Habc(18) , Hinv(36) , Hq(12) , Ivect(3)&
                  & , Jvect(3) , Kout(36) , Ksum(63) , Kvect(3) , L1 , L2 , Prod12(12) , Prod9(9) , S(18) , S1 , S2 , T(9) , Temp , &
                  & Temp1 , Temp18(18) , Temp2 , Temp9(9) , Tite(18) , Tjte(18) , U1 , U2 , V(2) , Vv(2) , X1 , X2 , Xsubb , Xsubc ,&
                  & Y1 , Y2 , Ysubc
   REAL Alp12 , Alpha1 , Alpha2 , Consts(5) , Cosang , Costh , Degra , Dum1(3) , Dumcl(145) , Dumtwo(2) , Ecpt(100) , Eltemp , G11 ,&
      & G12 , G13 , G22 , G23 , G2x211 , G2x212 , G2x222 , G33 , Gsube , Rho , Sigcom , Sigshe , Sigten , Sinang , Sinth , Stress , &
      & Theta , Tsub0 , V1(3) , V2(3) , V3(3)
   INTEGER Inflag , Ising , Km , Matid , Necpt(100) , Nogo , Npivot , Npoint , Npt1 , Npvt , Nsubc , Subsca , Subscb , Subscc
   COMMON /condas/ Consts
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alpha1 , Alpha2 , Alp12 , Tsub0 , Gsube , Sigten , Sigcom , Sigshe ,   &
                 & G2x211 , G2x212 , G2x222
   COMMON /pla42c/ Npvt , Dum1 , Dumcl , Nogo
   COMMON /pla42d/ A , S , T , Temp9 , Prod9 , Arr9 , Array9 , Hinv , Ksum , Xsubb , Xsubc , Ysubc , E , Temp , L1 , L2 , S1 , S2 , &
                 & C1 , C2 , X1 , X2 , Y1 , Y2 , Temp1 , Temp2 , Dumtwo , Determ , Npoint , Km , Subsca , Subscb , Subscc , Npivot ,&
                 & Theta , Nsubc , Ising , Npt1 , V , Vv , Ivect , Jvect , Kvect , U1 , U2 , Sinang , Cosang
   COMMON /pla4es/ Ecpt
!
! Local variable declarations
!
   DOUBLE PRECISION g(36) , r(2,4) , requiv(8)
   INTEGER i , j , k , m(9) , nbegin
!
! End of declarations
!
!  THIR ROUTINE CALCULATES THE STIFFNESS MATRIX FOR TRI-PLATES IN  PLA4
!
!     THIS ROUTINE GENERATES THE FOLLOWING
!
!                             3-6X6 STIFFNESS MATRICES WITH RESPECT
!                             TO ONE PIVOT POINT OF A TRIANGULAR PLATE
!                             ELEMENT.
!
!         REF. FMMS-55  NOVEMBER 1ST, 1967
!
!         CALLS FROM THIS ROUTINE ARE MADE TO
!                             PKTRBS - BASIC BENDING TRIANGLE
!                             TRANSD - SUPPLIES 3X3 TRANSFORMATIONS
!                             INVERD - MATRIX INVERSION ROUTINE
!                             PLA4B  - INSERTION ROUTINE
!                             GMMATD - GENERAL MATRIX MULITPLY AND
!                                      TRANSPOSE ROUTINE
!                             MESAGE - ERROR MESSAGE WRITER
!
!
!     ******************************************************************
!
!     ECPT LISTS AS OF AUGUST 4, 1967
!
!                 DEFINITION
!       ECPT      TRI.PLATE AND BASIC BENDING TRI.
!     ******************************************************************
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
!     ******************************************************************
!
!
   EQUIVALENCE (Consts(4),Degra)
   EQUIVALENCE (Necpt(1),Ecpt(1)) , (Prod12(1),A(13)) , (Habc(1),A(25)) , (Tite(1),A(37)) , (Tjte(1),A(55)) , (Kout(1),A(1)) ,      &
    & (Temp18(1),Hinv(1)) , (V1(1),Ecpt(14)) , (V2(1),Ecpt(18)) , (V3(1),Ecpt(22)) , (requiv(1),r(1,1)) , (D1(1),A(1)) ,            &
    & (D2(1),A(4)) , (Hq(1),A(1))
!
   DATA m/1 , 2 , 4 , 2 , 3 , 4 , 3 , 1 , 4/
!
!     DETERMINE PIVOT POINT NUMBER
!
   DO i = 1 , 3
      IF ( Npvt==Necpt(i+1) ) THEN
         Npivot = i
         GOTO 100
      ENDIF
   ENDDO
!
!
!     FALL THRU ABOVE LOOP IMPLIES ERROR CONDITION
   CALL mesage(-30,34,Ecpt(1))
!
 100  Theta = Ecpt(5)*Degra
   Sinang = sin(Theta)
   Cosang = cos(Theta)
!     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
!     SUB TRIANGLES. (2X4) FOR TRIANGULAR PLATE. (COLUMN 4 BLANK)
!     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
!
!     ZERO OUT R-MATRIX
   DO i = 1 , 8
      requiv(i) = 0.0D0
   ENDDO
!
   DO i = 1 , 3
      D2(i) = dble(V2(i)) - dble(V1(i))
      D1(i) = dble(V3(i)) - dble(V1(i))
   ENDDO
!
!     X2  GOES IN R(1,2)
   r(1,2) = dsqrt(D2(1)**2+D2(2)**2+D2(3)**2)
   IF ( r(1,2)/=0.0D0 ) THEN
      DO i = 1 , 3
         Ivect(i) = D2(i)/r(1,2)
      ENDDO
!
!     NON-NORMALIZED K-VECTOR
      Kvect(1) = Ivect(2)*D1(3) - D1(2)*Ivect(3)
      Kvect(2) = Ivect(3)*D1(1) - D1(3)*Ivect(1)
      Kvect(3) = Ivect(1)*D1(2) - D1(1)*Ivect(2)
!
!     Y3 GOES INTO R(2,3)
      r(2,3) = dsqrt(Kvect(1)**2+Kvect(2)**2+Kvect(3)**2)
      IF ( r(2,3)/=0.0D0 ) THEN
         DO i = 1 , 3
            Kvect(i) = Kvect(i)/r(2,3)
         ENDDO
!
!     J-VECTOR = K X I  VECTORS
         Jvect(1) = Kvect(2)*Ivect(3) - Ivect(2)*Kvect(3)
         Jvect(2) = Kvect(3)*Ivect(1) - Ivect(3)*Kvect(1)
         Jvect(3) = Kvect(1)*Ivect(2) - Ivect(1)*Kvect(2)
!     NORMALIZE J VECTOR TO MAKE SURE
         Temp = dsqrt(Jvect(1)**2+Jvect(2)**2+Jvect(3)**2)
         IF ( Temp/=0.0D0 ) THEN
            DO i = 1 , 3
               Jvect(i) = Jvect(i)/Temp
            ENDDO
!     X3 GOES INTO R(1,3) = D1 DOT IVECT
            r(1,3) = D1(1)*Ivect(1) + D1(2)*Ivect(2) + D1(3)*Ivect(3)
!
!     CENTROID POINT GOES INTO R(1,4) AND R(2,4)
            r(1,4) = (r(1,2)+r(1,3))/3.0D0
            r(2,4) = r(2,3)/3.0D0
!     ******************************************************************
!            THE COORDINATES AND CENTROID OF THE PLATE IN THE ELEMENT
!     SYSTEM ARE STORED IN THE R-MATRIX WHERE THE COLUMN DENOTES THE
!     POINT AND THE ROW DENOTES THE X OR Y COORDINATE FOR ROW 1 OR
!     ROW 2 RESPECTIVELY.
!     ******************************************************************
!
!     SET UP THE M-MATRIX FOR MAPPING TRIANGLES, IN DATA STATEMENT.
!
!     ******************************************************************
!     ZERO OUT THE KSUM MATRIX FOR 63 AND THE GSUM MATRIX FOR 36...
!
            DO i = 1 , 63
               Ksum(i) = 0.0D0
            ENDDO
            DO i = 1 , 36
               g(i) = 0.0D0
            ENDDO
!
!
            DO j = 1 , 3
               Km = 3*j - 3
!
               Subsca = m(Km+1)
               Subscb = m(Km+2)
               Subscc = m(Km+3)
!
               DO i = 1 , 2
                  V(i) = r(i,Subscb) - r(i,Subsca)
                  Vv(i) = r(i,Subscc) - r(i,Subsca)
               ENDDO
               Xsubb = dsqrt(V(1)**2+V(2)**2)
               U1 = V(1)/Xsubb
               U2 = V(2)/Xsubb
               Xsubc = U1*Vv(1) + U2*Vv(2)
               Ysubc = U1*Vv(2) - U2*Vv(1)
!
               Sinth = Sinang*U1 - Cosang*U2
               Costh = Cosang*U1 + Sinang*U2
               IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0E0
!
!     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR
!     TRIANGLE -J-
!
               CALL pktrbs(2)
!                         U
!     NOW HAVE AT HAND  K    I,J, =1,2,3.   9-3X3 MATRICES STORED AT
!                        IJ                 A(1) THROUGH A(81).
!
!           -1
!     ALSO H   (6X6) AT A(145) TO A(181) AND S (6X3) AT A(82) TO A(99)
!
!     NOW ADD CERTAIN OF THESE INTO THE SUMMED MATRICES
!
!
!     SET UP OF T-MATRIX
!
               T(1) = 1.0D0
               T(2) = 0.0D0
               T(3) = 0.0D0
               T(4) = 0.0D0
               T(5) = U1
               T(6) = U2
               T(7) = 0.0D0
               T(8) = -U2
               T(9) = U1
!
               DO i = 1 , 3
                  CALL gmmatd(T(1),3,3,1,A(27*i-8),3,3,0,Temp9(1))
                  CALL gmmatd(Temp9(1),3,3,0,T(1),3,3,0,Prod9(1))
!
!     ADD THIS PRODUCT IN NOW.
!     COMPUTE POINTER TO KSUM MATRIX DESIRED.  (ZERO POINTER)
                  Npoint = Km + i
                  Npoint = 9*m(Npoint) + 18
!
                  DO k = 1 , 9
                     Nsubc = Npoint + k
                     Ksum(Nsubc) = Ksum(Nsubc) + Prod9(k)
                  ENDDO
               ENDDO
               DO k = 1 , 2
                  Npoint = Km + k
                  IF ( m(Npoint)==Npivot ) THEN
                     CALL gmmatd(T(1),3,3,1,A(36*k-35),3,3,0,Temp9(1))
                     CALL gmmatd(Temp9(1),3,3,0,T(1),3,3,0,Prod9(1))
!
!     COMPUTE POINTER TO KSUM MATRIX (ZERO POINTER)
!
                     Npoint = 9*Npivot - 9
                     DO i = 1 , 9
                        Nsubc = Npoint + i
                        Ksum(Nsubc) = Ksum(Nsubc) + Prod9(i)
                     ENDDO
!
                     CALL gmmatd(T(1),3,3,1,A(18*k-8),3,3,0,Temp9(1))
                     CALL gmmatd(Temp9(1),3,3,0,T(1),3,3,0,Prod9(1))
!
!     COMPUTE ZERO POINTER TO KSUM MATRIX DESIRED
!
                     Npoint = Km + 3 - k
                     Npoint = 9*m(Npoint) - 9
                     DO i = 1 , 9
                        Nsubc = Npoint + i
                        Ksum(Nsubc) = Ksum(Nsubc) + Prod9(i)
                     ENDDO
                  ENDIF
               ENDDO
!
!     FORM HQ (2X6)
!
               Temp1 = Xsubb - Xsubc
               Temp2 = Ysubc**2
               L1 = dsqrt(Xsubc**2+Temp2)
               L2 = dsqrt(Temp1**2+Temp2)
               S1 = Xsubc/L1
               S2 = Temp1/L2
               C1 = Ysubc/L1
               C2 = Ysubc/L2
               X1 = Xsubc/2.0D0
               Y1 = Ysubc/2.0D0
               X2 = (Xsubb+Xsubc)/2.0D0
               Y2 = Y1
               Hq(1) = -Xsubc*C1
               Hq(2) = X1*S1 - Y1*C1
               Hq(3) = 2.0D0*Y1*S1
               Hq(4) = -3.0D0*X1*X1*C1
               Hq(5) = Y1*(2.0D0*X1*S1-Y1*C1)
               Hq(6) = 3.0D0*Y1*Y1*S1
               Hq(7) = 2.0D0*X2*C2
               Hq(8) = X2*S2 + Y2*C2
               Hq(9) = 2.0D0*Y2*S2
               Hq(10) = 3.0D0*X2*X2*C2
               Hq(11) = Y2*(2.0D0*X2*S2+Y2*C2)
               Hq(12) = 3.0D0*Y2*Y2*S2
!
!                      I                    -1
!     COMPUTE (H       I  H     )  = (HQ)(H)    STORE IN PROD12
!               PSI,B  I   PSI,C
!                      I
!
!
               CALL gmmatd(Hq(1),2,6,0,Hinv(1),6,6,0,Prod12(1))
!
!
!     COMPUTE (H     ) = -(PROD12)(S)
!               PSI,A
!
               CALL gmmatd(Prod12(1),2,6,0,S(1),6,3,0,Habc(1))
!
               Habc(1) = -Habc(1)
               Habc(2) = -Habc(2) + S1
               Habc(3) = -Habc(3) + C1
               Habc(4) = -Habc(4)
               Habc(5) = -Habc(5) + S2
               Habc(6) = -Habc(6) - C2
!
!     SPLIT (H     ) AND (H     )    PARTITION
!             PSI,B        PSI,C
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
!     TRIANGLE NUMBER = J, THE THREE POINTS ARE SUBSCA, SUBSCB, SUBSCC.
!
               DO i = 1 , 3
!
!     POINTER TO H  = 6*I-6
!                 I
!
!
!     TRANSFORM H SUB I
!
                  CALL gmmatd(Habc(6*i-5),2,3,0,T(1),3,3,0,Temp9(1))
!
!
                  Npoint = Km + i
                  Npoint = 9*m(Npoint) - 9
!
!     J = 1    ROW 1 OF H INTO ROW 1 OF G.
!              ROW 2 OF H INTO ROW 2 OF G.
!     J = 2    ROW 1 OF H INTO ROW 2 OF G.
!              ROW 2 OF H INTO ROW 3 OF G.
!     J = 3    ROW 1 OF H INTO ROW 3 OF G.
!              ROW 2 OF H INTO ROW 1 OF G.
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
!
!
!     END OF LOOP FOR BASIC TRIANGLES
!
            ENDDO
!     ******************************************************************
!
!     FILL E-MATRIX
!
            DO i = 1 , 18
               E(i) = 0.0D0
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
!              T
!     FORM   T   E      STORE IN TITE-MATRIX (6X3)
!             I
!
            IF ( Necpt(4*Npivot+9)==0 ) THEN
               DO k = 1 , 18
                  Tite(k) = E(k)
               ENDDO
            ELSE
               CALL transd(Necpt(4*Npivot+9),T(1))
               CALL gmmatd(T(1),3,3,1,E(1),3,3,0,Tite(1))
               CALL gmmatd(T(1),3,3,1,E(10),3,3,0,Tite(10))
            ENDIF
!
!     SOLVE NOW FOR ....
!
!    E                   T     T                       T
! (K  ) = (K  ) - (TERM ) (K  ) - (K  )(TERM ) + (TERM )(K  )(TERM )
!   IJ      IJ         I    J4      I4      J         I   44      J
!
!                        -1                               I=NPIVOT
! WHERE... (TERM ) = (G )  (G ) ,I=NPIVOT                 J=1,2,3
!               I      4     I
!
!                        -1
!          (TERM ) = (G )  (G ) ,J=1,2,3 AS ABOVE
!               J      4     J
!
!     AND WITH TRANSFORMATIONS....
!
!    G        T      E   T
! (K  ) = (C ) (E)(K  )(E )(C )
!   IJ      I       IJ       J
!
!
!     COMPUTE  (TERM        )  STORE IN PROD9
!                   I=NPIVOT
!
!                   -1
!     FIRST GET (G )
!                 4
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
            Ising = -1
            CALL inverd(3,g(28),3,Prod9,0,Determ,Ising,Temp9)
!
!     CHECK FOR SINGULARITY. ISING=2 IMPLIES SINGULARITY.
            IF ( Ising==1 ) THEN
!
               CALL gmmatd(g(28),3,3,0,g(9*Npivot-8),3,3,0,Prod9(1))
!
!                       T
!     GET  (TERM        )(K  )  STORE IN TEMP9
!               I=NPIVOT   44
!
               CALL gmmatd(Prod9(1),3,3,1,Ksum(55),3,3,0,Temp9(1))
!
!
!
!     THE TWO COMMON PRODUCTS ARE NOW AT HAND IN PROD9 AND TEMP9.
!
               DO j = 1 , 3
!
!                   T     T
!     (TERM        ) (K  )    STORE IN ARR9
!          I=NPIVOT    J4
!
                  CALL gmmatd(Prod9(1),3,3,1,Ksum(9*j+19),3,3,1,Arr9(1))
!
!     SUBTRACT FROM (K  )
!                     IJ
!
                  nbegin = 9*j - 9
                  DO i = 1 , 9
                     Npoint = nbegin + i
                     Ksum(Npoint) = Ksum(Npoint) - Arr9(i)
                  ENDDO
!
!
!      COMPUTE  (TERM )  STORE IN ARR9
!                   J
!
                  CALL gmmatd(g(28),3,3,0,g(9*j-8),3,3,0,Arr9(1))
!
!
!     GET  (K  )(TERM )  STORE IN ARRAY9
!            I4      J
!
                  CALL gmmatd(Ksum(9*Npivot+19),3,3,0,Arr9(1),3,3,0,Array9(1))
!
!     SUBTRACT FROM KIJ
!
                  DO i = 1 , 9
                     Npoint = nbegin + i
                     Ksum(Npoint) = Ksum(Npoint) - Array9(i)
                  ENDDO
!
!                           T
!     COMPUTE  (TERM        )(K  )(TERM ) = (TEMP9)(ARR9)
!                   I=NPOINT   44      J
!
                  CALL gmmatd(Temp9(1),3,3,0,Arr9(1),3,3,0,Array9(1))
!
!     ADD TO K
!             IJ
!
                  DO i = 1 , 9
                     Npoint = nbegin + i
                     Ksum(Npoint) = Ksum(Npoint) + Array9(i)
                  ENDDO
!
!       E
!     K    COMPLETE
!      IJ
!
!     TRANSFORM NOW, AND INSERT.
!
!
!     TRANSFORMATIONS AND INSERTION
!
                  IF ( Necpt(4*j+9)==0 ) THEN
                     DO k = 1 , 18
                        Tjte(k) = E(k)
                     ENDDO
                  ELSE
                     CALL transd(Necpt(4*j+9),T(1))
                     CALL gmmatd(T(1),3,3,1,E(1),3,3,0,Tjte(1))
                     CALL gmmatd(T(1),3,3,1,E(10),3,3,0,Tjte(10))
                  ENDIF
                  CALL gmmatd(Ksum(nbegin+1),3,3,0,Tjte(1),6,3,1,Temp18(1))
                  CALL gmmatd(Tite(1),6,3,0,Temp18(1),3,6,0,Kout(1))
                  CALL pla4b(Kout(1),Necpt(j+1))
!
               ENDDO
               RETURN
            ELSE
               CALL mesage(30,36,Ecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
               Nogo = 1
               RETURN
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   CALL mesage(30,26,Ecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
   Nogo = 1
END SUBROUTINE pktrpl
