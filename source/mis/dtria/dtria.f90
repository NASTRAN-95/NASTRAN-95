!*==dtria.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dtria(Iopt)
   IMPLICIT NONE
   USE C_CONDAS
   USE C_DS1AAA
   USE C_DS1ADP
   USE C_DS1AET
   USE C_MATIN
   USE C_MATOUT
   USE C_SYSTEM
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iopt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: cid1
   REAL*8 , DIMENSION(3) :: d1 , d2
   REAL*8 , DIMENSION(36) :: g , kout
   REAL*8 , DIMENSION(18) :: habc , temp18 , tite , tjte
   REAL*8 , DIMENSION(12) :: hq , prod12
   INTEGER :: i , icid , j , k , nbegin
   INTEGER , DIMENSION(9) , SAVE :: m
   INTEGER , DIMENSION(100) :: necpt
   REAL*8 , DIMENSION(8) :: requiv
   REAL , DIMENSION(3) :: v1 , v2 , v3
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE GENERATES THE FOLLOWING
!
!     THREE 6X6 DIFFERENTIAL STIFFNESS MATRIX PARTITION FOR ONE PIVOT
!     POINT FOR A TRIA1, TRIA2 OR TORA3 ELEMENT.
!
!
!     CALLS FROM THIS ROUTINE ARE MADE TO
!           DTRBSC - BASIC BENDING TRI. ROUTINE.
!           DTRMEM - TRIANGLULAR MEMBRANE ROUTINE
!           TRANSD - SUPPLIES 3X3 TRANSFORMATIONS
!           INVERD - MATRIX INVERSION ROUTINE
!           GMMATD - GENERAL MATRIX MULITPLY AND TRANSPOSE ROUTINE
!           DS1B   - INSERTION ROUTINE
!
!
!          IOPT  = 1               2           3
!     ECPT INDEX   TRIA1           TRIA2       TRIA3       TRMEM
!     **********   *********       ********    ********    ********
!           1      EL ID           EL ID       EL ID        EL ID
!           2      SIL1            SIL1        SIL1         SIL1
!           3      SIL2            SIL2        SIL2         SIL2
!           4      SIL3            SIL3        SIL3         SIL3
!           5      THETA           THETA       MEM T1       THETA
!           6      MAT ID 1        MAT ID      MEM T2       MAT ID
!           7      T1              T           MEM T3       T
!           8      MAT ID 2        NSM         THETA        NSM
!           9      INERTIA I       CID1        FLAG FOR 8   CID1
!          10      MAT ID 3        X1          GRD OFFSET   X1
!          11      T2              Y1          MAT ID1      Y1
!          12      NSM             Z1          THICKNESS    Z1
!          13      Z1              CID2        MAT ID2      CID2
!          14      Z2              X2          INERTIA I    X2
!          15      CID1            Y2          MAT ID 3     Y2
!          16      X1              Z2          TS/T         Z2
!          17      Y1              CID3        NSM          CID3
!          18      Z1              X3          Z1           X3
!          19      CID2            Y3          Z2           Y3
!          20      X2              Z3          MAT ID 4     Z3
!          21      Y2              EL TEMP     THETA        EL TEMP
!          22      Z2                          FLAG FOR 21  EL DEFORM
!          23      CID3                        INTEGRATION  LOAD TEMP
!          24      X3              U1          STRESS ANGLE U1
!          25      Y3              V1          FLAG FOR 24  V2
!          26      Z3              W1          ZOFF1        W3
!          27      EL TEMP         U2          CID1         U2
!          28      EL DEFORM       V2          X1           V2
!          29      EL LOAD TEMP    W2          Y1           W2
!          30      U1 -DISP FOR U1 U3          Z1           U3
!          31      V1 -DISP FOR V1 V3          CID2         V3
!          32      W1 -DISP FOR Z1 W3          X2           W3
!          33      U2 -DISP FOR X2             Y2
!          34      V2 -DISP FOR Y2             Z2
!          35      W2 -DISP FOR Z2             CID3
!          36      U3 -DISP FOR X3             X3
!          37      V3 -DISP FOR Y3             Y3
!          38      W3 -DISP FOR Z3             Z3
!          39                                  EL TEMP
!          40
!          41
!          42                                  U1
!          43                                  V1
!          44                                  W1
!          45                                  U2
!          46                                  V2
!          47                                  W2
!          48                                  U3
!          49                                  V3
!          50                                  W3
!
   !>>>>EQUIVALENCE (Necpt(1),Ecpt(1)) , (Prod12(1),A(13)) , (Habc(1),A(25)) , (Tite(1),A(37)) , (Tjte(1),S(1)) , (Kout(1),A(1)) ,       &
!>>>>    & (Temp18(1),Hinv(1)) , (V1(1),Ecpt(66)) , (V2(1),Ecpt(70)) , (V3(1),Ecpt(74)) , (Requiv(1),R(1,1)) , (D1(1),A(1)) ,            &
!>>>>    & (D2(1),A(4)) , (Hq(1),A(1))
!
!
   DATA m/1 , 2 , 4 , 2 , 3 , 4 , 3 , 1 , 4/ , cid1/65/
!
!
!     THE ECPT DATA IS COPIED TO ECPT(PLUS 50)
!     THE DATA IN ECPT(BELOW 50) IS THEN PUT INTO TRMEM FORMAT TO BE
!     USED BY DTRMEM
!     THE DATA IN ECPT(ABOVE 50, SPECIALLY 51 THRU 62, 65 THRU 88) IS
!     PUT INTO TRIA1 FORMAT, WHICH WILL BE USED BY DTRBSC AND LOCALLY
!
   icid = cid1 - 4
   DO i = 1 , 50
      Ecpt(i+50) = Ecpt(i)
   ENDDO
   IF ( Iopt==2 ) THEN
!
!     TRIA2
!
      Ecpt(58) = Ecpt(6)
      Ecpt(59) = (Ecpt(7)**3)/12.0
      Ecpt(60) = Ecpt(6)
      Ecpt(61) = Ecpt(7)
!
      j = 9
      DO i = 65 , 88
         Ecpt(i) = Ecpt(j)
         j = j + 1
      ENDDO
   ELSEIF ( Iopt==3 ) THEN
!
!     TRIA3
!
!     IF NECPT(9)=0, ECPT(8) IS MATERIAL PROPERTY ORIENTAION ANGLE THETA
!     IF NECPT(9).NE.0, NECPT(8) IS MATERIAL COORDINATE SYSTEM ID. IN
!     THIS CASE, WE CAN NOT CONTINUE (NEED MORE STUFFS TO COMPUTE THETA,
!     SEE SHCSGD)
!
      IF ( necpt(9)/=0 ) THEN
!
!     COULD NOT DO IT
!
         WRITE (Nout,99001) Sfm
99001    FORMAT (A25,', DEFFICIENT SOURCE CODE IN DTRIA TO HANDLE CTRIA3 ','ELEMENT WITH MATERIAL',/5X,                             &
                &'PROPERTY COORD. SYSTEM. ANGLE MUST BE SPECIFIED')
         Nogo = 1
         GOTO 99999
      ELSE
         Ecpt(5) = Ecpt(8)
         Ecpt(6) = Ecpt(11)
         Ecpt(7) = Ecpt(12)
         j = 27
         DO i = 9 , 32
            Ecpt(i) = Ecpt(j)
            j = j + 1
         ENDDO
!
         Ecpt(55) = Ecpt(58)
         j = 61
         DO i = 56 , 60
            Ecpt(i) = Ecpt(j)
            j = j + 1
         ENDDO
         Ecpt(61) = Ecpt(62)
         j = 77
         DO i = 65 , 88
            Ecpt(i) = Ecpt(j)
            j = j + 1
         ENDDO
      ENDIF
   ELSE
!
!     TRIA1
!
      j = 15
      DO i = 9 , 32
         Ecpt(i) = Ecpt(j)
         j = j + 1
      ENDDO
   ENDIF
!
   Theta = Ecpt(5)*Degra
   Sinang = sin(Theta)
   Cosang = cos(Theta)
   Sinth = Sinang
   Costh = Cosang
!
   CALL dtrmem(2)
!
!     SIGX, SIGY , SIGXY ARE NOW AVAILABLE. SAVE THEM.
!
   Stres(1) = Sigx
   Stres(2) = Sigy
   Stres(3) = Sigxy
!
   Eltemp = Ecpt(21)
!
!     DETERMINE PIVOT POINT NUMBER
!
   DO i = 1 , 3
      IF ( Npvt==necpt(i+1) ) THEN
         Npivot = i
         GOTO 100
      ENDIF
   ENDDO
   RETURN
!
!     FALL THRU ABOVE LOOP IMPLIES ERROR CONDITION
!
!
!     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
!     SUB TRIANGLES. (2X4) FOR TRIANGULAR PLATE. (COLUMN 4 BLANK)
!     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
!
!     ZERO OUT R-MATRIX
!
 100  DO i = 1 , 8
      requiv(i) = 0.0D0
   ENDDO
!
   DO i = 1 , 3
      d2(i) = dble(v2(i)) - dble(v1(i))
      d1(i) = dble(v3(i)) - dble(v1(i))
   ENDDO
!
!     X2  GOES IN R(1,2)
!
   R(1,2) = dsqrt(d2(1)**2+d2(2)**2+d2(3)**2)
   DO i = 1 , 3
      Ivect(i) = d2(i)/R(1,2)
   ENDDO
!
!     NON-NORMALIZED K-VECTOR
!
   Kvect(1) = Ivect(2)*d1(3) - d1(2)*Ivect(3)
   Kvect(2) = Ivect(3)*d1(1) - d1(3)*Ivect(1)
   Kvect(3) = Ivect(1)*d1(2) - d1(1)*Ivect(2)
!
!     Y3 GOES INTO R(2,3)
!
   R(2,3) = dsqrt(Kvect(1)**2+Kvect(2)**2+Kvect(3)**2)
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
   Temp = dsqrt(Jvect(1)**2+Jvect(2)**2+Jvect(3)**2)
   DO i = 1 , 3
      Jvect(i) = Jvect(i)/Temp
   ENDDO
!
!     X3 GOES INTO R(1,3) = D1 DOT IVECT
!
   R(1,3) = d1(1)*Ivect(1) + d1(2)*Ivect(2) + d1(3)*Ivect(3)
!
!     CENTROID POINT GOES INTO R(1,4) AND R(2,4)
!
   R(1,4) = (R(1,2)+R(1,3))/3.0D0
   R(2,4) = R(2,3)/3.0D0
!
!
!     THE COORDINATES AND CENTROID OF THE PLATE IN THE ELEMENT
!     SYSTEM ARE STORED IN THE R-MATRIX WHERE THE COLUMN DENOTES THE
!     POINT AND THE ROW DENOTES THE X OR Y COORDINATE FOR ROW 1 OR
!     ROW 2 RESPECTIVELY.
!
!
!     SET UP THE M-MATRIX FOR MAPPING TRIANGLES, IN DATA STATEMENT.
!
!     ZERO OUT THE KSUM MATRIX FOR 63 AND THE GSUM MATRIX FOR 36
!
   DO i = 1 , 63
      Ksum(i) = 0.0D0
   ENDDO
   DO i = 1 , 36
      g(i) = 0.0D0
   ENDDO
!
   DO j = 1 , 3
      Km = 3*j - 3
      Subsca = m(Km+1)
      Subscb = m(Km+2)
      Subscc = m(Km+3)
!
      DO i = 1 , 2
         V(i) = R(i,Subscb) - R(i,Subsca)
         Vv(i) = R(i,Subscc) - R(i,Subsca)
      ENDDO
      Xsubb = dsqrt(V(1)**2+V(2)**2)
      U1 = V(1)/Xsubb
      U2 = V(2)/Xsubb
      Xsubc = U1*Vv(1) + U2*Vv(2)
      Ysubc = U1*Vv(2) - U2*Vv(1)
!
      Sinth = Sinang*U1 - Cosang*U2
      Costh = Cosang*U1 + Sinang*U2
      IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0
!
!     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR TRIANGLE -J-
!
      C2 = U1**2
      S2 = U2**2
      L1 = U1*U2
      Sigx = C2*Stres(1) + S2*Stres(2) + 2.0D0*L1*Stres(3)
      Sigy = S2*Stres(1) + C2*Stres(2) - 2.0D0*L1*Stres(3)
      Sigxy = -L1*Stres(1) + L1*Stres(2) + (C2-S2)*Stres(3)
      Ipvt = 0
      DO i = 1 , 2
         Npoint = Km + i
         IF ( m(Npoint)==Npivot ) Ipvt = i
      ENDDO
      CALL dtrbsc(1,Ipvt)
!
!     NOW WE HAVE 6 MATRICES STORED AT A(1) TO A(54)- HIA,HIB,HIC
!                                                     HAC,HBC,HCC
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
         CALL gmmatd(T(1),3,3,1,A(9*i+19),3,3,0,Temp9(1))
         CALL gmmatd(Temp9(1),3,3,0,T(1),3,3,0,Prod9(1))
!
!     ADD THIS PRODUCT IN NOW.
!     COMPUTE POINTER TO KSUM MATRIX DESIRED.  (ZERO POINTER)
!
         Npoint = Km + i
         Npoint = 9*m(Npoint) + 18
!
         DO k = 1 , 9
            Nsubc = Npoint + k
            Ksum(Nsubc) = Ksum(Nsubc) + Prod9(k)
         ENDDO
      ENDDO
      IF ( Ipvt/=0 ) THEN
         DO i = 1 , 2
            Npoint = Km + i
            Npoint = 9*m(Npoint) - 9
!
!     TRANSFORM
!
            CALL gmmatd(T(1),3,3,1,A(9*i-8),3,3,0,Temp9(1))
            CALL gmmatd(Temp9(1),3,3,0,T(1),3,3,0,Prod9(1))
!
!     INSERT
!
            DO k = 1 , 9
               Nsubc = k + Npoint
               Ksum(Nsubc) = Ksum(Nsubc) + Prod9(k)
            ENDDO
         ENDDO
      ENDIF
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
      hq(1) = -Xsubc*C1
      hq(2) = X1*S1 - Y1*C1
      hq(3) = 2.0D0*Y1*S1
      hq(4) = -3.0D0*X1*X1*C1
      hq(5) = Y1*(2.0D0*X1*S1-Y1*C1)
      hq(6) = 3.0D0*Y1*Y1*S1
      hq(7) = 2.0D0*X2*C2
      hq(8) = X2*S2 + Y2*C2
      hq(9) = 2.0D0*Y2*S2
      hq(10) = 3.0D0*X2*X2*C2
      hq(11) = Y2*(2.0D0*X2*S2+Y2*C2)
      hq(12) = 3.0D0*Y2*Y2*S2
!
!                      I                    -1
!     COMPUTE (H       I  H     )  = (HQ)(H)    STORE IN PROD12
!               PSI,B  I   PSI,C
!                      I
!
!
      CALL gmmatd(hq(1),2,6,0,Hinv(1),6,6,0,prod12(1))
!
!
!     COMPUTE (H     ) = -(PROD12)(S)
!               PSI,A
!
      CALL gmmatd(prod12(1),2,6,0,S(1),6,3,0,habc(1))
!
      habc(1) = -habc(1)
      habc(2) = -habc(2) + S1
      habc(3) = -habc(3) + C1
      habc(4) = -habc(4)
      habc(5) = -habc(5) + S2
      habc(6) = -habc(6) - C2
!
!     SPLIT (H     ) AND (H     )    PARTITION
!             PSI,B        PSI,C
!
      habc(7) = prod12(1)
      habc(8) = prod12(2)
      habc(9) = prod12(3)
      habc(10) = prod12(7)
      habc(11) = prod12(8)
      habc(12) = prod12(9)
      habc(13) = prod12(4)
      habc(14) = prod12(5)
      habc(15) = prod12(6)
      habc(16) = prod12(10)
      habc(17) = prod12(11)
      habc(18) = prod12(12)
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
         CALL gmmatd(habc(6*i-5),2,3,0,T(1),3,3,0,Temp9(1))
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
!
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
   IF ( necpt(4*Npivot+icid)==0 ) THEN
!
      DO k = 1 , 18
         tite(k) = E(k)
      ENDDO
   ELSE
      CALL transd(necpt(4*Npivot+icid),T(1))
      CALL gmmatd(T(1),3,3,1,E(1),3,3,0,tite(1))
      CALL gmmatd(T(1),3,3,1,E(10),3,3,0,tite(10))
   ENDIF
!
!     SOLVE NOW FOR
!
!       E                   T     T                       T
!    (K  ) = (K  ) - (TERM ) (K  ) - (K  )(TERM ) + (TERM )(K  )(TERM )
!      IJ      IJ         I    J4      I4      J         I   44      J
!
!                           -1                               I=NPIVOT
!      WHERE  (TERM ) = (G )  (G ) ,I=NPIVOT                 J=1,2,3
!                  I      4     I
!
!                           -1
!             (TERM ) = (G )  (G ) ,J=1,2,3 AS ABOVE
!                  J      4     J
!
!     AND WITH TRANSFORMATIONS
!
!       G        T      E   T
!    (K  ) = (C ) (E)(K  )(E )(C )
!      IJ      I       IJ       J
!
!
!     COMPUTE  (TERM        )  STORE IN PROD9
!                   I=NPIVOT
!
!                  -1
!     FIRST GET (G )
!                 4
!
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
   Ising = -1
   CALL inverd(3,g(28),3,Prod9,0,Determ,Ising,Temp9)
!
   CALL gmmatd(g(28),3,3,0,g(9*Npivot-8),3,3,0,Prod9(1))
!
!                       T
!     GET  (TERM        )(K  ) -(K  )  STORE IN TEMP9
!               I=NPIVOT   44     I4
!
   CALL gmmatd(Prod9(1),3,3,1,Ksum(55),3,3,0,Temp9(1))
   DO k = 1 , 9
      Npoint = 9*Npivot + 18 + k
      Temp9(k) = Temp9(k) - Ksum(Npoint)
   ENDDO
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
!                            T
!     COMPUTE ((TERM        )(K  ) -(K  )) (TERM ) = (TEMP9)(ARR9)
!                   I=NPOINT   44     I4        J
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
      IF ( necpt(4*j+icid)==0 ) THEN
!
         DO k = 1 , 18
            tjte(k) = E(k)
         ENDDO
      ELSE
         CALL transd(necpt(4*j+icid),T(1))
         CALL gmmatd(T(1),3,3,1,E(1),3,3,0,tjte(1))
         CALL gmmatd(T(1),3,3,1,E(10),3,3,0,tjte(10))
      ENDIF
      CALL gmmatd(Ksum(nbegin+1),3,3,0,tjte(1),6,3,1,temp18(1))
      CALL gmmatd(tite(1),6,3,0,temp18(1),3,6,0,kout(1))
      CALL ds1b(kout(1),necpt(j+1))
   ENDDO
   RETURN
99999 END SUBROUTINE dtria
