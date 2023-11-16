
SUBROUTINE dquad(Itype)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION A(1) , A1(3) , D1(3) , D2(3) , Dpdum(1) , Dpdum2(43) , E(18) , H , Ivect(3) , Jvect(3) , Kout(36) , Ksum(36) ,  &
                  & Kvect(3) , Prod9(9) , R(2,4) , Requiv(8) , Sigx , Sigxy , Sigy , T(9) , Temp , Temp18(18) , Temp9(9) , Tite(18) &
                  & , Tjte(18) , U1 , U2 , V(2) , Vv(2) , Xsubb , Xsubc , Ysubc
   REAL Alp12 , Alpha1 , Alpha2 , Consts(5) , Cosang , Costh , Degra , Ecpt(100) , Eltemp , G11 , G12 , G13 , G22 , G23 , G2x211 ,  &
      & G2x212 , G2x222 , G33 , Gsube , Rho , Sigcom , Sigshe , Sigten , Sinang , Sinth , Sp1(2) , Stress , Theta , Tsub0 , Vq1(3) ,&
      & Vq2(3) , Vq3(3) , Vq4(3)
   INTEGER Ibuff , Icstm , Inflag , Ipvt , Ising , Jnot , Km , Matid , Nbegin , Ncstm , Necpt(100) , Nogo , Nout , Npivot , Npoint ,&
         & Npvt , Nsubc , Subsca , Subscb , Subscc
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /condas/ Consts
   COMMON /ds1aaa/ Npvt , Icstm , Ncstm
   COMMON /ds1adp/ Kout , Tite , Tjte , Temp18 , D1 , D2 , A1 , V , Vv , Prod9 , Temp9 , H , U1 , U2 , Dpdum , Temp , Dpdum2 , E ,  &
                 & Sigx , Sigy , Sigxy , Xsubb , Xsubc , Ysubc , Ksum , T , Ivect , Jvect , Kvect , R , Sp1 , Theta , Sinang ,      &
                 & Cosang , Km , Nbegin , Jnot , Npivot , Nsubc , Ising , Subsca , Subscb , Subscc , Npoint , Ipvt
   COMMON /ds1aet/ Ecpt
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alpha1 , Alpha2 , Alp12 , Tsub0 , Gsube , Sigten , Sigcom , Sigshe ,   &
                 & G2x211 , G2x212 , G2x222
   COMMON /system/ Ibuff , Nout , Nogo
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
!
! Dummy argument declarations
!
   INTEGER Itype
!
! Local variable declarations
!
   INTEGER i , j , k , m(12)
!
! End of declarations
!
!
!     THIS ROUTINE GENERATES THE FOLLOWING
!
!     FOUR 6X6 DIFFERENTIAL STIFFNESS MATRICES FOR ONE PIVOT POINT OF
!     A QUADRILATERAL
!
!
!     CALLS FROM THIS ROUTINE ARE MADE TO
!           DTRBSC - BASIC BENDING TRI. ROUTINE.
!           DTRMEM - TRIANGULAR MEMBRANE ROUTINE
!           TRANSD - SUPPLIES 3X3 TRANSFORMATIONS
!           GMMATD - GENERAL MATRIX MULITPLY AND TRANSPOSE ROUTINE
!           DS1B   - INSERTION ROUTINE
!
!
!        ITYPE    = 1             2                       4
!     ECPT INDEX    QUAD1         QUAD2        TRMEM      QUAD4
!     **********    *******       *******      *******    ********
!          1        EL. ID.       EL. ID.      EL. ID.    EL. ID
!          2        SIL1          SIL1         SIL1       SIL1
!          3        SIL2          SIL2         SIL2       SIL2
!          4        SIL3          SIL3         SIL3       SIL3
!          5        SIL4          SIL4         THETA      SIL4
!          6        THETA         THETA        MAT. ID.   MEM.T1
!          7        MAT. ID. 1    MAT. ID.     T          MEM.T2
!          8        T1            T            NSM        MEM.T3
!          9        MAT. ID. 2    NSM          CID1       MEM.T4
!         10        INERTIA I     CID1         X1         THETA
!         11        MAT ID  3     X1           Y1         FLAG FOR 10
!         12        T2            Y1           Z1         GRD OFFSET
!         13        NSM           Z1           CID2       MAT. ID 1
!         14        Z1            CID2         X2         THICKNESS
!         15        Z2            X2           Y2         MAT. ID 2
!         16        CID1          Y2           Z2         INERTIA I
!         17        X1            Z2           CID3       MAT. ID 3
!         18        Y1            CID3         X3         TS/T
!         19        Z1            X3           Y3         NSM
!         20        CID2          Y3           Z3         Z1
!         21        X2            Z3           EL TEMP    Z2
!         22        Y2            CID4         EL DEFORM  MAT. ID 4
!         23        Z2            X4           LOAD TEMP  THETA
!         24        CID3          Y4           U1         FLAG FOR 23
!         25        X3            Z4           V1         INTEGRATION
!         26        Y3            EL TEMP      W1         STRESS ANGLE
!         27        Z3            EL DEFORM    U2         FLAG FOR 26
!         28        CID4          LOAD TEMP    V2         ZOFF1
!         29        X4            U1           W2         CID1
!         30        Y4            V1           U3         X1
!         31        Z4            W1           V3         Y1
!         32        EL TEMP       U2           W3         Z1
!         33        EL DEFORM     V2                      CID2
!         34        LOAD TEMP     W2                      X2
!         35        U1            U3                      Y2
!         36        V1            V3                      Z2
!         37        W1            W3                      CID3
!         38        U2            U4                      X3
!         39        V2            V4                      Y3
!         40        W2            W4                      Z3
!         41        U3                                    CID4
!         42        V3                                    X4
!         43        W3                                    Y4
!         44        U4                                    Z4
!         45        V4                                    EL TEMP
!         46        W4
!         47
!         48                                              U1
!         49                                              V1
!         50                                              W1
!         51                                              U2
!         52                                              V2
!         53                                              W2
!         54                                              U3
!         55                                              V3
!         56                                              W3
!         57                                              U4
!         58                                              V4
!         59                                              W4
!
   EQUIVALENCE (Consts(4),Degra) , (Necpt(1),Ecpt(1)) , (Requiv(1),R(1,1)) , (Vq1(1),Ecpt(17)) , (Vq2(1),Ecpt(21)) ,                &
    & (Vq3(1),Ecpt(25)) , (Vq4(1),Ecpt(29)) , (A(1),Kout(1))
   DATA m/2 , 4 , 1 , 3 , 1 , 2 , 4 , 2 , 3 , 1 , 3 , 4/
!
!
!     IF ITYPE = 2, QUAD2 EST DATA IS MOVED AND STORED IN QUAD1 FORMAT
!     IF ITYPE = 4, QUAD4 EST DATA IS MOVED AND STORED IN QUAD1 FORMAT
!
   IF ( Itype==4 ) THEN
!
!     QUAD4
!
!     IF NECPT(11)=0, ECPT(10) IS THE MATERIAL PROPERTY ORIENTAION
!     ANGLE THETA. IF IT IS NOT, NECPT(10) IS MATERIAL COORDINATE
!     SYSTEM ID. IN THIS CASE, WE CAN NOT CONTINUE
!
      IF ( Necpt(11)/=0 ) THEN
!
!     COULD NOT CONTINUE
!
         WRITE (Nout,99001) Sfm
99001    FORMAT (A25,', DEFFICIENT SOURCE CODE IN DQUAD TO HANDLE CQUAD4 ','ELEMENT WITH MATERIAL',/5X,                             &
                &'PROPERTY COORD. SYSTEM. ANGLE MUST BE SPECIFIED')
         Nogo = 1
         GOTO 99999
      ELSE
         Ecpt(6) = Ecpt(10)
         Ecpt(7) = Ecpt(13)
         Ecpt(8) = Ecpt(14)
         Ecpt(9) = Ecpt(15)
         Ecpt(10) = Ecpt(16)
         Ecpt(11) = Ecpt(17)
         Ecpt(12) = Ecpt(14)
         DO i = 16 , 46
            Ecpt(i) = Ecpt(i+13)
         ENDDO
      ENDIF
   ELSEIF ( Itype==2 ) THEN
!
      DO i = 10 , 40
         Npoint = 50 - i
         Ecpt(Npoint+6) = Ecpt(Npoint)
      ENDDO
!
      Ecpt(9) = Ecpt(7)
      Ecpt(10) = (Ecpt(8)**3.0)/12.0
      Ecpt(11) = Ecpt(7)
      Ecpt(12) = Ecpt(8)
   ENDIF
   IF ( Ecpt(8)==0.0 ) RETURN
!
!     CALL BUG (4HQDET,5,ECPT,52-6*ITYPE)
!
!     DETERMINE PIVOT POINT NUMBER
!
   DO i = 1 , 4
      IF ( Npvt==Necpt(i+1) ) THEN
         Npivot = i
         GOTO 100
      ENDIF
   ENDDO
   RETURN
!
 100  Theta = Ecpt(6)*Degra
   Sinang = sin(Theta)
   Cosang = cos(Theta)
!
   IF ( Npivot<=2 ) THEN
      Jnot = Npivot + 2
   ELSE
      Jnot = Npivot - 2
   ENDIF
!
!     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
!     SUB TRIANGLES.  (2X4) FOR QUADRILATERAL PLATE...
!     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
!
!     ZERO OUT R-MATRIX
!
   DO i = 1 , 8
      Requiv(i) = 0.0D0
   ENDDO
!
   DO i = 1 , 3
      D1(i) = dble(Vq3(i)) - dble(Vq1(i))
      D2(i) = dble(Vq4(i)) - dble(Vq2(i))
      A1(i) = dble(Vq2(i)) - dble(Vq1(i))
   ENDDO
!
!     NON-NORMALIZED K-VECTOR = D1 CROSS D2
!
   Kvect(1) = D1(2)*D2(3) - D2(2)*D1(3)
   Kvect(2) = D1(3)*D2(1) - D2(3)*D1(1)
   Kvect(3) = D1(1)*D2(2) - D2(1)*D1(2)
!
!     NORMALIZE K-VECTOR
!
   Temp = dsqrt(Kvect(1)**2+Kvect(2)**2+Kvect(3)**2)
   IF ( Temp==0.0D0 ) CALL mesage(-30,26,Ecpt(1))
   DO i = 1 , 3
      Kvect(i) = Kvect(i)/Temp
   ENDDO
!
!     COMPUTE H = (A1 DOT KVECT) / 2
!
   Temp = (A1(1)*Kvect(1)+A1(2)*Kvect(2)+A1(3)*Kvect(3))/2.0D0
!
!     I-VECTOR =(A1) - H*(KVECT)    NON-NORMALIZED
!
   DO i = 1 , 3
      Ivect(i) = A1(i) - Temp*Kvect(i)
   ENDDO
!
!     NORMALIZE I-VECTOR
!
   Temp = dsqrt(Ivect(1)**2+Ivect(2)**2+Ivect(3)**2)
   IF ( Temp==0.0D0 ) CALL mesage(-30,26,Ecpt(1))
   DO i = 1 , 3
      Ivect(i) = Ivect(i)/Temp
   ENDDO
!
!     J-VECTOR = K CROSS I, AND X3 CALCULATION
!
   Jvect(1) = Kvect(2)*Ivect(3) - Ivect(2)*Kvect(3)
   Jvect(2) = Kvect(3)*Ivect(1) - Ivect(3)*Kvect(1)
   Jvect(3) = Kvect(1)*Ivect(2) - Ivect(1)*Kvect(2)
!
!     NORMALIZE J VECTOR TO MAKE SURE
!
   Temp = dsqrt(Jvect(1)**2+Jvect(2)**2+Jvect(3)**2)
   IF ( Temp==0.0D0 ) CALL mesage(-30,26,Ecpt(1))
   DO i = 1 , 3
      Jvect(i) = Jvect(i)/Temp
   ENDDO
!
!     X3 GOES INTO R(1,3) = D1 DOT IVECT
!
   R(1,3) = D1(1)*Ivect(1) + D1(2)*Ivect(2) + D1(3)*Ivect(3)
!
!     X2 GOES INTO R(1,2) AND Y3 GOES INTO R(2,3)
!
   R(1,2) = A1(1)*Ivect(1) + A1(2)*Ivect(2) + A1(3)*Ivect(3)
   R(2,3) = D1(1)*Jvect(1) + D1(2)*Jvect(2) + D1(3)*Jvect(3)
!
!     X4 GOES INTO R(1,4) AND Y4 GOES INTO R(2,4)
!
   R(1,4) = D2(1)*Ivect(1) + D2(2)*Ivect(2) + D2(3)*Ivect(3) + R(1,2)
   R(2,4) = D2(1)*Jvect(1) + D2(2)*Jvect(2) + D2(3)*Jvect(3)
!
!     AT THIS POINT, THE COORDINATES OF THE PLATE IN THE ELEMENT
!     SYSTEM ARE STORED IN THE R-MATRIX WHERE THE COLUMN DENOTES THE
!     POINT AND THE ROW DENOTES THE X OR Y COORDINATE FOR ROW 1 OR
!     ROW 2 RESPECTIVELY.
!
!     SET UP THE M-MATRIX FOR MAPPING TRIANGLES, IN DATA STATEMENT.
!
!     COMPUTE SUB-TRIANGLE COORDINATES
!
!     ZERO OUT KSUM MATRICES
!
   DO i = 1 , 36
      Ksum(i) = 0.0D0
   ENDDO
!
   Eltemp = Ecpt(32)
!
!     MOVE ECPT INTO POSITIONS 51-93
!
   DO i = 1 , 46
      Ecpt(i+50) = Ecpt(i)
   ENDDO
!
!     MOVE MISCELLANEOUS VARIABLES INTO TRMEM FORMAT
!
   Ecpt(6) = Ecpt(7)
   Ecpt(7) = Ecpt(8)
   Ecpt(21) = Ecpt(32)
   Ecpt(22) = Ecpt(33)
   Ecpt(23) = Ecpt(34)
!
   DO j = 1 , 4
      IF ( j/=Jnot ) THEN
         Km = 3*j - 3
         Ipvt = 0
         DO i = 1 , 3
            Npoint = Km + i
            Nsubc = m(Npoint)
            IF ( Nsubc==Npivot ) Ipvt = i
            Necpt(i+1) = Necpt(Nsubc+51)
            DO k = 1 , 4
               Npoint = 4*(Nsubc-1) + k + 65
               Subsca = 4*(i-1) + k + 8
               Ecpt(Subsca) = Ecpt(Npoint)
            ENDDO
            DO k = 1 , 3
               Npoint = 3*(Nsubc-1) + k + 84
               Subsca = 3*(i-1) + k + 23
               Ecpt(Subsca) = Ecpt(Npoint)
            ENDDO
         ENDDO
         IF ( Ipvt/=0 ) THEN
!
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
            Sinth = Sinang*U1 - Cosang*U2
            Costh = Cosang*U1 + Sinang*U2
            IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0
!
!     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR TRIANGLE -J-
!
            CALL dtrmem(3)
            CALL dtrbsc(2,Ipvt)
!
!     NOW WE HAVE AT HAND  K   I=NPIVOT,J=1,2,3   THREE 6X6 MATRICES
!                           IJ
!                                STORED AT  A(1) THROUGH A(27)
!
!     MAP THE THE 3X3 S FOR THE PIVOT ROW INTO THE SUMMATION ARRAYS
!
            DO i = 1 , 3
               Npoint = 9*i - 8
!
               CALL gmmatd(T,3,3,1,A(Npoint),3,3,0,Temp9)
               CALL gmmatd(Temp9,3,3,0,T,3,3,0,Prod9)
!
!     ADD THIS PRODUCT IN NOW.
!
               Npoint = Km + i
               Npoint = 9*m(Npoint) - 9
               DO k = 1 , 9
                  Npoint = Npoint + 1
                  Ksum(Npoint) = Ksum(Npoint) + Prod9(k)/2.0D0
               ENDDO
            ENDDO
         ENDIF
      ENDIF
!
   ENDDO
!
!     CALL BUG (4HQDKD,220,KSUM,72)
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
   IF ( Necpt(4*Npivot+62)==0 ) THEN
!
      DO k = 1 , 18
         Tite(k) = E(k)
      ENDDO
!
!     RESTORE ECPT FOR CKECKOUT
!
      DO k = 1 , 46
         Ecpt(k) = Ecpt(k+50)
      ENDDO
   ELSE
      CALL transd(Necpt(4*Npivot+62),T)
      CALL gmmatd(T,3,3,1,E(1),3,3,0,Tite(1))
      CALL gmmatd(T,3,3,1,E(10),3,3,0,Tite(10))
   ENDIF
!
   DO j = 1 , 4
!
!     TRANSFORMATIONS AND INSERTION
!
      IF ( Necpt(4*j+62)==0 ) THEN
!
         DO k = 1 , 18
            Tjte(k) = E(k)
         ENDDO
      ELSE
         CALL transd(Necpt(4*j+62),T)
         CALL gmmatd(T,3,3,1,E(1),3,3,0,Tjte(1))
         CALL gmmatd(T,3,3,1,E(10),3,3,0,Tjte(10))
      ENDIF
      CALL gmmatd(Ksum(9*j-8),3,3,0,Tjte,6,3,1,Temp18(1))
      CALL gmmatd(Tite(1),6,3,0,Temp18(1),3,6,0,Kout(1))
      CALL ds1b(Kout,Necpt(j+51))
   ENDDO
   RETURN
99999 RETURN
END SUBROUTINE dquad
