!*==dquad.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dquad(Itype)
   USE c_condas
   USE c_ds1aaa
   USE c_ds1adp
   USE c_ds1aet
   USE c_matin
   USE c_matout
   USE c_system
   USE c_xmssg
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Itype
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: a
   REAL :: degra
   INTEGER :: i , j , k
   INTEGER , DIMENSION(12) , SAVE :: m
   INTEGER , DIMENSION(100) :: necpt
   REAL(REAL64) , DIMENSION(8) :: requiv
   REAL , DIMENSION(3) :: vq1 , vq2 , vq3 , vq4
   EXTERNAL ds1b , dtrbsc , dtrmem , gmmatd , mesage , transd
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Consts(4),Degra) , (Necpt(1),Ecpt(1)) , (Requiv(1),R(1,1)) , (Vq1(1),Ecpt(17)) , (Vq2(1),Ecpt(21)) ,                &
!>>>>    & (Vq3(1),Ecpt(25)) , (Vq4(1),Ecpt(29)) , (A(1),Kout(1))
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
      IF ( necpt(11)/=0 ) THEN
!
!     COULD NOT CONTINUE
!
         WRITE (nout,99001) sfm
99001    FORMAT (A25,', DEFFICIENT SOURCE CODE IN DQUAD TO HANDLE CQUAD4 ','ELEMENT WITH MATERIAL',/5X,                             &
                &'PROPERTY COORD. SYSTEM. ANGLE MUST BE SPECIFIED')
         nogo = 1
         RETURN
      ELSE
         ecpt(6) = ecpt(10)
         ecpt(7) = ecpt(13)
         ecpt(8) = ecpt(14)
         ecpt(9) = ecpt(15)
         ecpt(10) = ecpt(16)
         ecpt(11) = ecpt(17)
         ecpt(12) = ecpt(14)
         DO i = 16 , 46
            ecpt(i) = ecpt(i+13)
         ENDDO
      ENDIF
   ELSEIF ( Itype==2 ) THEN
!
      DO i = 10 , 40
         npoint = 50 - i
         ecpt(npoint+6) = ecpt(npoint)
      ENDDO
!
      ecpt(9) = ecpt(7)
      ecpt(10) = (ecpt(8)**3.0)/12.0
      ecpt(11) = ecpt(7)
      ecpt(12) = ecpt(8)
   ENDIF
   IF ( ecpt(8)==0.0 ) RETURN
!
!     CALL BUG (4HQDET,5,ECPT,52-6*ITYPE)
!
!     DETERMINE PIVOT POINT NUMBER
!
   DO i = 1 , 4
      IF ( npvt==necpt(i+1) ) THEN
         npivot = i
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      USE ISO_FORTRAN_ENV                 
!
      theta = ecpt(6)*Degra
      sinang = sin(theta)
      cosang = cos(theta)
!
      IF ( Npivot<=2 ) THEN
         jnot = Npivot + 2
      ELSE
         jnot = Npivot - 2
      ENDIF
!
!     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
!     SUB TRIANGLES.  (2X4) FOR QUADRILATERAL PLATE...
!     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
!
!     ZERO OUT R-MATRIX
!
      DO I = 1 , 8
         Requiv(I) = 0.0D0
      ENDDO
!
      DO I = 1 , 3
         d1(I) = dble(Vq3(I)) - dble(Vq1(I))
         d2(I) = dble(Vq4(I)) - dble(Vq2(I))
         a1(I) = dble(Vq2(I)) - dble(Vq1(I))
      ENDDO
!
!     NON-NORMALIZED K-VECTOR = D1 CROSS D2
!
      kvect(1) = d1(2)*d2(3) - d2(2)*d1(3)
      kvect(2) = d1(3)*d2(1) - d2(3)*d1(1)
      kvect(3) = d1(1)*d2(2) - d2(1)*d1(2)
!
!     NORMALIZE K-VECTOR
!
      temp = dsqrt(kvect(1)**2+kvect(2)**2+kvect(3)**2)
      IF ( temp==0.0D0 ) CALL mesage(-30,26,ecpt(1))
      DO I = 1 , 3
         kvect(I) = kvect(I)/temp
      ENDDO
!
!     COMPUTE H = (A1 DOT KVECT) / 2
!
      temp = (a1(1)*kvect(1)+a1(2)*kvect(2)+a1(3)*kvect(3))/2.0D0
!
!     I-VECTOR =(A1) - H*(KVECT)    NON-NORMALIZED
!
      DO I = 1 , 3
         ivect(I) = a1(I) - temp*kvect(I)
      ENDDO
!
!     NORMALIZE I-VECTOR
!
      temp = dsqrt(ivect(1)**2+ivect(2)**2+ivect(3)**2)
      IF ( temp==0.0D0 ) CALL mesage(-30,26,ecpt(1))
      DO I = 1 , 3
         ivect(I) = ivect(I)/temp
      ENDDO
!
!     J-VECTOR = K CROSS I, AND X3 CALCULATION
!
      jvect(1) = kvect(2)*ivect(3) - ivect(2)*kvect(3)
      jvect(2) = kvect(3)*ivect(1) - ivect(3)*kvect(1)
      jvect(3) = kvect(1)*ivect(2) - ivect(1)*kvect(2)
!
!     NORMALIZE J VECTOR TO MAKE SURE
!
      temp = dsqrt(jvect(1)**2+jvect(2)**2+jvect(3)**2)
      IF ( temp==0.0D0 ) CALL mesage(-30,26,ecpt(1))
      DO I = 1 , 3
         jvect(I) = jvect(I)/temp
      ENDDO
!
!     X3 GOES INTO R(1,3) = D1 DOT IVECT
!
      r(1,3) = d1(1)*ivect(1) + d1(2)*ivect(2) + d1(3)*ivect(3)
!
!     X2 GOES INTO R(1,2) AND Y3 GOES INTO R(2,3)
!
      r(1,2) = a1(1)*ivect(1) + a1(2)*ivect(2) + a1(3)*ivect(3)
      r(2,3) = d1(1)*jvect(1) + d1(2)*jvect(2) + d1(3)*jvect(3)
!
!     X4 GOES INTO R(1,4) AND Y4 GOES INTO R(2,4)
!
      r(1,4) = d2(1)*ivect(1) + d2(2)*ivect(2) + d2(3)*ivect(3) + r(1,2)
      r(2,4) = d2(1)*jvect(1) + d2(2)*jvect(2) + d2(3)*jvect(3)
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
      DO I = 1 , 36
         ksum(I) = 0.0D0
      ENDDO
!
      eltemp = ecpt(32)
!
!     MOVE ECPT INTO POSITIONS 51-93
!
      DO I = 1 , 46
         ecpt(I+50) = ecpt(I)
      ENDDO
!
!     MOVE MISCELLANEOUS VARIABLES INTO TRMEM FORMAT
!
      ecpt(6) = ecpt(7)
      ecpt(7) = ecpt(8)
      ecpt(21) = ecpt(32)
      ecpt(22) = ecpt(33)
      ecpt(23) = ecpt(34)
!
      DO J = 1 , 4
         IF ( J/=jnot ) THEN
            km = 3*J - 3
            ipvt = 0
            DO I = 1 , 3
               Npoint = km + I
               nsubc = M(Npoint)
               IF ( nsubc==Npivot ) ipvt = I
               Necpt(I+1) = Necpt(nsubc+51)
               DO K = 1 , 4
                  Npoint = 4*(nsubc-1) + K + 65
                  subsca = 4*(I-1) + K + 8
                  ecpt(subsca) = ecpt(Npoint)
               ENDDO
               DO K = 1 , 3
                  Npoint = 3*(nsubc-1) + K + 84
                  subsca = 3*(I-1) + K + 23
                  ecpt(subsca) = ecpt(Npoint)
               ENDDO
            ENDDO
            IF ( ipvt/=0 ) THEN
!
               subsca = M(km+1)
               subscb = M(km+2)
               subscc = M(km+3)
!
               DO I = 1 , 2
                  v(I) = r(I,subscb) - r(I,subsca)
                  vv(I) = r(I,subscc) - r(I,subsca)
               ENDDO
               xsubb = dsqrt(v(1)**2+v(2)**2)
               u1 = v(1)/xsubb
               u2 = v(2)/xsubb
               xsubc = u1*vv(1) + u2*vv(2)
               ysubc = u1*vv(2) - u2*vv(1)
!
!     SET UP OF T-MATRIX
!
               t(1) = 1.0D0
               t(2) = 0.0D0
               t(3) = 0.0D0
               t(4) = 0.0D0
               t(5) = u1
               t(6) = u2
               t(7) = 0.0D0
               t(8) = -u2
               t(9) = u1
!
               sinth = sinang*u1 - cosang*u2
               costh = cosang*u1 + sinang*u2
               IF ( abs(sinth)<1.0E-06 ) sinth = 0.0
!
!     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR TRIANGLE -J-
!
               CALL dtrmem(3)
               CALL dtrbsc(2,ipvt)
!
!     NOW WE HAVE AT HAND  K   I=NPIVOT,J=1,2,3   THREE 6X6 MATRICES
!                           IJ
!                                STORED AT  A(1) THROUGH A(27)
!
!     MAP THE THE 3X3 S FOR THE PIVOT ROW INTO THE SUMMATION ARRAYS
!
               DO I = 1 , 3
                  Npoint = 9*I - 8
!
                  CALL gmmatd(t,3,3,1,A(Npoint),3,3,0,temp9)
                  CALL gmmatd(temp9,3,3,0,t,3,3,0,prod9)
!
!     ADD THIS PRODUCT IN NOW.
!
                  Npoint = km + I
                  Npoint = 9*M(Npoint) - 9
                  DO K = 1 , 9
                     Npoint = Npoint + 1
                     ksum(Npoint) = ksum(Npoint) + prod9(K)/2.0D0
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
      DO I = 1 , 18
         e(I) = 0.0D0
      ENDDO
      e(1) = kvect(1)
      e(4) = kvect(2)
      e(7) = kvect(3)
      e(11) = ivect(1)
      e(14) = ivect(2)
      e(17) = ivect(3)
      e(12) = jvect(1)
      e(15) = jvect(2)
      e(18) = jvect(3)
!
!              T
!     FORM   T   E      STORE IN TITE-MATRIX (6X3)
!             I
!
      IF ( Necpt(4*Npivot+62)==0 ) THEN
!
         DO K = 1 , 18
            tite(K) = e(K)
         ENDDO
!
!     RESTORE ECPT FOR CKECKOUT
!
         DO K = 1 , 46
            ecpt(K) = ecpt(K+50)
         ENDDO
      ELSE
         CALL transd(Necpt(4*Npivot+62),t)
         CALL gmmatd(t,3,3,1,e(1),3,3,0,tite(1))
         CALL gmmatd(t,3,3,1,e(10),3,3,0,tite(10))
      ENDIF
!
      DO J = 1 , 4
!
!     TRANSFORMATIONS AND INSERTION
!
         IF ( Necpt(4*J+62)==0 ) THEN
!
            DO K = 1 , 18
               tjte(K) = e(K)
            ENDDO
         ELSE
            CALL transd(Necpt(4*J+62),t)
            CALL gmmatd(t,3,3,1,e(1),3,3,0,tjte(1))
            CALL gmmatd(t,3,3,1,e(10),3,3,0,tjte(10))
         ENDIF
         CALL gmmatd(ksum(9*J-8),3,3,0,tjte,6,3,1,temp18(1))
         CALL gmmatd(tite(1),6,3,0,temp18(1),3,6,0,kout(1))
         CALL ds1b(kout,Necpt(J+51))
      ENDDO
   END SUBROUTINE spag_block_1
END SUBROUTINE dquad
