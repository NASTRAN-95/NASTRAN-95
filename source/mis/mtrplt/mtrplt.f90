!*==mtrplt.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE mtrplt
   IMPLICIT NONE
   USE C_CONDAS
   USE C_MATIN
   USE C_MATOUT
   USE C_SMA2CL
   USE C_SMA2DP
   USE C_SMA2ET
   USE C_SMA2IO
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(3) :: d1 , d2
   REAL :: degra
   REAL*8 :: em3
   REAL*8 , DIMENSION(36) :: g , mout , temp36 , tjte
   REAL*8 , DIMENSION(18) :: habc
   REAL*8 , DIMENSION(12) :: hq , prod12
   INTEGER :: i , j , k , nbegin , npt
   INTEGER , DIMENSION(9) , SAVE :: m
   REAL*8 , DIMENSION(63) :: msum
   INTEGER , DIMENSION(100) :: necpt
   REAL*8 , DIMENSION(2,4) :: r
   REAL*8 , DIMENSION(8) :: requiv
   REAL*8 , DIMENSION(9) :: tite
   REAL , DIMENSION(3) :: v1 , v2 , v3
!
! End of declarations rewritten by SPAG
!
!
!OMMENT.  ALL WRITE STATEMENTS WHICH HAVE BEEN COMMENTED OUT, HAVE BEEN
!         LEFT IN THE PROGRAMMING FOR ANY FUTURE DEBUGGING USE.
!
!
!     THIS ROUTINE GENERATES THE FOLLOWING
!
!                             3-6X6 STIFFNESS MATRICES WITH RESPECT
!                             TO ONE PIVOT POINT OF A TRIANGULAR PLATE
!                             ELEMENT.
!
!         REF. FMMS-66  JUNE 23, 1969
!
!         CALLS FROM THIS ROUTINE ARE MADE TO
!                             MTRBSC - BASIC BENDING TRI. ROUTINE.
!                             TRANSD - SUPPLIES 3X3 TRANSFORMATIONS
!                             INVERD - MATRIX INVERSION ROUTINE
!                             SMA2B  - INSERTION ROUTINE
!                             GMMATD - GENERAL MATRIX MULITPLY AND
!                                      TRANSPOSE ROUTINE
!                             MESAGE - ERROR MESSAGE WRITER
!
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
   !>>>>EQUIVALENCE (Consts(4),Degra)
   !>>>>EQUIVALENCE (Necpt(1),Ecpt(1)) , (Prod12(1),A(13)) , (Habc(1),A(25)) , (Tite(1),A(37)) , (Tjte(1),A(46)) , (Mout(1),A(1)) ,      &
!>>>>    & (Temp36(1),Hinv(1)) , (V1(1),Ecpt(14)) , (V2(1),Ecpt(18)) , (V3(1),Ecpt(22)) , (requiv(1),r(1,1)) , (D1(1),A(1)) ,            &
!>>>>    & (D2(1),A(4)) , (Hq(1),A(1))
!
   DATA m/1 , 2 , 4 , 2 , 3 , 4 , 3 , 1 , 4/
!
   Eltemp = Ecpt(25)
!     DETERMINE PIVOT POINT NUMBER
!
   DO i = 1 , 3
      IF ( Npvt==necpt(i+1) ) THEN
         Npivot = i
         GOTO 100
      ENDIF
   ENDDO
!
!
!     FALL THRU ABOVE LOOP IMPLIES ERROR CONDITION
   CALL mesage(-30,34,Ecpt(1))
!
 100  Theta = Ecpt(5)*degra
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
      d2(i) = dble(v2(i)) - dble(v1(i))
      d1(i) = dble(v3(i)) - dble(v1(i))
   ENDDO
!
!     X2  GOES IN R(1,2)
   r(1,2) = dsqrt(d2(1)**2+d2(2)**2+d2(3)**2)
   IF ( r(1,2)/=0.0D0 ) THEN
      DO i = 1 , 3
         Ivect(i) = d2(i)/r(1,2)
      ENDDO
!
!     NON-NORMALIZED K-VECTOR
      Kvect(1) = Ivect(2)*d1(3) - d1(2)*Ivect(3)
      Kvect(2) = Ivect(3)*d1(1) - d1(3)*Ivect(1)
      Kvect(3) = Ivect(1)*d1(2) - d1(1)*Ivect(2)
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
            r(1,3) = d1(1)*Ivect(1) + d1(2)*Ivect(2) + d1(3)*Ivect(3)
!
!     CENTROID POINT GOES INTO R(1,4) AND R(2,4)
            r(1,4) = (r(1,2)+r(1,3))/3.0D0
            r(2,4) = r(2,3)/3.0D0
!
!
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
!
!OMMENCE CALCULATIONS FOR ALL THREE SUBTRIANGLES
!  INITIALIZE TO ZERO..
!          MSUM MATRIX 7 (3X3) = 63 LONG,
!          G    MATRIX 4 (3X3) = 36 LONG.
!
            DO i = 1 , 63
               msum(i) = 0.0D0
            ENDDO
            DO i = 1 , 36
               g(i) = 0.0D0
            ENDDO
!
!HOOSE APPROPRIATE COORDINATE POINTS FOR EACH SUBTRIANGLE J = 1,2,3
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
!
!     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR
!     TRIANGLE -J-
!
               CALL mtrbsc
!                         U
!     NOW HAVE AT HAND  M    I,J, =1,2,3.   9-3X3 MATRICES STORED AT
!                        IJ                 A(1) THROUGH A(81).
!
!           -1
!     ALSO H   (6X6) AT A(100) TO A(135) AND S (6X3) AT A(82) TO A(99)
!
!
! NOTE..SUB-MATRICES FOR THE PIVOT POINT AND THE CENTROID POINT
!          ARE TRANSFORMED TO ELEMENT COORDINATES AND SUMMED WITH
!          THEIR CORRESPONDING SUB-MATRIX OF THE TRIANGULAR PLATE
!
!                                     ***           ***
!                   ***               * MSUM(28...36) *
!  M     I = 1,2,3    * ARE STORED IN * MSUM(37...45) *
!   I3   3 = CENTROID *               * MSUM(46...54) *
!                   ***               * MSUM(54...63) *
!                                     ***           ***
!             WHERE I DENOTES A POINT ON THE SUB-TRIANGLE, AND
!                     REFERENCES GRID POINTS 1, 2, OR 3
!                     ON THE TRIANGULAR PLATE
!
!                                     ***           ***
!                   ***               * MSUM( 1... 9) *
!  M     I = PIVOT PT * ARE STORED IN * MSUM(10...18) *
!   IJ   J = 1,2      *               * MSUM(19...27) *
!                   ***               ***           ***
!             WHERE I DENOTES A POINT ON THE SUB-TRIANGLE AND
!                     REFERENCES POINTS 1, 2, 3, OR 4
!                     ON THE TRIANGULAR PLATE
!
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
!
               DO i = 1 , 3
                  CALL gmmatd(T(1),3,3,1,A(27*i-8),3,3,0,Temp9(1))
                  CALL gmmatd(Temp9(1),3,3,0,T(1),3,3,0,Prod9(1))
!
!     ADD THIS PRODUCT IN NOW.
!     COMPUTE POINTER TO MSUM MATRIX DESIRED.  (ZERO POINTER)
                  Npoint = Km + i
                  Npoint = 9*m(Npoint) + 18
!
                  DO k = 1 , 9
                     Nsubc = Npoint + k
                     msum(Nsubc) = msum(Nsubc) + Prod9(k)
                  ENDDO
               ENDDO
!
!
               DO k = 1 , 2
                  Npoint = Km + k
                  IF ( m(Npoint)==Npivot ) THEN
                     CALL gmmatd(T(1),3,3,1,A(36*k-35),3,3,0,Temp9(1))
                     CALL gmmatd(Temp9(1),3,3,0,T(1),3,3,0,Prod9(1))
!
!     COMPUTE POINTER TO MSUM MATRIX (ZERO POINTER)
!
                     Npoint = 9*Npivot - 9
                     DO i = 1 , 9
                        Nsubc = Npoint + i
                        msum(Nsubc) = msum(Nsubc) + Prod9(i)
                     ENDDO
!
                     CALL gmmatd(T(1),3,3,1,A(18*k-8),3,3,0,Temp9(1))
                     CALL gmmatd(Temp9(1),3,3,0,T(1),3,3,0,Prod9(1))
!
!     COMPUTE ZERO POINTER TO MSUM MATRIX DESIRED
!
                     Npoint = Km + 3 - k
                     Npoint = 9*m(Npoint) - 9
                     DO i = 1 , 9
                        Nsubc = Npoint + i
                        msum(Nsubc) = msum(Nsubc) + Prod9(i)
                     ENDDO
                  ENDIF
               ENDDO
!
!
! NOTE..THE CENTROID POINT IS A DUMMY POINT SO IT MUST BE REMOVED.
!          THIS IS DONE BY TRANSFERRING THE DISPLACEMENTS IN THE
!          MIDDLE TO BE A DIRECT FUNCTION OF THE OTHER DISPLACEMENTS.
!       THE TRANSFERENCE IS DONE THROUGH THE CREATION OF 3 (2X3)
!          HABC MATRICES, EACH CORRESPONDING TO A POINT OF THE
!          SUB-TRIANGLE.  EACH HABC MATRIX IS SUMMED WITH ITS
!          CORRESPONDENT IN THE G MATRIX 4 (3X3) ONE FOR EACH GRID POINT
!                                                AND THE CENTROID  POINT
!
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
               hq(3) = Ysubc*S1
               hq(4) = -3.0D0*X1*X1*C1
               hq(5) = Y1*(Xsubc*S1-Y1*C1)
               hq(6) = 3.0D0*Y1*Y1*S1
               hq(7) = 2.0D0*X2*C2
               hq(8) = X2*S2 + Y2*C2
               hq(9) = Ysubc*S2
               hq(10) = 3.0D0*X2*X2*C2
               hq(11) = Y2*(2.0D0*X2*S2+Y2*C2)
               hq(12) = 3.0D0*Y2*Y2*S2
!
!
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
!
            ENDDO
!
!     ******************************************************************
!
!ALCULATE MASS MATRIX PARTITIONS FOR WHOLE PLATE , ACCOUNTING FOR
! DISPLACEMENT OF CENTER.  EXPAND PARTITIONS TO (6X6) AND
! TRANSFORM TO GLOBAL COORDINATES
!
!
!
            DO i = 1 , 36
               tjte(i) = 0.0D0
            ENDDO
            em3 = dble(Ecpt(10))/6.0D0*r(1,2)*r(2,3)
!
!     FILL E-MATRIX
!
            DO i = 1 , 9
               E(i) = 0.0D0
            ENDDO
            DO i = 1 , 3
               Npoint = 3*i - 2
               E(Npoint) = Ivect(i)
               E(Npoint+1) = Jvect(i)
               E(Npoint+2) = Kvect(i)
            ENDDO
!
!
!              T
!     FORM   T   E      STORE IN TITE-MATRIX (6X3)
!             I
!
            IF ( necpt(4*Npivot+9)==0 ) THEN
!
               DO k = 1 , 9
                  tite(k) = E(k)
               ENDDO
            ELSE
               CALL transd(necpt(4*Npivot+9),T(1))
!
               CALL gmmatd(T(1),3,3,1,E(1),3,3,0,tite(1))
            ENDIF
!
!
!     SOLVE NOW FOR ....
!
!    E                   T     T                       T
! (M  ) = (M  ) - (TERM ) (M  ) - (M  )(TERM ) + (TERM )(M  )(TERM )
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
! (M  ) = (C ) (E)(M  )(E )(C )
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
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USES SUBSEQUENTLY.
            Ising = -1
            CALL inverd(3,g(28),3,Prod9,0,Determ,Ising,Temp9)
!
!     CHECK FOR SINGULARITY. ISING=2 IMPLIES SINGULARITY.
            IF ( Ising==1 ) THEN
!
               CALL gmmatd(g(28),3,3,0,g(9*Npivot-8),3,3,0,Prod9(1))
!
!                       T
!     GET  (TERM        )(M  )  STORE IN TEMP9
!               I=NPIVOT   44
!
               CALL gmmatd(Prod9(1),3,3,1,msum(55),3,3,0,Temp9(1))
!
!
!
!     THE TWO COMMON PRODUCTS ARE NOW AT HAND IN PROD9 AND TEMP9.
!
               DO j = 1 , 3
!
!                   T     T
!     (TERM        ) (M  )    STORE IN ARR9
!          I=NPIVOT    J4
!
                  CALL gmmatd(Prod9(1),3,3,1,msum(9*j+19),3,3,1,Arr9(1))
!
!     SUBTRACT FROM (M  )
!                     IJ
!
                  nbegin = 9*j - 9
                  DO i = 1 , 36
                     M6x6(i) = 0.0D0
                  ENDDO
                  DO i = 1 , 9
                     Npoint = nbegin + i
                     msum(Npoint) = msum(Npoint) - Arr9(i)
                  ENDDO
!
!
!      COMPUTE  (TERM )  STORE IN ARR9
!                   J
!
                  CALL gmmatd(g(28),3,3,0,g(9*j-8),3,3,0,Arr9(1))
!
!
!     GET  (M  )(TERM )  STORE IN ARRAY9
!            I4      J
!
                  CALL gmmatd(msum(9*Npivot+19),3,3,0,Arr9(1),3,3,0,Array9(1))
!
!     SUBTRACT FROM MIJ
!
                  DO i = 1 , 9
                     Npoint = nbegin + i
                     msum(Npoint) = msum(Npoint) - Array9(i)
                  ENDDO
!
!                           T
!     COMPUTE  (TERM        )(M  )(TERM ) = (TEMP9)(ARR9)
!                   I=NPOINT   44      J
!
                  CALL gmmatd(Temp9(1),3,3,0,Arr9(1),3,3,0,Array9(1))
!
!     ADD TO M
!             IJ
!
                  DO i = 1 , 9
                     Npoint = nbegin + i
                     msum(Npoint) = msum(Npoint) + Array9(i)
                  ENDDO
!
!
!       E
!     M    COMPLETE
!      IJ
!
!     TRANSFORM NOW, AND INSERT.
!
!
!     TRANSFORMATIONS AND INSERTION
!
                  IF ( necpt(4*j+9)==0 ) THEN
!
                     DO i = 1 , 3
                        Npoint = 6*i - 5
                        npt = Npoint + 21
                        tjte(Npoint) = E(i)
                        tjte(Npoint+1) = E(i+3)
                        tjte(Npoint+2) = E(i+6)
                        tjte(npt) = E(i)
                        tjte(npt+1) = E(i+3)
                        tjte(npt+2) = E(i+6)
                     ENDDO
                  ELSE
                     CALL transd(necpt(4*j+9),T(1))
                     CALL gmmatd(E(1),3,3,1,T(1),3,3,0,tjte(1))
                     DO i = 1 , 3
                        Npoint = i + 21
                        tjte(Npoint) = tjte(i)
                        tjte(Npoint+6) = tjte(i+3)
                        tjte(Npoint+12) = tjte(i+6)
                     ENDDO
                     DO i = 1 , 3
                        Npoint = i + 21
                        tjte(i) = tjte(Npoint)
                        tjte(i+6) = tjte(Npoint+6)
                        tjte(i+12) = tjte(Npoint+12)
                        tjte(i+3) = 0.0D0
!
                     ENDDO
                  ENDIF
!
!
!  EXPAND THE MSUM MATRIX (3X3) TO M6X6 MATRIX (6X6)
                  IF ( Npivot==j ) THEN
                     M6x6(1) = em3
                     M6x6(8) = em3
                  ENDIF
                  DO i = 1 , 3
                     Npoint = nbegin + i
                     M6x6(i+14) = msum(Npoint)
                     M6x6(i+20) = msum(Npoint+3)
                     M6x6(i+26) = msum(Npoint+6)
                  ENDDO
!
!
                  CALL gmmatd(M6x6(1),6,6,0,tjte(1),6,6,0,temp36(1))
                  CALL gmmatd(tite(1),3,3,0,temp36(1),3,6,0,mout(1))
                  CALL gmmatd(tite(1),3,3,0,temp36(19),3,6,0,mout(19))
!
!
                  CALL sma2b(mout(1),necpt(j+1),-1,Ifmgg,0.0D0)
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
!
!
   CALL mesage(30,26,Ecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
   Nogo = 1
END SUBROUTINE mtrplt
