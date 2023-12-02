!*==qdplt.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE qdplt(Ti)
   IMPLICIT NONE
   USE C_CONDAS
   USE C_MATIN
   USE C_SSGTRI
   USE C_SSGWRK
   USE C_TRIMEX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: Ti
!
! Local variable declarations rewritten by SPAG
!
   REAL :: degra , ftemp
   INTEGER :: i , j , k , km , l
   INTEGER , DIMENSION(12) , SAVE :: m
   INTEGER , DIMENSION(100) :: necpt
   REAL , DIMENSION(10) :: requiv
   REAL , DIMENSION(3) :: vq1 , vq2 , vq3 , vq4
   EXTERNAL gbtran , gmmats , mesage , ssgkhi , trbsc
!
! End of declarations rewritten by SPAG
!
!
!     THERMAL LOADING FOR THE BENDING QUADRILATERAL
!
!                 DEFINITION                   DEFINITION
!       ECPT      BSC.BEND.TRI.------TYPE      QUAD.PLT.----------TYPE
!     ========   =================  ======     ================  =======
!     ECPT( 1) = ELEMENT ID         INTEGER ** ELEMENT           INTEGER
!     ECPT( 2) = GRID PT. A         INTEGER ** GRID PT.A         INTEGER
!     ECPT( 3) = GRID PT. B         INTEGER ** GRID PT.B         INTEGER
!     ECPT( 4) = GRID PT. C         INTEGER ** GRID PT.C         INTEGER
!     ECPT( 5) = THETA              REAL    ** GRID PT.D         INTEGER
!     ECPT( 6) = MAT ID 1           INTEGER ** THETA             REAL
!     ECPT( 7) = I  MOM. OF INERT.  REAL    ** MAT ID 1          INTEGER
!     ECPT( 8) = MAT ID 2           INTEGER ** I  MOM. OF INERT. REAL
!     ECPT( 9) = T2                 REAL    ** MAT ID 2          INTEGER
!     ECPT(10) = NON-STRUCT. MASS   REAL    ** T2                REAL
!     ECPT(11) = Z1                 REAL    ** NON-STRUCT. MASS  REAL
!     ECPT(12) = Z2                 REAL    ** Z1                REAL
!     ECPT(13) = COORD. SYS. ID 1   INTEGER ** Z2                REAL
!     ECPT(14) = X1                 REAL    ** COORD. SYS. ID 1  INTEGER
!     ECPT(15) = Y1                 REAL    ** X1                REAL
!     ECPT(16) = Z1                 REAL    ** Y1                REAL
!     ECPT(17) = COORD. SYS. ID 2   INTEGER ** Z1                REAL
!     ECPT(18) = X2                 REAL    ** COORD. SYS. ID 2  INTEGER
!     ECPT(19) = Y2                 REAL    ** X2                REAL
!     ECPT(20) = Z2                 REAL    ** Y2                REAL
!     ECPT(21) = COORD. SYS. ID 3   INTEGER ** Z2                REAL
!     ECPT(22) = X3                 REAL    ** COORD. SYS. ID 3  INTEGER
!     ECPT(23) = Y3                 REAL    ** X3                REAL
!     ECPT(24) = Z3                 REAL    ** Y3                REAL
!     ECPT(25) = ELEMENT TEMP       REAL    ** Z3                REAL
!     ECPT(26) =                            ** COORD. SYS. ID 4  INTEGER
!     ECPT(27) =                            ** X4                REAL
!     ECPT(28) =                            ** Y4                REAL
!     ECPT(29) =                            ** Z4                REAL
!     ECPT(30) =                            ** ELEMENT TEMP      REAL
!
   !>>>>EQUIVALENCE (Consts(4),Degra) , (Ecpt(1),Necpt(1)) , (Vq1(1),Ecpt(15)) , (Vq2(1),Ecpt(19)) , (Vq3(1),Ecpt(23)) ,                 &
!>>>>    & (Vq4(1),Ecpt(27)) , (Requiv(1),R(1,1))
   DATA m/2 , 4 , 1 , 3 , 1 , 2 , 4 , 2 , 3 , 1 , 3 , 4/
!
   Theta = Ecpt(6)*degra
   Sinang = sin(Theta)
   Cosang = cos(Theta)
!
!     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
!     SUB TRIANGLES. (2X5) FOR QUADRILATERAL PLATE.
!     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
!
!     ZERO OUT R-MATRIX
!
   DO i = 1 , 10
      requiv(i) = 0.0
   ENDDO
!
!     SHIFT ECPT UP TO MATCH STRBS1 FOR CERTAIN VARIABLES.
!
   DO i = 6 , 12
      Ecpt(i) = Ecpt(i+1)
   ENDDO
!
   DO i = 1 , 3
      D1(i) = vq3(i) - vq1(i)
      D2(i) = vq4(i) - vq2(i)
      A1(i) = vq2(i) - vq1(i)
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
   Temp = sqrt(Kvect(1)**2+Kvect(2)**2+Kvect(3)**2)
   DO i = 1 , 3
      Kvect(i) = Kvect(i)/Temp
   ENDDO
!
!     COMPUTE H = (A1 DOT KVECT)/2
!
   Temp = (A1(1)*Kvect(1)+A1(2)*Kvect(2)+A1(3)*Kvect(3))/2.0
!
!     I-VECTOR = (A1) - H*(KVECT)    NON-NORMALIZED
!
   DO i = 1 , 3
      Ivect(i) = A1(i) - Temp*Kvect(i)
   ENDDO
!
!     NORMALIZE I-VECTOR
!
   Temp = sqrt(Ivect(1)**2+Ivect(2)**2+Ivect(3)**2)
   DO i = 1 , 3
      Ivect(i) = Ivect(i)/Temp
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
!     STRESS CALCULATION POINT WHICH IS THE DIAGONALS INTERSECTION.
!
   ftemp = R(1,3)*R(2,4) + R(2,3)*(R(1,2)-R(1,4))
   IF ( ftemp==0.0 ) CALL mesage(-30,26,Ecpt(1))
   R(1,5) = R(1,2)*R(1,3)*R(2,4)/ftemp
   R(2,5) = R(1,2)*R(2,3)*R(2,4)/ftemp
!
!     CHECK OF 4 POINTS FOR ANGLE GREATER THAN OR EQUAL TO 180 DEGREES.
!
   IF ( R(2,3)<=0.0 .OR. R(2,4)<=0.0 ) THEN
      CALL mesage(-30,35,Ecpt(1))
   ELSE
      Temp = R(1,2) - (R(1,2)-R(1,3))*R(2,4)/R(2,3)
      IF ( R(1,4)>=Temp ) THEN
         CALL mesage(-30,35,Ecpt(1))
      ELSE
         Temp = R(2,3)*R(1,4)/R(2,4)
         IF ( R(1,3)<=Temp ) CALL mesage(-30,35,Ecpt(1))
      ENDIF
   ENDIF
!
!     SET UP THE M-MATRIX FOR MAPPING TRIANGLES, IN DATA STATEMENT...
!
!     COMPUTE SUB-TRIANGLE COORDINATES
!     CALL BASIC BENDING ROUTINE FOR ALL SUB-TRIANGLES.
!
   Eltemp = Ecpt(30)
   DO i = 1 , 60
      Ssum(i) = 0.0
   ENDDO
!
   DO j = 1 , 4
      km = 3*j - 3
      Subsca = m(km+1)
      Subscb = m(km+2)
      Subscc = m(km+3)
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
      Xc = sqrt((R(1,Subsca)-R(1,5))**2+(R(2,Subsca)-R(2,5))**2)
      Yc = 0.0
!
      Sinth = Sinang*U1 - Cosang*U2
      Costh = Cosang*U1 + Sinang*U2
      IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0
!
!     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR
!     TRIANGLE -J-
!
      CALL trbsc(1,Ti(1))
!
!     RETURNING FROM STRBS1 THE FOLLOWING QUANTITIES ARE AT HAND.
!
!       S  , S  , S  , EACH 5X3.   45 WORDS STORED IN A(1) THRU A(45)
!        A    B    C
!
!     COMPUTE KHI   (5X1)  AFTER THE FIRST SUB-TRIANGLE ONLY.
!                E
!
!     NFACTOR FOR THE QUADRILATERAL IS 2.0
!
!
!     SINTH  = SINANG
!     COSTH  = COSANG
!     STRESS = 0
!     MATID  = NECPT(6)
!     INFLAG = 2
!     CALL MAT (NECPT(1))
!
      IF ( j==1 ) CALL ssgkhi(Ti(1),Ti(1),2.0)
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
      V(1) = U1*U1*0.250
      V(2) = U2*U2*0.250
      V(11) = U1*U2*0.250
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
      V(19) = U1*0.250
      V(20) = -U2*0.250
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
         CALL gmmats(V,5,5,0,A(15*i-14),5,3,0,Temp15)
         CALL gmmats(Temp15,5,3,0,T,3,3,0,Prod15)
!
!     POINTER TO SSUM MATRIX
!
         Npoint = km + i
         Npoint = 15*m(Npoint) - 15
         DO k = 1 , 15
            Nsubc = Npoint + k
            Ssum(Nsubc) = Ssum(Nsubc) + Prod15(k)
         ENDDO
      ENDDO
!
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
   DO i = 1 , 4
!
!     DO WE NEED TRANSFORMATION T
!                                I
      Nsubc = 4*i + 10
      IF ( necpt(Nsubc)==0 ) THEN
!
         DO k = 1 , 18
            Tite(k) = E(k)
         ENDDO
      ELSE
         CALL gbtran(necpt(Nsubc),necpt(Nsubc+1),T(1))
         CALL gmmats(T,3,3,1,E(1),3,3,0,Tite(1))
         CALL gmmats(T,3,3,1,E(10),3,3,0,Tite(10))
      ENDIF
!
      CALL gmmats(Ssum(15*i-14),5,3,0,Tite,6,3,1,Ks(1))
!
!     COMPUTE LOAD VECTOR AND INSERT IT IN OPEN CORE
!
      CALL gmmats(Ks(1),5,6,1,Khi(1),5,1,0,P(1))
      k = necpt(i+1) - 1
      DO l = 1 , 6
         k = k + 1
         Z(k) = Z(k) + P(l)
      ENDDO
!
   ENDDO
END SUBROUTINE qdplt
