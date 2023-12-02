!*==pkqdm1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pkqdm1
   IMPLICIT NONE
   USE C_CONDAS
   USE C_MATIN
   USE C_PLA42C
   USE C_PLA42S
   USE C_PLA4ES
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(16) :: coord
   REAL :: degra
   INTEGER :: i , j , k
   INTEGER , DIMENSION(12) , SAVE :: m
   INTEGER , DIMENSION(100) :: necpt
   INTEGER , DIMENSION(4) :: ngrid
   REAL , DIMENSION(6) :: r
   REAL , DIMENSION(27) :: s
   EXTERNAL gmmats , mesage , pktrm1
!
! End of declarations rewritten by SPAG
!
!  THIS ROUTINE CALCULATES PHASE I OUTPUT FOR THE QUAD-MEMBRAND IN
!   PLA4
!
!
!
   !>>>>EQUIVALENCE (Consts(4),Degra)
   !>>>>EQUIVALENCE (Necpt(1),Ecpt(1))
   !>>>>EQUIVALENCE (R(1),Ivec(1)) , (Ngrid(1),Ecptsa(2)) , (Coord(1),Ecptsa(10)) , (S(1),Ph1out(10))
!
   DATA m/1 , 2 , 4 , 2 , 3 , 1 , 3 , 4 , 2 , 4 , 1 , 3/
!     ******************************************************************
!          ECPT                       ECPT
!       RECEIVED BY                REQUIRED BY
!         SQDME1                     STRME1
!     ******************************************************************
!     ECPT( 1) = EL. ID          ECPT( 1) = EL. ID
!     ECPT( 2) = GRD. PT. A      ECPT( 2) = GRD. PT. A
!     ECPT( 3) = GRD. PT. B      ECPT( 3) = GRD. PT. B
!     ECPT( 4) = GRD. PT. C      ECPT( 4) = GRD. PT. C
!     ECPT( 5) = GRD. PT. D      ECPT( 5) = THETA
!     ECPT( 6) = THETA           ECPT( 6) = MATERIAL ID
!     ECPT( 7) = MATERIAL ID     ECPT( 7) = T
!     ECPT( 8) = T               ECPT( 8) = NON-STRUCT. MASS
!     ECPT( 9) = NON-STRUCT. MASSECPT( 9) = COORD. SYS. ID 1
!     ECPT(10) = COORD. SYS. ID 1ECPT(10) = X1
!     ECPT(11) = X1              ECPT(11) = Y1
!     ECPT(12) = Y1              ECPT(12) = Z1
!     ECPT(13) = Z1              ECPT(13) = COORD. SYS. ID 2
!     ECPT(14) = COORD. SYS. ID 2ECPT(14) = X2
!     ECPT(15) = X2              ECPT(15) = Y2
!     ECPT(16) = Y2              ECPT(16) = Z2
!     ECPT(17) = Z2              ECPT(17) = COORD. SYS. ID 3
!     ECPT(18) = COORD. SYS. ID 3ECPT(18) = X3
!     ECPT(19) = X3              ECPT(19) = Y3
!     ECPT(20) = Y3              ECPT(20) = Z3
!     ECPT(21) = Z3              ECPT(21) = ELEMENT TEMPERATURE
!     ECPT(22) = COORD. SYS. ID 4    NOTE. THE FOLLOWING ARE INTEGERS...
!     ECPT(23) = X4                  GRID POINTS, MAT ID, EL.ID,
!     ECPT(24) = Y4                  COORD. SYS. IDS.
!     ECPT(25) = Z4                  ALL OTHERS ARE REAL IN THE ECPT.
!     ECPT(26) = ELEMENT TEMPERATURE
!     ******************************************************************
!
!
!     VECTORS D1 AND D2  FMMS-46 PAGE 6
!     A1 A2 A3 A4
!
   DO i = 1 , 3
      D1(i) = Ecpt(i+18) - Ecpt(i+10)
      D2(i) = Ecpt(i+22) - Ecpt(i+14)
      A1(i) = Ecpt(i+14) - Ecpt(i+10)
      A2(i) = Ecpt(i+18) - Ecpt(i+14)
      A3(i) = Ecpt(i+22) - Ecpt(i+18)
      A4(i) = Ecpt(i+10) - Ecpt(i+22)
   ENDDO
!
!     K-VECTOR = NORMALIZED D1 CROSS D2
!
   Kvec(1) = D1(2)*D2(3) - D1(3)*D2(2)
   Kvec(2) = D1(3)*D2(1) - D1(1)*D2(3)
   Kvec(3) = D1(1)*D2(2) - D1(2)*D2(1)
   Vecl = sqrt(Kvec(1)**2+Kvec(2)**2+Kvec(3)**2)
   IF ( Vecl>=1.0E-06 ) THEN
      Kvec(1) = Kvec(1)/Vecl
      Kvec(2) = Kvec(2)/Vecl
      Kvec(3) = Kvec(3)/Vecl
!
!     I-VECTOR = NORMALIZED A SUB 12 - H * KVECTOR
!     GET H FIRST = ( A SUB 12 DOT KVECTOR)/2
!
      H = (A1(1)*Kvec(1)+A1(2)*Kvec(2)+A1(3)*Kvec(3))/2.0E0
!
      Ivec(1) = A1(1) - H*Kvec(1)
      Ivec(2) = A1(2) - H*Kvec(2)
      Ivec(3) = A1(3) - H*Kvec(3)
      Vecl = sqrt(Ivec(1)**2+Ivec(2)**2+Ivec(3)**2)
      IF ( Vecl>=1.0E-06 ) THEN
         Ivec(1) = Ivec(1)/Vecl
         Ivec(2) = Ivec(2)/Vecl
         Ivec(3) = Ivec(3)/Vecl
!
!     J-VECTOR = K CROSS I
!
         Jvec(1) = Kvec(2)*Ivec(3) - Kvec(3)*Ivec(2)
         Jvec(2) = Kvec(3)*Ivec(1) - Kvec(1)*Ivec(3)
         Jvec(3) = Kvec(1)*Ivec(2) - Kvec(2)*Ivec(1)
!
         Vecl = sqrt(Jvec(1)**2+Jvec(2)**2+Jvec(3)**2)
         Jvec(1) = Jvec(1)/Vecl
         Jvec(2) = Jvec(2)/Vecl
         Jvec(3) = Jvec(3)/Vecl
!
         Theta = Ecpt(6)*degra
         Sinang = sin(Theta)
         Cosang = cos(Theta)
!
         V(1) = 1.0E0
         V(2) = 0.0E0
!
!     R ARRAY IS EQUIVALENCED TO IVECTOR AND JVECTOR
!
         CALL gmmats(r,2,3,0,A2,3,1,0,V(3))
         CALL gmmats(r,2,3,0,A3,3,1,0,V(5))
         CALL gmmats(r,2,3,0,A4,3,1,0,V(7))
!
!     NORMALIZE THE 4 2X1 V ARRAYS
!
         DO i = 1 , 4
            Vecl = sqrt(V(2*i-1)**2+V(2*i)**2)
            IF ( Vecl<1.0E-10 ) CALL mesage(-30,26,Ecpt(1))
            V(2*i-1) = V(2*i-1)/Vecl
            V(2*i) = V(2*i)/Vecl
         ENDDO
!
!     MAPPING MATRIX M IS IN DATA STATEMENT.
!
!     NOW MAKE 4 CALLS TO PKTRM1 WHICH WILL RETURN
!     S , S , S , S , T SUB 0
!      A   B   C   T
!
!     SAVE GRID SILS AND COORDINATE SYSTEMS.
!
         DO i = 1 , 36
            Ecptsa(i) = Ecpt(i)
         ENDDO
!
         Ecpt(6) = Ecpt(7)
         Ecpt(7) = Ecpt(8)
         Ecpt(8) = Ecpt(9)
!
!     ZERO OUT SUM MATRICES
!
         DO i = 1 , 36
            Sum(i) = 0.0E0
         ENDDO
         St(1) = 0.0E0
         St(2) = 0.0E0
         St(3) = 0.0E0
!
         DO i = 1 , 4
!
!     POINTER TO THE SILS IN THE MAPPING MATRIX
            Ncoord = 8
            Npoint = 3*i - 3
            DO j = 2 , 4
               Npoint = Npoint + 1
               Nsub1 = m(Npoint)
               DO k = 1 , 4
                  Nsub3 = 4*Nsub1 - 4 + k
                  Ncoord = Ncoord + 1
                  Ecpt(Ncoord) = coord(Nsub3)
               ENDDO
               necpt(j) = ngrid(Nsub1)
            ENDDO
!
!     SET UP T MATRIX FOR THIS TRIANGLE.  T IS 3X3
!
            U1 = V(2*i-1)
            U2 = V(2*i)
!
            T(1) = U1**2
            T(2) = U2**2
            T(7) = U1*U2
            T(3) = -2.0E0*T(7)
            T(4) = T(2)
            T(5) = T(1)
            T(6) = -T(3)
            T(8) = -T(7)
            T(9) = T(1) - T(2)
!
!     COMPUTE NET SINTH AND COSTH FOR ANISOTROPIC POSSIBILITY
!
            Sinth = Sinang*U1 - Cosang*U2
            Costh = Cosang*U1 + Sinang*U2
!
            CALL pktrm1(1)
!
!
!     NOW TRANSFORM AND ADD THE S MATRICES INTO THE RESPECTIVE SUM
!     MATRICES.
!
            DO j = 1 , 3
!
!     POINTER TO TRIANGLE I ROW IN THE MAPPING MATRIX
!
               Npoint = 3*i - 3
!
!     TRANSFORM S
!
               CALL gmmats(T,3,3,0,s(9*j-8),3,3,0,Stemp)
!
!     ADD STEMP INTO RESPECTIVE KSUM POSITIONS
!
!     ZERO POINTER INTO KSUM MATRICES
               Nsub1 = Npoint + j
               Nsub1 = m(Nsub1)*9 - 9
               DO k = 1 , 9
                  Nsub1 = Nsub1 + 1
                  Sum(Nsub1) = Sum(Nsub1) + Stemp(k)
               ENDDO
            ENDDO
         ENDDO
!
!     ALL MATRICES COMPLETE
!
!     FILL OUTPUT BLOCK
!
         DO i = 1 , 5
            Ph1out(i) = Ecptsa(i)
         ENDDO
         DO i = 1 , 36
            Ph1out(i+9) = 0.25E0*Sum(i)
         ENDDO
!     PHASE 1 COMPLETE OUTPUT BLOCK CONTAINS 45 WORDS
!
         RETURN
      ENDIF
   ENDIF
   CALL mesage(30,26,Ecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
   Nogo = 1
END SUBROUTINE pkqdm1
