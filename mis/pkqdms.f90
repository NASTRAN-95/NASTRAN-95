
SUBROUTINE pkqdms
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Consts(5) , Coord(16) , Cosang , Costh , Degra , Dum1(3) , Dum7(156) , Dumcl(145) , Dumm8(248) , Ecpt(100) , Eltemp ,       &
      & Ivec(3) , Jvec(3) , Kvec(3) , Pvec(3) , Si(3) , Sinang , Sinth , Stress , U1 , U2 , V(3) , Vecl , Vsubk(3)
   INTEGER Inflag , Matid , Mi , Mpoint , Necpt(5) , Ngrid(4) , Nogo , Npivot , Npvt , Nsubsc
   DOUBLE PRECISION K3x3(27) , Kij(36) , Ksum(36) , Temp
   COMMON /condas/ Consts
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /pla42c/ Npvt , Dum1 , Dumcl , Nogo
   COMMON /pla42d/ Kij , Dum7 , Ksum , Temp , Cosang , Sinang , Vecl , Ivec , Jvec , Kvec , Pvec , Vsubk , V , Si , Npivot ,        &
                 & Mpoint , Mi , Nsubsc , Ngrid , U1 , U2 , Coord , Dumm8
   COMMON /pla4es/ Ecpt
!
! Local variable declarations
!
   REAL angl
   INTEGER i , j , jnot , k , m(12) , npt1 , npt2 , npt3
!
! End of declarations
!
! THIS SUBROUTINE CALCULATES AND SHIPS TO PLA4B THE STIFFNESS MATRIX FOR
! PLA4
!     *** QUADRILATERAL MEMBRANE SUBROUTINE ***
!
!     CALLS FROM THIS ROUTINE ARE MADE TO THE FOLLOWING
!
!                 PKTRMS - TRIANGULAR MEMBRANE SUBROUTINE
!                 PLA4B  - INSERTION ROUTINE
!                MESAGE - ERROR MESSAGE WRITER
!
!
!
!
   EQUIVALENCE (Consts(4),Degra)
   EQUIVALENCE (K3x3(1),Kij(1)) , (Necpt(1),Ecpt(1))
!
   DATA m/1 , 2 , 4 , 2 , 3 , 1 , 3 , 4 , 2 , 4 , 1 , 3/
!     ******************************************************************
!          ECPT                       ECPT
!       RECEIVED BY                REQUIRED BY
!         KQDMEM                     KTRMEM
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
!     THE FOLLOWING COMPUTATION IS PERFORMED FOR USE WITH THE
!     COMPUTATION OF SINTH AND COSTH BELOW (ANISOTROPIC MATERIAL
!     POSSIBILITY)  NOTE  FMMS-46 PAGE -9-
!
   angl = Ecpt(6)*Degra
   Cosang = cos(angl)
   Sinang = sin(angl)
   Ivec(1) = Ecpt(15) - Ecpt(11)
   Ivec(2) = Ecpt(16) - Ecpt(12)
   Ivec(3) = Ecpt(17) - Ecpt(13)
   Vecl = sqrt(Ivec(1)**2+Ivec(2)**2+Ivec(3)**2)
   IF ( Vecl/=0.0E0 ) THEN
      Ivec(1) = Ivec(1)/Vecl
      Ivec(2) = Ivec(2)/Vecl
      Ivec(3) = Ivec(3)/Vecl
      Vsubk(1) = Ivec(2)*(Ecpt(25)-Ecpt(13)) - Ivec(3)*(Ecpt(24)-Ecpt(12))
      Vsubk(2) = Ivec(3)*(Ecpt(23)-Ecpt(11)) - Ivec(1)*(Ecpt(25)-Ecpt(13))
      Vsubk(3) = Ivec(1)*(Ecpt(24)-Ecpt(12)) - Ivec(2)*(Ecpt(23)-Ecpt(11))
      Vecl = sqrt(Vsubk(1)**2+Vsubk(2)**2+Vsubk(3)**2)
      IF ( Vecl/=0.0E0 ) THEN
         Kvec(1) = Vsubk(1)/Vecl
         Kvec(2) = Vsubk(2)/Vecl
         Kvec(3) = Vsubk(3)/Vecl
         Jvec(1) = Kvec(2)*Ivec(3) - Kvec(3)*Ivec(2)
         Jvec(2) = Kvec(3)*Ivec(1) - Kvec(1)*Ivec(3)
         Jvec(3) = Kvec(1)*Ivec(2) - Kvec(2)*Ivec(1)
         DO i = 1 , 3
            Pvec(i) = Cosang*Ivec(i) + Sinang*Jvec(i)
         ENDDO
!
!
!     SAVE COORDINATE SYSTEMS AND GRID POINT SIL NUMBERS
!
         Ngrid(1) = Necpt(2)
         Ngrid(2) = Necpt(3)
         Ngrid(3) = Necpt(4)
         Ngrid(4) = Necpt(5)
         DO i = 1 , 16
            Coord(i) = Ecpt(i+9)
         ENDDO
!
!     NOTE. COORD 1, 5, 9, AND 13  ARE INTEGER CSID NUMBERS.
!
!     CORRECT ECPT FOR MEMBRANE USE
         Ecpt(5) = Ecpt(6)
         Ecpt(6) = Ecpt(7)
         Ecpt(7) = Ecpt(8)/2.0E0
         Ecpt(8) = Ecpt(9)
!
!     FOR EACH TRIANGLE THEN THE THREE GRID POINTS AND COORDINATES
!     ARE INSERTED INTO THE ECPT BEFORE THE CALL TO KTRMEM.
!
!     FILL MAP MATRIX  (PERFORMED IN DATA STATEMENT - DO NOT ALTER)
!              A              B              C
!           M1 = 1         M2 = 2         M3 = 4      (TRIANGLE    I)
!
!           M4 = 2         M5 = 3         M6 = 1      (TRIANGLE   II)
!
!           M7 = 3         M8 = 4         M9 = 2      (TRIANGLE  III)
!
!           M10= 4         M11= 1         M12= 3      (TRIANGLE   IV)
!
!     ******************************************************************
!     FIND WHICH POINT IS THE PIVOT POINT.
         DO i = 1 , 4
            IF ( Npvt==Ngrid(i) ) THEN
               Npivot = i
               GOTO 20
            ENDIF
         ENDDO
!
!     FALL THRU ABOVE LOOP IMPLIES AN ERROR CONDITION.
!
         CALL mesage(-30,34,Ecpt(1))
!
!     COMPUTE JNOT WHICH EQUALS THE ONE TRIANGLE OF THE FOUR NOT USED
!     AND THUS NOT COMPUTED FOR THE PIVOT POINT IN QUESTION.  (NOTE THE
!     ROWS OF THE MAPPING MATRIX ABOVE AND THE TRIANGLE NUMBERS)
!
 20      IF ( Npivot<=2 ) THEN
            jnot = Npivot + 2
         ELSE
            jnot = Npivot - 2
         ENDIF
!
!     ZERO OUT KSUM FOR 36
         DO i = 1 , 36
            Ksum(i) = 0.0D0
         ENDDO
!
         DO j = 1 , 4
            IF ( j/=jnot ) THEN
!
!     FILL IN ECPT FOR TRIANGLE J
               Mpoint = 3*j - 3
               DO i = 1 , 3
                  npt1 = Mpoint + i
                  Nsubsc = m(npt1)
                  Necpt(i+1) = Ngrid(Nsubsc)
!
                  npt1 = 4*Nsubsc - 4
                  DO k = 1 , 4
                     npt2 = npt1 + k
                     npt3 = 4*i + 4 + k
                     Ecpt(npt3) = Coord(npt2)
                  ENDDO
               ENDDO
!
!     ECPT IS COMPLETE FOR TRIANGLE J
!
!     SET UP SINTH AND COSTH FOR THIS SUB TRIANGLE
!
               IF ( j/=1 ) THEN
!
!     NOTE FMMS-46 PAGE-9 FOR FOLLOWING
!
                  V(1) = Ecpt(14) - Ecpt(10)
                  V(2) = Ecpt(15) - Ecpt(11)
                  V(3) = Ecpt(16) - Ecpt(12)
                  Vecl = sqrt(V(1)**2+V(2)**2+V(3)**2)
                  IF ( Vecl==0.0E0 ) GOTO 100
                  U1 = (V(1)*Pvec(1)+V(2)*Pvec(2)+V(3)*Pvec(3))/Vecl
                  Si(1) = V(2)*Pvec(3) - V(3)*Pvec(2)
                  Si(2) = V(3)*Pvec(1) - V(1)*Pvec(3)
                  Si(3) = V(1)*Pvec(2) - V(2)*Pvec(1)
                  U2 = (Si(1)*Kvec(1)+Si(2)*Kvec(2)+Si(3)*Kvec(3))/Vecl
                  Vecl = sqrt(U1**2+U2**2)
                  IF ( Vecl==0.0E0 ) GOTO 100
                  U1 = U1/Vecl
                  U2 = U2/Vecl
                  Sinth = Sinang*U1 - Cosang*U2
                  Costh = Cosang*U1 + Sinang*U2
               ELSE
                  Sinth = Sinang
                  Costh = Cosang
               ENDIF
               IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0E0
!
               CALL pktrms(1)
!
!     RETURNING FROM PKTRMS THE 3 3X3 ARRAYS FOR THE PIVOT ARE STORED IN
!     COMMON UNDER THE NAME   K3X3(27)
!
!     NOW ADD THE 3 3X3 ARRAYS INTO THE 4 3X3 ARRAYS OF KSUM
!
               DO i = 1 , 3
                  npt1 = 9*i - 9
!     NPT1   POINTS TO THE ZERO POSITION OF THE I-TH K3X3.
!     MPOINT POINTS TO THE ZERO POSITION OF THE J-TH ROW OF MAP MATRIX
!
                  Mi = Mpoint + i
                  npt2 = 9*m(Mi) - 9
!     NPT2 NOW POINTS TO THE ZERO POSITION OF THE  M(MI) TH  SUM MATRIX
!
                  DO k = 1 , 9
                     npt3 = npt2 + k
                     Mi = npt1 + k
                     Ksum(npt3) = Ksum(npt3) + K3x3(Mi)
                  ENDDO
               ENDDO
            ENDIF
!
         ENDDO
!
!     ******************************************************************
!
!     NOW INSERT EACH OF THE 4-KSUM (3X3) MATRICES INTO A 6X6 AND
!     SHIP TO PLA4B
!
         DO i = 1 , 36
            Kij(i) = 0.0D0
         ENDDO
!
         DO j = 1 , 4
            Mpoint = 9*j - 9
!     MPOINT POINTS TO THE ZERO POSITION OF THE J-TH KSUM 3X3.
            Kij(1) = Ksum(Mpoint+1)
            Kij(2) = Ksum(Mpoint+2)
            Kij(3) = Ksum(Mpoint+3)
            Kij(7) = Ksum(Mpoint+4)
            Kij(8) = Ksum(Mpoint+5)
            Kij(9) = Ksum(Mpoint+6)
            Kij(13) = Ksum(Mpoint+7)
            Kij(14) = Ksum(Mpoint+8)
            Kij(15) = Ksum(Mpoint+9)
!
!     SHIP TO PLA4B
            CALL pla4b(Kij(1),Ngrid(j))
!
         ENDDO
!
         RETURN
      ENDIF
   ENDIF
 100  CALL mesage(30,26,Ecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
   Nogo = 1
END SUBROUTINE pkqdms
