!*==dqdmem.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dqdmem
   IMPLICIT NONE
   USE C_CONDAS
   USE C_DS1AAA
   USE C_DS1ADP
   USE C_DS1AET
   USE C_MATIN
!
! Local variable declarations rewritten by SPAG
!
   REAL :: degra
   INTEGER :: i , j , k
   INTEGER , DIMENSION(12) , SAVE :: m
   INTEGER , DIMENSION(5) :: necpt
   EXTERNAL dtrmem , mesage
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!
!     QUADRILATERAL MEMBRANE ROUTINE FOR DIFFERENTIAL STIFFNESS..
!
!
!
!
   !>>>>EQUIVALENCE (Consts(4),Degra)
   !>>>>EQUIVALENCE (Necpt(1),Ecpt(1))
!
   DATA m/1 , 2 , 4 , 2 , 3 , 1 , 3 , 4 , 2 , 4 , 1 , 3/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!     ******************************************************************
!          ECPT                       ECPT
!       RECEIVED BY                REQUIRED BY
!         DQDMEM                     DTRMEM
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
!     ECPT(21) = Z3              ECPT(21) = ELEMENT TEMP.
!     ECPT(22) = COORD. SYS. ID 4ECPT(22) = EL. DEF.
!     ECPT(23) = X4              ECPT(23) = LDTEMP
!     ECPT(24) = Y4              ECPT(24) = XT 1
!     ECPT(25) = Z4              ECPT(25) = YT 1
!     ECPT(26) = ELEMENT TEMP.   ECPT(26) = ZT 1
!     ECPT(27) = EL. DEF.        ECPT(27) = XT 2
!     ECPT(28) = LDTEMP          ECPT(28) = YT 2
!     ECPT(29) = XT 1            ECPT(29) = ZT 2
!     ECPT(30) = YT 1            ECPT(30) = XT 3
!     ECPT(31) = ZT 1            ECPT(31) = YT 3
!     ECPT(32) = XT 2            ECPT(32) = ZT 3
!     ECPT(33) = YT 2
!     ECPT(34) = ZT 2
!     ECPT(35) = XT 3
!     ECPT(36) = YT 3
!     ECPT(37) = ZT 3
!     ECPT(38) = XT 4
!     ECPT(39) = YT 4
!     ECPT(40) = ZT 4
!     ******************************************************************
!
!     THE FOLLOWING COMPUTATION IS PERFORMED FOR USE WITH THE
!     COMPUTATION OF SINTH AND COSTH BELOW (ANISOTROPIC MATERIAL
!     POSSIBILITY)  NOTE  FMMS-46 PAGE -9-
!
         Angl = Ecpt(6)*degra
         Cosang = cos(Angl)
         Sinang = sin(Angl)
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
!     SAVE COORDINATE SYSTEMS, GRID POINT SIL NUMBERS, AND DISP VECTOR.
!
               Ngrid(1) = necpt(2)
               Ngrid(2) = necpt(3)
               Ngrid(3) = necpt(4)
               Ngrid(4) = necpt(5)
!
               DO i = 1 , 16
                  Coord(i) = Ecpt(i+9)
               ENDDO
!
               DO i = 1 , 12
                  Sdisp(i) = Ecpt(i+28)
               ENDDO
!
!     NOTE. COORD 1, 5, 9, AND 13  ARE INTEGER CSID NUMBERS.
!
!     CORRECT ECPT FOR MEMBRANE USE
               Ecpt(5) = Ecpt(6)
               Ecpt(6) = Ecpt(7)
               Ecpt(7) = Ecpt(8)/2.0E0
               Ecpt(8) = Ecpt(9)
               Ecpt(21) = Ecpt(26)
               Ecpt(22) = Ecpt(27)
               Ecpt(23) = Ecpt(28)
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
                     GOTO 5
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
 5             IF ( Npivot<=2 ) THEN
                  Jnot = Npivot + 2
               ELSE
                  Jnot = Npivot - 2
               ENDIF
!
!
               DO j = 1 , 4
                  IF ( j/=Jnot ) THEN
!
!     FILL IN ECPT FOR TRIANGLE J
                     Mpoint = 3*j - 3
                     DO i = 1 , 3
                        Npt1 = Mpoint + i
                        Nsubsc = m(Npt1)
                        necpt(i+1) = Ngrid(Nsubsc)
!
                        Npt1 = 3*Nsubsc - 3
                        Npt3 = 3*i + 20
                        DO k = 1 , 3
                           Npt2 = Npt1 + k
                           Npt3 = Npt3 + 1
                           Ecpt(Npt3) = Sdisp(Npt2)
                        ENDDO
!
                        Npt1 = 4*Nsubsc - 4
                        DO k = 1 , 4
                           Npt2 = Npt1 + k
                           Npt3 = 4*i + 4 + k
                           Ecpt(Npt3) = Coord(Npt2)
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
                        IF ( Vecl==0.0E0 ) THEN
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        U1 = (V(1)*Pvec(1)+V(2)*Pvec(2)+V(3)*Pvec(3))/Vecl
                        Si(1) = V(2)*Pvec(3) - V(3)*Pvec(2)
                        Si(2) = V(3)*Pvec(1) - V(1)*Pvec(3)
                        Si(3) = V(1)*Pvec(2) - V(2)*Pvec(1)
                        U2 = (Si(1)*Kvec(1)+Si(2)*Kvec(2)+Si(3)*Kvec(3))/Vecl
                        Vecl = sqrt(U1**2+U2**2)
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
                     CALL dtrmem(1)
                  ENDIF
!
!     INSERTIONS ARE PERFORMED BY DTRMEM
!
               ENDDO
!
!     ******************************************************************
!
               RETURN
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(30,26,Ecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
         Nogo = 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dqdmem
