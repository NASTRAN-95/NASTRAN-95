
SUBROUTINE dpse3
   IMPLICIT NONE
   DOUBLE PRECISION C(9) , Dp(21) , Gamma , Kij(36) , Sgn , Sgn1 , Sgn2
   REAL Dum2(2) , Dum9(9) , Ecpt(21)
   INTEGER Icstm , Ncstm , Necpt(5) , Npvt
   COMMON /ds1aaa/ Npvt , Icstm , Ncstm
   COMMON /ds1adp/ Gamma , Kij , Sgn , Sgn1 , Sgn2 , Dp , C
   COMMON /ds1aet/ Ecpt , Dum2 , Dum9
   INTEGER i , j , k , k1 , k2 , kk2 , kk3 , ni , nj , nk , npivot
!
!     PRESSURE STIFFNESS CALCULATIONS FOR A TRIANGULAR MEMBRANE
!     ELEMENT (3 GRID POINTS).
!     THREE 6X6 STIFFNESS MATRICES FOR THE PIVOT POINT ARE INSERTED.
!
!     DOUBLE PRECISION VERSION
!
!     WRITTEN BY E. R. CHRISTENSEN/SVERDRUP, 9/91, VERSION 1.1
!     INSTALLED IN NASTRAN AS ELEMENT DPSE3 BY G.CHAN/UNISYS, 2/92
!
!     REFERENCE - E. CHRISTENEN: 'ADVACED SOLID ROCKET MOTOR (ASRM)
!                 MATH MODELS - PRESSURE STIFFNESS EFFECTS ANALYSIS',
!                 NASA TD 612-001-02, AUGUST 1991
!
!     LIMITATION -
!     (1) ALL GRID POINTS USED BY ANY IF THE CPSE2/3/4 ELEMENTS MUST BE
!         IN BASIC COORDINATE SYSTEM!!!
!     (2) CONSTANT PRESSURE APPLIED OVER AN ENCLOSED VOLUMN ENCOMPASSED
!         BY THE CPSE2/3/4 ELEMENTRS
!     (3) PRESSURE ACTS NORMALLY TO THE CPSE2/3/4 SURFACES
!
!     SEE NASTRAN DEMONSTRATION PROBLEM -  T13022A
!
!     COMMON /SYSTEM/  IBUF,NOUT
   EQUIVALENCE (Necpt(1),Ecpt(1))
!
!     ECPT FOR THE PRESSURE STIFFNESS CPES3 ELEMENT
!
!     ECPT( 1) = ELEMENT ID
!     ECPT( 2) = SIL FOR GRID POINT A OR 1
!     ECPT( 3) = SIL FOR GRID POINT B OR 2
!     ECPT( 4) = SIL FOR GRID POINT C OR 3
!     ECPT( 5) = PRESSURE
!     ECPT( 6) = NOT USED
!     ECPT( 7) = NOT USED
!     ECPT( 8) = NOT USED
!     ECPT( 9) = COORD. SYSTEM ID 1
!     ECPT(10) = X1
!     ECPT(11) = Y1
!     ECPT(12) = Z1
!     ECPT(13) = COORD. SYSTEM ID 2
!     ECPT(14) = X2
!     ECPT(15) = Y2
!     ECPT(16) = Z2
!     ECPT(17) = COORD. SYSTEM ID 3
!     ECPT(18) = X3
!     ECPT(19) = Y3
!     ECPT(20) = Z3
!     ECPT(21) = ELEMENT TEMPERATURE
!     ECPT(22) THRU (32) = DUM2 AND DUM9, NOT USED IN THIS ROUTINE
!
!     STORE ECPT IN DOUBLE PRECISION
!
   Dp(5) = Ecpt(5)
   k = 9
   DO i = 1 , 3
      DO j = 1 , 3
         k = k + 1
         Dp(k) = Ecpt(k)
      ENDDO
      k = k + 1
   ENDDO
!
!     CALCULATE THE THREE VECTORS R1, R2 AND R2 USED IN COMPUTING
!     THE PRESSURE STIFFNESS MATRICES:
!
!     R1 = RA - 2*RC + RB
!     R2 = 2*RB - RA - RC
!     R3 = RB - 2*RA + RC
!
!     R1 STORED IN C(1), C(2), C(3)
!     R2 STORED IN C(4), C(5), C(6)
!     R3 STORED IN C(7), C(8), C(9)
!
   C(1) = Dp(10) - 2.0D0*Dp(18) + Dp(14)
   C(2) = Dp(11) - 2.0D0*Dp(19) + Dp(15)
   C(3) = Dp(12) - 2.0D0*Dp(20) + Dp(16)
!
   C(4) = 2.0D0*Dp(14) - Dp(10) - Dp(18)
   C(5) = 2.0D0*Dp(15) - Dp(11) - Dp(19)
   C(6) = 2.0D0*Dp(16) - Dp(12) - Dp(20)
!
   C(7) = Dp(14) - 2.0D0*Dp(10) + Dp(18)
   C(8) = Dp(15) - 2.0D0*Dp(11) + Dp(19)
   C(9) = Dp(16) - 2.0D0*Dp(12) + Dp(20)
!
   DO i = 1 , 3
      IF ( Necpt(i+1)==Npvt ) THEN
         npivot = i
         GOTO 100
      ENDIF
   ENDDO
   RETURN
!
!     GENERATE THE THREE BY THREE PARTITIONS IN GLOBAL COORDINATES HERE
!
!     SET COUNTERS ACCORDING TO WHICH GRID POINT IS THE PIVOT
!
 100  IF ( npivot<2 ) THEN
!
!     SET COUNTERS AND POINTERS FOR CALCULATING KAB, KAC
!
      ni = 2
      nj = 3
      nk = 1
      k1 = 1
      k2 = 4
      Sgn1 = 1.0D0
      Sgn2 = 1.0D0
   ELSEIF ( npivot==2 ) THEN
!
!     SET COUNTERS AND POINTERS FOR CALCULATING KBA, KBC
!     NOTE THAT KBA = -KAB
!
      ni = 1
      nj = 3
      nk = 2
      k1 = 1
      k2 = 7
      Sgn1 = -1.0D0
      Sgn2 = 1.0D0
   ELSE
!
!     SET COUNTERS AND POINTERS FOR CALCULATING KCA, KCB
!     NOTE THAT KCA = -KAC, KCB = -KBC
!
      ni = 1
      nj = 2
      nk = 1
      k1 = 4
      k2 = 7
      Sgn1 = -1.0D0
      Sgn2 = -1.0D0
   ENDIF
!
   Gamma = -Dp(5)/12.0D0
   Sgn = Sgn1*Gamma
   k = k1
   DO i = ni , nj , nk
      DO j = 1 , 36
         Kij(j) = 0.0D0
      ENDDO
      kk2 = k + 1
      kk3 = k + 2
      Kij(2) = -C(kk3)*Sgn
      Kij(3) = C(kk2)*Sgn
      Kij(7) = C(kk3)*Sgn
      Kij(9) = -C(k)*Sgn
      Kij(13) = -C(kk2)*Sgn
      Kij(14) = C(k)*Sgn
      CALL ds1b(Kij(1),Necpt(i+1))
      Sgn = Sgn2*Gamma
      k = k2
   ENDDO
!
END SUBROUTINE dpse3
