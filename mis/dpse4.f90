
SUBROUTINE dpse4
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION C(12) , Dp(26) , Gamma , Kij(36) , Sign(3)
   REAL Dum12(12) , Dum2(2) , Ecpt(26)
   INTEGER Icstm , Ik(3) , Ncstm , Necpt(6) , Nk(3) , Npvt
   COMMON /ds1aaa/ Npvt , Icstm , Ncstm
   COMMON /ds1adp/ Gamma , Kij , Dp , C , Sign , Nk , Ik
   COMMON /ds1aet/ Ecpt , Dum2 , Dum12
!
! Local variable declarations
!
   INTEGER i , ias , j , k , k1 , k2 , k3 , npivot
   REAL sg
!
! End of declarations
!
!
!     PRESSURE STIFFNESS CALCULATIONS FOR A QUADRILATERAL MEMBRANE
!     ELEMENT, WHICH HAS 4 GRID POINTS.
!     THREE 6X6 STIFFNESS MATRICES FOR THE PIVOT POINT ARE INSERTED.
!
!     DOUBLE PRECISION VERSION
!
!     WRITTEN BY E. R. CHRISTENSEN/SVERDRUP,  9/91,  VERSION 1.1
!     INSTALLED IN NASTRAN AS ELEMENT DPSE4 BY G.CHAN/UNISYS, 2/92
!
!     REFERENCE - E. CHRISTENEN: 'ADVACED SOLID ROCKET MOTOR (ASRM)
!                 MATH MODELS - PRESSURE STIFFNESS EFFECTS ANALYSIS',
!                 NASA TD 612-001-02, AUGUST 1991
!
!     LIMITATION -
!     (1) ALL GRID POINTS USED BY ANY OF THE CPSE2/3/4 ELEMENTS MUST BE
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
!     ECPT FOR THE PRESSURE STIFFNESS CPES4 ELEMENT
!
!     ECPT( 1) = ELEMENT ID
!     ECPT( 2) = SIL FOR GRID POINT A OR 1
!     ECPT( 3) = SIL FOR GRID POINT B OR 2
!     ECPT( 4) = SIL FOR GRID POINT C OR 3
!     ECPT( 5) = SIL FOR GRID POINT C OR 4
!     ECPT( 6) = PRESSURE
!     ECPT( 7) = NOT USED
!     ECPT( 8) = NOT USED
!     ECPT( 9) = NOT USED
!     ECPT(10) = COORD. SYSTEM ID 1
!     ECPT(11) = X1
!     ECPT(12) = Y1
!     ECPT(13) = Z1
!     ECPT(14) = COORD. SYSTEM ID 2
!     ECPT(15) = X2
!     ECPT(16) = Y2
!     ECPT(17) = Z2
!     ECPT(18) = COORD. SYSTEM ID 3
!     ECPT(19) = X3
!     ECPT(20) = Y3
!     ECPT(21) = Z3
!     ECPT(22) = COORD. SYSTEM ID 4
!     ECPT(23) = X4
!     ECPT(24) = Y4
!     ECPT(25) = Z4
!     ECPT(26) = ELEMENT TEMPERATURE
!     ECPT(27) THRU ECPT(40) = DUM2 AND DUM12, NOT USED IN THIS ROUTINE
!
!     STORE ECPT IN DOUBLE PRECISION
!
   Dp(6) = Ecpt(6)
   k = 10
   DO i = 1 , 4
      DO j = 1 , 3
         k = k + 1
         Dp(k) = Ecpt(k)
      ENDDO
      k = k + 1
   ENDDO
!
!     CALCULATE THE FOUR VECTORS GAB, GAC, GAD, AND GBD USED IN
!     COMPUTING THE PRESSURE STIFFNESS MATRIC
!
!     GAB = RA + RB - RC - RD
!     GAC = RB - RD
!     GAD =-RA + RB + RC - RD
!     GBD =-RA + RC
!
!     GAB STORED IN C( 1), C( 2), C( 3)
!     GAC STORED IN C( 4), C( 5), C( 6)
!     GAD STORED IN C( 7), C( 8), C( 9)
!     GBD STORED IN C(10), C(11), C(12)
!
   C(1) = Dp(11) + Dp(15) - Dp(19) - Dp(23)
   C(2) = Dp(12) + Dp(16) - Dp(20) - Dp(24)
   C(3) = Dp(13) + Dp(17) - Dp(21) - Dp(25)
!
   C(4) = Dp(15) - Dp(23)
   C(5) = Dp(16) - Dp(24)
   C(6) = Dp(17) - Dp(25)
!
   C(7) = -Dp(11) + Dp(15) + Dp(19) - Dp(23)
   C(8) = -Dp(12) + Dp(16) + Dp(20) - Dp(24)
   C(9) = -Dp(13) + Dp(17) + Dp(21) - Dp(25)
!
   C(10) = -Dp(11) + Dp(19)
   C(11) = -Dp(12) + Dp(20)
   C(12) = -Dp(13) + Dp(21)
!
   DO i = 1 , 4
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
 100  IF ( npivot==4 ) THEN
!
      Nk(1) = 1
      Nk(2) = 2
      Nk(3) = 3
      Ik(1) = 7
      Ik(2) = 10
      Ik(3) = 1
      Sign(1) = -1.0D0
      Sign(2) = -1.0D0
      Sign(3) = 1.0D0
   ELSEIF ( npivot<2 ) THEN
!
!     SET COUNTERS AND POINTERS FOR CALCULATING KAB, KAC, KAD
!
      Nk(1) = 2
      Nk(2) = 3
      Nk(3) = 4
      Ik(1) = 1
      Ik(2) = 4
      Ik(3) = 7
      Sign(1) = 1.0D0
      Sign(2) = 1.0D0
      Sign(3) = 1.0D0
   ELSEIF ( npivot==2 ) THEN
!
!     SET COUNTERS AND POINTERS FOR CALCULATING KBA, KBC, KBD
!     NOTE THAT KBA = -KAB
!
      Nk(1) = 1
      Nk(2) = 3
      Nk(3) = 4
      Ik(1) = 1
      Ik(2) = 7
      Ik(3) = 10
      Sign(1) = -1.0D0
      Sign(2) = 1.0D0
      Sign(3) = 1.0D0
   ELSE
!
!     SET COUNTERS AND POINTERS FOR CALCULATING KCA, KCB, KCD
!     NOTE THAT KCA = -KAC, KCB = -KBC
!
      Nk(1) = 1
      Nk(2) = 2
      Nk(3) = 4
      Ik(1) = 4
      Ik(2) = 7
      Ik(3) = 1
      Sign(1) = -1.0D0
      Sign(2) = -1.0D0
      Sign(3) = -1.0D0
   ENDIF
!
   Gamma = -Dp(6)/12.0D0
   DO i = 1 , 3
      DO j = 1 , 36
         Kij(j) = 0.0D0
      ENDDO
      k1 = Ik(i)
      k2 = k1 + 1
      k3 = k1 + 2
      sg = Gamma*Sign(i)
      Kij(2) = -C(k3)*sg
      Kij(3) = C(k2)*sg
      Kij(7) = C(k3)*sg
      Kij(9) = -C(k1)*sg
      Kij(13) = -C(k2)*sg
      Kij(14) = C(k1)*sg
!
!     ASSEMBLE INTO THE GLOBAL STIFFNESS MATRIX
!
      ias = Nk(i)
      CALL ds1b(Kij(1),Necpt(ias+1))
   ENDDO
!
END SUBROUTINE dpse4