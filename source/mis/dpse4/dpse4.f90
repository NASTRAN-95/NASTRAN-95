!*==dpse4.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dpse4
   IMPLICIT NONE
   USE c_ds1aaa
   USE c_ds1adp
   USE c_ds1aet
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ias , j , k , k1 , k2 , k3 , npivot
   INTEGER , DIMENSION(6) :: necpt
   REAL :: sg
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Necpt(1),Ecpt(1))
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
   dp(6) = ecpt(6)
   k = 10
   DO i = 1 , 4
      DO j = 1 , 3
         k = k + 1
         dp(k) = ecpt(k)
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
   c(1) = dp(11) + dp(15) - dp(19) - dp(23)
   c(2) = dp(12) + dp(16) - dp(20) - dp(24)
   c(3) = dp(13) + dp(17) - dp(21) - dp(25)
!
   c(4) = dp(15) - dp(23)
   c(5) = dp(16) - dp(24)
   c(6) = dp(17) - dp(25)
!
   c(7) = -dp(11) + dp(15) + dp(19) - dp(23)
   c(8) = -dp(12) + dp(16) + dp(20) - dp(24)
   c(9) = -dp(13) + dp(17) + dp(21) - dp(25)
!
   c(10) = -dp(11) + dp(19)
   c(11) = -dp(12) + dp(20)
   c(12) = -dp(13) + dp(21)
!
   DO i = 1 , 4
      IF ( necpt(i+1)==npvt ) THEN
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
      nk(1) = 1
      nk(2) = 2
      nk(3) = 3
      ik(1) = 7
      ik(2) = 10
      ik(3) = 1
      sign(1) = -1.0D0
      sign(2) = -1.0D0
      sign(3) = 1.0D0
   ELSEIF ( npivot<2 ) THEN
!
!     SET COUNTERS AND POINTERS FOR CALCULATING KAB, KAC, KAD
!
      nk(1) = 2
      nk(2) = 3
      nk(3) = 4
      ik(1) = 1
      ik(2) = 4
      ik(3) = 7
      sign(1) = 1.0D0
      sign(2) = 1.0D0
      sign(3) = 1.0D0
   ELSEIF ( npivot==2 ) THEN
!
!     SET COUNTERS AND POINTERS FOR CALCULATING KBA, KBC, KBD
!     NOTE THAT KBA = -KAB
!
      nk(1) = 1
      nk(2) = 3
      nk(3) = 4
      ik(1) = 1
      ik(2) = 7
      ik(3) = 10
      sign(1) = -1.0D0
      sign(2) = 1.0D0
      sign(3) = 1.0D0
   ELSE
!
!     SET COUNTERS AND POINTERS FOR CALCULATING KCA, KCB, KCD
!     NOTE THAT KCA = -KAC, KCB = -KBC
!
      nk(1) = 1
      nk(2) = 2
      nk(3) = 4
      ik(1) = 4
      ik(2) = 7
      ik(3) = 1
      sign(1) = -1.0D0
      sign(2) = -1.0D0
      sign(3) = -1.0D0
   ENDIF
!
   gamma = -dp(6)/12.0D0
   DO i = 1 , 3
      DO j = 1 , 36
         kij(j) = 0.0D0
      ENDDO
      k1 = ik(i)
      k2 = k1 + 1
      k3 = k1 + 2
      sg = gamma*sign(i)
      kij(2) = -c(k3)*sg
      kij(3) = c(k2)*sg
      kij(7) = c(k3)*sg
      kij(9) = -c(k1)*sg
      kij(13) = -c(k2)*sg
      kij(14) = c(k1)*sg
!
!     ASSEMBLE INTO THE GLOBAL STIFFNESS MATRIX
!
      ias = nk(i)
      CALL ds1b(kij(1),necpt(ias+1))
   ENDDO
!
END SUBROUTINE dpse4
