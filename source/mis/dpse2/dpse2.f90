!*==dpse2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dpse2
   IMPLICIT NONE
   USE c_ds1aaa
   USE c_ds1adp
   USE c_ds1aet
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ielem , itemp , k1 , ka , kb , kb1 , kb2 , nogo
   INTEGER , DIMENSION(3) :: iecpt
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
!     THIS ROUTINE COMPUTES THE TWO 6 X 6 MATRICES K(NPVT,NPVT) AND
!     K(NPVT,J), PRESSURE STIFFNESS MATRICES FOR A CPSE2 PRESSURE
!     STIFFNESS ELEMENT (ROD, 2 GRID POINTS)
!
!     DOUBLE PRECISION VERSION
!
!     WRITTEN BY E. R. CHRISTENSEN/SVERDRUP  7/91, VERSION 1.0
!     INSTALLED IN NASTRAN AS ELEMENT DPSE2 BY G.CHAN/UNISYS, 2/92
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
!     SEE NASTRAN DEMONSTRATION PROBLEM -  T13021A
!
!     ECPT FOR THE PRESSURE STIFFNESS
!     CPSE2 ELEMENT                                CARD
!                                                  TYPE  TYPE   TABLE
!                                                 ------ ----- ------
!     ECPT( 1) ELEMENT ID.                         CPSE2   I     ECT
!     ECPT( 2) SCALAR INDEX NUMBER FOR GRD.PT. A   CPSE2   I     ECT
!     ECPT( 3) SCALAR INDEX NUMBER FOR GRD.PT. B   CPSE2   I     ECT
!     ECPT( 4) PRESSURE P                          PPSE    R     EPT
!     ECPT( 5) NOT USED                            PPSE    R     EPT
!     ECPT( 6) NOT USED                            PPSE    R     EPT
!     ECPT( 7) NOT USED                            PPSE    R     EPT
!     ECPT( 8) COOR. SYS. ID. NO. FOR GRD.PT. A    GRID    I    BGPDT
!     ECPT( 9) X-COORDINATE OF GRD.PT. A (IN BASIC COOR)   R    BGPDT
!     ECPT(10) Y-COORDINATE OF GRD.PT. A (IN BASIC COOR)   R    BGPDT
!     ECPT(11) Z-COORDINATE OF GRD.PT. A (IN BASIC COOR)   R    BGPDT
!     ECPT(12) COOR. SYS. ID. NO. FOR GRD.PT. B            I    BGPDT
!     ECPT(13) X-COORDINATE OF GRD.PT. B (IN BASIC COOR)   R    BGPDT
!     ECPT(14) Y-COORDINATE OF GRD.PT. B (IN BASIC COOR)   R    BGPDT
!     ECPT(15) Z-COORDINATE OF GRD.PT. B (IN BASIC COOR)   R    BGPDT
!     ECPT(16) ELEMENT TEMPERATURE
!     ECPT(17) THRU ECPT(24) = DUM2 AND DUM6, NOT USED IN THIS ROUTINE
!
!     COMMON /SYSTEM/  IBUF,NOUT
   !>>>>EQUIVALENCE (Ecpt(1),Iecpt(1))
!
   ielem = iecpt(1)
   IF ( iecpt(2)==npvt ) THEN
      ka = 8
      kb = 12
      alpha = 1.0D0
   ELSE
      IF ( iecpt(3)/=npvt ) CALL mesage(-30,34,iecpt(1))
      itemp = iecpt(2)
      iecpt(2) = iecpt(3)
      iecpt(3) = itemp
      ka = 12
      kb = 8
      alpha = -1.0D0
   ENDIF
!
!     AT THIS POINT KA POINTS TO THE COOR. SYS. ID. OF THE PIVOT GRID
!     POINT. SIMILARLY FOR KB AND THE NON-PIVOT GRID POINT.
!
!     NOW COMPUTE THE LENGTH OF THE CPSE2 ELEMENT.
!
!
!     WE STORE THE COORDINATES IN THE D ARRAY SO THAT ALL ARITHMETIC
!     WILL BE DOUBLE PRECISION
!
!     CHECK TO SEE THAT THE CPSE2 HAS A NONZERO LENGTH
!
   d(1) = ecpt(ka+1)
   d(2) = ecpt(ka+2)
   d(3) = ecpt(ka+3)
   d(4) = ecpt(kb+1)
   d(5) = ecpt(kb+2)
   d(6) = ecpt(kb+3)
   x = d(1) - d(4)
   y = d(2) - d(5)
   z = d(3) - d(6)
   xl = dsqrt(x**2+y**2+z**2)
   IF ( xl==0.0D0 ) THEN
!
!     ERROR
!
      CALL mesage(30,26,iecpt(1))
      nogo = 1
      GOTO 99999
   ELSE
!
!     COMPUTE THE 3 X 3 NON-ZERO SUBMATRIX OF KDGG(NPVT,NONPVT)
!
      d(1) = 0.0D0
      d(2) = alpha*ecpt(4)/2.0D0
      d(3) = d(2)
      d(4) = -d(2)
      d(5) = 0.0D0
      d(6) = d(2)
      d(7) = d(4)
      d(8) = d(4)
      d(9) = 0.0D0
!
!     ZERO OUT KE MATRIX
!
      DO i = 1 , 36
         ke(i) = 0.0D0
      ENDDO
!
!     FILL UP THE 6 X 6 KE
!
!     IF PIVOT GRID POINT IS IN BASIC COORDINATES, GO TO 40
!
      k1 = 1
      IF ( iecpt(ka)==0 ) THEN
!
!     IF NON-PIVOT GRID POINT IS IN BASIC COORDINATES, GO TO 60
!
         kb1 = 1
         kb2 = 10
      ELSE
         CALL transd(ecpt(ka),ta)
         CALL gmmatd(ta,3,3,1,d(1),3,3,0,d(10))
         k1 = 10
         kb1 = 10
         kb2 = 1
      ENDIF
      IF ( iecpt(kb)/=0 ) THEN
         CALL transd(ecpt(kb),tb)
         CALL gmmatd(d(kb1),3,3,0,tb,3,3,0,d(kb2))
         k1 = kb2
      ENDIF
   ENDIF
!
   ke(1) = d(k1)
   ke(2) = d(k1+1)
   ke(3) = d(k1+2)
   ke(7) = d(k1+3)
   ke(8) = d(k1+4)
   ke(9) = d(k1+5)
   ke(13) = d(k1+6)
   ke(14) = d(k1+7)
   ke(15) = d(k1+8)
   CALL ds1b(ke,iecpt(3))
99999 END SUBROUTINE dpse2
