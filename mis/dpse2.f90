
SUBROUTINE dpse2
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION Alpha , D(18) , Ke(36) , Ta(9) , Tb(9) , X , Xl , Y , Z
   REAL Dum2(2) , Dum6(6) , Ecpt(16)
   INTEGER Icstm , Iecpt(3) , Ncstm , Npvt
   COMMON /ds1aaa/ Npvt , Icstm , Ncstm
   COMMON /ds1adp/ Ke , Ta , Tb , D , X , Y , Z , Xl , Alpha
   COMMON /ds1aet/ Ecpt , Dum2 , Dum6
!
! Local variable declarations
!
   INTEGER i , ielem , itemp , k1 , ka , kb , kb1 , kb2 , nogo
!
! End of declarations
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
   EQUIVALENCE (Ecpt(1),Iecpt(1))
!
   ielem = Iecpt(1)
   IF ( Iecpt(2)==Npvt ) THEN
      ka = 8
      kb = 12
      Alpha = 1.0D0
   ELSE
      IF ( Iecpt(3)/=Npvt ) CALL mesage(-30,34,Iecpt(1))
      itemp = Iecpt(2)
      Iecpt(2) = Iecpt(3)
      Iecpt(3) = itemp
      ka = 12
      kb = 8
      Alpha = -1.0D0
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
   D(1) = Ecpt(ka+1)
   D(2) = Ecpt(ka+2)
   D(3) = Ecpt(ka+3)
   D(4) = Ecpt(kb+1)
   D(5) = Ecpt(kb+2)
   D(6) = Ecpt(kb+3)
   X = D(1) - D(4)
   Y = D(2) - D(5)
   Z = D(3) - D(6)
   Xl = dsqrt(X**2+Y**2+Z**2)
   IF ( Xl==0.0D0 ) THEN
!
!     ERROR
!
      CALL mesage(30,26,Iecpt(1))
      nogo = 1
      GOTO 99999
   ELSE
!
!     COMPUTE THE 3 X 3 NON-ZERO SUBMATRIX OF KDGG(NPVT,NONPVT)
!
      D(1) = 0.0D0
      D(2) = Alpha*Ecpt(4)/2.0D0
      D(3) = D(2)
      D(4) = -D(2)
      D(5) = 0.0D0
      D(6) = D(2)
      D(7) = D(4)
      D(8) = D(4)
      D(9) = 0.0D0
!
!     ZERO OUT KE MATRIX
!
      DO i = 1 , 36
         Ke(i) = 0.0D0
      ENDDO
!
!     FILL UP THE 6 X 6 KE
!
!     IF PIVOT GRID POINT IS IN BASIC COORDINATES, GO TO 40
!
      k1 = 1
      IF ( Iecpt(ka)==0 ) THEN
!
!     IF NON-PIVOT GRID POINT IS IN BASIC COORDINATES, GO TO 60
!
         kb1 = 1
         kb2 = 10
      ELSE
         CALL transd(Ecpt(ka),Ta)
         CALL gmmatd(Ta,3,3,1,D(1),3,3,0,D(10))
         k1 = 10
         kb1 = 10
         kb2 = 1
      ENDIF
      IF ( Iecpt(kb)/=0 ) THEN
         CALL transd(Ecpt(kb),Tb)
         CALL gmmatd(D(kb1),3,3,0,Tb,3,3,0,D(kb2))
         k1 = kb2
      ENDIF
   ENDIF
!
   Ke(1) = D(k1)
   Ke(2) = D(k1+1)
   Ke(3) = D(k1+2)
   Ke(7) = D(k1+3)
   Ke(8) = D(k1+4)
   Ke(9) = D(k1+5)
   Ke(13) = D(k1+6)
   Ke(14) = D(k1+7)
   Ke(15) = D(k1+8)
   CALL ds1b(Ke,Iecpt(3))
   RETURN
99999 END SUBROUTINE dpse2
