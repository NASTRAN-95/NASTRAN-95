!*==krod.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE krod
   IMPLICIT NONE
   USE C_BLANK
   USE C_HMTOUT
   USE C_MATIN
   USE C_MATOUT
   USE C_SMA1BK
   USE C_SMA1CL
   USE C_SMA1DP
   USE C_SMA1ET
   USE C_SMA1HT
   USE C_SMA1IO
   USE C_SYSTEM
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iretrn , itemp , k1 , k2 , ka , kb , nonpvt
   INTEGER , DIMENSION(4) :: iecpt
!
! End of declarations rewritten by SPAG
!
!*****
! THIS ROUTINE COMPUTES THE TWO 6 X 6 MATRICES  K(NPVT,NPVT) AND
! K(NPVT,J) FOR A ROD HAVING END POINTS NUMBERED NPVT AND J.
!*****
!
!                        E C P T  F O R  T H E  R O D
!
!                                                                CARD
!                                                 TYPE   TABLE   TYPE
! ECPT( 1)ELEMENT ID.                               I     ECT    CROD
! ECPT( 2)SCALAR INDEX NUMBER FOR GRID POINT A      I     ECT    CROD
! ECPT( 3)SCALAR INDEX NUMBER FOR GRID POINT B      I     ECT    CROD
! ECPT( 4)MATERIAL ID.                              I     EPT    PROD
! ECPT( 5)AREA  (A)                                 R     EPT    PROD
! ECPT( 6)POLAR MOMENT OF INERTIA (J)               R     EPT    PROD
! ECPT( 7) TORSIONAL STRESS COEFF (C)                R    EPT    PROD
! ECPT( 8) NON-STRUCTRAL MASS (MU)                   R    EPT    PROD
! ECPT( 9) COOR. SYS. ID. NO. FOR GRID POINT A       I   BGPDT   GRID
! ECPT(10) X-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
! ECPT(11) Y-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
! ECPT(12) Z-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
! ECPT(13) COOR. SYS. ID. NO. FOR GRID POINT B       I   BGPDT
! ECPT(14) X-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
! ECPT(15) Y-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
! ECPT(16) Z-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
! ECPT(17) ELEMENT TEMPERATURE
!
!
!
!
! SMA1 I/O PARAMETERS
!
!
! SMA1 VARIABLE CORE BOOKKEEPING PARAMETERS
!
!
! SMA1 PROGRAM CONTROL PARAMETERS
!
!
! ECPT COMMON BLOCK
!
!
! INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
!
! LOCAL DOUBLE PRECISION VARIABLES
!
!
!
! NOTE THAT EQUIVALENCE IS NECESSARY SINCE ECPT IS A MIXED --- INTEGERS
! AND REAL --- ARRAY
!
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1))
!*****
!  BRANCH ON HEAT FORMULATION.
!*****
   IF ( Heat ) THEN
!*****
!  HEAT FORMULATION.  FIRST COMPUTE LENGTH OF ELEMENT.
!*****
      X = Ecpt(14) - Ecpt(10)
      Y = Ecpt(15) - Ecpt(11)
      Z = Ecpt(16) - Ecpt(12)
      Xl = dsqrt(X**2+Y**2+Z**2)
      IF ( Xl<=0 ) CALL mesage(-30,26,iecpt(1))
!
!     GET MATERIAL PROPERTY -K- FROM HMAT ROUTINE
!
      Matflg = 1
      Matidc = iecpt(4)
      Eltemp = Ecpt(17)
      CALL hmat(iecpt)
!
      Xl = dble(Fk)*dble(Ecpt(5))/Xl
!
      IF ( Npvt==iecpt(3) ) Xl = -Xl
      DO i = 1 , 2
         CALL sma1b(Xl,iecpt(i+1),Npvt,Ifkgg,0.0D0)
         Xl = -Xl
      ENDDO
      GOTO 99999
   ELSE
      IF ( iecpt(2)==Npvt ) THEN
         ka = 9
         kb = 13
      ELSE
         IF ( iecpt(3)/=Npvt ) CALL mesage(-30,34,iecpt(1))
         itemp = iecpt(2)
         iecpt(2) = iecpt(3)
         iecpt(3) = itemp
         ka = 13
         kb = 9
      ENDIF
!
! AT THIS POINT KA POINTS TO THE COOR. SYS. ID. OF THE PIVOT GRID POINT.
! SIMILARLY FOR KB AND THE NON-PIVOT GRID POINT.
! NOW COMPUTE THE LENGTH OF THE ROD.
!
! WE STORE THE COORDINATES IN THE D ARRAY SO THAT ALL ARITHMETIC WILL BE
! DOUBLE PRECISION
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
      IF ( Xl/=0.0D0 ) THEN
!
! CALCULATE A NORMALIZED DIRECTION VECTOR IN BASIC COORDINATES.
!
         Xn(1) = X/Xl
         Xn(2) = Y/Xl
         Xn(3) = Z/Xl
!
! LOCATE E = YOUNG-S MODULUS, G = SHEAR MODULUS AND DAMPC = DAMPING
! CONSTANT IN THE MAT1 TABLE AND COMPUTE DSCL = A * E / XL AND
! DSCR = J * G / XL.  A IS ECPT(5) AND J IS ECPT(6)
!
         Matidc = iecpt(4)
         Matflg = 1
         Eltemp = Ecpt(17)
         CALL mat(iecpt(1))
!
! WE STORE ECPT(5), ECPT(6), E AND G IN DOUBLE PRECISION LOCATIONS SO
! THAT ALL ARITHMETIC WILL BE DOUBLE PRECISION
!
         D(1) = Ecpt(5)
         D(2) = E
         D(3) = Ecpt(6)
         D(4) = G
         Dscl = D(1)*D(2)/Xl
         Dscr = D(3)*D(4)/Xl
         Dampc = Gsube
!
! SET UP THE -N- MATRIX AND STORE AT D(1)
!
         D(1) = Xn(1)*Xn(1)
         D(2) = Xn(1)*Xn(2)
         D(3) = Xn(1)*Xn(3)
         D(4) = D(2)
         D(5) = Xn(2)*Xn(2)
         D(6) = Xn(2)*Xn(3)
         D(7) = D(3)
         D(8) = D(6)
         D(9) = Xn(3)*Xn(3)
!
! ZERO OUT THE 6X6 WHICH WILL BE USED FOR STORAGE OF KGG(NPVT,NONPVT),
! NONPVT = NPVT,J
! KGG(NPVT,NONPVT), NONPVT = NPVT,J
!
         DO i = 1 , 36
            Ke(i) = 0.0D0
         ENDDO
         nonpvt = 2
         k2 = 1
!
! IF PIVOT GRID POINT IS IN BASIC COORDINATES, GO TO 70
!
         IF ( iecpt(ka)==0 ) THEN
            ASSIGN 200 TO iretrn
         ELSE
            CALL transd(Ecpt(ka),Ti(1))
            CALL gmmatd(Ti(1),3,3,1,D(1),3,3,0,D(10))
            CALL gmmatd(D(10),3,3,0,Ti(1),3,3,0,D(1))
!
! AT THIS POINT D(1) CONTAINS THE MATRIX PRODUCT TAT * N * TA
! AND D(10) CONTAINS THE MATRIX PRODUCT TAT * N.
!
            ASSIGN 300 TO iretrn
         ENDIF
      ELSE
         CALL mesage(30,26,iecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULA
!
         Nogo = 1
         RETURN
      ENDIF
   ENDIF
!
! FILL THE KE MATRIX
!
 100  Ke(1) = Dscl*D(k2)
   Ke(2) = Dscl*D(k2+1)
   Ke(3) = Dscl*D(k2+2)
   Ke(7) = Dscl*D(k2+3)
   Ke(8) = Dscl*D(k2+4)
   Ke(9) = Dscl*D(k2+5)
   Ke(13) = Dscl*D(k2+6)
   Ke(14) = Dscl*D(k2+7)
   Ke(15) = Dscl*D(k2+8)
   Ke(22) = Dscr*D(k2)
   Ke(23) = Dscr*D(k2+1)
   Ke(24) = Dscr*D(k2+2)
   Ke(28) = Dscr*D(k2+3)
   Ke(29) = Dscr*D(k2+4)
   Ke(30) = Dscr*D(k2+5)
   Ke(34) = Dscr*D(k2+6)
   Ke(35) = Dscr*D(k2+7)
   Ke(36) = Dscr*D(k2+8)
   CALL sma1b(Ke,Ecpt(nonpvt),-1,Ifkgg,0.0D0)
   IF ( Iopt4/=0 .AND. Gsube/=0.0 ) THEN
      K4ggsw = 1
      CALL sma1b(Ke,Ecpt(nonpvt),-1,If4gg,Dampc)
   ENDIF
!
!  RETURN  FROM  FILL  CODE W/ IRETRN = 90 IMPLIES G.P. A WAS IN BASIC
!    .      .     .      .      .     =100 IMPLIES G.P. A WAS NOT BASIC
!    .      .     .      .      .     =140 IMPLIES THE K(NPVT,NONPVT)
!                                      HAS BEEN COMPUTED AND INSERTED
!                                      AND HENCE WE ARE FINISHED.
!
   GOTO iretrn
 200  k1 = 1
   k2 = 10
   GOTO 400
 300  k1 = 10
   k2 = 1
 400  nonpvt = 3
!
! IF NON-PIVOT GRID POINT IS IN BASIC COORDINATES, GO TO 120
!
   IF ( iecpt(kb)==0 ) THEN
      k2 = k1
   ELSE
      CALL transd(Ecpt(kb),Ti(1))
!
! RECALL THAT D(K1) CONTAINS TAT * N.
!
!
! AT THIS POINT D(K2) CONTAINS TAT * N * TB.
!
      CALL gmmatd(D(k1),3,3,0,Ti(1),3,3,0,D(k2))
   ENDIF
   ASSIGN 500 TO iretrn
!
! SET CONSTANTS NEGATIVE TO PROPERLY COMPUTE K(NPVT,NONPVT)
!
   Dscr = -Dscr
   Dscl = -Dscl
   GOTO 100
!
! A TRANSFER TO STATEMENT NO. 140 IMPLIES KGG AND/OR K4GG CALCULATIONS
! HAVE BEEN COMPLETED.
!
 500  RETURN
99999 END SUBROUTINE krod
