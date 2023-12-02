!*==krod.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE krod
   IMPLICIT NONE
   USE c_blank
   USE c_hmtout
   USE c_matin
   USE c_matout
   USE c_sma1bk
   USE c_sma1cl
   USE c_sma1dp
   USE c_sma1et
   USE c_sma1ht
   USE c_sma1io
   USE c_system
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iretrn , itemp , k1 , k2 , ka , kb , nonpvt
   INTEGER , DIMENSION(4) :: iecpt
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
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
   IF ( heat ) THEN
!*****
!  HEAT FORMULATION.  FIRST COMPUTE LENGTH OF ELEMENT.
!*****
      x = ecpt(14) - ecpt(10)
      y = ecpt(15) - ecpt(11)
      z = ecpt(16) - ecpt(12)
      xl = dsqrt(x**2+y**2+z**2)
      IF ( xl<=0 ) CALL mesage(-30,26,iecpt(1))
!
!     GET MATERIAL PROPERTY -K- FROM HMAT ROUTINE
!
      matflg = 1
      matidc = iecpt(4)
      eltemp = ecpt(17)
      CALL hmat(iecpt)
!
      xl = dble(fk)*dble(ecpt(5))/xl
!
      IF ( npvt==iecpt(3) ) xl = -xl
      DO i = 1 , 2
         CALL sma1b(xl,iecpt(i+1),npvt,ifkgg,0.0D0)
         xl = -xl
      ENDDO
      GOTO 99999
   ELSE
      IF ( iecpt(2)==npvt ) THEN
         ka = 9
         kb = 13
      ELSE
         IF ( iecpt(3)/=npvt ) CALL mesage(-30,34,iecpt(1))
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
      IF ( xl/=0.0D0 ) THEN
!
! CALCULATE A NORMALIZED DIRECTION VECTOR IN BASIC COORDINATES.
!
         xn(1) = x/xl
         xn(2) = y/xl
         xn(3) = z/xl
!
! LOCATE E = YOUNG-S MODULUS, G = SHEAR MODULUS AND DAMPC = DAMPING
! CONSTANT IN THE MAT1 TABLE AND COMPUTE DSCL = A * E / XL AND
! DSCR = J * G / XL.  A IS ECPT(5) AND J IS ECPT(6)
!
         matidc = iecpt(4)
         matflg = 1
         eltemp = ecpt(17)
         CALL mat(iecpt(1))
!
! WE STORE ECPT(5), ECPT(6), E AND G IN DOUBLE PRECISION LOCATIONS SO
! THAT ALL ARITHMETIC WILL BE DOUBLE PRECISION
!
         d(1) = ecpt(5)
         d(2) = e
         d(3) = ecpt(6)
         d(4) = g
         dscl = d(1)*d(2)/xl
         dscr = d(3)*d(4)/xl
         dampc = gsube
!
! SET UP THE -N- MATRIX AND STORE AT D(1)
!
         d(1) = xn(1)*xn(1)
         d(2) = xn(1)*xn(2)
         d(3) = xn(1)*xn(3)
         d(4) = d(2)
         d(5) = xn(2)*xn(2)
         d(6) = xn(2)*xn(3)
         d(7) = d(3)
         d(8) = d(6)
         d(9) = xn(3)*xn(3)
!
! ZERO OUT THE 6X6 WHICH WILL BE USED FOR STORAGE OF KGG(NPVT,NONPVT),
! NONPVT = NPVT,J
! KGG(NPVT,NONPVT), NONPVT = NPVT,J
!
         DO i = 1 , 36
            ke(i) = 0.0D0
         ENDDO
         nonpvt = 2
         k2 = 1
!
! IF PIVOT GRID POINT IS IN BASIC COORDINATES, GO TO 70
!
         IF ( iecpt(ka)==0 ) THEN
            ASSIGN 200 TO iretrn
         ELSE
            CALL transd(ecpt(ka),ti(1))
            CALL gmmatd(ti(1),3,3,1,d(1),3,3,0,d(10))
            CALL gmmatd(d(10),3,3,0,ti(1),3,3,0,d(1))
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
         nogo = 1
         RETURN
      ENDIF
   ENDIF
!
! FILL THE KE MATRIX
!
 100  ke(1) = dscl*d(k2)
   ke(2) = dscl*d(k2+1)
   ke(3) = dscl*d(k2+2)
   ke(7) = dscl*d(k2+3)
   ke(8) = dscl*d(k2+4)
   ke(9) = dscl*d(k2+5)
   ke(13) = dscl*d(k2+6)
   ke(14) = dscl*d(k2+7)
   ke(15) = dscl*d(k2+8)
   ke(22) = dscr*d(k2)
   ke(23) = dscr*d(k2+1)
   ke(24) = dscr*d(k2+2)
   ke(28) = dscr*d(k2+3)
   ke(29) = dscr*d(k2+4)
   ke(30) = dscr*d(k2+5)
   ke(34) = dscr*d(k2+6)
   ke(35) = dscr*d(k2+7)
   ke(36) = dscr*d(k2+8)
   CALL sma1b(ke,ecpt(nonpvt),-1,ifkgg,0.0D0)
   IF ( iopt4/=0 .AND. gsube/=0.0 ) THEN
      k4ggsw = 1
      CALL sma1b(ke,ecpt(nonpvt),-1,if4gg,dampc)
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
      CALL transd(ecpt(kb),ti(1))
!
! RECALL THAT D(K1) CONTAINS TAT * N.
!
!
! AT THIS POINT D(K2) CONTAINS TAT * N * TB.
!
      CALL gmmatd(d(k1),3,3,0,ti(1),3,3,0,d(k2))
   ENDIF
   ASSIGN 99999 TO iretrn
!
! SET CONSTANTS NEGATIVE TO PROPERLY COMPUTE K(NPVT,NONPVT)
!
   dscr = -dscr
   dscl = -dscl
   GOTO 100
!
! A TRANSFER TO STATEMENT NO. 140 IMPLIES KGG AND/OR K4GG CALCULATIONS
! HAVE BEEN COMPLETED.
!
99999 END SUBROUTINE krod
