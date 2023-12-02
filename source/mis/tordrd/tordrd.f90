!*==tordrd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tordrd
USE C_CONDAD
USE C_EMGDIC
USE C_EMGEST
USE C_EMGPRM
USE C_MATIN
USE C_MATOUT
USE C_SYSTEM
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: a1 , a2 , cosa1 , cosa2 , degrad , del , djp1 , ep , et , phib , r1 , r2 , rp , s , sina1 , sina2 , twopi , vpt ,&
                 & vtp , z1 , z2
   REAL(REAL64) , DIMENSION(144) :: ak , am , d , gambq , gamrs , kout , mout
   REAL(REAL64) , DIMENSION(36) :: aki , akm
   REAL :: d1 , d2 , dict5
   REAL(REAL64) , DIMENSION(66) :: delint
   INTEGER , DIMENSION(9) :: dict
   REAL , DIMENSION(17) :: ecpt
   REAL(REAL64) , DIMENSION(4) :: ee
   REAL(REAL64) , DIMENSION(72) :: gambqf
   REAL(REAL64) , DIMENSION(48) :: gambqm
   INTEGER :: i , iai , iapp , ic , ii , ij , iout , ip , ipp , ipr , ir , itord , j , ji , jj , jp1 , k , ki , kj , kk , kl
   INTEGER , DIMENSION(2) :: ics
   INTEGER , DIMENSION(18) :: iecpt
   REAL(REAL64) , DIMENSION(2) :: r , z
   EXTERNAL dmatrx , emgout , gmmatd , mat , mesage , rombdk , transd
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     THIS SUBROUTINE COMPUTES THE STIFFNESS MATRIX AND THE MASS MATRIX
!     FOR AN AXI-SYMMETRIC TORDIDAL THIN SHELL RING
!
!     DOUBLE PRECISION VERSION
!
!     THIS  SUBROUTINE USES ROUTINES  ROMBDK , DMATRX
!
!
!*****
!
!                        ECPT FOR THE TOROIDAL RING
!
!                                                       TYPE
! ECPT( 1) ELEMENT IDENTIFICATION                         I
! ECPT( 2) SCALAR INDEX NO. FOR GRID POINT A              I
! ECPT( 3) SCALAR INDEX NO. FOR GRID POINT B              I
! ECPT( 4) ANGLE OF CURVATURE AT GRID POINT A             R
! ECPT( 5) ANGLE OF CURVATURE AT GRID POINT B(NOT USED)   R
! ECPT( 6) MATERIAL ORIENTATION (NOT USED)                R
! ECPT( 7) MATERIAL IDENTIFICATION                        I
! ECPT( 8) MEMBRANE THICKNESS                             R
! ECPT( 9) FLEXURE THICKNESS                              R
! ECPT(10) COOR. SYS. ID. FOR GRID POINT A                I
! ECPT(11) X-COOR. OF GRID POINT A (IN BASIC COOR.)       R
! ECPT(12) Y-COOR. OF GRID POINT A (IN BASIC COOR.)       R
! ECPT(13) Z-COOR. OF GRID POINT A (IN BASIC COOR.)       R
! ECPT(14) COOR. SYS. ID. FOR GRID POINT B                I
! ECPT(15) X-COOR. OF GRID POINT B (IN BASIC COOR.)       R
! ECPT(16) Y-COOR. OF GRID POINT B (IN BASIC COOR.)       R
! ECPT(17) Z-COOR. OF GRID POINT B (IN BASIC COOR.)       R
! ECPT(18) EL. TEMPERATURE FOR MATERIAL PROPERTIES        R
!
!*****
!
!
!
!
!
!
!
   !>>>>EQUIVALENCE (dict5,dict(5))
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1),Idel)
   !>>>>EQUIVALENCE (Constd(2),Twopi)
   !>>>>EQUIVALENCE (Constd(4),Degrad)
   !>>>>EQUIVALENCE (gambqf(1),gambq(1))
   !>>>>EQUIVALENCE (gambqm(1),gambq(73))
   !>>>>EQUIVALENCE (delint(1),gambq(1))
   !>>>>EQUIVALENCE (gamrs(1),gambq(1))
   !>>>>EQUIVALENCE (r1,r(1)) , (r2,r(2)) , (z1,z(1)) , (z2,z(2))
!
!
! ----------------------------------------------------------------------
!
!     SET UP THE DICT ARRAY
!
         ipr = Iprec
         dict(1) = Estid
         dict(3) = 12
         dict(4) = 63
         ics(1) = iecpt(10)
         ics(2) = iecpt(14)
         r(1) = ecpt(11)
         d1 = ecpt(12)
         z(1) = ecpt(13)
         r(2) = ecpt(15)
         d2 = ecpt(16)
         z(2) = ecpt(17)
!
!
! TEST THE VALIDITY OF THE GRID POINT COORDINATES
!
         IF ( r1<0. .OR. r2<0. ) THEN
!
!
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
            CALL mesage(30,37,Idel)
         ELSEIF ( d1/=0. .OR. d2/=0. ) THEN
            CALL mesage(30,37,Idel)
         ELSE
!
!
! DETERMINE IF ELEMENT IS A TOROIDAL, CONICAL OR CYLINDRICAL RING
!
            itord = 0
            IF ( abs(Alph(1)-Alph(2))<=1.E-6 ) itord = 1
            IF ( itord==1 .AND. abs(Alph(1)-90.)<=1.E-5 ) itord = -1
!
!
! COMPUTE THE ELEMENT COORDINATES
!
            a1 = dble(Alph(1))*degrad
            a2 = dble(Alph(2))*degrad
            phib = a2 - a1
            sina1 = dsin(a1)
            cosa1 = dcos(a1)
            sina2 = dsin(a2)
            cosa2 = dcos(a2)
!
            IF ( itord/=0 ) THEN
!
!  FOR THE CONICAL OR CYLINDRICAL RING
!
               rp = 0.D0
               s = dsqrt((r2-r1)**2+(z2-z1)**2)
            ELSE
!
! FOR THE TOROIDAL RING
!
               rp = dsqrt((r2-r1)**2+(z2-z1)**2)/(2.D0*dsin(phib/2.D0))
               s = phib*rp
            ENDIF
!
!  COMPUTE THE BASIC AND REQUIRED INTEGRALS
!
!  SET UP THE ARRAY OF CONSTANTS FOR ROMBER INTEGRATION ROUTINE
!
            d(21) = 0.D0
            d(22) = rp
            d(23) = r1
            d(24) = cosa1
            d(25) = sina1
!
! COMPUTE CONSTANTS NEEDED FOR INTEGRAL CALCULATIONS
!
            d(30) = r1 - rp*sina1
            d(31) = rp*cosa1
            d(32) = rp*sina1
            d(33) = cosa1**2
            d(34) = sina1*cosa1
            d(35) = sina1**2
            d(36) = 0.5 - d(35)
!
! START LOOP  FOR CALCULATIONS OF INTEGRALS
!
            DO jp1 = 1 , 11
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     j = jp1 - 1
                     k = (j*6) + 1
                     djp1 = jp1
!
! TEST FOR ELEMENT SHAPE
!
                     IF ( itord<0 ) THEN
!
! THE CYLINDRICAL RING BASIC INTEGRALS WILL BE COMPUTED IN
! LOCATIONS D(1) AND D(2)
!
!
! COMPUTE I(J,1)
!
                        d(1) = (s**jp1)/djp1
!
! COMPUTE I(J,2)
!
                        d(2) = d(1)/r1
!
! THE CYLINDRICAL RING REQUIRED INTEGRALS
!
                        delint(k) = r1*d(1) + cosa1*(s**(jp1+1)/(djp1+1.))
                        delint(k+1) = sina1*d(1)
                        delint(k+2) = d(35)*d(2)
                        delint(k+3) = 0.
                        delint(k+4) = 0.
                        delint(k+5) = 0.
                        CYCLE
                     ELSEIF ( itord==0 ) THEN
!
! THE TOROIDAL RING BASIC INTEGRALS WILL BE COMPUTED IN
! LOCATIONS D(1),...,D(6)
!
                        d(20) = (rp**jp1)
!
! COMPUTE I(J,1)
!
                        d(1) = d(20)*(phib**jp1)/djp1
!
! COMPUTE I(J,2)
!
                        d(2) = (phib**(jp1+1))/(djp1+1.)
                        d(10) = 1.
                        DO i = 1 , 20
                           ip = jp1 + 2*i + 1
                           d(11) = 2*i + 1
                           d(10) = d(10)*d(11)*(d(11)-1.)
                           d(12) = (-1.)**i*phib**ip/((djp1+d(11))*d(10))
                           d(13) = dabs(d(12)/d(2))
                           d(2) = d(2) + d(12)
                           IF ( d(13)<=1.D-10 ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                        ENDDO
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
!
! THE CONICAL RING BASIC INTEGRALS WILL BE COMPUTED IN
! LOCATIONS D(1) AND D(2)
!
!
! COMPUTE I(J,1)
!
                        d(1) = (s**jp1)/djp1
                        IF ( j<1 ) THEN
!
!   COMPUTE  I(0,2)
!
                           d(2) = dlog((r1+s*cosa1)/r1)/cosa1
                        ELSEIF ( j==1 ) THEN
!
!    COMPUTE I(1,2)
!
                           d(2) = (s-(r1/cosa1)*dlog((r1+s*cosa1)/r1))/cosa1
                        ELSE
!
!    COMPUTE I(J,2) WHERE J .GT.1
!
                           d(2) = 1./djp1
                           d(10) = -s*cosa1/r1
                           DO i = 1 , 1000
                              d(11) = jp1 + i
                              d(12) = (d(10)**i)/d(11)
                              d(2) = d(2) + d(12)
                              IF ( d(12)<1.D-4 ) THEN
                                 spag_nextblock_2 = 4
                                 CYCLE SPAG_DispatchLoop_2
                              ENDIF
                           ENDDO
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        spag_nextblock_2 = 5
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  CASE (2)
                     d(2) = d(20)*d(2)
!
! COMPUTE I(J,3)
!
                     d(3) = (phib**jp1)/djp1
                     d(10) = 1.
                     DO i = 1 , 20
                        ip = jp1 + 2*i
                        d(11) = 2*i
                        d(10) = d(10)*d(11)*(d(11)-1.)
                        d(12) = (-1.)**i*phib**ip/((djp1+d(11))*d(10))
                        d(13) = dabs(d(12)/d(3))
                        d(3) = d(3) + d(12)
                        IF ( d(13)<=1.D-10 ) THEN
                           spag_nextblock_2 = 3
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  CASE (3)
                     d(3) = d(20)*d(3)
                     d(26) = djp1
!
! COMPUTE I(J,4)
!
                     CALL rombdk(phib,d(10),ip,d(4),1,d(21))
                     IF ( ip>=15 ) CALL mesage(30,26,Idel)
                     d(4) = d(20)*d(4)
!
! COMPUTE I(J,5)
!
                     CALL rombdk(phib,d(10),ip,d(5),2,d(21))
                     IF ( ip>=15 ) CALL mesage(30,26,Idel)
                     d(5) = d(20)*d(5)
!
! COMPUTE I(J,6)
!
                     CALL rombdk(phib,d(10),ip,d(6),3,d(21))
                     IF ( ip>=15 ) CALL mesage(30,26,Idel)
                     d(6) = d(20)*d(6)
!
! THE TOROIDAL RING REQUIRED INTEGRALS
!
                     delint(k) = d(30)*d(1) + d(31)*d(2) + d(32)*d(3)
                     delint(k+1) = cosa1*d(2) + sina1*d(3)
                     delint(k+2) = d(33)*d(4) + d(34)*d(5) + d(35)*d(6)
                     delint(k+3) = cosa1*d(3) - sina1*d(2)
                     delint(k+4) = d(34)*(d(6)-d(4)) + d(36)*d(5)
                     delint(k+5) = d(33)*d(6) - d(34)*d(5) + d(35)*d(4)
                     CYCLE
                  CASE (4)
                     d(2) = ((s**jp1)/r1)*d(2)
                     spag_nextblock_2 = 5
                  CASE (5)
!
! THE CONICAL RING REQUIRED INTEGRALS
!
                     delint(k) = r1*d(1) + cosa1*(s**(jp1+1)/(djp1+1.))
                     delint(k+1) = sina1*d(1)
                     delint(k+2) = d(35)*d(2)
                     delint(k+3) = cosa1*d(1)
                     delint(k+4) = d(34)*d(2)
                     delint(k+5) = d(33)*d(2)
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
!
!
            ENDDO
!
!   IF STIFFNESS MATRIX NOT REQUIRED  GO TO MASS ROUTINE
!
!
!
! LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3 TABLE
!
            Matidc = Matid
            Matflg = 7
            Eltemp = Tempe
            CALL mat(Idel)
!
!
! SET MATERIAL PROPERTIES IN LOCAL VARIABLES
!
            ep = E(1)
            et = E(2)
            vpt = Anu(1)
            vtp = vpt*et/ep
            del = 1. - vpt*vtp
            dict5 = Gsube
!
!
! GENERATE THE ELASTIC CONSTANTS MATRIX(2X2)
!
            ee(1) = ep/del
            ee(2) = et*vpt/del
            ee(3) = ee(2)
            ee(4) = et/del
!
!
! FORM THE STIFFNESS MATRIX IN FIELD COORDINATES
!
! COMPUTE CONSTANTS NEEDED IN DMATRX SUBROUTINE
!
            d(1) = ep/et
            d(7) = 0.
            IF ( itord==0 ) d(7) = 1./rp
            d(2) = d(1)*d(7)
            d(3) = d(2)*d(7)
            d(4) = vpt*d(7)
            d(5) = (ep*Tm/(d(1)-vpt**2))*twopi
            d(6) = (ep*Tf**3)/(12.*(d(1)-vpt**2))*twopi
!
! CALL THE DMATRIX SUBROUTINE TO COMPUTE THE STIFFNESS MATRIX (10X10)
!
! NOTE THE DOUBLE SUBSCRIPTING USED IN DMATRIX SUBROUTINE IS
! COMPATIBLE WITH THE CALLING PROGRAM. THE DELINT ARRAY OF INTEGRALS
! IS A (11X6) SINGLY SUBSCRIPTED ARRAY (STORED ROWWISE) IN THE CALLING
! PROGRAM AND IT IS A (6X11) DOUBLY SUBSCRIPTED ARRAY (STORED
! COLUMNWISE) IN DMATRX ROUTINE.
!
            IF ( Ismb(1)/=0 ) CALL dmatrx(ak(1),vpt,d(1),d(2),d(3),d(4),d(5),d(6),delint(1))
            IF ( Ismb(2)/=0 ) THEN
               DO i = 1 , 100
                  am(i) = 0.
               ENDDO
               am(1) = delint(1)
               am(2) = delint(7)
               am(3) = delint(13)
               am(4) = delint(19)
               am(11) = delint(7)
               am(12) = delint(13)
               am(13) = delint(19)
               am(14) = delint(25)
               am(21) = delint(13)
               am(22) = delint(19)
               am(23) = delint(25)
               am(24) = delint(31)
               am(31) = delint(19)
               am(32) = delint(25)
               am(33) = delint(31)
               am(34) = delint(37)
               am(45) = delint(1)
               am(46) = delint(7)
               am(47) = delint(13)
               am(48) = delint(19)
               am(49) = delint(25)
               am(50) = delint(31)
               am(55) = delint(7)
               am(56) = delint(13)
               am(57) = delint(19)
               am(58) = delint(25)
               am(59) = delint(31)
               am(60) = delint(37)
               am(65) = delint(13)
               am(66) = delint(19)
               am(67) = delint(25)
               am(68) = delint(31)
               am(69) = delint(37)
               am(70) = delint(43)
               am(75) = delint(19)
               am(76) = delint(25)
               am(77) = delint(31)
               am(78) = delint(37)
               am(79) = delint(43)
               am(80) = delint(49)
               am(85) = delint(25)
               am(86) = delint(31)
               am(87) = delint(37)
               am(88) = delint(43)
               am(89) = delint(49)
               am(90) = delint(55)
               am(95) = delint(31)
               am(96) = delint(37)
               am(97) = delint(43)
               am(98) = delint(49)
               am(99) = delint(55)
               am(100) = delint(61)
!
               d(1) = twopi*Rho*Tm
               DO i = 1 , 100
                  am(i) = d(1)*am(i)
               ENDDO
            ENDIF
!
!
!
! FORM THE TRANSFORMATION MATRIX(10X12) FROM FIELD COORDINATES TO GRID
! POINT DEGREES OF FREEDOM
!
            DO i = 1 , 72
               gambqf(i) = 0.
            ENDDO
            d(1) = s
            d(2) = s**2
            d(3) = s**3
            d(4) = s**4
            d(5) = s**5
            gambqf(3) = 1.
            gambqf(16) = 1.
            gambqf(30) = .5
            gambqf(39) = -10./d(3)
            gambqf(40) = -6./d(2)
            gambqf(42) = -1.5/d(1)
            gambqf(45) = -gambqf(39)
            gambqf(46) = -4./d(2)
            gambqf(48) = .5/d(1)
            gambqf(51) = 15./d(4)
            gambqf(52) = 8./d(3)
            gambqf(54) = 1.5/d(2)
            gambqf(57) = -gambqf(51)
            gambqf(58) = 7./d(3)
            gambqf(60) = -1./d(2)
            gambqf(63) = -6./d(5)
            gambqf(64) = -3./d(4)
            gambqf(66) = -.5/d(3)
            gambqf(69) = -gambqf(63)
            gambqf(70) = gambqf(64)
            gambqf(72) = -gambqf(66)
            DO i = 1 , 48
               gambqm(i) = 0.
            ENDDO
            gambqm(1) = 1.
            gambqm(17) = 1.
            gambqm(25) = -3./d(2)
            gambqm(29) = -2./d(1)
            gambqm(31) = -gambqm(25)
            gambqm(35) = -1./d(1)
            gambqm(37) = 2./d(3)
            gambqm(41) = 1./d(2)
            gambqm(43) = -gambqm(37)
            gambqm(47) = gambqm(41)
!
!
! TRANSFORM THE STIFFNESS MATRIX TO GRID POINT DEGREES OF FREEDOM
!
            IF ( Ismb(1)/=0 ) THEN
               CALL gmmatd(gambq(1),10,12,1,ak(1),10,10,0,d(1))
               CALL gmmatd(d(1),12,10,0,gambq(1),10,12,0,ak(1))
            ENDIF
            IF ( Ismb(2)/=0 ) THEN
!     REARRANGE GAMBQ FOR MASS MATRIX CALCULATIONS
               DO i = 1 , 72
                  d(i+48) = gambq(i)
               ENDDO
               DO i = 1 , 48
                  d(i) = gambq(i+72)
               ENDDO
               DO i = 1 , 120
                  gambq(i) = d(i)
               ENDDO
               CALL gmmatd(gambq(1),10,12,1,am(1),10,10,0,d(1))
               CALL gmmatd(d(1),12,10,0,gambq(1),10,12,0,am(1))
            ENDIF
!
!
!
! FORM THE TRANSFORMATION MATRIX (12X12) FROM ELEMENT TO BASIC
! COORDINATES
!
            DO i = 1 , 144
               gamrs(i) = 0.
            ENDDO
            gamrs(1) = cosa1
            gamrs(3) = -sina1
            gamrs(25) = sina1
            gamrs(27) = cosa1
            gamrs(40) = -1.
            gamrs(53) = 1.
            gamrs(66) = 1.
            gamrs(79) = cosa2
            gamrs(81) = -sina2
            gamrs(103) = sina2
            gamrs(105) = cosa2
            gamrs(118) = -1.
            gamrs(131) = 1.
            gamrs(144) = 1.
!
!
! TRANSFORM THE STIFFNESS MATRIX FROM ELEMENT TO BASIC COORDINATES
!
            IF ( Ismb(1)/=0 ) THEN
               CALL gmmatd(gamrs(1),12,12,1,ak(1),12,12,0,d(1))
               CALL gmmatd(d(1),12,12,0,gamrs(1),12,12,0,ak(1))
            ENDIF
            IF ( Ismb(2)/=0 ) THEN
               CALL gmmatd(gamrs(1),12,12,1,am(1),12,12,0,d(1))
               CALL gmmatd(d(1),12,12,0,gamrs(1),12,12,0,am(1))
            ENDIF
!
!
! LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL COORDINATES
! FOR THE TWO GRID POINTS AND EXPAND TO (6X6)
! THE TWO MATRICES WILL BE STORED IN D(1),...,D(36) AND D(37),...,D(72)
! RESPECTIVELY
!
            DO i = 1 , 72
               d(i) = 0.
            ENDDO
            DO i = 1 , 2
               IF ( ics(i)/=0 ) THEN
                  k = 36*(i-1)
                  CALL transd(ics(1),d(73))
                  DO j = 1 , 3
                     kk = k + 6*(j-1) + 1
                     kl = 3*(j-1) + 73
                     kj = k + 6*(j+2) + j + 3
                     d(kk) = d(kl)
                     d(kk+1) = d(kl+1)
                     d(kk+2) = d(kl+2)
                     d(kj) = 1.
                  ENDDO
               ENDIF
            ENDDO
!
!  DIVIDE THE STIFFNESS MATRIX INTO 4 SUBMATRICES WHICH CAN THEN BE
!  TRANSFORMED FROM BASIC TO LOCAL COORDINATES THEN REINSERTED IN THE
!  STIFFNESS MATRIX IN INCREASING SIL ORDER
!
            DO ip = 1 , 2
               ipp = ip
               IF ( Igp(1)>=Igp(2) ) ipp = 3 - ip
               ir = 72*(ipp-1)
               iapp = 36*(ipp-1) + 1
               DO ji = 1 , 2
                  i = ji
                  IF ( ip/=ipp ) i = 3 - ji
!
!   PLACE THE APPROPRIATE SUBMATRIX INTO A (6X6) MATRIX
!
                  ic = 6*(i-1)
                  k = 0
                  DO ii = 1 , 6
                     kl = ir + 12*(ii-1) + ic
                     DO ij = 1 , 6
                        k = k + 1
                        kk = kl + ij
                        aki(k) = ak(kk)
                        akm(k) = am(kk)
                     ENDDO
                  ENDDO
!
!   TRANSFORM FROM BASIC TO LOCAL  COORDINATES
!
                  IF ( ics(ipp)/=0 ) THEN
                     IF ( Ismb(1)/=0 ) THEN
                        CALL gmmatd(d(iapp),6,6,1,aki(1),6,6,0,d(73))
                        DO j = 1 , 36
                           aki(j) = d(j+72)
                        ENDDO
                     ENDIF
                     IF ( Ismb(2)/=0 ) THEN
                        CALL gmmatd(d(iapp),6,6,1,akm(1),6,6,0,d(73))
                        DO j = 1 , 36
                           akm(i) = d(j+72)
                        ENDDO
                     ENDIF
                  ENDIF
!
                  IF ( ics(i)/=0 ) THEN
                     iai = 36*(i-1) + 1
                     IF ( Ismb(1)/=0 ) THEN
                        CALL gmmatd(aki(1),6,6,0,d(iai),6,6,0,d(73))
                        DO j = 1 , 36
                           aki(j) = d(j+72)
                        ENDDO
                     ENDIF
                     IF ( Ismb(2)/=0 ) THEN
                        CALL gmmatd(akm(1),6,6,0,d(iai),6,6,0,d(73))
                        DO j = 1 , 36
                           akm(j) = d(j+72)
                        ENDDO
                     ENDIF
                  ENDIF
!
!    REINSERT INTO OVERALL STIFFNESS MATRIX ACCORDING TO INCREASING SIL
!
                  DO ii = 1 , 6
                     DO jj = 1 , 6
                        ki = (ii-1)*6 + jj
                        iout = (ip-1)*72 + (ji-1)*6 + (ii-1)*12 + jj
                        kout(iout) = aki(ki)
                        mout(iout) = akm(ki)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
!
!     OUTPUT THE MATRIX BY EMGOUT
!
            dict(2) = 1
            IF ( Ismb(1)/=0 ) CALL emgout(kout,kout,144,1,dict,1,ipr)
!
            IF ( Ismb(2)/=0 ) CALL emgout(mout,mout,144,1,dict,2,ipr)
!
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         Nogo = .TRUE.
         RETURN
      CASE (3)
         CALL mesage(30,26,Idel)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE tordrd
