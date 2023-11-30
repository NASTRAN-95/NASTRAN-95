
SUBROUTINE ttordr(Ti,Pg)
   IMPLICIT NONE
   REAL Alf(3) , Anu(3) , Consts(5) , Costh , Degra , E(3) , Ecpt(18) , Eltemp , G(3) , Gsube , Rho , Sinth , Stress , Twopi , Tzero
   INTEGER Iecpt(18) , Matflg , Matidc
   COMMON /condas/ Consts
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero , Gsube
   COMMON /trimex/ Ecpt
   REAL Pg(1) , Ti(2)
   REAL a1 , a2 , aki(36) , alph(2) , cosa1 , cosa2 , d(36) , del , delint(42) , djp1 , dtf1 , dtf2 , dtm1 , dtm2 , ee(4) , ep ,    &
      & et , ffe(40) , fme(40) , gambl(144) , gambq(144) , gambqf(72) , gambqm(48) , gamrs(144) , phib , r(2) , r1 , r2 , rp , s ,  &
      & sina1 , sina2 , tempe , tf , tl(12) , tm , tz , vpt , vtp , z(2) , z1 , z2
   INTEGER i , ics(2) , idel , igp(2) , ip , itord , j , jp1 , k , kj , kk , kl , l , matid
!
!
!*****
! THIS ROUTINE COMPUTES THE THERMAL LOAD FOR AN AXI-SYMMETRIC
! TOROIDAL THIN SHELL RING
!*****
!
!
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
!
!
!
   EQUIVALENCE (Consts(2),Twopi)
   EQUIVALENCE (Consts(4),Degra)
   EQUIVALENCE (Iecpt(1),Ecpt(1))
   EQUIVALENCE (a1,alph(1)) , (a2,alph(2))
   EQUIVALENCE (r1,r(1)) , (r2,r(2))
   EQUIVALENCE (z1,z(1)) , (z2,z(2))
   EQUIVALENCE (gambqm(1),gambq(1))
   EQUIVALENCE (gambqf(1),gambq(49))
   EQUIVALENCE (delint(1),gambq(1))
   EQUIVALENCE (fme(1),gambq(43))
   EQUIVALENCE (ffe(1),gambq(83))
   EQUIVALENCE (gamrs(1),gambq(1))
   EQUIVALENCE (aki(1),gambq(1))
   EQUIVALENCE (gambl(1),gambq(1))
!
! ----------------------------------------------------------------------
!
! STORE ECPT PARAMETERS IN LOCAL VARIABLES
!
   idel = Iecpt(1)
   igp(1) = Iecpt(2)
   igp(2) = Iecpt(3)
   matid = Iecpt(7)
   ics(1) = Iecpt(10)
   ics(2) = Iecpt(14)
   alph(1) = Ecpt(4)
   alph(2) = Ecpt(5)
   tm = Ecpt(8)
   tf = Ecpt(9)
   r(1) = Ecpt(11)
   d(1) = Ecpt(12)
   z(1) = Ecpt(13)
   r(2) = Ecpt(15)
   d(2) = Ecpt(16)
   z(2) = Ecpt(17)
   tempe = Ecpt(18)
!
!
! TEST THE VALIDITY OF THE GRID POINT COORDINATES
!
   DO i = 1 , 2
      IF ( r(i)<0.0E0 ) CALL mesage(-30,37,idel)
      IF ( d(i)/=0.0E0 ) CALL mesage(-30,37,idel)
   ENDDO
!
!
! DETERMINE IF ELEMENT IS A TOROIDAL, CONICAL OR CYLINDRICAL RING
!
   itord = 0
   IF ( abs(a1-a2)<=.000001 ) itord = 1
   IF ( itord==1 .AND. abs(a1-90.0E0)<=.00001 ) itord = -1
!
!
! COMPUTE THE ELEMENT COORDINATES
!
   a1 = a1*Degra
   a2 = a2*Degra
   phib = a2 - a1
   sina1 = sin(a1)
   cosa1 = cos(a1)
   sina2 = sin(a2)
   cosa2 = cos(a2)
!
   IF ( itord/=0 ) THEN
!
! FOR THE CONICAL OR CYLINDRICAL RING
!
      rp = 0.0D0
      s = sqrt((r2-r1)**2+(z2-z1)**2)
   ELSE
!
! FOR THE TOROIDAL RING
!
      rp = sqrt((r2-r1)**2+(z2-z1)**2)/(2.0E0*sin(phib/2.0E0))
      s = phib*rp
   ENDIF
!
!
!
! COMPUTE THE BASIC AND REQUIRED INTEGRALS
!
!
! SET UP ARRAY OF CONSTANTS FOR ROMBER INTEGRATION ROUTINE
!
   d(21) = 0.0E0
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
   DO jp1 = 1 , 7
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
         delint(k) = r1*d(1) + cosa1*((s**(jp1+1))/(djp1+1.0E0))
         delint(k+1) = sina1*d(1)
         delint(k+2) = d(35)*d(2)
         delint(k+3) = 0.0E0
         delint(k+4) = 0.0E0
         delint(k+5) = 0.0E0
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
         d(2) = (phib**(jp1+1))/(djp1+1.0E0)
         d(10) = 1.0E0
         DO i = 1 , 20
            ip = jp1 + 2*i + 1
            d(11) = 2*i + 1
            d(10) = d(10)*d(11)*(d(11)-1.0E0)
            d(12) = (-1.0E0)**i*phib**ip/((djp1+d(11))*d(10))
            d(13) = abs(d(12)/d(2))
            d(2) = d(2) + d(12)
            IF ( d(13)<=1.0E-10 ) GOTO 20
         ENDDO
         CALL mesage(-30,26,idel)
 20      d(2) = d(20)*d(2)
!
! COMPUTE I(J,3)
!
         d(3) = (phib**jp1)/djp1
         d(10) = 1.0E0
         DO i = 1 , 20
            ip = jp1 + 2*i
            d(11) = 2*i
            d(10) = d(10)*d(11)*(d(11)-1.0E0)
            d(12) = (-1.0E0)**i*phib**ip/((djp1+d(11))*d(10))
            d(13) = abs(d(12)/d(3))
            d(3) = d(3) + d(12)
            IF ( d(13)<=1.0E-10 ) GOTO 40
         ENDDO
         CALL mesage(-30,26,idel)
 40      d(3) = d(20)*d(3)
         d(26) = djp1
!
! COMPUTE I(J,4)
!
         CALL romber(phib,d(10),ip,d(4),1,d(21))
         IF ( ip>=15 ) CALL mesage(30,26,idel)
         d(4) = d(20)*d(4)
!
! COMPUTE I(J,5)
!
         CALL romber(phib,d(10),ip,d(5),2,d(21))
         IF ( ip>=15 ) CALL mesage(30,26,idel)
         d(5) = d(20)*d(5)
!
! COMPUTE I(J,6)
!
         CALL romber(phib,d(10),ip,d(6),3,d(21))
         IF ( ip>=15 ) CALL mesage(30,26,idel)
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
      ELSE
!
! THE CONICAL RING BASIC INTEGRALS WILL BE COMPUTED IN
! LOCATIONS D(1) AND D(2)
!
!
! COMPUTE I(J,1)
!
         d(1) = (s**jp1)/djp1
!
         IF ( j<1 ) THEN
!
! COMPUTE I(0,2)
!
            d(2) = alog((r1+s*cosa1)/r1)/cosa1
         ELSEIF ( j==1 ) THEN
!
! COMPUTE I(1,2)
!
            d(2) = (s-(r1/cosa1)*alog((r1+s*cosa1)/r1))/cosa1
         ELSE
!
! COMPUTE I(J,2) WHERE J .GT. 1
!
            d(2) = 1.0E0/djp1
            d(10) = -s*cosa1/r1
            DO i = 1 , 1000
               d(11) = jp1 + i
               d(12) = (d(10)**i)/d(11)
               d(2) = d(2) + d(12)
               IF ( d(12)<1.0E-4 ) GOTO 50
            ENDDO
            CALL mesage(-30,26,idel)
 50         d(2) = ((s**jp1)/r1)*d(2)
         ENDIF
!
! THE CONICAL RING REQUIRED INTEGRALS
!
         delint(k) = r1*d(1) + cosa1*((s**(jp1+1))/(djp1+1.0E0))
         delint(k+1) = sina1*d(1)
         delint(k+2) = d(35)*d(2)
         delint(k+3) = cosa1*d(1)
         delint(k+4) = d(34)*d(2)
         delint(k+5) = d(33)*d(2)
      ENDIF
!
   ENDDO
!
!
! LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3 TABLE
!
   Matidc = matid
   Matflg = 7
   Eltemp = tempe
   CALL mat(idel)
!
!
! SET MATERIAL PROPERTIES IN LOCAL VARIABLES
!
   ep = E(1)
   et = E(2)
   vpt = Anu(1)
   tz = Tzero
   vtp = vpt*et/ep
   del = 1.0E0 - vpt*vtp
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
! CALL THE FCURL   SUBROUTINE TO FORM THE FOUR (2X10) MATRICES OF
! INTEGRALS (TRANSPOSED)
!
! COMPUTE CONSTANTS NEEDED IN FCURL  SUBROUTINE
!
   d(1) = 0.0E0
   IF ( itord==0 ) d(1) = 1.0E0/rp
!
! NOTE THE DOUBLE SUBSCRIPTING USED IN  FCURL  SUBROUTINE IS
! COMPATIBLE WITH THE CALLING PROGRAM. THE DELINT ARRAY OF INTEGRALS
! IS A ( 7X6) SINGLY SUBSCRIPTED ARRAY (STORED ROWWISE) IN THE CALLING
! PROGRAM AND IT IS A (6X 7) DOUBLY SUBSCRIPTED ARRAY (STORED
! COLUMNWISE) IN FCURL  ROUTINE.
!
!
   CALL fcurl(fme(1),fme(21),ffe(1),ffe(21),delint(1),s,d(1))
!
   d(1) = Twopi*tm
   d(2) = Twopi*(tf**3)/12.0E0
   DO i = 1 , 40
      fme(i) = d(1)*fme(i)
      ffe(i) = d(2)*ffe(i)
   ENDDO
!
!
! FORM THE THERMAL STRAINS
!
   dtm1 = Ti(1) - tz
   dtm2 = Ti(2) - Ti(1)
   dtf1 = 0.0E0
   dtf2 = 0.0E0
!
! THE TERMS DTF1 AND DTF2 ARE FUNCTIONS OF THE FLEXURAL GRADIENT
! TEMPERATURE BUT SINCE THESE TEMPERATURES ARE NOT AVAILABLE
! THE TERMS WILL BE SET TO ZERO. THEY ARE USUALLY DEFINED AS FOLLOWS,
!     DTF1 = TF(1) - TZ
!     DTF2 = TF(2) - TF(1)
! WHERE TF(1) AND TF(2) ARE THE FLEXURAL GRADIENT TEMPERATURES AT
! GRID POINTS 1 AND 2 RESPECTIVELY.
!
   d(1) = dtm1*Alf(1)
   d(2) = dtm1*Alf(2)
   d(3) = dtm2*Alf(1)
   d(4) = dtm2*Alf(2)
   d(5) = dtf1*Alf(1)
   d(6) = dtf1*Alf(2)
   d(7) = dtf2*Alf(1)
   d(8) = dtf2*Alf(2)
!
!
! FORM THE   THERMAL LOAD   IN FIELD COORDINATES
!
   CALL gmmats(ee(1),2,2,0,d(1),2,1,0,d(11))
   CALL gmmats(ee(1),2,2,0,d(3),2,1,0,d(13))
   CALL gmmats(ee(1),2,2,0,d(5),2,1,0,d(15))
   CALL gmmats(ee(1),2,2,0,d(7),2,1,0,d(17))
!
!
   CALL gmmats(fme(1),2,10,1,d(11),2,1,0,tl(1))
   CALL gmmats(fme(21),2,10,-1,d(13),2,1,0,tl(1))
   CALL gmmats(ffe(1),2,10,-1,d(15),2,1,0,tl(1))
   CALL gmmats(ffe(21),2,10,-1,d(17),2,1,0,tl(1))
!
!
! FORM THE TRANSFORMATION MATRIX(10X12) FROM FIELD COORDINATES TO GRID
! POINT DEGREES OF FREEDOM
!
   DO i = 1 , 72
      gambqf(i) = 0.0E0
   ENDDO
   d(1) = s
   d(2) = s**2
   d(3) = s**3
   d(4) = s**4
   d(5) = s**5
   gambqf(3) = 1.0E0
   gambqf(16) = 1.0E0
   gambqf(30) = 0.5E0
   gambqf(39) = -10.0E0/d(3)
   gambqf(40) = -6.0E0/d(2)
   gambqf(42) = -1.5E0/d(1)
   gambqf(45) = -gambqf(39)
   gambqf(46) = -4.0E0/d(2)
   gambqf(48) = 0.5E0/d(1)
   gambqf(51) = 15.0E0/d(4)
   gambqf(52) = 8.0E0/d(3)
   gambqf(54) = 1.5E0/d(2)
   gambqf(57) = -gambqf(51)
   gambqf(58) = 7.0E0/d(3)
   gambqf(60) = -1.0E0/d(2)
   gambqf(63) = -6.0E0/d(5)
   gambqf(64) = -3.0E0/d(4)
   gambqf(66) = -0.5E0/d(3)
   gambqf(69) = -gambqf(63)
   gambqf(70) = gambqf(64)
   gambqf(72) = -gambqf(66)
   DO i = 1 , 48
      gambqm(i) = 0.0E0
   ENDDO
   gambqm(1) = 1.0E0
   gambqm(17) = 1.0E0
   gambqm(25) = -3.0E0/d(2)
   gambqm(29) = -2.0E0/d(1)
   gambqm(31) = -gambqm(25)
   gambqm(35) = -1.0E0/d(1)
   gambqm(37) = 2.0E0/d(3)
   gambqm(41) = 1.0E0/d(2)
   gambqm(43) = -gambqm(37)
   gambqm(47) = gambqm(41)
!
!
! TRANSFORM THE   THERMAL LOAD   TO GRID POINT DEGREES OF FREEDOM
!
   CALL gmmats(gambq(1),10,12,1,tl(1),10,1,0,d(1))
!
!
! FORM THE TRANSFORMATION MATRIX (12X12) FROM ELEMENT TO BASIC
! COORDINATES
!
   DO i = 1 , 144
      gamrs(i) = 0.0E0
   ENDDO
   gamrs(1) = cosa1
   gamrs(3) = -sina1
   gamrs(25) = sina1
   gamrs(27) = cosa1
   gamrs(40) = -1.0E0
   gamrs(53) = 1.0E0
   gamrs(66) = 1.0E0
   gamrs(79) = cosa2
   gamrs(81) = -sina2
   gamrs(103) = sina2
   gamrs(105) = cosa2
   gamrs(118) = -1.0E0
   gamrs(131) = 1.0E0
   gamrs(144) = 1.0E0
!
!
!
! TRANSFORM THE   THERMAL LOAD   FROM ELEMENT TO BASIC COORDINATES
!
   CALL gmmats(gamrs(1),12,12,1,d(1),12,1,0,tl(1))
!
!
! LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL COORDINATES
! FOR THE TWO GRID POINTS AND EXPAND TO (6X6)
!
   DO i = 1 , 144
      gambl(i) = 0.0E0
   ENDDO
   DO i = 1 , 2
      CALL gbtran(ics(i),Ecpt(4*i+10),d(1))
      k = 78*(i-1)
      DO j = 1 , 3
         kk = k + 12*(j-1) + 1
         kl = 3*(j-1) + 1
         kj = k + 12*(j+2) + j + 3
         gambl(kk) = d(kl)
         gambl(kk+1) = d(kl+1)
         gambl(kk+2) = d(kl+2)
         gambl(kj) = 1.0E0
      ENDDO
   ENDDO
!
!
!
! TRANSFORM THE   THERMAL LOAD   FROM BASIC TO LOCAL COORDINATES
!
   CALL gmmats(gambl(1),12,12,1,tl(1),12,1,0,d(1))
!
!
!
! ADD THE ELEMENT THERMAL LOAD TO THE STRUCTURE THERMAL LOAD
!
   k = 0
   DO i = 1 , 2
      l = igp(i) - 1
      DO j = 1 , 6
         k = k + 1
         l = l + 1
         Pg(l) = Pg(l) + d(k)
      ENDDO
   ENDDO
!
!
END SUBROUTINE ttordr
