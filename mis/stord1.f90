
SUBROUTINE stord1
   IMPLICIT NONE
   REAL A1 , A2 , Ak(144) , Alf(3) , Alph(2) , Anu(3) , Consts(5) , Costh , D(180) , Degra , Dum5(82) , E(3) , Ecpt(18) , Eltemp ,  &
      & G(3) , Gsube , R(2) , R1 , R2 , Rho , Sel(180) , Sinth , Stress , Ts(30) , Twopi , Tz , Tzero , Z(2) , Z1 , Z2
   INTEGER Idel , Iecpt(18) , Igp(2) , Matflg , Matidc
   COMMON /condas/ Consts
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero , Gsube
   COMMON /sdr2x5/ Ecpt , Dum5 , Idel , Igp , Tz , Sel , Ts , Ak
   COMMON /sdr2x6/ D , R , Z , Alph
   REAL aki(36) , cosa1 , cosa2 , del , delint(66) , djp1 , ee(4) , ep , et , gambl(144) , gambq(144) , gambqf(72) , gambqm(48) ,   &
      & gamrs(144) , phib , rp , s , sina1 , sina2 , tempe , tf , tm , vpt , vtp
   INTEGER i , ics(2) , ip , itord , j , jp1 , k , kj , kk , kl , matid
!
!
!*****
! THIS ROUTINE IS PHASE  I OF STRESS DATA RECOVERY FOR AN AXI-SYMMETRIC
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
   EQUIVALENCE (A1,Alph(1)) , (A2,Alph(2))
   EQUIVALENCE (R1,R(1)) , (R2,R(2))
   EQUIVALENCE (Z1,Z(1)) , (Z2,Z(2))
   EQUIVALENCE (gambqf(1),gambq(1))
   EQUIVALENCE (gambqm(1),gambq(73))
   EQUIVALENCE (delint(1),gambq(1))
   EQUIVALENCE (gamrs(1),gambq(1))
   EQUIVALENCE (aki(1),gambq(1))
   EQUIVALENCE (gambl(1),gambq(1))
!
! ----------------------------------------------------------------------
!
! STORE ECPT PARAMETERS IN LOCAL VARIABLES
!
   Idel = Iecpt(1)
   Igp(1) = Iecpt(2)
   Igp(2) = Iecpt(3)
   matid = Iecpt(7)
   ics(1) = Iecpt(10)
   ics(2) = Iecpt(14)
   Alph(1) = Ecpt(4)
   Alph(2) = Ecpt(5)
   tm = Ecpt(8)
   tf = Ecpt(9)
   R(1) = Ecpt(11)
   D(1) = Ecpt(12)
   Z(1) = Ecpt(13)
   R(2) = Ecpt(15)
   D(2) = Ecpt(16)
   Z(2) = Ecpt(17)
   tempe = Ecpt(18)
!
!
! TEST THE VALIDITY OF THE GRID POINT COORDINATES
!
   DO i = 1 , 2
      IF ( R(i)<0.0E0 ) CALL mesage(-30,37,Idel)
      IF ( D(i)/=0.0E0 ) CALL mesage(-30,37,Idel)
   ENDDO
!
!
! DETERMINE IF ELEMENT IS A TOROIDAL, CONICAL OR CYLINDRICAL RING
!
   itord = 0
   IF ( abs(A1-A2)<=.000001 ) itord = 1
   IF ( itord==1 .AND. abs(A1-90.0E0)<=.00001 ) itord = -1
!
!
! COMPUTE THE ELEMENT COORDINATES
!
   A1 = A1*Degra
   A2 = A2*Degra
   phib = A2 - A1
   sina1 = sin(A1)
   cosa1 = cos(A1)
   sina2 = sin(A2)
   cosa2 = cos(A2)
!
   IF ( itord/=0 ) THEN
!
! FOR THE CONICAL OR CYLINDRICAL RING
!
      rp = 0.0D0
      s = sqrt((R2-R1)**2+(Z2-Z1)**2)
   ELSE
!
! FOR THE TOROIDAL RING
!
      rp = sqrt((R2-R1)**2+(Z2-Z1)**2)/(2.0E0*sin(phib/2.0E0))
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
   D(21) = 0.0E0
   D(22) = rp
   D(23) = R1
   D(24) = cosa1
   D(25) = sina1
!
! COMPUTE CONSTANTS NEEDED FOR INTEGRAL CALCULATIONS
!
   D(30) = R1 - rp*sina1
   D(31) = rp*cosa1
   D(32) = rp*sina1
   D(33) = cosa1**2
   D(34) = sina1*cosa1
   D(35) = sina1**2
   D(36) = 0.5 - D(35)
!
! START LOOP  FOR CALCULATIONS OF INTEGRALS
!
   DO jp1 = 1 , 11
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
         D(1) = (s**jp1)/djp1
!
! COMPUTE I(J,2)
!
         D(2) = D(1)/R1
!
! THE CYLINDRICAL RING REQUIRED INTEGRALS
!
         delint(k) = R1*D(1) + cosa1*((s**(jp1+1))/(djp1+1.0E0))
         delint(k+1) = sina1*D(1)
         delint(k+2) = D(35)*D(2)
         delint(k+3) = 0.0E0
         delint(k+4) = 0.0E0
         delint(k+5) = 0.0E0
      ELSEIF ( itord==0 ) THEN
!
! THE TOROIDAL RING BASIC INTEGRALS WILL BE COMPUTED IN
! LOCATIONS D(1),...,D(6)
!
         D(20) = (rp**jp1)
!
! COMPUTE I(J,1)
!
         D(1) = D(20)*(phib**jp1)/djp1
!
! COMPUTE I(J,2)
!
         D(2) = (phib**(jp1+1))/(djp1+1.0E0)
         D(10) = 1.0E0
         DO i = 1 , 20
            ip = jp1 + 2*i + 1
            D(11) = 2*i + 1
            D(10) = D(10)*D(11)*(D(11)-1.0E0)
            D(12) = (-1.0E0)**i*phib**ip/((djp1+D(11))*D(10))
            D(13) = abs(D(12)/D(2))
            D(2) = D(2) + D(12)
            IF ( D(13)<=1.0E-10 ) GOTO 20
         ENDDO
         CALL mesage(-30,26,Idel)
 20      D(2) = D(20)*D(2)
!
! COMPUTE I(J,3)
!
         D(3) = (phib**jp1)/djp1
         D(10) = 1.0E0
         DO i = 1 , 20
            ip = jp1 + 2*i
            D(11) = 2*i
            D(10) = D(10)*D(11)*(D(11)-1.0E0)
            D(12) = (-1.0E0)**i*phib**ip/((djp1+D(11))*D(10))
            D(13) = abs(D(12)/D(3))
            D(3) = D(3) + D(12)
            IF ( D(13)<=1.0E-10 ) GOTO 40
         ENDDO
         CALL mesage(-30,26,Idel)
 40      D(3) = D(20)*D(3)
         D(26) = djp1
!
! COMPUTE I(J,4)
!
         CALL romber(phib,D(10),ip,D(4),1,D(21))
         IF ( ip>=15 ) CALL mesage(30,26,Idel)
         D(4) = D(20)*D(4)
!
! COMPUTE I(J,5)
!
         CALL romber(phib,D(10),ip,D(5),2,D(21))
         IF ( ip>=15 ) CALL mesage(30,26,Idel)
         D(5) = D(20)*D(5)
!
! COMPUTE I(J,6)
!
         CALL romber(phib,D(10),ip,D(6),3,D(21))
         IF ( ip>=15 ) CALL mesage(30,26,Idel)
         D(6) = D(20)*D(6)
!
! THE TOROIDAL RING REQUIRED INTEGRALS
!
         delint(k) = D(30)*D(1) + D(31)*D(2) + D(32)*D(3)
         delint(k+1) = cosa1*D(2) + sina1*D(3)
         delint(k+2) = D(33)*D(4) + D(34)*D(5) + D(35)*D(6)
         delint(k+3) = cosa1*D(3) - sina1*D(2)
         delint(k+4) = D(34)*(D(6)-D(4)) + D(36)*D(5)
         delint(k+5) = D(33)*D(6) - D(34)*D(5) + D(35)*D(4)
      ELSE
!
! THE CONICAL RING BASIC INTEGRALS WILL BE COMPUTED IN
! LOCATIONS D(1) AND D(2)
!
!
! COMPUTE I(J,1)
!
         D(1) = (s**jp1)/djp1
!
         IF ( j<1 ) THEN
!
! COMPUTE I(0,2)
!
            D(2) = alog((R1+s*cosa1)/R1)/cosa1
         ELSEIF ( j==1 ) THEN
!
! COMPUTE I(1,2)
!
            D(2) = (s-(R1/cosa1)*alog((R1+s*cosa1)/R1))/cosa1
         ELSE
!
! COMPUTE I(J,2) WHERE J .GT. 1
!
            D(2) = 1.0E0/djp1
            D(10) = -s*cosa1/R1
            DO i = 1 , 1000
               D(11) = jp1 + i
               D(12) = (D(10)**i)/D(11)
               D(2) = D(2) + D(12)
               IF ( D(12)<1.0E-4 ) GOTO 50
            ENDDO
            CALL mesage(-30,26,Idel)
 50         D(2) = ((s**jp1)/R1)*D(2)
         ENDIF
!
! THE CONICAL RING REQUIRED INTEGRALS
!
         delint(k) = R1*D(1) + cosa1*((s**(jp1+1))/(djp1+1.0E0))
         delint(k+1) = sina1*D(1)
         delint(k+2) = D(35)*D(2)
         delint(k+3) = cosa1*D(1)
         delint(k+4) = D(34)*D(2)
         delint(k+5) = D(33)*D(2)
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
   CALL mat(Idel)
!
!
! SET MATERIAL PROPERTIES IN LOCAL VARIABLES
!
   ep = E(1)
   et = E(2)
   vpt = Anu(1)
   Tz = Tzero
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
! FORM THE STIFFNESS MATRIX IN FIELD COORDINATES
!
! COMPUTE CONSTANTS NEEDED IN DMATRX SUBROUTINE
!
   D(1) = ep/et
   D(7) = 0.0E0
   IF ( itord==0 ) D(7) = 1.0E0/rp
   D(2) = D(1)*D(7)
   D(3) = D(2)*D(7)
   D(4) = vpt*D(7)
   D(5) = (ep*tm/(D(1)-vpt**2))*Twopi
   D(6) = (ep*(tf**3)/(12.0E0*(D(1)-vpt**2)))*Twopi
!
! CALL THE AMATRIX SUBROUTINE TO COMPUTE THE STIFFNESS MATRIX (10X10)
!
! NOTE THE DOUBLE SUBSCRIPTING USED IN AMATRIX SUBROUTINE IS
! COMPATIBLE WITH THE CALLING PROGRAM. THE DELINT ARRAY OF INTEGRALS
! IS A (11X6) SINGLY SUBSCRIPTED ARRAY (STORED ROWWISE) IN THE CALLING
! PROGRAM AND IT IS A (6X11) DOUBLY SUBSCRIPTED ARRAY (STORED
! COLUMNWISE) IN AMATRX ROUTINE.
!
!
   CALL amatrx(Ak(1),vpt,D(1),D(2),D(3),D(4),D(5),D(6),delint(1))
!
!
! FORM THE STRESS MATRIX IN FIELD COORDINATES
!
! COMPUTE THE CONSTANTS NEEDED IN THE SCRLM SUBROUTINE
!
   D(1) = 0.0E0
   IF ( itord==0 ) D(1) = 1.0E0/rp
   D(2) = 0.0E0
   D(3) = s/2.0E0
   D(4) = s
!
! CALL THE SCRLM SUBROUTINE TO COMPUTE THE STRESS MATRIX TRANSPOSED
!
! NOTE THE DOUBLE SUBSCRIPTING USED IN THE SCRLM SUBROUTINE IS
! COMPATIBLE WITH THE CALLING PROGRAM. THE SEL ARRAY WILL RETURN WITH
! THE STRESS MATRIX TRANSPOSED (10X15, STORED ROWWISE) BUT IN THE SCRLM
! SUBROUTINE THE STRESS MATRIX IS COMPUTED AS A DOUBLY SUBSCRIPTED
! 15X10 ARRAY (STORED COLUMNWISE).
!
!
   CALL scrlm(Sel(1),D(2),ee(1),tm,0.0E0,rp,A1,R1,D(1),tf)
!
!
! FORM THE TRANSFORMATION MATRIX(10X12) FROM FIELD COORDINATES TO GRID
! POINT DEGREES OF FREEDOM
!
   DO i = 1 , 72
      gambqf(i) = 0.0E0
   ENDDO
   D(1) = s
   D(2) = s**2
   D(3) = s**3
   D(4) = s**4
   D(5) = s**5
   gambqf(3) = 1.0E0
   gambqf(16) = 1.0E0
   gambqf(30) = 0.5E0
   gambqf(39) = -10.0E0/D(3)
   gambqf(40) = -6.0E0/D(2)
   gambqf(42) = -1.5E0/D(1)
   gambqf(45) = -gambqf(39)
   gambqf(46) = -4.0E0/D(2)
   gambqf(48) = 0.5E0/D(1)
   gambqf(51) = 15.0E0/D(4)
   gambqf(52) = 8.0E0/D(3)
   gambqf(54) = 1.5E0/D(2)
   gambqf(57) = -gambqf(51)
   gambqf(58) = 7.0E0/D(3)
   gambqf(60) = -1.0E0/D(2)
   gambqf(63) = -6.0E0/D(5)
   gambqf(64) = -3.0E0/D(4)
   gambqf(66) = -0.5E0/D(3)
   gambqf(69) = -gambqf(63)
   gambqf(70) = gambqf(64)
   gambqf(72) = -gambqf(66)
   DO i = 1 , 48
      gambqm(i) = 0.0E0
   ENDDO
   gambqm(1) = 1.0E0
   gambqm(17) = 1.0E0
   gambqm(25) = -3.0E0/D(2)
   gambqm(29) = -2.0E0/D(1)
   gambqm(31) = -gambqm(25)
   gambqm(35) = -1.0E0/D(1)
   gambqm(37) = 2.0E0/D(3)
   gambqm(41) = 1.0E0/D(2)
   gambqm(43) = -gambqm(37)
   gambqm(47) = gambqm(41)
!
!
! TRANSFORM THE STIFFNESS MATRIX TO GRID POINT DEGREES OF FREEDOM
!
   CALL gmmats(gambq(1),10,12,1,Ak(1),10,10,0,D(1))
   CALL gmmats(D(1),12,10,0,gambq(1),10,12,0,Ak(1))
!
!
! RE-ARRANGE THE TRANSFORMATION MATRIX (GAMBQ) SUCH THAT THE MEMBRANE
! AND FLEXURE TERMS ARE REVERSED
!
   DO i = 1 , 72
      D(i) = gambqf(i)
   ENDDO
   DO i = 1 , 48
      gambq(i) = gambqm(i)
   ENDDO
   DO i = 1 , 72
      gambq(i+48) = D(i)
   ENDDO
!
!
! TRANSFORM THE STRESS MATRIX TO GRID POINT DEGREES OF FREEDOM
!
   CALL gmmats(Sel(1),10,15,1,gambq(1),10,12,0,D(1))
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
! TRANSFORM THE STRESS MATRIX FROM ELEMENT TO BASIC COORDINATES
!
   CALL gmmats(D(1),15,12,0,gamrs(1),12,12,0,Sel(1))
!
!
! TRANSFORM THE STIFFNESS MATRIX FROM ELEMENT TO BASIC COORDINATES
!
   CALL gmmats(gamrs(1),12,12,1,Ak(1),12,12,0,D(1))
   CALL gmmats(D(1),12,12,0,gamrs(1),12,12,0,Ak(1))
!
!
! LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL COORDINATES
! FOR THE TWO GRID POINTS AND EXPAND TO (6X6)
!
   DO i = 1 , 144
      gambl(i) = 0.0E0
   ENDDO
   DO i = 1 , 2
      CALL transs(ics(i),D(1))
      k = 78*(i-1)
      DO j = 1 , 3
         kk = k + 12*(j-1) + 1
         kl = 3*(j-1) + 1
         kj = k + 12*(j+2) + j + 3
         gambl(kk) = D(kl)
         gambl(kk+1) = D(kl+1)
         gambl(kk+2) = D(kl+2)
         gambl(kj) = 1.0E0
      ENDDO
   ENDDO
!
!
!
! TRANSFORM THE STIFFNESS MATRIX FROM BASIC TO LOCAL COORDINATES
!
   CALL gmmats(gambl(1),12,12,1,Ak(1),12,12,0,D(1))
   CALL gmmats(D(1),12,12,0,gambl(1),12,12,0,Ak(1))
!
!
! TRANSFORM THE STRESS MATRIX FROM BASIC TO LOCAL COORDINATES
!
   CALL gmmats(Sel(1),15,12,0,gambl(1),12,12,0,D(1))
!
   DO i = 1 , 180
      Sel(i) = D(i)
   ENDDO
!
!
! FORM THE THERMAL STRESS VECTOR (30X1)
!
! THE MEMBRANE TEMPERATURE TERMS WILL BE STORED IN TS(1),...,TS(15) AND
! THE FLEXURE GRADIENT TEMP. TERMS WILL BE STORED IN TS(16),...,TS(30)
!
!
! COMPUTE CONSTANTS NEEDED IN THE THERMAL STRESS CALCULATIONS
!
   D(1) = 0.0E0
   D(2) = s/2.0E0
   D(3) = s
   D(4) = ee(1)*Alf(1) + ee(2)*Alf(2)
   D(5) = ee(3)*Alf(1) + ee(4)*Alf(2)
   D(6) = (ee(1)-ee(2))*Alf(1) + (ee(3)-ee(4))*Alf(2)
   D(7) = tf**3/12.0E0
   D(8) = tm/s
   D(9) = D(7)/s
!
! START THE LOOP TO FORM THE THERMAL STRESS VECTORS AT EACH OF THE
! THREE STRESS POINTS
!
   DO i = 1 , 3
      CALL solve1(A1,R1,rp,D(i),D(12),D(13),D(14),0.0E0)
      k = 5*(i-1)
      kk = k + 15
      Ts(k+1) = tm*D(4)
      Ts(k+2) = tm*D(5)
      Ts(k+3) = D(7)*D(4)
      Ts(k+4) = -D(7)*D(5)
      Ts(k+5) = D(7)*D(12)*D(6)
      Ts(kk+1) = D(8)*D(i)*D(4)
      Ts(kk+2) = D(8)*D(i)*D(5)
      Ts(kk+3) = D(9)*D(i)*D(4)
      Ts(kk+4) = -D(9)*D(i)*D(5)
      Ts(kk+5) = D(9)*(D(4)+D(i)*D(12)*D(6))
   ENDDO
!
!
END SUBROUTINE stord1
