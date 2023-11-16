
SUBROUTINE stpax1
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Acurl(208) , Acurp1(48) , Acurp2(16) , Ak(144) , Akph2(16) , Akuph(48) , Alf(3) , Anu(3) , Consts(5) , Costh , D(144) ,     &
      & Degrad , Dum5(61) , Dum75(75) , E(3) , E1(36) , Ecpt(39) , Eltemp , G(3) , Gsube , Phi(14) , Pi , Pzout(51) , R(5) , R1 ,   &
      & R2 , R3 , R4 , Rho , Sel(360) , Selp1(120) , Selp2(180) , Selp3(60) , Setmat , Sinth , Stress , Ts(06) , Twopi , Tz ,       &
      & Tzero , Wj(6,12) , Z(5) , Z1 , Z2 , Z3 , Z4
   INTEGER Ibuf , Idel , Iecpt(40) , Igp(4) , Iout , Ksys78 , Matflg , Matidc , Moskp(9)
   CHARACTER*23 Ufm
   COMMON /condas/ Consts
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero , Gsube , Moskp , Setmat
   COMMON /matpz / Pzout
   COMMON /sdr2x5/ Ecpt , Dum5 , Idel , Igp , Tz , Sel , Ts , Ak , Phi , Akph2 , Akuph , Selp1 , Selp2 , Selp3
   COMMON /sdr2x6/ D , E1 , Wj , R , Z
   COMMON /system/ Ibuf , Iout , Dum75 , Ksys78
   COMMON /xmssg / Ufm
!
! Local variable declarations
!
   REAL ajho , ajjho , alfb(6) , c2 , c2s2 , c3 , c4 , cosg , cs , cs2 , d1(48) , d2(16) , del , delint(12) , dgama , dgamr , ee(63)&
      & , er , et , ez , gababq(12,12) , gbp(4,4) , gor , grz , gzo , rmax , rmin , rsum , s2 , s3 , s4 , sc2 , sing , tempe ,      &
      & teo(45) , vor , voz , vro , vrz , vzo , vzr , wjp(3,4) , zdr , zmin , zsum
   INTEGER i , i1 , ib , ic , ics(4) , idel1 , iki , ip , iq , ising , j , k , kp1 , kp2 , kp3 , ksave , matid , sp(50)
   LOGICAL lsys78 , pzmat
   REAL rzints
!
! End of declarations
!
!
!     THIS ROUTINE IS PHASE I OF STRESS DATA RECOVERY FOR THE AXI-
!     SYMMETRIC WITH A TRAPEZOIDAL CROSS SECTION
!
!
!     ECPT (01) = ELEMENT ID                                I
!     ECPT (02) = SIL A                                     I
!     ECPT (03) = SIL B                                     I
!     ECPT (04) = SIL C                                     I
!     ECPT (05) = SIL D
!     ECPT (06) = MATERIAL ORIENTATION ANGLE(DEGREES)       R
!     ECPT (08) = MATERIAL ID                               I
!     ECPT (09) TO ECPT (22) FOR PHI
!     ECPT (23) = COOR. SYS. FOR GRID POINT A               I
!     ECPT (24) = X-COOR. OF GRID POINT A (IN BASIC COOR)   R
!     ECPT (25) = Z-COOR. OF GRID POINT A (IN BASIC COOR)   R
!     ECPT (26) = 0.0
!     ECPT (27) = COOR. SYS. FOR GRID POINT B
!     ECPT (28) = X-COOR. OF GRID POINT B (IN BASIC COOR)   R
!     ECPT (29) = Z-COOR. OF GRID POINT B (IN BASIC COOR)   R
!     ECPT (30) = 0.0
!     ECPT (31) = COOR. SYS. FOR GRID POINT C               I
!     ECPT (32) = X-COOR. FOR GRID POINT C                  R
!     ECPT (33) = Z-COOR. FOR GRID POINT C                  R
!     ECPT (34) = 0.0
!     ECPT (35) = COOR. SYS. FOR GRID POINT D               I
!     ECPT (36) = X-COOR FOR GRID POINT D                   R
!     ECPT (37) = Z-COOR FOR GRID POINT D                   R
!     ECPT (38) = 0.0
!     ECPT (39) = EL. TEMPERATURE FOR MATERIAL PROP         R
!
!     ANY GROUP OF STATEMENTS PREFACED BY AN IF STATEMENT CONTAINING
!     ...KSYS78 OR LSYS78 ...  INDICATES CODING NECESSARY FOR THIS
!     ELEMENT*S PIEZOELECTRIC CAPABILITY
!
!     KSYS78 = 0   ELASTIC, NON-PIEZOELECTRIC MATERIAL
!     KSYS78 = 1   ELECTRICAL-ELASTIC COUPLED, PIEZOELETRIC MATERIAL
!     KSYS78 = 2   ELASTIC ONLY, PIEZOELECTRIC MATERIAL
!     LSYS78 = .TRUE. IF KSYS78 = 0, OR 2
!
!
!     ECPT COMMON BLOCK
!
!
!     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
!     COMMON /MATPZ / CE11,CE12,CE13,CE14,CE15,CE16,CE22,CE23,CE24,CE25,
!                     CE26,CE33,CE34,CE35,CE36,CE44,CE45,CE46,CE55,CE56,
!                     CE66,E11,E12,E13,E14,E15,E16,E21,E22,E23,E24,E25,
!                     E26,E31,E32,E33,E34,E35,E36,EPS11,EPS12,EPS13,
!                     EPS22,EPS23,EPS33,RHO,A1,A2,A12,TREF,GE
   EQUIVALENCE (Consts(1),Pi) , (Consts(2),Twopi) , (Consts(4),Degrad) , (Acurl(1),Ak(1)) , (Iecpt(1),Ecpt(1)) , (R(1),R1) ,        &
    & (R(2),R2) , (R(3),R3) , (R(4),R4) , (Z(1),Z1) , (Z(2),Z2) , (Z(3),Z3) , (Z(4),Z4) , (Acurp1(1),Acurl(145)) ,                  &
    & (Acurp2(1),Acurl(193))
!
   lsys78 = .FALSE.
   IF ( Ksys78==0 .OR. Ksys78==2 ) lsys78 = .TRUE.
!
!     START EXECUTION
!
!     STORE ECPT PARAMETERS IN LOCAL VARIABLES
!
   Idel = Iecpt(1)
   Igp(1) = Iecpt(2)
   Igp(2) = Iecpt(3)
   Igp(3) = Iecpt(4)
   Igp(4) = Iecpt(5)
   matid = Iecpt(8)
   ics(1) = Iecpt(23)
   ics(2) = Iecpt(27)
   ics(3) = Iecpt(31)
   R(1) = Ecpt(24)
   D(1) = Ecpt(26)
   Z(1) = Ecpt(25)
   R(2) = Ecpt(28)
   Z(2) = Ecpt(29)
   D(2) = Ecpt(30)
   R(3) = Ecpt(32)
   Z(3) = Ecpt(33)
   D(3) = Ecpt(34)
   ics(4) = Iecpt(35)
   Z(4) = Ecpt(37)
   D(4) = Ecpt(38)
   R(4) = Ecpt(36)
   tempe = Ecpt(39)
   dgama = Ecpt(6)
!
!     TEST THE VALIDITY OF THE GRID POINT COORDINATES
!
   DO i = 1 , 4
      IF ( R(i)<=0.0 ) GOTO 100
      IF ( D(i)/=0.0 ) GOTO 100
   ENDDO
!
!     COMPUTE THE ELEMENT COORDINATES
!
   zmin = amin1(Z1,Z2,Z3,Z4)
   Z1 = Z1 - zmin
   Z2 = Z2 - zmin
   Z3 = Z3 - zmin
   Z4 = Z4 - zmin
   rmin = amin1(R1,R2,R3,R4)
   rmax = amax1(R1,R2,R3,R4)
   IF ( rmax/rmin<=10. ) THEN
!
!     FORM THE TRANSFORMMATION MATRIX(12X12) FROM FIELD COOR, TO GRID
!     POINT DEGREES OF FREEDOM
!
      DO i = 1 , 144
         gababq(i,1) = 0.0
      ENDDO
      gababq(1,1) = 1.0
      gababq(2,1) = R1
      gababq(3,1) = Z1
      gababq(4,1) = R1*Z1
      gababq(5,2) = 1.0
      gababq(6,2) = R1
      gababq(7,2) = Z1
      gababq(8,2) = gababq(4,1)
      gababq(9,3) = 1.0
      gababq(10,3) = R1
      gababq(11,3) = Z1
      gababq(12,3) = gababq(4,1)
      gababq(1,4) = 1.0
      gababq(2,4) = R2
      gababq(3,4) = Z2
      gababq(4,4) = R2*Z2
      gababq(5,5) = 1.0
      gababq(6,5) = R2
      gababq(7,5) = Z2
      gababq(8,5) = gababq(4,4)
      gababq(9,6) = 1.0
      gababq(10,6) = R2
      gababq(11,6) = Z2
      gababq(12,6) = gababq(4,4)
      gababq(1,7) = 1.0
      gababq(2,7) = R3
      gababq(3,7) = Z3
      gababq(4,7) = R3*Z3
      gababq(5,8) = 1.0
      gababq(6,8) = R3
      gababq(7,8) = Z3
      gababq(8,8) = gababq(4,7)
      gababq(9,9) = 1.0
      gababq(10,9) = R3
      gababq(11,9) = Z3
      gababq(12,9) = gababq(4,7)
      gababq(1,10) = 1.0
      gababq(2,10) = R4
      gababq(3,10) = Z4
      gababq(4,10) = R4*Z4
      gababq(5,11) = 1.0
      gababq(6,11) = R4
      gababq(7,11) = Z4
      gababq(8,11) = gababq(4,10)
      gababq(9,12) = 1.0
      gababq(10,12) = R4
      gababq(11,12) = Z4
      gababq(12,12) = gababq(4,10)
!
      IF ( .NOT.(lsys78) ) THEN
         gbp(1,1) = 1.0
         gbp(2,1) = R(1)
         gbp(3,1) = Z(1)
         gbp(4,1) = R(1)*Z(1)
         gbp(1,2) = 1.0
         gbp(2,2) = R(2)
         gbp(3,2) = Z(2)
         gbp(4,2) = R(2)*Z(2)
         gbp(1,3) = 1.0
         gbp(2,3) = R(3)
         gbp(3,3) = Z(3)
         gbp(4,3) = R(3)*Z(3)
         gbp(1,4) = 1.0
         gbp(2,4) = R(4)
         gbp(3,4) = Z(4)
         gbp(4,4) = R(4)*Z(4)
      ENDIF
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
      ising = -1
      CALL invers(12,gababq,12,D(10),0,D(11),ising,sp)
      IF ( ising==2 ) THEN
         i = 26
         CALL mesage(-30,i,Idel)
         IF ( .NOT.pzmat ) Ksys78 = ksave
      ELSE
!
         IF ( Ksys78==1 ) CALL invers(4,gbp,4,D(10),0,D(11),ising,sp)
         IF ( ising==2 ) THEN
            i = 26
            CALL mesage(-30,i,Idel)
            IF ( .NOT.pzmat ) Ksys78 = ksave
         ELSE
!
!     MODIFY THE TRANSFORMATION MATRIX IF ELEMENT IS A CORE ELEMENT
!
!     CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT
!
!       DELINT(1) = (-1,0)
!       DELINT(02)= (-1,1)
!       DELINT(03)= (-1,2)
!       DELINT(04)= ( 0,0)
!       DELINT(05)= ( 0,1)
!       DELINT(06)= ( 0,2)
!       DELINT(07)= ( 1,0)
!       DELINT(08)= ( 1,1)
!       DELINT(09)= ( 1,2)
!       DELINT(10)= ( 2,0)
!       DELINT(11)= ( 2,1)
!       DELINT(12)= ( 3,0)
!
            i1 = 0
            DO i = 1 , 4
               ip = i - 2
               DO j = 1 , 3
                  iq = j - 1
                  i1 = i1 + 1
                  IF ( i1==12 ) THEN
                     ip = 3
                     iq = 0
                  ENDIF
                  delint(i1) = rzints(ip,iq,R,Z,4)
               ENDDO
            ENDDO
!
!     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3
!
            Matidc = matid
            Matflg = 7
            IF ( Ksys78>0 ) Matflg = 9
            Eltemp = tempe
            dgamr = dgama*Degrad
            Sinth = sin(dgamr)
            Costh = cos(dgamr)
            cosg = Costh
            sing = Sinth
            CALL mat(Idel)
            pzmat = .FALSE.
            IF ( Setmat==4. .OR. Setmat==5. ) pzmat = .TRUE.
            IF ( pzmat ) THEN
               Rho = Pzout(46)
               Alf(1) = Pzout(47)
               Alf(2) = Pzout(48)
               Alf(3) = Pzout(49)
               Tzero = Pzout(50)
               Gsube = Pzout(51)
            ELSE
               ksave = Ksys78
               Ksys78 = 0
               lsys78 = .TRUE.
            ENDIF
            IF ( Setmat==2.0 ) THEN
               i = 126
               CALL mesage(-30,i,Idel)
               IF ( .NOT.pzmat ) Ksys78 = ksave
            ELSE
               Tz = Tzero
               IF ( Ksys78<=0 ) THEN
!
!     SET MATERIAL PROPERTIES IN DOUBLE PRECISION VARIABLES
!
                  er = E(1)
                  et = E(2)
                  ez = E(3)
                  vro = Anu(1)
                  voz = Anu(2)
                  vzr = Anu(3)
                  gor = G(1)
                  gzo = G(2)
                  grz = G(3)
                  vor = vro*et/er
                  vzo = voz*ez/et
                  vrz = vzr*er/ez
                  del = 1.0/(1.0-vro*vor-voz*vzo-vzr*vrz-vro*voz*vzr-vrz*vor*vzo)
               ENDIF
!
!     COMPUTE ELASTIC CONSTANTS MATRIX FROM MATERIAL TO ELEMENT AXIS
!
               DO i = 1 , 45
                  teo(i) = 0.0
               ENDDO
!
               IF ( Ksys78>0 ) THEN
!
!     PIEZOELECTRIC MATERIAL PROPERTIES STORED IN TEO(22-39)
!     DIELECTRIC MATERIAL PROPERTIES STORED IN TEO(40-45)
!     TEO(22-39) CONTAINS E-TRANSPOSE
!
                  teo(1) = Pzout(1)
                  teo(2) = Pzout(2)
                  teo(3) = Pzout(7)
                  teo(4) = Pzout(3)
                  teo(5) = Pzout(8)
                  teo(6) = Pzout(12)
                  teo(7) = Pzout(4)
                  teo(8) = Pzout(9)
                  teo(9) = Pzout(13)
                  teo(10) = Pzout(16)
                  teo(11) = Pzout(5)
                  teo(12) = Pzout(10)
                  teo(13) = Pzout(14)
                  teo(14) = Pzout(17)
                  teo(15) = Pzout(19)
                  teo(16) = Pzout(6)
                  teo(17) = Pzout(11)
                  teo(18) = Pzout(15)
                  teo(19) = Pzout(18)
                  teo(20) = Pzout(20)
                  teo(21) = Pzout(21)
!
                  IF ( Ksys78/=2 ) THEN
                     teo(22) = Pzout(22)
                     teo(23) = Pzout(28)
                     teo(24) = Pzout(34)
                     teo(25) = Pzout(23)
                     teo(26) = Pzout(29)
                     teo(27) = Pzout(35)
                     teo(28) = Pzout(24)
                     teo(29) = Pzout(30)
                     teo(30) = Pzout(36)
                     teo(31) = Pzout(25)
                     teo(32) = Pzout(31)
                     teo(33) = Pzout(37)
                     teo(34) = Pzout(26)
                     teo(35) = Pzout(32)
                     teo(36) = Pzout(38)
                     teo(37) = Pzout(27)
                     teo(38) = Pzout(33)
                     teo(39) = Pzout(39)
                     teo(40) = -Pzout(40)
                     teo(41) = -Pzout(41)
                     teo(42) = -Pzout(42)
                     teo(43) = -Pzout(43)
                     teo(44) = -Pzout(44)
                     teo(45) = -Pzout(45)
                  ENDIF
               ELSE
                  teo(1) = er*(1.0-voz*vzo)*del
                  teo(2) = er*(vzr+vzo*vor)*del
                  teo(3) = ez*(1.0-vro*vor)*del
                  teo(4) = er*(vor+vzr*voz)*del
                  teo(5) = et*(vzo+vro*vzr)*del
                  teo(6) = et*(1.0-vrz*vzr)*del
                  teo(10) = grz
                  teo(15) = gor
                  teo(21) = gzo
               ENDIF
!
               c2 = cosg*cosg
               c4 = c2*c2
               s2 = sing*sing
               s4 = s2*s2
               c2s2 = c2*s2
               c3 = cosg*c2
               s3 = sing*s2
               cs2 = cosg*s2
               sc2 = sing*c2
               cs = cosg*sing
!
               ee(1) = teo(1)*c4 + teo(3)*s4 + 2.0*c2s2*(teo(2)+2.0*teo(10))
               ee(2) = teo(2)*(c4+s4) + c2s2*(teo(1)+teo(3)-4.0*teo(10))
               ee(3) = teo(1)*s4 + 2.0*c2s2*(teo(2)+2.0*teo(10)) + teo(3)*c4
               ee(4) = teo(4)*c2 + teo(5)*s2
               ee(5) = teo(4)*s2 + teo(5)*c2
               ee(6) = teo(6)
               ee(7) = cosg*sing*s2*(teo(2)-teo(3)+2.0*teo(10)) + sing*cosg*c2*(teo(1)-teo(2)-2.0*teo(10))
               ee(8) = sing*cosg*c2*(teo(2)-teo(3)+2.0*teo(10)) + cosg*sing*s2*(teo(1)-teo(2)-2.0*teo(10))
               ee(9) = sing*cosg*(teo(4)-teo(5))
               ee(10) = c2s2*(teo(1)-2.0*teo(2)+teo(3)) + teo(10)*(c2-s2)**2
               ee(12) = 0.0
               ee(13) = 0.0
               ee(15) = teo(15)*c2 + teo(21)*s2
               ee(20) = cosg*sing*(teo(15)-teo(21))
               ee(21) = teo(15)*s2 + teo(21)*c2
!
               IF ( .NOT.(lsys78) ) THEN
!
!     PIEZOELECTRIC MATERIAL PROPERTIES IN ELEMENT COORDINATES
!
                  ee(37) = c3*teo(22) - s3*teo(26) + cs2*(teo(25)+2.0*teo(32)) - sc2*(teo(23)+2.0*teo(31))
                  ee(38) = c3*teo(23) + s3*teo(25) + cs2*(teo(26)-2.0*teo(31)) + sc2*(teo(22)-2.0*teo(32))
                  ee(39) = s2*teo(27) + c2*teo(24) - 2.0*cs*teo(33)
                  ee(40) = c3*teo(25) - s3*teo(23) + cs2*(teo(22)-2.0*teo(32)) - sc2*(teo(26)-2.0*teo(31))
                  ee(41) = c3*teo(26) + s3*teo(22) + cs2*(teo(23)+2.0*teo(31)) + sc2*(teo(25)+2.0*teo(32))
                  ee(42) = s2*teo(24) + c2*teo(27) + 2.0*cs*teo(33)
                  ee(43) = cosg*teo(28) - sing*teo(29)
                  ee(44) = cosg*teo(29) + sing*teo(28)
                  ee(45) = teo(30)
                  ee(46) = c3*teo(31) + s3*teo(32) - cs2*(teo(23)-teo(26)+teo(31)) + sc2*(-teo(32)-teo(25)+teo(22))
                  ee(47) = c3*teo(32) - s3*teo(31) - cs2*(teo(25)-teo(22)+teo(32)) + sc2*(teo(23)+teo(31)-teo(26))
                  ee(48) = (c2-s2)*teo(33) + cs*(teo(24)-teo(27))
                  ee(49) = c2*teo(34) + s2*teo(38) - cs*(teo(35)+teo(37))
                  ee(50) = c2*teo(35) - s2*teo(37) + cs*(teo(34)-teo(38))
                  ee(51) = cosg*teo(36) - sing*teo(39)
                  ee(52) = c2*teo(37) - s2*teo(35) - cs*(teo(38)-teo(34))
                  ee(53) = c2*teo(38) + s2*teo(34) + cs*(teo(35)+teo(37))
                  ee(54) = cosg*teo(39) + sing*teo(36)
!
!     DIELECTRIC MATERIAL PROPERTIES IN ELEMENT COORDINTES
!
                  ee(55) = s2*teo(43) - 2.0*cs*teo(41) + c2*teo(40)
                  ee(56) = (c2-s2)*teo(41) - cs*(teo(43)-teo(40))
                  ee(57) = -sing*teo(44) + cosg*teo(42)
                  ee(59) = c2*teo(43) + 2.0*cs*teo(41) + s2*teo(40)
                  ee(60) = cosg*teo(44) + sing*teo(42)
                  ee(63) = teo(45)
               ENDIF
!
!     COMPUTE HARMONIC COEFFICIENT
!
               Iecpt(1) = Iecpt(1) - (Iecpt(1)/1000)*1000 - 1
               ajho = Iecpt(1)
               ajjho = ajho*ajho
!
!     FORM THE ELEMENT STIFFNESS MATRIX IN FIELD SYSTEM
!
               Acurl(1) = (ee(6)+ajjho*ee(15))*delint(1)
               Acurl(2) = (ee(4)+ee(6)+ajjho*ee(15))*delint(4)
               Acurl(3) = (ee(6)+ajjho*ee(15))*delint(2) + ee(9)*delint(4)
               Acurl(4) = (ee(4)+ee(6)+ajjho*ee(15))*delint(5) + ee(9)*delint(7)
               Acurl(5) = ajho*(ee(6)+ee(15))*delint(1)
               Acurl(6) = ajho*ee(6)*delint(4)
               Acurl(7) = ajho*(ee(6)+ee(15))*delint(2) - ajho*ee(20)*delint(4)
               Acurl(8) = ajho*ee(6)*delint(5) - ajho*ee(20)*delint(7)
               Acurl(9) = ajjho*ee(20)*delint(1)
               Acurl(10) = delint(4)*(ee(9)+ajjho*ee(20))
               Acurl(11) = delint(4)*ee(5) + ajjho*delint(2)*ee(20)
               Acurl(12) = delint(7)*ee(5) + delint(5)*(ee(9)+ajjho*ee(20))
               Acurl(14) = (ee(1)+2.0*ee(4)+ee(6)+ajjho*ee(15))*delint(7)
               Acurl(15) = (ee(4)+ee(6)+ajjho*ee(15))*delint(5) + (ee(7)+ee(9))*delint(7)
               Acurl(16) = (ee(1)+2.0*ee(4)+ajjho*ee(15)+ee(6))*delint(8) + (ee(7)+ee(9))*delint(10)
               Acurl(17) = ajho*(ee(4)+ee(6)+ee(15))*delint(4)
               Acurl(18) = ajho*(ee(4)+ee(6))*delint(7)
               Acurl(19) = ajho*(ee(4)+ee(6)+ee(15))*delint(5) - ajho*ee(20)*delint(7)
               Acurl(20) = ajho*(ee(4)+ee(6))*delint(8) - ajho*ee(20)*delint(10)
               Acurl(21) = ajjho*ee(20)*delint(4)
               Acurl(22) = delint(7)*(ee(7)+ee(9)+ajjho*ee(20))
               Acurl(23) = delint(7)*(ee(2)+ee(5)) + ajjho*delint(5)*ee(20)
               Acurl(24) = delint(10)*(ee(2)+ee(5)) + delint(8)*(ee(7)+ee(9)) + delint(8)*ajjho*ee(20)
               Acurl(27) = (ee(6)+ajjho*ee(15))*delint(3) + 2.0*ee(9)*delint(5) + ee(10)*delint(7)
               Acurl(28) = (ee(4)+ee(6)+ajjho*ee(15))*delint(6) + ee(10)*delint(10) + (ee(7)+2.0*ee(9))*delint(8)
               Acurl(29) = ajho*(ee(6)+ee(15))*delint(2) + ajho*ee(9)*delint(4)
               Acurl(30) = ajho*ee(6)*delint(5) + ajho*ee(9)*delint(7)
               Acurl(31) = ajho*(ee(6)+ee(15))*delint(3) + ajho*(ee(9)-ee(20))*delint(5)
               Acurl(32) = ajho*(ee(9)-ee(20))*delint(8) + ajho*ee(6)*delint(6)
               Acurl(33) = ajjho*ee(20)*delint(2)
               Acurl(34) = delint(7)*ee(10) + delint(5)*(ee(9)+ajjho*ee(20))
               Acurl(35) = delint(7)*ee(8) + delint(5)*ee(5) + ajjho*delint(3)*ee(20)
               Acurl(36) = delint(10)*ee(8) + delint(8)*(ee(5)+ee(10)) + delint(6)*(ee(9)+ajjho*ee(20))
               Acurl(40) = (ee(1)+2.0*ee(4)+ee(6)+ajjho*ee(15))*delint(9) + (2.0*ee(7)+2.0*ee(9))*delint(11) + ee(10)*delint(12)
               Acurl(41) = ajho*(ee(4)+ee(6)+ee(15))*delint(5) + ajho*ee(9)*delint(7)
               Acurl(42) = ajho*(ee(4)+ee(6))*delint(8) + ajho*ee(9)*delint(10)
               Acurl(43) = ajho*(ee(4)+ee(6)+ee(15))*delint(6) + ajho*(ee(9)-ee(20))*delint(8)
               Acurl(44) = ajho*(ee(4)+ee(6))*delint(9) + ajho*(ee(9)-ee(20))*delint(11)
               Acurl(45) = ajjho*ee(20)*delint(5)
               Acurl(46) = delint(8)*(ee(7)+ee(9)+ajjho*ee(20)) + delint(10)*ee(10)
               Acurl(47) = delint(8)*(ee(2)+ee(5)) + delint(10)*ee(8) + ajjho*delint(6)*ee(20)
               Acurl(48) = delint(11)*(ee(2)+ee(5)+ee(10)) + delint(12)*ee(8) + delint(9)*(ee(7)+ee(9)+ajjho*ee(20))
               Acurl(53) = (ee(15)+ajjho*ee(6))*delint(1)
               Acurl(54) = ajjho*ee(6)*delint(4)
               Acurl(55) = (ee(15)+ajjho*ee(6))*delint(2) - ee(20)*delint(4)
               Acurl(56) = ajjho*ee(6)*delint(5) - ee(20)*delint(7)
               Acurl(57) = ajho*ee(20)*delint(1)
               Acurl(58) = ajho*delint(4)*(ee(9)+ee(20))
               Acurl(59) = ajho*(delint(4)*ee(5)+delint(2)*ee(20))
               Acurl(60) = ajho*(delint(7)*ee(5)+delint(5)*(ee(9)+ee(20)))
               Acurl(66) = ajjho*ee(6)*delint(7)
               Acurl(67) = ajjho*ee(6)*delint(5)
               Acurl(68) = ajjho*ee(6)*delint(8)
               Acurl(69) = 0.0
               Acurl(70) = ajho*delint(7)*ee(9)
               Acurl(71) = ajho*delint(7)*ee(5)
               Acurl(72) = ajho*(delint(10)*ee(5)+delint(8)*ee(9))
               Acurl(79) = (ee(15)+ajjho*ee(6))*delint(3) - 2.0*ee(20)*delint(5) + ee(21)*delint(7)
               Acurl(80) = ajjho*ee(6)*delint(6) - ee(20)*delint(8) + ee(21)*delint(10)
               Acurl(81) = ajho*(ee(20)*delint(2)-ee(21)*delint(4))
               Acurl(82) = ajho*(delint(5)*(ee(9)+ee(20))-delint(7)*ee(21))
               Acurl(83) = ajho*(delint(5)*(ee(5)-ee(21))+delint(3)*ee(20))
               Acurl(84) = ajho*(delint(8)*(ee(5)-ee(21))+delint(6)*(ee(9)+ee(20)))
               Acurl(92) = ee(21)*delint(12) + ajjho*ee(6)*delint(9)
               Acurl(93) = -ajho*ee(21)*delint(7)
               Acurl(94) = ajho*(delint(8)*ee(9)-delint(10)*ee(21))
               Acurl(95) = ajho*delint(8)*(ee(5)-ee(21))
               Acurl(96) = ajho*(delint(11)*(ee(5)-ee(21))+delint(9)*ee(9))
               Acurl(105) = ajjho*ee(21)*delint(1)
               Acurl(106) = ajjho*delint(4)*ee(21)
               Acurl(107) = ajjho*delint(2)*ee(21)
               Acurl(108) = ajjho*delint(5)*ee(21)
               Acurl(118) = delint(7)*(ee(10)+ajjho*ee(21))
               Acurl(119) = delint(7)*ee(8) + ajjho*delint(5)*ee(21)
               Acurl(120) = delint(10)*ee(8) + delint(8)*(ee(10)+ajjho*ee(21))
               Acurl(131) = delint(7)*ee(3) + ajjho*delint(3)*ee(21)
               Acurl(132) = delint(10)*ee(3) + delint(8)*ee(8) + ajjho*delint(6)*ee(21)
               Acurl(144) = delint(12)*ee(3) + 2.0*delint(11)*ee(8) + delint(9)*(ee(10)+ajjho*ee(21))
!
               IF ( .NOT.(lsys78) ) THEN
                  Acurl(145) = delint(1)*ajho*(ajho*ee(51)-ee(45))
                  Acurl(146) = delint(4)*(ee(43)+ajho*(ajho*ee(51)-ee(49)-ee(45)))
                  Acurl(147) = delint(2)*ajho*(ajho*ee(51)-ee(45)) + delint(4)*(ee(44)-ajho*ee(50))
                  Acurl(148) = delint(5)*(ee(43)+ajho*(ajho*ee(51)-ee(49)-ee(45))) + delint(7)*(ee(44)-ajho*ee(50))
                  Acurl(149) = delint(4)*ajho*(ajho*ee(51)-ee(45)-ee(39))
                  Acurl(150) = delint(7)*(ee(43)+ee(37)+ajho*(ajho*ee(51)-ee(49)-ee(45)-ee(39)))
                  Acurl(151) = delint(5)*ajho*(ajho*ee(51)-ee(45)-ee(39)) + delint(7)*(ee(44)+ee(38)-ajho*ee(50))
                  Acurl(152) = delint(8)*(ee(43)+ee(37)+ajho*(ajho*ee(51)-ee(49)-ee(45)-ee(39))) + delint(10)                       &
                             & *(ee(44)+ee(38)-ajho*ee(50))
                  Acurl(153) = delint(2)*ajho*(ajho*ee(51)-ee(45)) - delint(4)*ajho*ee(48)
                  Acurl(154) = delint(5)*(ee(43)+ajho*(ajho*ee(51)-ee(49)-ee(45))) + delint(7)*(ee(46)-ajho*ee(48))
                  Acurl(155) = delint(3)*ajho*(ajho*ee(51)-ee(45)) + delint(5)*(ee(44)-ajho*(ee(50)+ee(48))) + delint(7)*ee(47)
                  Acurl(156) = delint(6)*(ee(43)+ajho*(ajho*ee(51)-ee(49)-ee(45))) + delint(8)*(ee(46)+ee(44)-ajho*(ee(50)+ee(48))) &
                             & + delint(10)*ee(47)
                  Acurl(157) = delint(5)*ajho*(ajho*ee(51)-ee(45)-ee(39)) - delint(7)*ajho*ee(48)
                  Acurl(158) = delint(8)*(ee(43)+ee(47)+ajho*(ajho*ee(51)-ee(49)-ee(45)-ee(39))) - delint(10)*(ee(46)-ajho*ee(48))
                  Acurl(159) = delint(6)*ajho*(ajho*ee(51)-ee(45)-ee(39)) + delint(8)*(ee(44)+ee(38)-ajho*(ee(50)+ee(48)))          &
                             & + delint(10)*ee(47)
                  Acurl(160) = delint(9)*(ee(43)+ee(37)+ajho*(ajho*ee(51)-ee(49)-ee(45)-ee(39))) + delint(11)                       &
                             & *(ee(46)+ee(44)+ee(38)-ajho*(ee(50)+ee(48))) + delint(12)*ee(47)
                  Acurl(161) = delint(1)*ajho*(ee(51)-ajho*ee(45))
                  Acurl(162) = delint(4)*(-ee(49)+ajho*(ee(51)+ee(43)-ajho*ee(45)))
                  Acurl(163) = delint(2)*ajho*(ee(51)-ajho*ee(45)) + delint(4)*(ajho*ee(44)-ee(50))
                  Acurl(164) = delint(5)*(-ee(49)+ajho*(ee(51)+ee(43)-ajho*ee(51))) + delint(7)*(ajho*ee(44)-ee(50))
                  Acurl(165) = -delint(4)*ajjho*ee(45)
                  Acurl(166) = delint(7)*ajho*(ee(43)-ajho*ee(45))
                  Acurl(167) = delint(7)*ajho*ee(44) - delint(5)*ajjho*ee(45)
                  Acurl(168) = delint(8)*ajho*(ee(43)-ajho*ee(45)) + delint(10)*ajho*ee(44)
                  Acurl(169) = delint(2)*ajho*(ee(51)-ajho*ee(45)) - delint(4)*ajho*ee(54)
                  Acurl(170) = delint(5)*(-ee(49)+ajho*(ee(51)+ee(43)-ajho*ee(45))) + delint(7)*(ee(52)-ajho*ee(54))
                  Acurl(171) = delint(3)*ajho*(ee(51)-ajho*ee(45)) + delint(5)*(ajho*(ee(44)-ee(54))-ee(50)) + delint(7)*ee(53)
                  Acurl(172) = delint(6)*(-ee(49)+ajho*(ee(51)+ee(43)-ajho*ee(45))) + delint(8)*(ee(52)-ee(50)+ajho*(ee(44)-ee(54)))&
                             & + delint(10)*ee(53)
                  Acurl(173) = -delint(5)*ajjho*ee(45) - delint(7)*ajho*ee(54)
                  Acurl(174) = delint(8)*ajho*(ee(43)-ajho*ee(45)) + delint(10)*(ee(54)-ajho*ee(54))
                  Acurl(175) = -delint(6)*ajjho*ee(45) + delint(8)*ajho*(ee(44)-ee(54)) + delint(10)*ee(53)
                  Acurl(176) = delint(9)*ajho*(ee(43)-ajho*ee(45)) + delint(11)*(ee(52)+ajho*(ee(44)-ee(54))) + delint(12)*ee(53)
                  Acurl(177) = delint(1)*ajjho*ee(54)
                  Acurl(178) = delint(4)*ajho*(ajho*ee(54)-ee(52))
                  Acurl(179) = delint(2)*ajjho*ee(54) - delint(4)*ajho*ee(53)
                  Acurl(180) = delint(5)*ajho*(ajho*ee(54)-ee(52)) - delint(7)*ajho*ee(53)
                  Acurl(181) = delint(4)*ajho*(ajho*ee(54)-ee(48))
                  Acurl(182) = delint(7)*(ee(46)+ajho*(ajho*ee(54)-ee(52)-ee(48)))
                  Acurl(183) = delint(5)*ajho*(ajho*ee(54)-ee(48)) + delint(7)*(ee(47)-ajho*ee(53))
                  Acurl(184) = delint(8)*(ee(46)+ajho*(ajho*ee(54)-ee(52)-ee(48))) + delint(10)*(ee(47)-ajho*ee(53))
                  Acurl(185) = delint(2)*ajjho*ee(54) - delint(4)*ajho*ee(42)
                  Acurl(186) = delint(5)*ajho*(ajho*ee(54)-ee(52)) + delint(7)*(ee(40)-ajho*ee(42))
                  Acurl(187) = delint(3)*ajjho*ee(54) - delint(5)*ajho*(ee(53)+ee(42)) + delint(7)*ee(41)
                  Acurl(188) = delint(6)*ajho*(ajho*ee(54)-ee(52)) + delint(8)*(ee(40)-ajho*(ee(53)+ee(42))) + delint(10)*ee(41)
                  Acurl(189) = -delint(5)*ajho*ee(48) + delint(4)*ajjho*ee(54) - delint(7)*ajho*ee(42)
                  Acurl(190) = delint(8)*(ee(46)-ajho*ee(48)) + delint(7)*ajho*(ajho*ee(54)-ee(52)) + delint(10)                    &
                             & *(ee(40)-ajho*ee(42))
                  Acurl(191) = -delint(6)*ajho*ee(48) + delint(5)*ajjho*ee(54) + delint(8)*(ee(47)-ajho*ee(42)) - delint(7)         &
                             & *ajho*ee(53) + delint(10)*ee(41)
                  Acurl(192) = delint(9)*(ee(46)-ajho*ee(48)) + delint(8)*ajho*(ajho*ee(54)-ee(52)) + delint(11)                    &
                             & *(ee(47)+ee(40)-ajho*ee(42)) - delint(10)*ajho*ee(53) + delint(12)*ee(41)
!
                  Acurl(193) = delint(1)*ajjho*ee(63)
                  Acurl(194) = delint(4)*ajho*(ajho*ee(63)-ee(57))
                  Acurl(195) = delint(2)*ajjho*ee(63) - delint(4)*ajho*ee(60)
                  Acurl(196) = delint(5)*ajho*(ajho*ee(63)-ee(57)) - delint(7)*ajho*ee(60)
                  Acurl(197) = delint(4)*ajho*(ajho*ee(63)-ee(57))
                  Acurl(198) = delint(7)*(ajjho*ee(63)-2.0*ajho*ee(57)+ee(55))
                  Acurl(199) = delint(5)*ajho*(ajho*ee(63)-ee(57)) + delint(7)*(ee(56)-ajho*ee(60))
                  Acurl(200) = delint(8)*(ajjho*ee(63)-2.0*ajho*ee(57)+ee(55)) + delint(10)*(ee(56)-ajho*ee(60))
                  Acurl(201) = delint(2)*ajjho*ee(63) - delint(4)*ajho*ee(60)
                  Acurl(202) = delint(5)*ajho*(ajho*ee(63)-ee(57)) + delint(7)*(ee(56)-ajho*ee(60))
                  Acurl(203) = delint(3)*ajjho*ee(63) - delint(5)*2.0*ajho*ee(60) + delint(7)*ee(59)
                  Acurl(204) = delint(6)*ajho*(ajho*ee(63)-ee(57)) + delint(8)*(ee(56)-2.0*ajho*ee(60)) + delint(10)*ee(59)
                  Acurl(205) = delint(5)*ajho*(ajho*ee(63)-ee(57)) - delint(7)*ajho*ee(60)
                  Acurl(206) = delint(8)*(ajjho*ee(63)-2.0*ee(57)+ee(55)) + delint(10)*(ee(56)-ajho*ee(60))
                  Acurl(207) = delint(6)*ajho*(ajho*ee(63)-ee(57)) + delint(8)*(ee(56)-2.0*ajho*ee(60)) + delint(10)*ee(59)
                  Acurl(208) = delint(9)*(ajjho*ee(63)-2.0*ajho*ee(57)+ee(55)) + 2.0*delint(11)*(ee(56)-ajho*ee(60)) + delint(12)   &
                             & *ee(59)
               ENDIF
!
!     TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD SYSTEM
!     TO GRID POINT DEGREES OF FREEDOM
!
!     EXPAND ACURL INTO (12X12)
!
               DO ib = 2 , 12
                  ic = 13*ib - 25
                  i = ic
                  DO j = ib , 12
                     ic = ic + 12
                     i = i + 1
                     Acurl(ic) = Acurl(i)
                  ENDDO
               ENDDO
!
               dgama = Pi
               IF ( ajho==0.0 ) dgama = Twopi
               DO i = 1 , 144
                  Acurl(i) = Acurl(i)*dgama
               ENDDO
!
               IF ( .NOT.(lsys78) ) THEN
                  DO i = 145 , 208
                     Acurl(i) = Acurl(i)*dgama
                  ENDDO
               ENDIF
!
               CALL gmmats(gababq,12,12,1,Acurl,12,12,0,D)
               CALL gmmats(D,12,12,0,gababq,12,12,0,Ak)
!
               IF ( .NOT.(lsys78) ) THEN
                  CALL gmmats(gababq,12,12,1,Acurp1,12,4,0,d1)
                  CALL gmmats(d1,12,4,0,gbp,4,4,0,Akuph)
                  CALL gmmats(gbp,4,4,1,Acurp2,4,4,0,d2)
                  CALL gmmats(d2,4,4,0,gbp,4,4,0,Akph2)
               ENDIF
!
!     ********** COORDINATE SYSTEM NOT POSSIBLE ***********************
!     *** WITH RINGAX.  THE FOLLOWING CODE WILL IMPLEMENT IT  *********
!     IF FOLLOWING CODE IS IMPLEMENTED MUST BE MODIFIED FOR PIEZO-
!     ELECTRIC
!
!     ZERO OUT THE AKI MATRIX
!
!.    DO 700 I = 1,144
!.    AKI(I) = 0.0
!.700 CONTINUE
!.    DO 800 I = 1,4
!.    CALL TRANSS (ICS(I),D(1))
!.    K = 39 * (I-1) + 1
!.    DO 800 J = 1, 3
!.    KK = K + 12*(J-1)
!.    JJ = 3*(J-1) + 1
!.    AKI (KK  ) = D (JJ  )
!.    AKI (KK+1) = D (JJ+1)
!.    AKI (KK+2) = D (JJ+2)
!.800 CONTINUE
!
!     TRANSFORM THE STIFFNESS MATRIX FROM BASIC TO LOCAL COORDINATES
!
!.    CALL GMMATS (AKI(1),12,12,1, AK(1),12,12,0, D(1))
!.    CALL GMMATS (D(1),12,12,0, AKI(1),12,12,0, AK(1))
!
!     COMPUTE THE FIFTH GRID POINT
!
               R(5) = (R1+R2+R3+R4)/4.0
               Z(5) = (Z1+Z2+Z3+Z4)/4.0
!
!     FORM WJ MATRIX
!
               DO iki = 1 , 5
                  DO i = 1 , 72
                     Wj(i,1) = 0.0
                  ENDDO
                  rsum = R(iki)
                  zsum = Z(iki)
                  zdr = zsum/rsum
                  Wj(1,2) = 1.0
                  Wj(1,4) = zsum
                  Wj(2,11) = 1.0
                  Wj(2,12) = rsum
                  Wj(3,1) = 1.0/rsum
                  Wj(3,2) = 1.0
                  Wj(3,3) = zdr
                  Wj(3,4) = zsum
                  Wj(3,5) = ajho/rsum
                  Wj(3,6) = ajho
                  Wj(3,7) = ajho*zdr
                  Wj(3,8) = ajho*zsum
                  Wj(4,3) = 1.0
                  Wj(4,4) = rsum
                  Wj(4,10) = 1.0
                  Wj(4,12) = zsum
                  Wj(5,1) = -ajho/rsum
                  Wj(5,2) = -ajho
                  Wj(5,3) = -ajho*zdr
                  Wj(5,4) = -ajho*zsum
                  Wj(5,5) = -1.0/rsum
                  Wj(5,7) = -zdr
                  Wj(6,7) = 1.0
                  Wj(6,8) = rsum
                  Wj(6,9) = -ajho/rsum
                  Wj(6,10) = -ajho
                  Wj(6,11) = -ajho*zdr
                  Wj(6,12) = -ajho*zsum
!
                  IF ( .NOT.(lsys78) ) THEN
!
!     FORM WJP MATRIX
!
                     DO i = 1 , 3
                        DO j = 1 , 4
                           wjp(i,j) = 0.0
                        ENDDO
                     ENDDO
!
                     wjp(1,2) = 1.0
                     wjp(1,4) = zsum
                     wjp(2,3) = 1.0
                     wjp(2,4) = rsum
                     wjp(3,1) = -ajho/rsum
                     wjp(3,2) = -ajho
                     wjp(3,3) = -ajho*zdr
                     wjp(3,4) = -ajho*zsum
                  ENDIF
!
!     EXPAND EE(21) INTO E1(36)
!
                  DO i = 1 , 36
                     E1(i) = 0.0
                  ENDDO
                  E1(1) = ee(1)
                  E1(2) = ee(2)
                  E1(3) = ee(4)
                  E1(4) = ee(7)
                  E1(7) = ee(2)
                  E1(8) = ee(3)
                  E1(9) = ee(5)
                  E1(10) = ee(8)
                  E1(13) = ee(4)
                  E1(14) = ee(5)
                  E1(15) = ee(6)
                  E1(16) = ee(9)
                  E1(19) = ee(7)
                  E1(20) = ee(8)
                  E1(21) = ee(9)
                  E1(22) = ee(10)
                  E1(29) = ee(15)
                  E1(36) = ee(21)
!
!     COMPUTE THE STRESS MATRICES
!
                  k = 72*(iki-1) + 1
                  CALL gmmats(Wj,12,6,1,gababq,12,12,0,D(1))
                  CALL gmmats(E1(1),6,6,0,D(1),6,12,0,Sel(k))
!
                  IF ( .NOT.(lsys78) ) THEN
                     kp1 = 24*(iki-1) + 1
                     CALL gmmats(wjp,4,3,1,gbp,4,4,0,d2(1))
                     CALL gmmats(ee(37),6,3,0,d2(1),3,4,0,Selp1(kp1))
                     kp2 = 36*(iki-1) + 1
                     CALL gmmats(ee(37),6,3,1,D(1),6,12,0,Selp2(kp2))
                     kp3 = 12*(iki-1) + 1
                     CALL gmmats(ee(55),3,3,0,d2(1),3,4,0,Selp3(kp3))
                  ENDIF
!
!     ** COORDINATE SYSTEMS NOT POSSIBLE WITH RINGAX *******************
!     ** THE FOLLOWING CODE WILL IMPLEMENT IT **************************
!     ** NOTE THAT WJ IS SEL(K) IN FOLLOWING GMMATS ********************
!
!     ** IF FOLLOWING CODE IS IMPLEMENTED MUST BE MODIFIED FOR PIEZO-
!     ELECTRIC TRANSFORM THE STRESS MATRIX FROM BASIC TO LOCAL
!     COORDINATES
!..   CALL GMMATS (WJ,6,12,0, AKI(1),12,12,0, SEL(K) )
!
!
               ENDDO
!
!     COMPUTE THE THERMAL STRAIN
!
               alfb(1) = Alf(1)
               alfb(2) = Alf(3)
               alfb(3) = Alf(2)
               alfb(4) = 0.0
               alfb(5) = 0.0
               alfb(6) = 0.0
!
!     COMPUTE THE THERMAL STRESS
!
               Ts(1) = ee(1)*alfb(1) + ee(2)*alfb(2) + ee(4)*alfb(3)
               Ts(2) = ee(2)*alfb(1) + ee(3)*alfb(2) + ee(5)*alfb(3)
               Ts(3) = ee(4)*alfb(1) + ee(5)*alfb(2) + ee(6)*alfb(3)
               Ts(4) = ee(7)*alfb(1) + ee(8)*alfb(2) + ee(9)*alfb(3)
               Ts(5) = 0.0
               Ts(6) = 0.0
!
!     SAVE ECPT(9) TO ECP(22)
!
               DO iki = 1 , 14
                  Phi(iki) = Ecpt(8+iki)
               ENDDO
               IF ( .NOT.pzmat ) Ksys78 = ksave
            ENDIF
         ENDIF
      ENDIF
      GOTO 99999
   ELSE
!
!     RATIO OF RADII IS TOO LARGE FOR GAUSS QUADRATURE FOR IP=-1
!
      idel1 = Idel/1000
      WRITE (Iout,99001) Ufm , idel1
99001 FORMAT (A23,', TRAPAX ELEMENT',I9,' HAS A MAXIMUM TO MINIMUM ','RADIUS RATIO EXCEEDING 10.',/5X,'ACCURACY OF NUMERICAL',      &
             &' INTEGRATION WOULD BE IN DOUBT.')
   ENDIF
!
!     SET FATAL ERROR FLAG AND ALLOWING ERROR MESSAGES TO ACCUMLATE
!
 100  i = 37
   CALL mesage(-30,i,Idel)
   IF ( .NOT.pzmat ) Ksys78 = ksave
99999 RETURN
END SUBROUTINE stpax1
