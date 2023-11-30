
SUBROUTINE strax1
   IMPLICIT NONE
   REAL Ak(81) , Akph2(9) , Akuph(27) , Alf(3) , Anu(3) , Consts(5) , Costh , D(81) , Degrad , Dum5(66) , E(3) , Ecpt(34) , Eltemp ,&
      & G(3) , Gababq(9,9) , Gsube , Phi(14) , Pi , Pzout(51) , Rho , Sel(54) , Selp1(18) , Selp2(27) , Selp3(9) , Setmat , Sinth , &
      & Stress , Ts(6) , Tz , Tzero , Wj(6,9)
   INTEGER Idel , Iecpt(35) , Igp(3) , Ksys78 , Ksystm(77) , Matflg , Matidc , Moskp(9)
   COMMON /condas/ Consts
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero , Gsube , Moskp , Setmat
   COMMON /matpz / Pzout
   COMMON /sdr2x5/ Ecpt , Dum5 , Idel , Igp , Tz , Sel , Ts , Ak , Phi , Selp1 , Akph2 , Akuph , Selp2 , Selp3
   COMMON /sdr2x6/ Gababq , D , Wj
   COMMON /system/ Ksystm , Ksys78
   REAL aa , acurl(117) , acurp1(27) , acurp2(9) , ajho , ajjho , area , c1 , c2 , c2s2 , c3 , c4 , cosg , cs , cs2 , d1(27) ,      &
      & d2(9) , del , delint(12) , dgama , dgamr , dr , ee(63) , er , et , ez , gababp(3,3) , gor , grz , gzo , r(3) , r1 , r2 ,    &
      & r3 , ra , rh , rsum , s2 , s3 , s4 , sc2 , sing , tempe , teo(45) , vor , voz , vro , vrz , vzo , vzr , wjp(3,3) , z(3) ,   &
      & z1 , z2 , z3 , za , zdr , zmin , zsum
   REAL ais
   INTEGER i , i1 , ib , ic , ics(3) , iki , ip , iq , j , ksave , matid , nogo
   LOGICAL lsys78 , pzmat
!
!     THIS ROUTINE IS PHASE I OF STRESS DATA RECOVERY FOR THE AXI-
!     SYMMETRIC RING WITH TRIANGULAR CROSS SECTION RING
!
!     ECPT ( 1) = ELEMENT ID
!     ECPT ( 2) = SIL A                                   I
!     ECPT ( 3) = SIL B                                   I
!     ECPT ( 4) = SIL C                                   I
!     ECPT ( 5) = MATERIAL ORIENTATION ANGLE(DEGREES)     R
!     ECPT ( 7) = MATERIAL ID                             I
!     ECPT ( 8) TO ECPT(21) = STRESS PHASE ANG.           R
!     ECPT (22) = CORD. SYS. GRID POINT A (NOT USED)      I
!     ECPT (23) = R-CORD OF GRID A                        R
!     ECPT (24) = Z-CORD OF GRID A                        R
!     ECPT (25) = 0.0                                     R
!     ECPT (26) = CORD. SYS. GRID POINT B (NOT USED)      I
!     ECPT (27) = R-CORD OF GRID B                        R
!     ECPT (28) = Z-CORD OF GRID B                        R
!     ECPT (29) = 0.0                                     R
!     ECPT (30) = CORD. SYS. GRID POINT C (NOT USED)      I
!     ECPT (31) = R-CORD OF GRID C                        R
!     ECPT (32) = Z-CORD OF GRID C                        R
!     ECPT (33) = 0.0                                     R
!     ECPT (34) = EL. TEMPERATURE FOR MATERIAL PROP       R
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
!     ECPT COMMON BLOCK
!     COMMON /MATPZ / CE11,CE12,CE13,CE14,CE15,CE16,CE22,CE23,CE24,CE25,
!                     CE26,CE33,CE34,CE35,CE36,CE44,CE45,CE46,CE55,CE56,
!                     CE66,E11,E12,E13,E14,E15,E16,E21,E22,E23,E24,E25,
!                     E26,E31,E32,E33,E34,E35,E36,EPS11,EPS12,EPS13,EPS2
!                     EPS23,EPS33,RHO,A1,A2,A12,TREF,GE
!
   EQUIVALENCE (Iecpt(1),Ecpt(1)) , (z(1),z1) , (z(2),z2) , (r(1),r1) , (r(2),r2) , (r(3),r3) , (z(3),z3) , (Consts(1),Pi) ,        &
    & (Consts(4),Degrad) , (acurp1(1),acurl(82)) , (acurp2(1),acurl(109))
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
   matid = Iecpt(7)
   ics(1) = Iecpt(22)
   r(1) = Ecpt(23)
   z(1) = Ecpt(24)
   D(1) = Ecpt(25)
   ics(2) = Iecpt(26)
   r(2) = Ecpt(27)
   z(2) = Ecpt(28)
   D(2) = Ecpt(29)
   ics(3) = Iecpt(30)
   r(3) = Ecpt(31)
   z(3) = Ecpt(32)
   D(3) = Ecpt(33)
   dgama = Ecpt(5)
   tempe = Ecpt(34)
!
!     TEST THE VALIDITY OF THE GRID POINT COORDINATES
!
   DO i = 1 , 3
      IF ( r(i)<=0.0 ) GOTO 100
      IF ( D(i)/=0.0 ) GOTO 100
   ENDDO
!
!     COMPUTE THE ELEMENT COORDINATES
!
   zmin = amin1(z1,z2,z3)
   z1 = z1 - zmin
   z2 = z2 - zmin
   z3 = z3 - zmin
!
!     FORM THE TRANSFORMATION MATRIX GABABQ (9X9) FROM FIELD COORDINATES
!     TO GRID POINT DEGREES OF FREEDOM
!
   DO i = 1 , 81
      Gababq(i,1) = 0.0
   ENDDO
   aa = r2*z3 + r1*z2 + z1*r3 - z2*r3 - r1*z3 - r2*z1
   aa = 1.0/aa
   c1 = aa*(r2*z3-z2*r3)
   c2 = -aa*(z3-z2)
   c3 = aa*(r3-r2)
   Gababq(1,1) = c1
   Gababq(1,2) = c2
   Gababq(1,3) = c3
   Gababq(2,4) = c1
   Gababq(2,5) = c2
   Gababq(2,6) = c3
   Gababq(3,7) = c1
   Gababq(3,8) = c2
   Gababq(3,9) = c3
   IF ( .NOT.(lsys78) ) THEN
      gababp(1,1) = c1
      gababp(1,2) = c2
      gababp(1,3) = c3
   ENDIF
   c1 = -aa*(r1*z3-z1*r3)
   c2 = aa*(z3-z1)
   c3 = -aa*(r3-r1)
   Gababq(4,1) = c1
   Gababq(4,2) = c2
   Gababq(4,3) = c3
   Gababq(5,4) = c1
   Gababq(5,5) = c2
   Gababq(5,6) = c3
   Gababq(6,7) = c1
   Gababq(6,8) = c2
   Gababq(6,9) = c3
   IF ( .NOT.(lsys78) ) THEN
      gababp(2,1) = c1
      gababp(2,2) = c2
      gababp(2,3) = c3
   ENDIF
   c1 = aa*(r1*z2-z1*r2)
   c2 = -aa*(z2-z1)
   c3 = aa*(r2-r1)
   Gababq(7,1) = c1
   Gababq(7,2) = c2
   Gababq(7,3) = c3
   Gababq(8,4) = c1
   Gababq(8,5) = c2
   Gababq(8,6) = c3
   Gababq(9,7) = c1
   Gababq(9,8) = c2
   Gababq(9,9) = c3
   IF ( .NOT.(lsys78) ) THEN
      gababp(3,1) = c1
      gababp(3,2) = c2
      gababp(3,3) = c3
   ENDIF
!
!     COMPUTE THE INTEGRAL VALUES IN ARRAY DELINT THE ORDER IS INDICATED
!     BY THE FOLLOWING TABLE
!
!       DELINT(01) = (-1,0)
!       DELINT(02) = (-1,1)
!       DELINT(03) = (-1,2)
!       DELINT(04) = (0, 0)
!       DELINT(05) = (0, 1)
!       DELINT(06) = (1, 0)
!
   ra = (r1+r2+r3)/3.0
   za = (z1+z2+z3)/3.0
   rh = amin1(r1,r2,r3)/10.0
   dr = amax1(abs(r1-r2),abs(r2-r3),abs(r3-r1))
   area = (r1*(z2-z3)+r2*(z3-z1)+r3*(z1-z2))/2.0
   i1 = 0
   DO i = 1 , 2
      ip = i - 2
      DO j = 1 , 3
         iq = j - 1
         i1 = i1 + 1
         IF ( i1==6 ) THEN
            ip = 1
            iq = 0
         ENDIF
         IF ( dr>rh ) THEN
            delint(i1) = ais(3,ip,iq,r,z)
         ELSE
            delint(i1) = ((ra**ip)*(za**iq))*area
         ENDIF
         delint(i1) = abs(delint(i1))
      ENDDO
   ENDDO
!
!     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3
!
   dgamr = dgama*Degrad
   cosg = cos(dgamr)
   sing = sin(dgamr)
   Sinth = sing
   Costh = cosg
   Matidc = matid
   Matflg = 7
   IF ( Ksys78>0 ) Matflg = 9
   Eltemp = tempe
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
      GOTO 200
   ELSE
      Tz = Tzero
      IF ( Ksys78<=0 ) THEN
!
!     SET MATERIAL PROPERTIES IN LOCAL VARIABLES (AGAIN)
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
      DO i = 5 , 63
         ee(i) = 0.0
      ENDDO
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
      ee(3) = teo(4)*c2 + teo(5)*s2
      ee(4) = cosg*sing*s2*(teo(2)-teo(3)+2.0*teo(10)) + sing*cosg*c2*(teo(1)-teo(2)-2.0*teo(10))
      ee(7) = ee(2)
      ee(8) = teo(1)*s4 + 2.0*c2s2*(teo(2)+2.0*teo(10)) + teo(3)*c4
      ee(9) = teo(4)*s2 + teo(5)*c2
      ee(10) = sing*cosg*c2*(teo(2)-teo(3)+2.0*teo(10)) + cosg*sing*s2*(teo(1)-teo(2)-2.0*teo(10))
      ee(13) = ee(3)
      ee(14) = ee(9)
      ee(15) = teo(6)
      ee(16) = sing*cosg*(teo(4)-teo(5))
      ee(19) = ee(4)
      ee(20) = ee(10)
      ee(21) = ee(16)
      ee(22) = c2s2*(teo(1)-2.0*teo(2)+teo(3)) + teo(10)*(c2-s2)**2
      ee(29) = teo(15)*c2 + teo(21)*s2
      ee(30) = sing*cosg*(teo(15)-teo(21))
      ee(35) = ee(30)
      ee(36) = teo(15)*s2 + teo(21)*c2
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
      Iecpt(1) = Iecpt(1) - (Iecpt(1)/1000)*1000 - 1
      ajho = Iecpt(1)
      ajjho = ajho*ajho
!
!     FORM THE ELEMENT STIFFNESS MATRIX IN FIELD SYSTEM
!
      acurl(1) = (ee(15)+ajjho*ee(29))*delint(1)
      acurl(2) = (ee(3)+ee(15)+ajjho*ee(29))*delint(4)
      acurl(3) = (ee(15)+ajjho*ee(29))*delint(2) + ee(16)*delint(4)
      acurl(4) = (ee(15)+ee(29))*ajho*delint(1)
      acurl(5) = ee(15)*ajho*delint(4)
      acurl(6) = (ee(15)+ee(29))*ajho*delint(2) - ee(30)*ajho*delint(4)
      acurl(7) = ajjho*delint(1)*ee(35)
      acurl(8) = (ee(16)+ajjho*ee(35))*delint(4)
      acurl(9) = ee(9)*delint(4) + ajjho*delint(2)*ee(35)
      acurl(11) = (ee(1)+2.0*ee(3)+ee(15)+ajjho*ee(29))*delint(6)
      acurl(12) = (ee(3)+ee(15)+ajjho*ee(29))*delint(5) + (ee(4)+ee(16))*delint(6)
      acurl(13) = (ee(3)+ee(15)+ee(29))*ajho*delint(4)
      acurl(14) = (ee(3)+ee(15))*delint(6)*ajho
      acurl(15) = (ee(3)+ee(15)+ee(29))*ajho*delint(5) - ajho*ee(30)*delint(6)
      acurl(16) = ajjho*delint(4)*ee(35)
      acurl(17) = (ee(4)+ee(16)+ajjho*ee(35))*delint(6)
      acurl(18) = (ee(2)+ee(9))*delint(6) + ajjho*delint(5)*ee(35)
      acurl(21) = (ee(15)+ajjho*ee(29))*delint(3) + ee(22)*delint(6) + 2.0*ee(16)*delint(5)
      acurl(22) = (ee(15)+ee(29))*ajho*delint(2) + ajho*delint(4)*ee(16)
      acurl(23) = ee(15)*ajho*delint(5) + ajho*delint(6)*ee(16)
      acurl(24) = (ee(15)+ee(29))*ajho*delint(3) + (ee(16)-ee(30))*ajho*delint(5)
      acurl(25) = ajjho*delint(2)*ee(35)
      acurl(26) = ee(22)*delint(6) + (ee(21)+ajjho*ee(35))*delint(5)
      acurl(27) = ee(9)*delint(5) + ee(10)*delint(6) + ajjho*delint(3)*ee(35)
      acurl(31) = (ee(29)+ajjho*ee(15))*delint(1)
      acurl(32) = ee(15)*ajjho*delint(4)
      acurl(33) = (ee(29)+ajjho*ee(15))*delint(2) - ee(30)*delint(4)
      acurl(34) = ajho*delint(1)*ee(35)
      acurl(35) = ajho*(ee(16)+ee(35))*delint(4)
      acurl(36) = ee(9)*ajho*delint(4) + ajho*delint(2)*ee(35)
      acurl(41) = ajjho*delint(6)*ee(15)
      acurl(42) = ee(15)*ajjho*delint(5)
      acurl(43) = 0.0
      acurl(44) = ajho*delint(6)*ee(16)
      acurl(45) = ee(9)*ajho*delint(6)
      acurl(51) = (ee(29)+ajjho*ee(15))*delint(3) + ee(36)*delint(6) - 2.0*delint(5)*ee(35)
      acurl(52) = ajho*(delint(2)*ee(30)-delint(4)*ee(36))
      acurl(53) = -ee(36)*ajho*delint(6) + ajho*(ee(16)+ee(35))*delint(5)
      acurl(54) = (ee(9)-ee(36))*ajho*delint(5) + ajho*delint(3)*ee(35)
      acurl(61) = ee(36)*ajjho*delint(1)
      acurl(62) = ee(36)*ajjho*delint(4)
      acurl(63) = (ee(36))*ajjho*delint(2)
      acurl(71) = (ee(22)+ajjho*ee(36))*delint(6)
      acurl(72) = ee(36)*ajjho*delint(5) + ee(20)*delint(6)
      acurl(81) = ee(36)*ajjho*delint(3) + ee(8)*delint(6)
!
      IF ( .NOT.(lsys78) ) THEN
         acurl(82) = -(ee(45)-ajho*ee(51))*ajho*delint(1)
         acurl(83) = (ee(43)-ajho*ee(45)-ajho*ee(49)+ajjho*ee(51))*delint(4)
         acurl(84) = (ee(44)-ajho*ee(50))*delint(4) - (ee(45)-ajho*ee(51))*ajho*delint(2)
         acurl(85) = -(ee(39)+ee(45)-ajho*ee(51))*ajho*delint(4)
         acurl(86) = (ee(37)+ee(43)-(ee(39)+ee(45)+ee(49)-ajho*ee(51))*ajho)*delint(6)
         acurl(87) = (ee(38)+ee(44)-ajho*ee(50))*delint(6) - (ee(39)+ee(45)-ajho*ee(51))*ajho*delint(5)
         acurl(88) = -(ee(45)-ajho*ee(51))*ajho*delint(2) - ee(48)*ajho*delint(4)
         acurl(89) = (ee(43)-ajho*ee(45)-ajho*ee(49)+ajjho*ee(51))*delint(5) + (ee(46)-ee(48)*ajho)*delint(6)
         acurl(90) = (ee(44)-ajho*ee(48)-ajho*ee(50))*delint(5) + ee(47)*delint(6) - (ee(45)-ajho*ee(51))*ajho*delint(3)
         acurl(91) = -(ee(45)*ajho-ee(51))*ajho*delint(1)
         acurl(92) = (ajho*ee(43)-ajjho*ee(45)-ee(49)+ajho*ee(51))*delint(4)
         acurl(93) = (ee(44)*ajho-ee(50))*delint(4) - (ee(45)*ajho-ee(51))*ajho*delint(2)
         acurl(94) = -ee(45)*ajjho*delint(4)
         acurl(95) = (ee(43)-ajho*ee(45))*ajho*delint(6)
         acurl(96) = ee(44)*ajho*delint(6) - ee(45)*ajjho*delint(5)
         acurl(97) = -(ee(45)*ajho-ee(51))*ajho*delint(2) - ee(54)*ajho*delint(4)
         acurl(98) = (ee(43)*ajho-ajjho*ee(45)-ee(49)+ee(51)*ajho)*delint(5) + (ee(52)-ajho*ee(54))*delint(6)
         acurl(99) = (ee(44)*ajho-ee(50)-ee(54)*ajho)*delint(5) + ee(53)*delint(6) - (ee(45)*ajho-ee(51))*ajho*delint(3)
         acurl(100) = ee(54)*ajjho*delint(1)
         acurl(101) = -(ee(52)-ee(54)*ajho)*ajho*delint(4)
         acurl(102) = -(ee(53)*delint(4)-ee(54)*ajho*delint(2))*ajho
         acurl(103) = -(ee(48)-ee(54)*ajho)*ajho*delint(4)
         acurl(104) = (ee(46)-ee(48)*ajho-ee(52)*ajho+ee(54)*ajjho)*delint(6)
         acurl(105) = (ee(47)-ee(53)*ajho)*delint(6) - (ee(48)-ee(54)*ajho)*ajho*delint(5)
         acurl(106) = ee(54)*ajjho*delint(2) - ee(42)*ajho*delint(4)
         acurl(107) = (ee(40)-ee(42)*ajho)*delint(6) - (ee(52)-ee(54)*ajho)*ajho*delint(5)
         acurl(108) = ee(41)*delint(6) + (-ee(42)-ee(53))*ajho*delint(5) + ee(54)*ajjho*delint(3)
         acurl(109) = ee(63)*ajjho*delint(1)
         acurl(110) = (-ee(57)+ee(63)*ajho)*ajho*delint(4)
         acurl(111) = -ee(60)*ajho*delint(4) + ee(63)*ajjho*delint(2)
         acurl(112) = acurl(110)
         acurl(113) = (ee(55)-2.0*ee(57)*ajho+ee(63)*ajjho)*delint(6)
         acurl(114) = (ee(56)-ee(60)*ajho)*delint(6) + (-ee(57)+ee(63)*ajho)*ajho*delint(5)
         acurl(115) = acurl(111)
         acurl(116) = acurl(114)
         acurl(117) = ee(59)*delint(6) - 2.0*ee(60)*ajho*delint(5) + ee(63)*ajjho*delint(3)
      ENDIF
!
!     EXPAND ACURL INTO (9X9)
!
      DO ib = 2 , 9
         ic = 10*ib - 19
         i = ic
         DO j = ib , 9
            ic = ic + 9
            i = i + 1
            acurl(ic) = acurl(i)
         ENDDO
      ENDDO
      dgamr = Pi
      IF ( ajho==0 ) dgamr = 2.0*Pi
      DO i = 1 , 81
         acurl(i) = dgamr*acurl(i)
      ENDDO
!
      IF ( .NOT.(lsys78) ) THEN
         DO i = 82 , 117
            acurl(i) = acurl(i)*dgamr
         ENDDO
      ENDIF
!
!     TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD SYSTEM
!     TO GRID POINT DEGREES OF FREEDOM
!
      CALL gmmats(Gababq,9,9,1,acurl,9,9,0,D)
      CALL gmmats(D,9,9,0,Gababq,9,9,0,Ak)
!
      IF ( .NOT.(lsys78) ) THEN
         CALL gmmats(Gababq,9,9,1,acurp1,9,3,0,d1)
         CALL gmmats(d1,9,3,0,gababp,3,3,0,Akuph)
         CALL gmmats(gababp,3,3,1,acurp2,3,3,0,d2)
         CALL gmmats(d2,3,3,0,gababp,3,3,0,Akph2)
      ENDIF
!
!
!     LOCATE THE TRANSFORMATION MATRICES FOR THE THREE GRID POINTS
!     **** COORDINATE SYS NOT POSSIBLE WITH RINGAX ********
!     **** THE FOLLOWING CODE COULD IMPLEMENT IT   ********
!     **   IF FOLLOWING CODE IS IMPLEMENTED MUST BE MODIFIED FOR
!          PIEZOELECTRIC
!.    DO 750 I = 1,81
!.750 AKI(I) = 0.00
!.    DO 800 I = 1,3
!.    CALL TRANSS (ICS(I),D(1))
!.    K = 30*(I-1) + 1
!.    DO 800 J = 1,3
!.    KK = K + 9*(J-1)
!.    JJ = 3*(J-1) + 1
!.    AKI(KK  ) = D(JJ  )
!.    AKI(KK+1) = D(JJ+1)
!.    AKI(KK+2) = D(JJ+2)
!.800 CONTINUE
!
!     TRANSFORM THE STIFFNESS MATRIX FROM BASIC TO LOCAL COORDINATES
!
!.    CALL GMMATS (AKI,9,9,1, AK,9,9,0,D )
!.    CALL GMMATS (  D,9,9,0,AKI,9,9,0,AK)
!
!     FORM WJ MATRIX
!
      DO i = 1 , 6
         DO j = 1 , 9
            Wj(i,j) = 0.0
         ENDDO
      ENDDO
      rsum = 0.0
      zsum = 0.0
      DO i = 1 , 3
         rsum = rsum + r(i)
         zsum = zsum + z(i)
      ENDDO
      rsum = rsum/3.0
      zsum = zsum/3.0
      zdr = zsum/rsum
      Wj(1,2) = 1.0
      Wj(2,9) = 1.0
      Wj(3,1) = 1.0/rsum
      Wj(3,2) = 1.0
      Wj(3,3) = zdr
      Wj(3,4) = ajho/rsum
      Wj(3,5) = ajho
      Wj(3,6) = ajho*zdr
      Wj(4,3) = 1.0
      Wj(4,8) = 1.0
      Wj(5,1) = -ajho/rsum
      Wj(5,2) = -ajho
      Wj(5,3) = -ajho*zdr
      Wj(5,4) = -1.0/rsum
      Wj(5,6) = -zdr
      Wj(6,6) = 1.0
      Wj(6,7) = -ajho/rsum
      Wj(6,8) = -ajho
      Wj(6,9) = -ajho*zdr
!
      IF ( .NOT.(lsys78) ) THEN
!
!     FORM WJP MATRIX
!
         DO i = 1 , 3
            DO j = 1 , 3
               wjp(i,j) = 0.0
            ENDDO
         ENDDO
!
         wjp(1,2) = 1.0
         wjp(2,3) = 1.0
         wjp(3,1) = -ajho/rsum
         wjp(3,2) = -ajho
         wjp(3,3) = -ajho*zdr
      ENDIF
!
!     COMPUTE THE STRESS MATRIX
!
      CALL gmmats(Wj,9,6,1,Gababq,9,9,0,D(1))
      CALL gmmats(ee(1),6,6,0,D(1),6,9,0,Sel)
      IF ( .NOT.(lsys78) ) THEN
         CALL gmmats(wjp,3,3,1,gababp,3,3,0,d2(1))
         CALL gmmats(ee(37),6,3,0,d2(1),3,3,0,Selp1)
         CALL gmmats(ee(37),6,3,1,D(1),6,9,0,Selp2)
         CALL gmmats(ee(55),3,3,0,d2(1),3,3,0,Selp3)
      ENDIF
!
!    *** MORE CORD SYS REMOVAL.  FEL ABOVE IS WJ **********
!    **  IF FOLLOWING CODE IS IMPLEMENTED MUST BE MODIFIED FOR
!        PIEZOELECTRIC
!
!.    CALL GMMATS (WJ,6,9,0, AK,9,9,0, SEL)
!
!
!     TRANSFORM THE STRESS MATRIX FROM BASIC TO LOCAL COORDINATES
!
!     COMPUTE THE THE THERMAL STRESS
!
      Ts(1) = ee(1)*Alf(1) + ee(2)*Alf(3) + ee(3)*Alf(2)
      Ts(2) = ee(7)*Alf(1) + ee(8)*Alf(3) + ee(9)*Alf(2)
      Ts(3) = ee(13)*Alf(1) + ee(14)*Alf(3) + ee(15)*Alf(2)
      Ts(4) = ee(19)*Alf(1) + ee(20)*Alf(3) + ee(21)*Alf(2)
      Ts(5) = 0.0
      Ts(6) = 0.0
      DO iki = 1 , 14
         Phi(iki) = Ecpt(7+iki)
      ENDDO
      GOTO 300
   ENDIF
!
!     SET FATAL ERROR FLAG AND ALLOWING ERROR MESSAGES TO ACCUMULATE
!
 100  i = 37
 200  CALL mesage(30,i,Idel)
   nogo = 1
 300  IF ( .NOT.pzmat ) Ksys78 = ksave
END SUBROUTINE strax1
