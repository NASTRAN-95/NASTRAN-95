!*==triaax.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE triaax
   IMPLICIT NONE
   USE C_CONDAD
   USE C_EMGDIC
   USE C_EMGEST
   USE C_EMGPRM
   USE C_MATIN
   USE C_MATOUT
   USE C_MATPZ
   USE C_SYSTEM
!
! Local variable declarations rewritten by SPAG
!
   REAL :: aa , ajho , ajjho , area , c1 , c2 , c2s2 , c3 , c4 , convm , cosg , cs , cs2 , degrad , del , dgamr , dict5 , dr , er , &
         & et , ez , gor , grz , gzo , pi , ra , rh , rhod , s2 , s3 , s4 , sc2 , sing , twopi , vor , voz , vro , vrz , vzo , vzr ,&
         & z1 , z2 , z3 , za , zmin
   REAL , DIMENSION(117) :: acurl
   REAL , DIMENSION(27) :: acurp1 , akuph , d1
   REAL , DIMENSION(9) :: acurp2 , akip , akph2 , amt , d2
   REAL , DIMENSION(81) :: ak , aki , akjm , akm , d
   REAL , DIMENSION(144) :: akj
   REAL , DIMENSION(16) :: akt
   REAL , DIMENSION(9,9) :: bmass , gababq
   REAL , DIMENSION(12) :: delint , delm
   INTEGER , DIMENSION(11) :: dict
   REAL , DIMENSION(10) :: ecpt
   REAL , DIMENSION(63) :: ee
   REAL , DIMENSION(3,3) :: gababp
   INTEGER :: i , i1 , ib , ic , ic1 , idel1 , ii , ij , iout , ip , ipp , ipr , iq , ir1 , irc , ircc , isil , j , j1 , jj , k ,   &
            & ki , ksave , l , m , mjho , n
   INTEGER , DIMENSION(3) :: ics , isort
   INTEGER , SAVE :: idel2 , jax
   INTEGER , DIMENSION(34) :: iecpt
   LOGICAL :: lsys78 , pzmat
   REAL , DIMENSION(3) :: r , z
   REAL , DIMENSION(45) :: teo
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE COMPUTES THE STIFFNESS AND MASS MATRICES FOR THE
!     ASSYMMETRIC RING WITH A TRIANGULAR CROSS SECTION, TO BE USED BY
!     THE ELEMENT MATRIX GENERATOR.
!
!     SINGLE PRECISION VERSION
!
!     THIS CURRENT VERSION ALLOWS FOR COORDINATE SYSTEM
!     THIS SUBROUTINE USES THE ADDITIONAL ROUTINES DKL, DELTKL
!
!     THE ECPT FOR THE TRIAX ELEMENT IS
!
!     ECPT (01) = ELEMENT ID                              I
!     ECPT (02) = SIL A                                   I
!     ECPT (03) = SIL B                                   I
!     ECPT (04) = SIL C                                   I
!     ECPT (05) = MATERIAL ORIENTATION ANGLE(DEGREES)     R
!     ECPT (07) = MATERIAL ID                             I
!     ECPT (08) TO ECPT(21) = STRESS PHASE ANG.           R
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
!
!
!     COMMON /MATPZ / CE11,CE12,CE13,CE14,CE15,CE16,CE22,CE23,CE24,CE25,
!                     CE26,CE33,CE34,CE35,CE36,CE44,CE45,CE46,CE55,CE56,
!                     CE66,E11,E12,E13,E14,E15,E16,E21,E22,E23,E24,E25,
!                     E26,E31,E32,E33,E34,E35,E36,EPS11,EPS12,EPS13,
!                     EPS22,EPS23,EPS33,RHO,A1,A2,A12,TREF,GE
!
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1),Idel) , (dict(5),dict5) , (z(1),z1) , (z(2),z2) , (z(3),z3) , (aki(1),gababq(1,1)) ,               &
!>>>>    & (bmass(1,1),akm(1)) , (Consts(1),Pi) , (Consts(4),Degrad) , (Consts(2),Twopi) , (akip(1),gababp(1,1)) , (acurp1(1),acurl(82)) &
!>>>>    & , (acurp2(1),acurl(109))
   DATA idel2 , jax/0 , 4HTRIA/
!
   lsys78 = .FALSE.
   IF ( Ksys78==0 .OR. Ksys78==2 ) lsys78 = .TRUE.
   idel1 = Idel/1000
!
!     INITALIZE
!
   DO i = 1 , 3
      r(i) = ecpt(4*i+19)
      z(i) = ecpt(4*i+20)
      ics(i) = iecpt(4*i+18)
   ENDDO
!
   dict(1) = Estid
   dict(2) = 1
   dict(3) = 12
   dict(4) = 15
   ipr = Iprec
!
   IF ( R1<=0. .OR. R2<=0. .OR. R3<=0. ) THEN
!
!     ERROR EXITS
!
      i = 37
      GOTO 400
   ELSE
!
!     COMPUTE THE ELEMENT COORDINATES
!
      zmin = amin1(z1,z2,z3)
      z1 = z1 - zmin
      z2 = z2 - zmin
      z3 = z3 - zmin
!
!     FORM TRANSFORMATION MATRIX GABABQ (9X9) FROM FIELD COORDINATES TO
!     GRID POINT DEGREES OF FREEDOM
!
      DO i = 1 , 9
         DO j = 1 , 9
            gababq(i,j) = 0.
         ENDDO
      ENDDO
!
      aa = 1./(R2*z3+R1*z2+z1*R3-z2*R3-R1*z3-R2*z1)
      c1 = aa*(R2*z3-z2*R3)
      c2 = -aa*(z3-z2)
      c3 = aa*(R3-R2)
      gababq(1,1) = c1
      gababq(1,2) = c2
      gababq(1,3) = c3
      gababq(2,4) = c1
      gababq(2,5) = c2
      gababq(2,6) = c3
      gababq(3,7) = c1
      gababq(3,8) = c2
      gababq(3,9) = c3
      IF ( .NOT.(lsys78) ) THEN
         gababp(1,1) = c1
         gababp(1,2) = c2
         gababp(1,3) = c3
      ENDIF
      c1 = -aa*(R1*z3-z1*R3)
      c2 = aa*(z3-z1)
      c3 = -aa*(R3-R1)
      gababq(4,1) = c1
      gababq(4,2) = c2
      gababq(4,3) = c3
      gababq(5,4) = c1
      gababq(5,5) = c2
      gababq(5,6) = c3
      gababq(6,7) = c1
      gababq(6,8) = c2
      gababq(6,9) = c3
      IF ( .NOT.(lsys78) ) THEN
         gababp(2,1) = c1
         gababp(2,2) = c2
         gababp(2,3) = c3
      ENDIF
      c1 = aa*(R1*z2-z1*R2)
      c2 = -aa*(z2-z1)
      c3 = aa*(R2-R1)
      gababq(7,1) = c1
      gababq(7,2) = c2
      gababq(7,3) = c3
      gababq(8,4) = c1
      gababq(8,5) = c2
      gababq(8,6) = c3
      gababq(9,7) = c1
      gababq(9,8) = c2
      gababq(9,9) = c3
      IF ( .NOT.(lsys78) ) THEN
         gababp(3,1) = c1
         gababp(3,2) = c2
         gababp(3,3) = c3
      ENDIF
!
!     COMPUTE THE INTEGRAL VALUES IN ARRAY DELINT THE ORDER IS INDICATED
!     THE FOLLOWING TABLE
!
!     DELINT(01) = (-1,0)
!     DELINT(02) = (-1,1)
!     DELINT(03) = (-1,2)
!     DELINT(04) = (0, 0)
!     DELINT(05) = (0, 1)
!     DELINT(06) = (1, 0)
!
!     OR FOR THE MASS MATRIX
!
!     DELINT(1)  =  (1,0)
!     DELINT(2)  =  (1,1)
!     DELINT(3)  =  (1,2)
!     DELINT(4)  =  (2,0)
!     DELINT(5)  =  (2,1)
!     DELINT(7)  =  (3,0)
!
!
      IF ( Ismb(1)/=0 ) THEN
         ra = (R1+R2+R3)/3.0
         za = (z1+z2+z3)/3.0
         rh = amin1(R1,R2,R3)/10.0
         dr = amax1(abs(R1-R2),abs(R2-R3),abs(R3-R1))
         area = (R1*(z2-z3)+R2*(z3-z1)+R3*(z1-z2))/2.0
!
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
                  delint(i1) = dkls(3,ip,iq,r,z)
               ELSE
                  delint(i1) = ((ra**ip)*(za**iq))*area
               ENDIF
               delint(i1) = abs(delint(i1))
            ENDDO
         ENDDO
!
!     MASS MATRIX
!
         IF ( Ismb(2)==0 ) GOTO 100
      ENDIF
      CALL delkls(akj,r,z,0)
      delm(1) = akj(2)
      delm(2) = akj(7)
      delm(3) = akj(8)
      delm(4) = akj(10)
      delm(5) = akj(9)
      delm(7) = akj(12)
   ENDIF
!
!     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3
!
 100  dgamr = Dgama*degrad
   cosg = cos(dgamr)
   sing = sin(dgamr)
   Sinth = sing
   Costh = cosg
   Matidc = Matid
   Matflg = 7
   IF ( Ksys78>0 ) Matflg = 9
   Eltemp = Tempe
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
   IF ( Setmat==2. ) THEN
      i = 126
      GOTO 400
   ELSE
      dict5 = Gsube
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
         del = 1./(1.-vro*vor-voz*vzo-vzr*vrz-vro*voz*vzr-vrz*vor*vzo)
      ENDIF
!
!     COMPUTE ELASTIC CONSTANTS MATRIX FROM MATERIAL TO ELEMENT AXIS
!
      DO i = 1 , 45
         teo(i) = 0.
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
         teo(1) = er*(1.-voz*vzo)*del
         teo(2) = er*(vzr+vzo*vor)*del
         teo(3) = ez*(1.-vro*vor)*del
         teo(4) = er*(vor+vzr*voz)*del
         teo(5) = et*(vzo+vro*vzr)*del
         teo(6) = et*(1.-vrz*vzr)*del
         teo(10) = grz
         teo(15) = gor
         teo(21) = gzo
      ENDIF
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
      ee(1) = teo(1)*c4 + teo(3)*s4 + 2.*c2s2*(teo(2)+2.*teo(10))
      ee(2) = teo(2)*(c4+s4) + c2s2*(teo(1)+teo(3)-4.*teo(10))
      ee(3) = teo(4)*c2 + teo(5)*s2
      ee(4) = cosg*sing*s2*(teo(2)-teo(3)+2.*teo(10)) + sing*cosg*c2*(teo(1)-teo(2)-2.*teo(10))
      ee(7) = ee(2)
      ee(8) = teo(1)*s4 + 2.*c2s2*(teo(2)+2.*teo(10)) + teo(3)*c4
      ee(9) = teo(4)*s2 + teo(5)*c2
      ee(10) = sing*cosg*c2*(teo(2)-teo(3)+2.*teo(10)) + cosg*sing*s2*(teo(1)-teo(2)-2.*teo(10))
      ee(13) = ee(3)
      ee(14) = ee(9)
      ee(15) = teo(6)
      ee(16) = sing*cosg*(teo(4)-teo(5))
      ee(19) = ee(4)
      ee(20) = ee(10)
      ee(21) = ee(16)
      ee(22) = c2s2*(teo(1)-2.*teo(2)+teo(3)) + teo(10)*(c2-s2)**2
      ee(29) = teo(15)*c2 + teo(21)*s2
      ee(30) = sing*cosg*(teo(15)-teo(21))
      ee(35) = ee(30)
      ee(36) = teo(15)*s2 + teo(21)*c2
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
      mjho = mod(iecpt(1),1000) - 1
      ajho = mjho
      ajjho = ajho*ajho
      rhod = Rho*pi
      IF ( ajho==0. ) rhod = 2.*rhod
      IF ( Ismb(1)/=0 ) THEN
!
!     FORM THE ELEMENT STIFFNESS MATRIX IN FIELD SYSTEM
!
         acurl(1) = (ee(15)+ajjho*ee(29))*delint(1)
         acurl(2) = (ee(03)+ee(15)+ajjho*ee(29))*delint(4)
         acurl(3) = (ee(15)+ajjho*ee(29))*delint(2) + ee(16)*delint(4)
         acurl(4) = (ee(15)+ee(29))*ajho*delint(1)
         acurl(05) = ee(15)*ajho*delint(4)
         acurl(06) = (ee(15)+ee(29))*ajho*delint(2) - ee(30)*ajho*delint(4)
         acurl(07) = ajjho*delint(1)*ee(35)
         acurl(08) = (ee(16)+ajjho*ee(35))*delint(4)
         acurl(09) = ee(9)*delint(4) + ajjho*delint(2)*ee(35)
         acurl(11) = (ee(1)+2.*ee(3)+ee(15)+ajjho*ee(29))*delint(6)
         acurl(12) = (ee(3)+ee(15)+ajjho*ee(29))*delint(5) + (ee(4)+ee(16))*delint(6)
         acurl(13) = (ee(3)+ee(15)+ee(29))*ajho*delint(4)
         acurl(14) = (ee(3)+ee(15))*delint(6)*ajho
         acurl(15) = (ee(3)+ee(15)+ee(29))*ajho*delint(5) - ajho*ee(30)*delint(6)
         acurl(16) = ajjho*delint(4)*ee(35)
         acurl(17) = (ee(4)+ee(16)+ajjho*ee(35))*delint(6)
         acurl(18) = (ee(2)+ee(9))*delint(6) + ajjho*delint(5)*ee(35)
         acurl(21) = (ee(15)+ajjho*ee(29))*delint(3) + ee(22)*delint(6) + 2.*ee(16)*delint(5)
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
         acurl(43) = 0.
         acurl(44) = ajho*delint(6)*ee(16)
         acurl(45) = ee(9)*ajho*delint(6)
         acurl(51) = (ee(29)+ajjho*ee(15))*delint(3) + ee(36)*delint(6) - 2.*ee(35)*delint(5)
         acurl(52) = ajho*(delint(2)*ee(30)-delint(4)*ee(36))
         acurl(53) = -ee(36)*ajho*delint(6) + ajho*(ee(16)+ee(35))*delint(5)
         acurl(54) = (ee(9)-ee(36))*ajho*delint(5) + ajho*delint(3)*ee(35)
         acurl(61) = ee(36)*ajjho*delint(1)
         acurl(62) = ee(36)*ajjho*delint(4)
         acurl(63) = (ee(36))*ajjho*delint(2)
         acurl(71) = (ee(22)+ajjho*ee(36))*delint(6)
         acurl(72) = ee(36)*ajjho*delint(5) + ee(20)*delint(6)
         acurl(81) = ee(36)*ajjho*delint(3) + ee(8)*delint(6)
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
         Dgama = pi
         IF ( ajho==0. ) Dgama = twopi
         DO i = 1 , 81
            acurl(i) = acurl(i)*Dgama
         ENDDO
         IF ( .NOT.(lsys78) ) THEN
!
            DO i = 82 , 117
               acurl(i) = acurl(i)*Dgama
            ENDDO
         ENDIF
      ENDIF
!
      IF ( Ismb(2)/=0 ) THEN
         IF ( Icmbar<0 ) THEN
            area = (R1*(z2-z3)+R2*(z3-z1)+R3*(z1-z2))/2.
            convm = rhod*(R1+R2+R3)/3.*area
         ELSE
!
!     CONSISTENT MASS IN FIELD COORDINATES
!
            DO i = 1 , 9
               DO j = 1 , 9
                  bmass(i,j) = 0.
               ENDDO
            ENDDO
            bmass(1,1) = rhod*delm(1)
            bmass(1,2) = rhod*delm(4)
            bmass(1,3) = rhod*delm(2)
            bmass(2,1) = rhod*delm(4)
            bmass(2,2) = rhod*delm(7)
            bmass(2,3) = rhod*delm(5)
            bmass(3,1) = rhod*delm(2)
            bmass(3,2) = rhod*delm(5)
            bmass(3,3) = rhod*delm(3)
            bmass(4,4) = rhod*delm(1)
            bmass(4,5) = rhod*delm(4)
            bmass(4,6) = rhod*delm(2)
            bmass(5,4) = rhod*delm(4)
            bmass(5,5) = rhod*delm(7)
            bmass(5,6) = rhod*delm(5)
            bmass(6,4) = rhod*delm(2)
            bmass(6,5) = rhod*delm(5)
            bmass(6,6) = rhod*delm(3)
            bmass(7,7) = rhod*delm(1)
            bmass(7,8) = rhod*delm(4)
            bmass(7,9) = rhod*delm(2)
            bmass(8,7) = rhod*delm(4)
            bmass(8,8) = rhod*delm(7)
            bmass(8,9) = rhod*delm(5)
            bmass(9,7) = rhod*delm(2)
            bmass(9,8) = rhod*delm(5)
            bmass(9,9) = rhod*delm(3)
         ENDIF
      ENDIF
!
!     TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD SYSTEM
!     TO GRID POINT DEGREES OF FREEDOM
!
      IF ( Ismb(1)/=0 ) THEN
         CALL gmmats(aki,9,9,1,acurl,9,9,0,d)
         CALL gmmats(d,9,9,0,aki,9,9,0,ak)
         IF ( .NOT.(lsys78) ) THEN
            CALL gmmats(aki,9,9,1,acurp1,9,3,0,d1)
            CALL gmmats(d1,9,3,0,akip,3,3,0,akuph)
            CALL gmmats(akip,3,3,1,acurp2,3,3,0,d2)
            CALL gmmats(d2,3,3,0,akip,3,3,0,akph2)
         ENDIF
!
         IF ( Ismb(2)==0 .OR. Icmbar<0 ) GOTO 200
      ENDIF
      CALL gmmats(aki,9,9,1,bmass,9,9,0,d)
      CALL gmmats(d,9,9,0,aki,9,9,0,akm)
   ENDIF
!
 200  DO i = 1 , 81
      akj(i) = 0.
      akjm(i) = 0.
   ENDDO
   DO i = 82 , 117
      akj(i) = 0.0
!
   ENDDO
!
!     COORDINATE SYSTEMS POSSIBLE WITH RINGAX THRU CODE BELOW
! **  IF FOLLOWING CODE IS IMPLEMENTED MUST BE MODIFIED FOR PIEZOELECTRI
!
!     DO 470 I = 1,3
!     IF (ICS(I) .EQ. 0) GO TO 470
!     K = 9*(I-1) + 1
!     CALL TRANSS (ICS(I),D(K))
! 470 CONTINUE
!
!     CREATE AN ARRAY OF SORTED GRID POINTS
!
   DO i = 1 , 3
      isort(i) = Igp(i)
   ENDDO
   i = -3
   DO
      j = 0
      DO k = 1 , 3
         IF ( isort(k)>=j ) THEN
            j = isort(k)
            l = k
         ENDIF
      ENDDO
      isort(l) = i
      i = i + 1
      IF ( i>=0 ) THEN
         DO i = 1 , 3
            isort(i) = -isort(i)
         ENDDO
!
!     TRANSFORM 3 X 3 TO 6 X 6 FOR COORD SYSTEM TRANSFORMATIONS
!
         DO isil = 1 , 3
            ipp = isort(isil)
!
            ir1 = 3*(isil-1) + 1
            DO ii = 1 , 3
               i = isort(ii)
               ic1 = 3*(ii-1) + 1
               irc = (ir1-1)*9 + ic1
               DO j = 1 , 3
                  j1 = (j-1)*4 + 1
                  ircc = irc + (j-1)*9
                  IF ( Ismb(1)/=0 ) THEN
                     akt(j1) = ak(ircc)
                     akt(j1+1) = ak(ircc+1)
                     akt(j1+2) = ak(ircc+2)
                     IF ( .NOT.(lsys78) ) THEN
                        m = ircc/3 + 1
                        n = (m-1)/9 + 1 + (ii-1)*9 + (j-1)*3
                        akt(j1+3) = akuph(m)
                        akt(j1+15-j*3) = akuph(n)
                        akt(16) = akph2(ir1+ii-1)
                     ENDIF
                  ENDIF
!
                  IF ( Ismb(2)/=0 .AND. Icmbar>=1 ) THEN
                     j1 = (j-1)*3 + 1
                     amt(j1) = akm(ircc)
                     amt(j1+1) = akm(ircc+1)
                     amt(j1+2) = akm(ircc+2)
                  ENDIF
!
               ENDDO
!
!    ABOVE GO TO MAKES CST CODE BELOW INTO DEAD CODE
!    COORDINATE SYSTEM TRANSFORMATION CODE
! ** IF FOLLOWING CODE IS IMPLEMENTED MUST BE MODIFIED FOR PIEZOELECTRIC
!
!     IF (ICS(IPP) .EQ. 0) GO TO 520
!     IAA = 9*(IPP-1) + 1
!     CALL GMMATS (D(IAA),3,3,1,AKT(1),3,3,0,D(28))
!     CALL GMMATS (D(IAA),3,3,1,AMT(1),3,3,0,D(37))
!     DO 510 J = 1,9
!     AKT(J) = D(J+27)
! 510 AKM(J) = D(J+36)
!
! 520 IF (ICS(I) .EQ. 0) GO TO 540
!     IAI = 9*(I-1) + 1
!     CALL GMMATS (AKT(1),3,3,0,D(IAI),3,3,0,D(28))
!     CALL GMMATS (AMT(1),3,3,0,D(IAI),3,3,0,D(37))
!     DO 530 J = 1,9
!     AKT(J) = D(J+27)
! 530 AMT(J) = D(J+36)
!
!     NOW INSERT  AKT AND AMT INTO THE OVERALL STIFFNESS MATRICES
!     ACCORDING TO INCREASING SIL VALUE
!
               DO ij = 1 , 3
                  DO jj = 1 , 3
                     ki = (ij-1)*3 + jj
                     iout = (ipp-1)*27 + (i-1)*3 + (ij-1)*9 + jj
                     akjm(iout) = amt(ki)
                  ENDDO
               ENDDO
               DO ij = 1 , 4
                  DO jj = 1 , 4
                     ki = (ij-1)*4 + jj
                     iout = (ipp-1)*48 + (i-1)*4 + (ij-1)*12 + jj
                     akj(iout) = akt(ki)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
!
!     NOW OUTPUT THE MATRIX VIA EMG OUT
!
         dict(2) = 1
         IF ( Ismb(1)/=0 ) CALL emgout(akj,akj,144,1,dict,1,ipr)
         IF ( Ismb(2)==0 .AND. .NOT.pzmat ) Ksys78 = ksave
         IF ( Ismb(2)==0 ) RETURN
         dict(3) = 9
         dict(4) = 7
         IF ( Icmbar<0 ) THEN
!
!     GENERATE LUMPED MASS MATRIX HERE
!
            DO i = 1 , 9
               akjm(i) = convm/3.0
            ENDDO
            dict(2) = 2
            CALL emgout(akjm,akjm,9,1,dict,2,ipr)
         ELSE
            CALL emgout(akjm,akjm,81,1,dict,2,ipr)
         ENDIF
         EXIT
      ENDIF
   ENDDO
 300  IF ( .NOT.pzmat ) Ksys78 = ksave
   RETURN
 400  IF ( idel1/=Idel ) THEN
      idel2 = idel1
      ics(1) = idel1
      ics(2) = jax
      CALL mesage(30,i,ics)
   ENDIF
   Nogo = .TRUE.
   GOTO 300
END SUBROUTINE triaax
