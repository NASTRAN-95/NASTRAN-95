
SUBROUTINE triaad
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION Acurl(117) , Acurp1(27) , Acurp2(9) , Aki(81) , Akj(144) , Akt(16) , Consts(5) , Degrad , Gababq(9,9) , Pi ,    &
                  & Teo(45) , Twopi
   REAL Alf(3) , Anu(3) , Costh , Dgama , Dm1 , Dum(14) , Dxx , E(3) , Ecpt(34) , Eltemp , G(3) , Gsube , Pzout(51) , R1 , R2 , R3 ,&
      & Rho , Setmat , Sinth , Spa(14) , Stress , Tempe , Tzero , Zer , Zer2 , Zer3 , Zz1 , Zz2 , Zz3
   INTEGER Elid , Estid , Icmbar , Ics1 , Ics2 , Ics3 , Idel , Iecpt(34) , Igp(3) , Iprec , Ismb(3) , Ixtra , Ksys78 , Ksystm(77) , &
         & Ldict , Matflg , Matid , Matidc , Moskp(9) , Ngrids
   LOGICAL Heat , Nogo
   COMMON /condad/ Consts
   COMMON /emgdic/ Dxx , Ldict , Ngrids , Elid , Estid
   COMMON /emgest/ Idel , Igp , Dgama , Dm1 , Matid , Spa , Ics1 , R1 , Zz1 , Zer , Ics2 , R2 , Zz2 , Zer2 , Ics3 , R3 , Zz3 ,      &
                 & Zer3 , Tempe
   COMMON /emgprm/ Ixtra , Dum , Ismb , Iprec , Nogo , Heat , Icmbar
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero , Gsube , Moskp , Setmat
   COMMON /matpz / Pzout
   COMMON /system/ Ksystm , Ksys78
   COMMON /triaxx/ Aki , Akt , Acurl , Akj , Teo
!
! Local variable declarations
!
   DOUBLE PRECISION aa , ajho , ajjho , ak(81) , akip(9) , akjm(81) , akm(81) , akph2(9) , akuph(27) , amt(9) , area , bmass(9,9) , &
                  & c1 , c2 , c2s2 , c3 , c4 , convm , cosg , d(81) , d1(27) , d2(9) , del , delint(12) , delm(12) , dgamar ,       &
                  & dgamr , ee(63) , er , et , ez , gababp(3,3) , gor , grz , gzo , r(3) , rhod , s2 , s4 , sing , vor , voz , vro ,&
                  & vrz , vzo , vzr , z(3) , z1 , z2 , z3 , za , zmin
   REAL cs , cs2 , dict5 , dr , ra , rh , s3 , sc2
   INTEGER dict(11) , i , i1 , ib , ic , ic1 , ics(3) , idel1 , idel2 , ii , ij , iout , ip , ipp , ipr , iq , ir1 , irc , ircc ,   &
         & isil , isort(3) , j , j1 , jax , jj , k , ki , ksave , l , m , mjho , n
   DOUBLE PRECISION dkl
   LOGICAL lsys78 , pzmat
!
! End of declarations
!
!
!     THIS SUBROUTINE COMPUTES THE STIFFNESS AND MASS MATRICES FOR THE
!     ASSYMMETRIC RING WITH A TRIANGULAR CROSS SECTION, TO BE USED BY
!     THE ELEMENT MATRIX GENERATOR.
!
!     DOUBLE PRECISION VERSION
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
!     COMMON /MATPZ / CE11,CE12,CE13,CE14,CE15,CE16,CE22,CE23,CE24,CE25,
!                     CE26,CE33,CE34,CE35,CE36,CE44,CE45,CE46,CE55,CE56,
!                     CE66,E11,E12,E13,E14,E15,E16,E21,E22,E23,E24,E25,
!                     E26,E31,E32,E33,E34,E35,E36,EPS11,EPS12,EPS13,
!                     EPS22,EPS23,EPS33,RHO,A1,A2,A12,TREF,GE
!
   EQUIVALENCE (Iecpt(1),Ecpt(1),Idel) , (dict(5),dict5) , (z(1),z1) , (z(2),z2) , (z(3),z3) , (Aki(1),Gababq(1,1)) ,               &
    & (bmass(1,1),akm(1)) , (Consts(1),Pi) , (Consts(4),Degrad) , (Consts(2),Twopi) , (akip(1),gababp(1,1)) , (Acurp1(1),Acurl(82)) &
    & , (Acurp2(1),Acurl(109))
   DATA idel2 , jax/0 , 4HTRIA/
!
   lsys78 = .FALSE.
   IF ( Ksys78==0 .OR. Ksys78==2 ) lsys78 = .TRUE.
   idel1 = Idel/1000
!
!     INITALIZE
!
   DO i = 1 , 403
      Aki(i) = 0.D0
   ENDDO
   DO i = 1 , 3
      r(i) = Ecpt(4*i+19)
      z(i) = Ecpt(4*i+20)
      ics(i) = Iecpt(4*i+18)
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
      zmin = dmin1(z1,z2,z3)
      z1 = z1 - zmin
      z2 = z2 - zmin
      z3 = z3 - zmin
!
!     FORM TRANSFORMATION MATRIX GABABQ (9X9) FROM FIELD COORDINATES TO
!     GRID POINT DEGREES OF FREEDOM
!
      DO i = 1 , 9
         DO j = 1 , 9
            Gababq(i,j) = 0.D0
         ENDDO
      ENDDO
!
      aa = 1.D0/(R2*z3+R1*z2+z1*R3-z2*R3-R1*z3-R2*z1)
      c1 = aa*(R2*z3-z2*R3)
      c2 = -aa*(z3-z2)
      c3 = aa*(R3-R2)
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
      c1 = -aa*(R1*z3-z1*R3)
      c2 = aa*(z3-z1)
      c3 = -aa*(R3-R1)
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
      c1 = aa*(R1*z2-z1*R2)
      c2 = -aa*(z2-z1)
      c3 = aa*(R2-R1)
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
!     THE FOLLOWING TABLE
!
!     DELINT(01) = (-1,0)
!     DELINT(02) = (-1,1)
!     DELINT(03) = (-1,2)
!     DELINT(04) = ( 0,0)
!     DELINT(05) = ( 0,1)
!     DELINT(06) = ( 1,0)
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
         za = (z1+z2+z3)/3.0D0
         rh = amin1(R1,R2,R3)/10.0
         dr = amax1(abs(R1-R2),abs(R2-R3),abs(R3-R1))
         area = (R1*(z2-z3)+R2*(z3-z1)+R3*(z1-z2))/2.0D0
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
                  delint(i1) = dkl(3,ip,iq,r,z)
               ELSE
                  delint(i1) = ((ra**ip)*(za**iq))*area
               ENDIF
               delint(i1) = dabs(delint(i1))
            ENDDO
         ENDDO
!
!     MASS MATRIX
!
         IF ( Ismb(2)==0 ) GOTO 100
      ENDIF
      CALL deltkl(Akj,r,z,0)
      delm(1) = Akj(2)
      delm(2) = Akj(7)
      delm(3) = Akj(8)
      delm(4) = Akj(10)
      delm(5) = Akj(9)
      delm(7) = Akj(12)
   ENDIF
!
!     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3
!
 100  dgamr = dble(Dgama)*Degrad
   cosg = dcos(dgamr)
   sing = dsin(dgamr)
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
         del = 1.D0/(1.D0-vro*vor-voz*vzo-vzr*vrz-vro*voz*vzr-vrz*vor*vzo)
      ENDIF
!
!     COMPUTE ELASTIC CONSTANTS MATRIX FROM MATERIAL TO ELEMENT AXIS
!
      DO i = 1 , 45
         Teo(i) = 0.D0
      ENDDO
!
      IF ( Ksys78>0 ) THEN
!
!     PIEZOELECTRIC MATERIAL PROPERTIES STORED IN TEO(22-39)
!     DIELECTRIC MATERIAL PROPERTIES STORED IN TEO(40-45)
!     TEO(22-39) CONTAINS E-TRANSPOSE
!
         Teo(1) = Pzout(1)
         Teo(2) = Pzout(2)
         Teo(3) = Pzout(7)
         Teo(4) = Pzout(3)
         Teo(5) = Pzout(8)
         Teo(6) = Pzout(12)
         Teo(7) = Pzout(4)
         Teo(8) = Pzout(9)
         Teo(9) = Pzout(13)
         Teo(10) = Pzout(16)
         Teo(11) = Pzout(5)
         Teo(12) = Pzout(10)
         Teo(13) = Pzout(14)
         Teo(14) = Pzout(17)
         Teo(15) = Pzout(19)
         Teo(16) = Pzout(6)
         Teo(17) = Pzout(11)
         Teo(18) = Pzout(15)
         Teo(19) = Pzout(18)
         Teo(20) = Pzout(20)
         Teo(21) = Pzout(21)
         IF ( Ksys78/=2 ) THEN
            Teo(22) = Pzout(22)
            Teo(23) = Pzout(28)
            Teo(24) = Pzout(34)
            Teo(25) = Pzout(23)
            Teo(26) = Pzout(29)
            Teo(27) = Pzout(35)
            Teo(28) = Pzout(24)
            Teo(29) = Pzout(30)
            Teo(30) = Pzout(36)
            Teo(31) = Pzout(25)
            Teo(32) = Pzout(31)
            Teo(33) = Pzout(37)
            Teo(34) = Pzout(26)
            Teo(35) = Pzout(32)
            Teo(36) = Pzout(38)
            Teo(37) = Pzout(27)
            Teo(38) = Pzout(33)
            Teo(39) = Pzout(39)
            Teo(40) = -Pzout(40)
            Teo(41) = -Pzout(41)
            Teo(42) = -Pzout(42)
            Teo(43) = -Pzout(43)
            Teo(44) = -Pzout(44)
            Teo(45) = -Pzout(45)
         ENDIF
      ELSE
         Teo(1) = er*(1.-voz*vzo)*del
         Teo(2) = er*(vzr+vzo*vor)*del
         Teo(3) = ez*(1.-vro*vor)*del
         Teo(4) = er*(vor+vzr*voz)*del
         Teo(5) = et*(vzo+vro*vzr)*del
         Teo(6) = et*(1.-vrz*vzr)*del
         Teo(10) = grz
         Teo(15) = gor
         Teo(21) = gzo
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
      ee(1) = Teo(1)*c4 + Teo(3)*s4 + 2.*c2s2*(Teo(2)+2.*Teo(10))
      ee(2) = Teo(2)*(c4+s4) + c2s2*(Teo(1)+Teo(3)-4.*Teo(10))
      ee(3) = Teo(4)*c2 + Teo(5)*s2
      ee(4) = cosg*sing*s2*(Teo(2)-Teo(3)+2.*Teo(10)) + sing*cosg*c2*(Teo(1)-Teo(2)-2.*Teo(10))
      ee(7) = ee(2)
      ee(8) = Teo(1)*s4 + 2.*c2s2*(Teo(2)+2.*Teo(10)) + Teo(3)*c4
      ee(9) = Teo(4)*s2 + Teo(5)*c2
      ee(10) = sing*cosg*c2*(Teo(2)-Teo(3)+2.*Teo(10)) + cosg*sing*s2*(Teo(1)-Teo(2)-2.*Teo(10))
      ee(13) = ee(3)
      ee(14) = ee(9)
      ee(15) = Teo(6)
      ee(16) = sing*cosg*(Teo(4)-Teo(5))
      ee(19) = ee(4)
      ee(20) = ee(10)
      ee(21) = ee(16)
      ee(22) = c2s2*(Teo(1)-2.*Teo(2)+Teo(3)) + Teo(10)*(c2-s2)**2
      ee(29) = Teo(15)*c2 + Teo(21)*s2
      ee(30) = sing*cosg*(Teo(15)-Teo(21))
      ee(35) = ee(30)
      ee(36) = Teo(15)*s2 + Teo(21)*c2
!
      IF ( .NOT.(lsys78) ) THEN
!
!     PIEZOELECTRIC MATERIAL PROPERTIES IN ELEMENT COORDINATES
!
         ee(37) = c3*Teo(22) - s3*Teo(26) + cs2*(Teo(25)+2.0*Teo(32)) - sc2*(Teo(23)+2.0*Teo(31))
         ee(38) = c3*Teo(23) + s3*Teo(25) + cs2*(Teo(26)-2.0*Teo(31)) + sc2*(Teo(22)-2.0*Teo(32))
         ee(39) = s2*Teo(27) + c2*Teo(24) - 2.0*cs*Teo(33)
         ee(40) = c3*Teo(25) - s3*Teo(23) + cs2*(Teo(22)-2.0*Teo(32)) - sc2*(Teo(26)-2.0*Teo(31))
         ee(41) = c3*Teo(26) + s3*Teo(22) + cs2*(Teo(23)+2.0*Teo(31)) + sc2*(Teo(25)+2.0*Teo(32))
         ee(42) = s2*Teo(24) + c2*Teo(27) + 2.0*cs*Teo(33)
         ee(43) = cosg*Teo(28) - sing*Teo(29)
         ee(44) = cosg*Teo(29) + sing*Teo(28)
         ee(45) = Teo(30)
         ee(46) = c3*Teo(31) + s3*Teo(32) - cs2*(Teo(23)-Teo(26)+Teo(31)) + sc2*(-Teo(32)-Teo(25)+Teo(22))
         ee(47) = c3*Teo(32) - s3*Teo(31) - cs2*(Teo(25)-Teo(22)+Teo(32)) + sc2*(Teo(23)+Teo(31)-Teo(26))
         ee(48) = (c2-s2)*Teo(33) + cs*(Teo(24)-Teo(27))
         ee(49) = c2*Teo(34) + s2*Teo(38) - cs*(Teo(35)+Teo(37))
         ee(50) = c2*Teo(35) - s2*Teo(37) + cs*(Teo(34)-Teo(38))
         ee(51) = cosg*Teo(36) - sing*Teo(39)
         ee(52) = c2*Teo(37) - s2*Teo(35) - cs*(Teo(38)-Teo(34))
         ee(53) = c2*Teo(38) + s2*Teo(34) + cs*(Teo(35)+Teo(37))
         ee(54) = cosg*Teo(39) + sing*Teo(36)
!
!     DIELECTRIC MATERIAL PROPERTIES IN ELEMENT COORDINTES
!
         ee(55) = s2*Teo(43) - 2.0*cs*Teo(41) + c2*Teo(40)
         ee(56) = (c2-s2)*Teo(41) - cs*(Teo(43)-Teo(40))
         ee(57) = -sing*Teo(44) + cosg*Teo(42)
         ee(59) = c2*Teo(43) + 2.0*cs*Teo(41) + s2*Teo(40)
         ee(60) = cosg*Teo(44) + sing*Teo(42)
         ee(63) = Teo(45)
      ENDIF
!
!     COMPUTE HARMONIC COEFFICIENT
!
      mjho = mod(Iecpt(1),1000) - 1
      ajho = mjho
      ajjho = ajho*ajho
      rhod = Rho*Pi
      IF ( ajho==0.D0 ) rhod = 2.*rhod
      IF ( Ismb(1)/=0 ) THEN
!
!     FORM THE ELEMENT STIFFNESS MATRIX IN FIELD SYSTEM
!
         Acurl(01) = (ee(15)+ajjho*ee(29))*delint(1)
         Acurl(02) = (ee(03)+ee(15)+ajjho*ee(29))*delint(4)
         Acurl(03) = (ee(15)+ajjho*ee(29))*delint(2) + ee(16)*delint(4)
         Acurl(04) = (ee(15)+ee(29))*ajho*delint(1)
         Acurl(05) = ee(15)*ajho*delint(4)
         Acurl(06) = (ee(15)+ee(29))*ajho*delint(2) - ee(30)*ajho*delint(4)
         Acurl(07) = ajjho*delint(1)*ee(35)
         Acurl(08) = (ee(16)+ajjho*ee(35))*delint(4)
         Acurl(09) = ee(09)*delint(4) + ajjho*delint(2)*ee(35)
         Acurl(11) = (ee(1)+2.*ee(3)+ee(15)+ajjho*ee(29))*delint(6)
         Acurl(12) = (ee(3)+ee(15)+ajjho*ee(29))*delint(5) + (ee(4)+ee(16))*delint(6)
         Acurl(13) = (ee(3)+ee(15)+ee(29))*ajho*delint(4)
         Acurl(14) = (ee(3)+ee(15))*delint(6)*ajho
         Acurl(15) = (ee(3)+ee(15)+ee(29))*ajho*delint(5) - ajho*ee(30)*delint(6)
         Acurl(16) = ajjho*delint(4)*ee(35)
         Acurl(17) = (ee(4)+ee(16)+ajjho*ee(35))*delint(6)
         Acurl(18) = (ee(2)+ee(9))*delint(6) + ajjho*delint(5)*ee(35)
         Acurl(21) = (ee(15)+ajjho*ee(29))*delint(3) + ee(22)*delint(6) + 2.*ee(16)*delint(5)
         Acurl(22) = (ee(15)+ee(29))*ajho*delint(2) + ajho*delint(4)*ee(16)
         Acurl(23) = ee(15)*ajho*delint(5) + ajho*delint(6)*ee(16)
         Acurl(24) = (ee(15)+ee(29))*ajho*delint(3) + (ee(16)-ee(30))*ajho*delint(5)
         Acurl(25) = ajjho*delint(2)*ee(35)
         Acurl(26) = ee(22)*delint(6) + (ee(21)+ajjho*ee(35))*delint(5)
         Acurl(27) = ee(9)*delint(5) + ee(10)*delint(6) + ajjho*delint(3)*ee(35)
         Acurl(31) = (ee(29)+ajjho*ee(15))*delint(1)
         Acurl(32) = ee(15)*ajjho*delint(4)
         Acurl(33) = (ee(29)+ajjho*ee(15))*delint(2) - ee(30)*delint(4)
         Acurl(34) = ajho*delint(1)*ee(35)
         Acurl(35) = ajho*(ee(16)+ee(35))*delint(4)
         Acurl(36) = ee(9)*ajho*delint(4) + ajho*delint(2)*ee(35)
         Acurl(41) = ajjho*delint(06)*ee(15)
         Acurl(42) = ee(15)*ajjho*delint(5)
         Acurl(43) = 0.
         Acurl(44) = ajho*delint(6)*ee(16)
         Acurl(45) = ee(9)*ajho*delint(6)
         Acurl(51) = (ee(29)+ajjho*ee(15))*delint(3) + ee(36)*delint(6) - 2.*ee(35)*delint(5)
         Acurl(52) = ajho*(delint(2)*ee(30)-delint(4)*ee(36))
         Acurl(53) = -ee(36)*ajho*delint(6) + ajho*(ee(16)+ee(35))*delint(5)
         Acurl(54) = (ee(9)-ee(36))*ajho*delint(5) + ajho*delint(3)*ee(35)
         Acurl(61) = ee(36)*ajjho*delint(1)
         Acurl(62) = ee(36)*ajjho*delint(4)
         Acurl(63) = ee(36)*ajjho*delint(2)
         Acurl(71) = (ee(22)+ajjho*ee(36))*delint(6)
         Acurl(72) = ee(36)*ajjho*delint(5) + ee(20)*delint(6)
         Acurl(81) = ee(36)*ajjho*delint(3) + ee(8)*delint(6)
         IF ( .NOT.(lsys78) ) THEN
            Acurl(82) = -(ee(45)-ajho*ee(51))*ajho*delint(1)
            Acurl(83) = (ee(43)-ajho*ee(45)-ajho*ee(49)+ajjho*ee(51))*delint(4)
            Acurl(84) = (ee(44)-ajho*ee(50))*delint(4) - (ee(45)-ajho*ee(51))*ajho*delint(2)
            Acurl(85) = -(ee(39)+ee(45)-ajho*ee(51))*ajho*delint(4)
            Acurl(86) = (ee(37)+ee(43)-(ee(39)+ee(45)+ee(49)-ajho*ee(51))*ajho)*delint(6)
            Acurl(87) = (ee(38)+ee(44)-ajho*ee(50))*delint(6) - (ee(39)+ee(45)-ajho*ee(51))*ajho*delint(5)
            Acurl(88) = -(ee(45)-ajho*ee(51))*ajho*delint(2) - ee(48)*ajho*delint(4)
            Acurl(89) = (ee(43)-ajho*ee(45)-ajho*ee(49)+ajjho*ee(51))*delint(5) + (ee(46)-ee(48)*ajho)*delint(6)
            Acurl(90) = (ee(44)-ajho*ee(48)-ajho*ee(50))*delint(5) + ee(47)*delint(6) - (ee(45)-ajho*ee(51))*ajho*delint(3)
            Acurl(91) = -(ee(45)*ajho-ee(51))*ajho*delint(1)
            Acurl(92) = (ajho*ee(43)-ajjho*ee(45)-ee(49)+ajho*ee(51))*delint(4)
            Acurl(93) = (ee(44)*ajho-ee(50))*delint(4) - (ee(45)*ajho-ee(51))*ajho*delint(2)
            Acurl(94) = -ee(45)*ajjho*delint(4)
            Acurl(95) = (ee(43)-ajho*ee(45))*ajho*delint(6)
            Acurl(96) = ee(44)*ajho*delint(6) - ee(45)*ajjho*delint(5)
            Acurl(97) = -(ee(45)*ajho-ee(51))*ajho*delint(2) - ee(54)*ajho*delint(4)
            Acurl(98) = (ee(43)*ajho-ajjho*ee(45)-ee(49)+ee(51)*ajho)*delint(5) + (ee(52)-ajho*ee(54))*delint(6)
            Acurl(99) = (ee(44)*ajho-ee(50)-ee(54)*ajho)*delint(5) + ee(53)*delint(6) - (ee(45)*ajho-ee(51))*ajho*delint(3)
            Acurl(100) = ee(54)*ajjho*delint(1)
            Acurl(101) = -(ee(52)-ee(54)*ajho)*ajho*delint(4)
            Acurl(102) = -(ee(53)*delint(4)-ee(54)*ajho*delint(2))*ajho
            Acurl(103) = -(ee(48)-ee(54)*ajho)*ajho*delint(4)
            Acurl(104) = (ee(46)-ee(48)*ajho-ee(52)*ajho+ee(54)*ajjho)*delint(6)
            Acurl(105) = (ee(47)-ee(53)*ajho)*delint(6) - (ee(48)-ee(54)*ajho)*ajho*delint(5)
            Acurl(106) = ee(54)*ajjho*delint(2) - ee(42)*ajho*delint(4)
            Acurl(107) = (ee(40)-ee(42)*ajho)*delint(6) - (ee(52)-ee(54)*ajho)*ajho*delint(5)
            Acurl(108) = ee(41)*delint(6) + (-ee(42)-ee(53))*ajho*delint(5) + ee(54)*ajjho*delint(3)
!
            Acurl(109) = ee(63)*ajjho*delint(1)
            Acurl(110) = (-ee(57)+ee(63)*ajho)*ajho*delint(4)
            Acurl(111) = -ee(60)*ajho*delint(4) + ee(63)*ajjho*delint(2)
            Acurl(112) = Acurl(110)
            Acurl(113) = (ee(55)-2.0*ee(57)*ajho+ee(63)*ajjho)*delint(6)
            Acurl(114) = (ee(56)-ee(60)*ajho)*delint(6) + (-ee(57)+ee(63)*ajho)*ajho*delint(5)
            Acurl(115) = Acurl(111)
            Acurl(116) = Acurl(114)
            Acurl(117) = ee(59)*delint(6) - 2.0*ee(60)*ajho*delint(5) + ee(63)*ajjho*delint(3)
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
               Acurl(ic) = Acurl(i)
            ENDDO
         ENDDO
         dgamar = Pi
         IF ( ajho==0.D0 ) dgamar = Twopi
         DO i = 1 , 81
            Acurl(i) = Acurl(i)*dgamar
         ENDDO
         IF ( .NOT.(lsys78) ) THEN
!
            DO i = 82 , 117
               Acurl(i) = Acurl(i)*dgamar
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
            DO i = 1 , 81
               bmass(i,1) = 0.
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
         CALL gmmatd(Aki,9,9,1,Acurl,9,9,0,d)
         CALL gmmatd(d,9,9,0,Aki,9,9,0,ak)
         IF ( .NOT.(lsys78) ) THEN
            CALL gmmatd(Aki,9,9,1,Acurp1,9,3,0,d1)
            CALL gmmatd(d1,9,3,0,akip,3,3,0,akuph)
            CALL gmmatd(akip,3,3,1,Acurp2,3,3,0,d2)
            CALL gmmatd(d2,3,3,0,akip,3,3,0,akph2)
         ENDIF
!
         IF ( Ismb(2)==0 .OR. Icmbar<0 ) GOTO 200
      ENDIF
      CALL gmmatd(Aki,9,9,1,bmass,9,9,0,d)
      CALL gmmatd(d,9,9,0,Aki,9,9,0,akm)
   ENDIF
!
 200  DO i = 1 , 81
      Akj(i) = 0.D0
      akjm(i) = 0.D0
   ENDDO
   DO i = 82 , 117
      Akj(i) = 0.D0
!
   ENDDO
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
                     Akt(j1) = ak(ircc)
                     Akt(j1+1) = ak(ircc+1)
                     Akt(j1+2) = ak(ircc+2)
                     IF ( .NOT.(lsys78) ) THEN
                        m = ircc/3 + 1
                        n = (m-1)/9 + 1 + (ii-1)*9 + (j-1)*3
                        Akt(j1+3) = akuph(m)
                        Akt(j1+15-j*3) = akuph(n)
                        Akt(16) = akph2(ir1+ii-1)
                     ENDIF
                  ENDIF
!
                  IF ( Ismb(2)/=0 .AND. Icmbar>=1 ) THEN
                     j1 = (j-1)*3 + 1
                     amt(j1) = akm(ircc)
                     amt(j1+1) = akm(ircc+1)
                     amt(j1+2) = akm(ircc+2)
                  ENDIF
               ENDDO
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
                     Akj(iout) = Akt(ki)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
!
!     NOW OUTPUT THE MATRIX VIA EMG OUT
!
         dict(2) = 1
         IF ( Ismb(1)/=0 ) CALL emgout(Akj,Akj,144,1,dict,1,ipr)
         IF ( Ismb(2)==0 .AND. .NOT.pzmat ) Ksys78 = ksave
         IF ( Ismb(2)==0 ) RETURN
         dict(3) = 9
         dict(4) = 7
         IF ( Icmbar<0 ) THEN
!
!     GENERATE LUMPED MASS MATRIX HERE
!
            DO i = 1 , 9
               akjm(i) = convm/3.0D0
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
 400  IF ( idel1/=idel2 ) THEN
      idel2 = idel1
      ics(1) = idel1
      ics(2) = jax
      CALL mesage(30,i,ics)
   ENDIF
   Nogo = .TRUE.
   GOTO 300
END SUBROUTINE triaad