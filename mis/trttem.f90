
SUBROUTINE trttem(Ti,Pg)
   IMPLICIT NONE
   REAL Alf(3) , Anu(3) , Consts(5) , Costh , Degrad , E(3) , Ecpt(34) , Eltemp , G(3) , Gsube , Pi , Rho , Setmat , Sinth ,        &
      & Stress , Tzero
   INTEGER Iecpt(34) , Matflg , Matidc , Moskp(9)
   COMMON /condas/ Consts
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero , Gsube , Moskp , Setmat
   COMMON /trimex/ Ecpt
   REAL Pg(1) , Ti(3)
   REAL aa , ajho , c1 , c2 , c2s2 , c3 , c4 , cosg , d(3) , del , delint(12) , dgama , dgamr , dtt(9) , ee01 , ee02 , ee03 , ee04 ,&
      & ee08 , ee09 , ee10 , ee15 , ee16 , er , et , ez , fij(9) , gababq(9,9) , gor , grz , gzo , r(3) , r1 , r2 , r3 , s2 , s4 ,  &
      & sing , t , t1 , t2 , t3 , t4 , tempe , teo(21) , tl(9) , vor , voz , vro , vrz , vzo , vzr , z(3) , z1 , z2 , z3 , zmin
   REAL ais
   INTEGER i , ics(3) , idel , igp(3) , j , k , l , matid
!****
! THIS ROUTINE COMPUTES THE THERMAL LOAD FOR THE ASSYMMETRIC RING ELE
! WITH A TRIANGULAR CROSS SECTION
!****
! ECPT (01) = ELEMENT ID                         I
! ECPT (02) = SIL A                              I
! ECPT (03) = SIL B                              I
! ECPT (04) = SIL C                              I
! ECPT (05) = MATERIAL ORIENTATION ANGLE(DEGREES)R
! ECPT (07) = MATERIAL ID                        I
! ECPT (08) TO  ECPT (21) = PHI                  R
! ECPT (22) = COOR. SYS. FOR GRID POINT A        I
! ECPT (23) = R-CORD OF GRID A                   R
! ECPT (24) = Z-CORD OF GRID A                   R
! ECPT (25) = 0.0                                R
! ECPT (26) = CORD. SYS. GRID POINT B (NOT USED) I
! ECPT (27) = R-CORD OF GRID B                   R
! ECPT (28) = Z-CORD OF GRID B                   R
! ECPT (29) = 0.0                                R
! ECPT (30) = CORD. SYS. GRID POINT C (NOT USED) I
! ECPT (31) = R-CORD OF GRID C                   R
! ECPT (32) = Z-CORD OF GRID C                   R
! ECPT (33) = 0.0                                R
! ECPT (34) = EL. TEMPERATURE FOR MATERIAL       R
!
!
!  . ECPT COMMON BLOCK
!
!  . MATERIAL INPUT AND OUTPUT...
!
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1)) , (z(1),z1) , (z(2),z2) , (z(3),z3) , (r(1),r1) , (r(2),r2) , (r(3),r3)
   !>>>>EQUIVALENCE (Consts(1),Pi) , (Consts(4),Degrad)
!
! START EXECUTION
!
! STORE ECPT PARAMETERS IN LOCAL VARIABLES
   idel = Iecpt(1)
   igp(1) = Iecpt(2)
   igp(2) = Iecpt(3)
   igp(3) = Iecpt(4)
   matid = Iecpt(07)
   ics(1) = Iecpt(22)
   ics(2) = Iecpt(26)
   ics(3) = Iecpt(30)
   r(1) = Ecpt(23)
   r(2) = Ecpt(27)
   r(3) = Ecpt(31)
   z(2) = Ecpt(28)
   d(2) = Ecpt(29)
   z(1) = Ecpt(24)
   d(1) = Ecpt(25)
   z(3) = Ecpt(32)
   d(3) = Ecpt(33)
   dgama = Ecpt(05)
   tempe = Ecpt(34)
!
! COMPUTE THE ELEMENT COORDINATES
   zmin = amin1(z1,z2,z3)
   z1 = z1 - zmin
   z2 = z2 - zmin
   z3 = z3 - zmin
!
! FORM THE TRANSFORMATION MATRIX GABABQ (9X9) FROM FIELD COORDINATES TO
! GRID POINT DEGREES OF FREEDOM
   DO i = 1 , 9
      DO j = 1 , 9
         gababq(i,j) = 0.000
      ENDDO
   ENDDO
   aa = r2*z3 + r1*z2 + z1*r3 - z2*r3 - r1*z3 - r2*z1
   aa = 1.0E0/aa
   c1 = aa*(r2*z3-z2*r3)
   c2 = -aa*(z3-z2)
   c3 = aa*(r3-r2)
   gababq(1,1) = c1
   gababq(2,4) = c1
   gababq(3,7) = c1
   gababq(1,2) = c2
   gababq(2,5) = c2
   gababq(3,8) = c2
   gababq(1,3) = c3
   gababq(2,6) = c3
   gababq(3,9) = c3
   c1 = -aa*(r1*z3-z1*r3)
   c2 = aa*(z3-z1)
   c3 = -aa*(r3-r1)
   gababq(4,1) = c1
   gababq(4,2) = c2
   gababq(4,3) = c3
   gababq(5,4) = c1
   gababq(5,5) = c2
   gababq(5,6) = c3
   gababq(6,7) = c1
   gababq(6,8) = c2
   gababq(6,9) = c3
   c1 = aa*(r1*z2-z1*r2)
   c2 = -aa*(z2-z1)
   c3 = aa*(r2-r1)
   gababq(7,1) = c1
   gababq(7,2) = c2
   gababq(7,3) = c3
   gababq(8,4) = c1
   gababq(8,5) = c2
   gababq(8,6) = c3
   gababq(9,7) = c1
   gababq(9,8) = c2
   gababq(9,9) = c3
!
! LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3
   dgamr = dgama*Degrad
   cosg = cos(dgamr)
   sing = sin(dgamr)
   Costh = cosg
   Sinth = sing
   Matidc = matid
   Matflg = 7
   Eltemp = tempe
   CALL mat(idel)
   IF ( Setmat==2.0 ) THEN
      CALL mesage(-30,37,Ecpt(1))
   ELSE
!
!  . SET MATERIAL PROPERTIES IN LOCAL VARIABLES...
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
      del = 1.0E0/(1.0E0-vro*vor-voz*vzo-vzr*vrz-vro*voz*vzr-vrz*vor*vzo)
!
! COMPUTE ELASTIC CONSTANTS MATRIX FROM MATERIAL TO ELEMENT AXIS
      DO i = 1 , 21
         teo(i) = 0.0E0
      ENDDO
      teo(1) = er*(1.0E0-voz*vzo)*del
      teo(2) = er*(vzr+vzo*vor)*del
      teo(3) = ez*(1.0E0-vro*vor)*del
      teo(4) = er*(vor+vzr*voz)*del
      teo(5) = et*(vzo+vro*vzr)*del
      teo(6) = et*(1.0E0-vrz*vzr)*del
      teo(10) = grz
      teo(15) = gor
      teo(21) = gzo
      c2 = cosg*cosg
      c4 = c2*c2
      s2 = sing*sing
      s4 = s2*s2
      c2s2 = c2*s2
      ee01 = teo(1)*c4 + teo(3)*s4 + (teo(2)+2.0E0*teo(10))*2.0E0*c2s2
      ee02 = teo(2)*(s4+c4) + (teo(1)+teo(3)-4.0E0*teo(10))*c2s2
      ee03 = teo(4)*c2 + teo(5)*s2
      ee04 = sing*cosg*(teo(1)*c2-teo(3)*s2+(teo(2)+2.0E0*teo(10))*(s2-c2))
      ee08 = teo(1)*s4 + teo(3)*c4 + (2.0E0*teo(2)+4.0E0*teo(10))*c2s2
      ee09 = teo(4)*s2 + teo(5)*c2
      ee10 = sing*cosg*(teo(1)*s2-teo(3)*c2+(teo(2)+2.0E0*teo(10))*(c2-s2))
      ee15 = teo(6)
      ee16 = sing*cosg*(teo(4)-teo(5))
!
! COMPUTE HARMONIC COEFFICIENT
      ajho = Iecpt(1) - (Iecpt(1)/1000)*1000 - 1
!
!  . CALCULATE THE INTEGRAL VALUES IN DELINT...
!
!         DELINT(4) = 0,0
!         DELINT(5) = 0,1
!         DELINT(6) = 1,0
!
      delint(4) = ais(3,0,0,r,z)
      delint(5) = ais(3,0,1,r,z)
      delint(6) = ais(3,1,0,r,z)
!
      t1 = ee01*Alf(1) + ee02*Alf(3) + ee03*Alf(2)
      t2 = ee02*Alf(1) + ee08*Alf(3) + ee09*Alf(2)
      t3 = ee03*Alf(1) + ee09*Alf(3) + ee15*Alf(2)
      t4 = ee04*Alf(1) + ee10*Alf(3) + ee16*Alf(2)
! GENERATE DTT MATRIX
      dtt(1) = delint(4)*t3
      dtt(2) = delint(6)*(t1+t3)
      dtt(3) = delint(5)*t3 + delint(6)*t4
      dtt(4) = ajho*dtt(1)
      dtt(5) = ajho*delint(6)*t3
      dtt(6) = ajho*delint(5)*t3
      dtt(7) = 0.0
      dtt(8) = delint(6)*t4
      dtt(9) = delint(6)*t2
!
! TRANSFORM THE THERMAL LOAD TO GRID POINT DEGREES OF FREEDOM
      CALL gmmats(gababq,9,9,1,dtt,9,1,0,fij)
      t = Tzero
      IF ( ajho>0.0 ) t = 0.0
      t = ((Ti(1)+Ti(2)+Ti(3))/3.0E0-t)*Pi
      IF ( ajho==0.0 ) t = t*2.0E0
      DO i = 1 , 9
         tl(i) = t*fij(i)
      ENDDO
!
!**** THE FOLLOWING CODE REMOVED.  CORD.SYS. NOT POSSIBLE WITH RINGAX **
!.959 FIJ(I) = T*FIJ(I)
!.
!. LOCATE THE TRANSFORMATION MATRICES FOR THE THREE GRID POINTS
!.    DO 750 I=1,81
!.750 AKI(I) = 0.0
!.     DO 800 I = 1,3
!.    CALL GBTRAN(ICS(I),IECPT(4*I+22),DTT(1))  **R,TH,Z NEEDED**
!.    K=30*(I-1) + 1
!.     DO 800 J=1,3
!.    KK = K+9*(J-1)
!.    JJ=3*(J-1)+1
!.    AKI(KK) = DTT(JJ)
!.    AKI(KK+1) = DTT(JJ+1)
!.    AKI(KK+2) = DTT(JJ+2)
!.800 CONTINUE
!.
!. TRANSFORM THE THERMAL LOAD FROM BASIC TO LOCAL COORD...
!.    CALL GMMATS (AKI(1),9,9,1, FIJ(1),9,1,0, TL(1))
!
! ADD THE ELEMENT THERMAL LOAD TO THE STRUCTURE THERMAL LOAD
      k = 0
      DO i = 1 , 3
         l = igp(i) - 1
         DO j = 1 , 3
            k = k + 1
            l = l + 1
            Pg(l) = Pg(l) + tl(k)
         ENDDO
      ENDDO
   ENDIF
END SUBROUTINE trttem