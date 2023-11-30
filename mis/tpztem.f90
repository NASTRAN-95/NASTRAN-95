
SUBROUTINE tpztem(Ti,Pg)
   IMPLICIT NONE
   REAL Alf(3) , Anu(3) , Consts(5) , Costh , Degrad , E(3) , Ecpt(39) , Eltemp , G(3) , Gsube , Pi , Rho , Setmat , Sinth ,        &
      & Stress , Tzero
   INTEGER Ibuf , Iecpt(39) , Iout , Matflg , Matidc , Moskp(9)
   COMMON /condas/ Consts
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero , Gsube , Moskp , Setmat
   COMMON /system/ Ibuf , Iout
   COMMON /trimex/ Ecpt
   REAL Pg(1) , Ti(4)
   REAL a , a1 , a2 , a3 , a4 , ajho , aki(144) , cosg , cosg2 , cosg4 , d(144) , dela , delint(15) , dgama , dgamr , ee01 , ee02 , &
      & ee03 , ee04 , ee05 , ee06 , ee07 , ee08 , ee09 , er , et , ez , gababq(12,12) , gor , grz , gzo , h(4,4) , htn(12,4) ,      &
      & r(4) , r1 , r2 , r21a , r3 , r34a , r4 , rmax , rmin , sing , sing2 , sing4 , tempe , teo(21) , tl(12) , vor , voz , vro ,  &
      & vrz , vzo , vzr , z(4) , z1 , z2 , z3 , z4 , zmin
   INTEGER i , i1 , ics(4) , idel , idel1 , idel2 , igp(4) , ip , iq , ising , j , jax , k , l , matid , sp(36)
   REAL rzints
!
!     THIS ROUTINE COMPUTES THE THERMAL LOAD FOR THE AXI-SYMMETRIC
!     TRAPEZOIDAL CROSS SECTION RING
!
!     ECPT COMMON BLOCK IS,
!
!     ECPT( 1) = ELEMENT ID                                I
!     ECPT( 2) = SIL A                                     I
!     ECPT( 3) = SIL B                                     I
!     ECPT( 4) = SIL C                                     I
!     ECPT( 5) = SIL D
!     ECPT( 6) = MATERIAL ORIENTATION ANGLE(DEGREES)       R
!     ECPT( 8) = MATERIAL ID                               I
!     ECPT( 9) TO ECPT (22) FOR PHI
!     ECPT(23) = COOR. SYS. FOR GRID POINT A               I
!     ECPT(24) = X-COOR. OF GRID POINT A (IN BASIC COOR)   R
!     ECPT(25) = Z-COOR. OF GRID POINT A (IN BASIC COOR)   R
!     ECPT(26) = 0.0
!     ECPT(27) = COOR. SYS. FOR GRID POINT B
!     ECPT(28) = X-COOR. OF GRID POINT B (IN BASIC COOR)   R
!     ECPT(29) = Z-COOR. OF GRID POINT B (IN BASIC COOR)   R
!     ECPT(30) = 0.0
!     ECPT(31) = COOR. SYS. FOR GRID POINT C               I
!     ECPT(32) = X-COOR. FOR GRID POINT C                  R
!     ECPT(33) = Z-COOR. FOR GRID POINT C                  R
!     ECPT(34) = 0.0
!     ECPT(35) = COOR. SYS. FOR GRID POINT D               I
!     ECPT(36) = X-COOR FOR GRID POINT D                   R
!     ECPT(37) = Z-COOR FOR GRID POINT D                   R
!     ECPT(38) = 0.0
!     ECPT(39) = EL. TEMPERATURE FOR MATERIAL PROP         R
!
!
!     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1)) , (z(1),z1) , (z(2),z2) , (z(3),z3) , (r(1),r1) , (r(2),r2) , (r(3),r3) , (r(4),r4) , (z(4),z4) , &
!>>>>    & (gababq(1,1),aki(1)) , (Consts(1),Pi) , (Consts(4),Degrad)
   DATA idel2 , jax/0 , 4HAX  /
!
!     START EXECUTION
!
!     STORE ECPT PARAMETERS IN LOCAL VARIABLES
!
   idel = Iecpt(1)
   igp(1) = Iecpt(2)
   igp(2) = Iecpt(3)
   igp(3) = Iecpt(4)
   igp(4) = Iecpt(5)
   matid = Iecpt(8)
   ics(1) = Iecpt(23)
   ics(3) = Iecpt(31)
   ics(2) = Iecpt(27)
   r(1) = Ecpt(24)
   r(2) = Ecpt(28)
   r(3) = Ecpt(32)
   ics(4) = Iecpt(35)
   z(1) = Ecpt(25)
   d(1) = Ecpt(26)
   z(2) = Ecpt(29)
   d(2) = Ecpt(30)
   z(3) = Ecpt(33)
   d(3) = Ecpt(34)
   z(4) = Ecpt(37)
   d(4) = Ecpt(38)
   r(4) = Ecpt(36)
   tempe = Ecpt(39)
   dgama = Ecpt(6)
   idel1 = idel/1000
!
!     COMPUTE THE ELEMENT COORDINATES
!
   zmin = amin1(z1,z2,z3,z4)
   z1 = z1 - zmin
   z2 = z2 - zmin
   z3 = z3 - zmin
   z4 = z4 - zmin
!
!     FATAL IF RATIO OF RADII IS TOO LARGE FOR GUASS QUADRATURE
!
   rmin = amin1(r1,r2,r3,r4)
   rmax = amax1(r1,r2,r3,r4)
   IF ( rmin/=0.0 ) THEN
      IF ( rmax/rmin>10. ) THEN
         i = 218
         j = 30
         IF ( idel1==idel2 ) GOTO 99999
         idel2 = idel1
         sp(2) = jax
         GOTO 100
      ENDIF
   ENDIF
!
   IF ( abs(z1-z2)>.001 ) THEN
!
      i = 37
      j = -30
   ELSEIF ( abs(z3-z4)>.001 ) THEN
      i = 37
      j = -30
   ELSE
      d(5) = (r1+r4)/2.0
      d(6) = (r2+r3)/2.0
      IF ( d(5)/=0.0 ) THEN
         IF ( abs((r1-r4)/d(5))<=.005 ) THEN
            r1 = d(5)
            r4 = d(5)
         ENDIF
      ENDIF
      IF ( d(6)/=0.0 ) THEN
         IF ( abs((r2-r3)/d(6))<=.005 ) THEN
            r2 = d(6)
            r3 = d(6)
         ENDIF
      ENDIF
!
!     FORM THE TRANSFORMMATION MATRIX(12X12) FROM FIELD COOR, TO GRID
!     POINT DEGREES OF FREEDOM
!
      DO i = 1 , 144
         gababq(i,1) = 0.000
      ENDDO
      gababq(1,1) = 1.000
      gababq(2,1) = r1
      gababq(3,1) = z1
      gababq(4,1) = r1*z1
      gababq(5,2) = 1.000
      gababq(6,2) = r1
      gababq(7,2) = z1
      gababq(8,2) = gababq(4,1)
      gababq(9,3) = 1.000
      gababq(10,3) = r1
      gababq(11,3) = z1
      gababq(12,3) = gababq(4,1)
      gababq(1,4) = 1.000
      gababq(2,4) = r2
      gababq(3,4) = z2
      gababq(4,4) = r2*z2
      gababq(5,5) = 1.000
      gababq(6,5) = r2
      gababq(7,5) = z2
      gababq(8,5) = gababq(4,4)
      gababq(9,6) = 1.000
      gababq(10,6) = r2
      gababq(11,6) = z2
      gababq(12,6) = gababq(4,4)
      gababq(1,7) = 1.000
      gababq(2,7) = r3
      gababq(3,7) = z3
      gababq(4,7) = r3*z3
      gababq(5,8) = 1.000
      gababq(6,8) = r3
      gababq(7,8) = z3
      gababq(8,8) = gababq(4,7)
      gababq(9,9) = 1.000
      gababq(10,9) = r3
      gababq(11,9) = z3
      gababq(12,9) = gababq(4,7)
      gababq(1,10) = 1.000
      gababq(2,10) = r4
      gababq(3,10) = z4
      gababq(4,10) = r4*z4
      gababq(5,11) = 1.000
      gababq(6,11) = r4
      gababq(7,11) = z4
      gababq(8,11) = gababq(4,10)
      gababq(9,12) = 1.000
      gababq(10,12) = r4
      gababq(11,12) = z4
      gababq(12,12) = gababq(4,10)
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
      ising = -1
      CALL invers(12,gababq,12,d(10),0,d(11),ising,sp)
!
!     CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT
!
!       DELINT(04) = (0,0)
!       DELINT(05) = (0,1)
!       DELINT(06) = (0,2)
!       DELINT(07) = (1,0)
!       DELINT(08) = (1,1)
!       DELINT(09) = (1,2)
!       DELINT(10) = (2,0)
!       DELINT(11) = (2,1)
!       DELINT(12) = (2,2)
!       DELINT(13) = (3,0)
!       DELINT(14) = (3,1)
!       DELINT(15) = (3,2)
!
      i1 = 3
      DO i = 1 , 4
         ip = i - 1
         DO j = 1 , 3
            iq = j - 1
            i1 = i1 + 1
            delint(i1) = rzints(ip,iq,r,z,4)
         ENDDO
      ENDDO
!
!     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3
!
      Matidc = matid
      Matflg = 7
      Eltemp = tempe
      dgamr = dgama*Degrad
      Sinth = sin(dgamr)
      Costh = cos(dgamr)
      sing = Sinth
      cosg = Costh
      CALL mat(idel)
      IF ( Setmat==2.0 ) THEN
         i = 37
         j = -30
      ELSE
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
         dela = 1.0/(1.0-vro*vor-voz*vzo-vzr*vrz-vro*voz*vzr-vrz*vor*vzo)
!
!     COMPUTE ELASTIC CONSTANTS MATRIX FROM MATERIAL TO ELEMENT AXIS
!
         DO i = 1 , 21
            teo(i) = 0.0
         ENDDO
         teo(1) = er*(1.0-voz*vzo)*dela
         teo(2) = er*(vzr+vzo*vor)*dela
         teo(3) = ez*(1.0-vro*vor)*dela
         teo(4) = er*(vor+vzr*voz)*dela
         teo(5) = et*(vzo+vro*vzr)*dela
         teo(6) = et*(1.0-vrz*vzr)*dela
         teo(10) = grz
         teo(15) = gor
         teo(21) = gzo
         sing2 = sing**2
         cosg2 = cosg**2
         sing4 = sing**4
         cosg4 = cosg**4
         ee01 = teo(1)*cosg4 + teo(3)*sing4 + (teo(2)+2.0*teo(10))*2.0*sing2*cosg2
         ee02 = teo(2)*(sing4+cosg4) + (teo(1)+teo(3)-4.0*teo(10))*sing2*cosg2
         ee03 = teo(1)*sing4 + teo(3)*cosg4 + (2.0*teo(2)+4.0*teo(10))*sing2*cosg2
         ee04 = teo(4)*cosg2 + teo(5)*sing2
         ee05 = teo(4)*sing2 + teo(5)*cosg2
         ee06 = teo(6)
         ee07 = (teo(1)*cosg2-teo(3)*sing2+(teo(2)+2.0*teo(10))*(sing2-cosg2))*sing*cosg
         ee08 = (teo(1)*sing2-teo(3)*cosg2+(teo(2)+2.0*teo(10))*(cosg2-sing2))*sing*cosg
         ee09 = sing*cosg*(teo(4)-teo(5))
!
!     COMPUTE HARMONIC COEFFICIENT
!
         ajho = Iecpt(1) - (Iecpt(1)/1000)*1000 - 1
!
!     COMPUTE THERMAL LOAD
!
         a1 = ee01*Alf(1) + ee02*Alf(3) + ee04*Alf(2)
         a2 = ee02*Alf(1) + ee03*Alf(3) + ee05*Alf(2)
         a3 = ee04*Alf(1) + ee05*Alf(3) + ee06*Alf(2)
         a4 = ee07*Alf(1) + ee08*Alf(3) + ee09*Alf(2)
!
!     FORM HTN MATRIX
!
         htn(1,1) = a3*delint(4)
         htn(1,2) = a3*delint(7)
         htn(1,3) = a3*delint(5)
         htn(1,4) = a3*delint(8)
         htn(2,1) = (a1+a3)*delint(7)
         htn(2,2) = (a1+a3)*delint(10)
         htn(2,3) = (a1+a3)*delint(8)
         htn(2,4) = (a1+a3)*delint(11)
         htn(3,1) = a3*delint(5) + a4*delint(7)
         htn(3,2) = a3*delint(8) + a4*delint(10)
         htn(3,3) = a3*delint(6) + a4*delint(8)
         htn(3,4) = a3*delint(9) + a4*delint(11)
         htn(4,1) = (a1+a3)*delint(8) + a4*delint(10)
         htn(4,2) = (a1+a3)*delint(11) + a4*delint(13)
         htn(4,3) = (a1+a3)*delint(9) + a4*delint(11)
         htn(4,4) = (a1+a3)*delint(12) + a4*delint(14)
         htn(5,1) = ajho*a3*delint(4)
         htn(5,2) = ajho*a3*delint(7)
         htn(5,3) = ajho*a3*delint(5)
         htn(5,4) = ajho*a3*delint(8)
         htn(6,1) = ajho*a3*delint(7)
         htn(6,2) = ajho*a3*delint(10)
         htn(6,3) = ajho*a3*delint(8)
         htn(6,4) = ajho*a3*delint(11)
         htn(7,1) = ajho*a3*delint(5)
         htn(7,2) = ajho*a3*delint(8)
         htn(7,3) = ajho*a3*delint(6)
         htn(7,4) = ajho*a3*delint(9)
         htn(8,1) = ajho*a3*delint(8)
         htn(8,2) = ajho*a3*delint(11)
         htn(8,3) = ajho*a3*delint(9)
         htn(8,4) = ajho*a3*delint(12)
         htn(9,1) = 0.0
         htn(9,2) = 0.0
         htn(9,3) = 0.0
         htn(9,4) = 0.0
         htn(10,1) = a4*delint(7)
         htn(10,2) = a4*delint(10)
         htn(10,3) = a4*delint(8)
         htn(10,4) = a4*delint(11)
         htn(11,1) = a2*delint(7)
         htn(11,2) = a2*delint(10)
         htn(11,3) = a2*delint(8)
         htn(11,4) = a2*delint(11)
         htn(12,1) = a2*delint(10) + a4*delint(8)
         htn(12,2) = a2*delint(13) + a4*delint(11)
         htn(12,3) = a2*delint(11) + a4*delint(9)
         htn(12,4) = a2*delint(14) + a4*delint(12)
!
!     COMPUTE LITTLE H MATRIX (INVERSE OF PARTITION OF GABABQ)
!
         IF ( abs(r2-r1)>=1.0E-16 ) THEN
            IF ( abs(r3-r4)>=1.0E-16 ) THEN
               IF ( abs(z4-z1)>=1.0E-16 ) THEN
                  a = 1.0/((r2-r1)*(r3-r4)*(z4-z1))
                  r34a = a*(r3-r4)
                  r21a = a*(r2-r1)
                  h(1,1) = r34a*r2*z4
                  h(1,2) = -r1*z4*r34a
                  h(1,3) = r4*z1*r21a
                  h(1,4) = -r3*z1*r21a
                  h(2,1) = -z4*r34a
                  h(2,2) = z4*r34a
                  h(2,3) = -z1*r21a
                  h(2,4) = z1*r21a
                  h(3,1) = -r2*a*(r2-r4)
                  h(3,2) = r1*r34a
                  h(3,3) = -r4*r21a
                  h(3,4) = r3*r21a
                  h(4,1) = r34a
                  h(4,2) = -r34a
                  h(4,3) = r21a
                  h(4,4) = -r21a
!
!     COMPUTE TI
!
                  dgamr = Tzero
                  IF ( ajho>0.0 ) dgamr = 0.0
                  DO i = 1 , 4
                     Ti(i) = Ti(i) - dgamr
                  ENDDO
!
!     COMPUTE THE THEMAL LOAD IN FIELD COORDINATES
!
                  CALL gmmats(h,4,4,1,Ti(1),4,1,0,tl(1))
                  CALL gmmats(htn,4,12,1,tl(1),4,1,0,d(1))
!
!     TRANSFORM THE THERMAL LOAD TO GRID POINT DEGREES OF FREEDOM
!     ***  COORDINATE SYSTEMS NOT POSSIBLE  *******
!     ***  WITH RINGAX.  THE FOLLOWING CODE WILL IMPLEMENT IT. LRK ***
!     ***  THE FOLLOWING GMMATS HAS D(20) INSTEAD OF TL(1)        ****
!
                  CALL gmmats(gababq,12,12,1,d(1),12,1,0,tl(1))
!
!     LOCATE THE TRANSFORMATION MATRICES FOR THE THREE GRID POINTS
!.    DO 750 I = 1,144
!.    AKI (I) = 0.0
!.750 CONTINUE
!.    DO 800 I = 1,4
!.    CALL GBTRAN (ICS(I),IECPT(4*I+20),D)   $ THIS IS WRONG ANYWAY
!.    K = 39*(I-1) + 1
!.    DO 800 J = 1,3
!.    KK = K + 12*(J-1)
!.    JJ = 3*(J-1) + 1
!.    AKI(KK  ) = D(JJ  )
!.    AKI(KK+1) = D(JJ+1)
!.    AKI(KK+2) = D(JJ+2)
!.800 CONTINUE
!
!     ADD THE ELEMENT THERMAL LOAD TO THE STRUCTURE THERMAL LOAD
!
!.    CALL GMMATS ( AKI(1), 12, 12, 1, D(20), 12, 1, 0, TL(1) )
!
                  dgamr = Pi
                  IF ( ajho==0.0 ) dgamr = 2.0*Pi
!
                  DO i = 1 , 12
                     tl(i) = dgamr*tl(i)
                  ENDDO
!
                  k = 0
                  DO i = 1 , 4
                     l = igp(i) - 1
                     DO j = 1 , 3
                        k = k + 1
                        l = l + 1
                        Pg(l) = Pg(l) + tl(k)
                     ENDDO
                  ENDDO
                  GOTO 99999
               ENDIF
            ENDIF
         ENDIF
         i = 31
         j = -30
      ENDIF
   ENDIF
 100  sp(1) = idel1
   CALL mesage(j,i,sp)
99999 RETURN
END SUBROUTINE tpztem