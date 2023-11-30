
SUBROUTINE strap1
   IMPLICIT NONE
   REAL Ak(144) , Alf(3) , Anu(3) , Consts(5) , Costh , D(144) , Degra , Delint(12) , Dum5(76) , E(3) , Ecpt(24) , Eltemp , G(3) ,  &
      & Gambl(144) , R(5) , R1 , R2 , R3 , R4 , Rho , Sel(240) , Sinth , Sp(24) , Stress , Teo(16) , Ts(4) , Twopi , Tz , Tzero ,   &
      & Z(5) , Z1 , Z2 , Z3 , Z4
   INTEGER Ibuf , Idel , Iecpt(24) , Igp(4) , Iout , Matflg , Matidc
   CHARACTER*23 Ufm
   COMMON /condas/ Consts
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero
   COMMON /sdr2x5/ Ecpt , Dum5 , Idel , Igp , Tz , Sel , Ts , Ak
   COMMON /sdr2x6/ D , Gambl , R , Z
   COMMON /system/ Ibuf , Iout
   COMMON /xmssg / Ufm
   REAL alfb(4) , cosg , del , dgama , dgamr , dzero(32) , ee(16) , ee48 , er , et , ez , gambq(64) , gamqs(96) , grz , rmax ,      &
      & rmin , sing , tempe , vrt , vrz , vtr , vtz , vzr , vzt , zmin
   INTEGER i , i1 , icore , ics(4) , ip , iq , ising , j , jj , jj1 , jj2 , jrz(2) , k , kk , matid
   REAL rzints
!
!     THIS ROUTINE IS PHASE I OF STRESS DATA RECOVERY FOR THE
!     TRAPEZOIDAL CROSS SECTION RING
!
!     ECPT FOR THE TRAPEZOIDAL RING
!                                                          TYPE
!     ECPT( 1) ELEMENT IDENTIFICATION                        I
!     ECPT( 2) SCALAR INDEX NO. FOR GRID POINT A             I
!     ECPT( 3) SCALAR INDEX NO. FOR GRID POINT B             I
!     ECPT( 4) SCALAR INDEX NO. FOR GRID POINT C             I
!     ECPT( 5) SCALAR INDEX NO. FOR GRID POINT D             I
!     ECPT( 6) MATERIAL ORIENTATION ANGLE(DEGREES)           R
!     ECPT( 7) MATERIAL IDENTIFICATION                       I
!     ECPT( 8) COOR. SYS. ID. FOR GRID POINT A               I
!     ECPT( 9) X-COOR. OF GRID POINT A (IN BASIC COOR.)      R
!     ECPT(10) Y-COOR. OF GRID POINT A (IN BASIC COOR.)      R
!     ECPT(11) Z-COOR. OF GRID POINT A (IN BASIC COOR.)      R
!     ECPT(12) COOR. SYS. ID. FOR GRID POINT B               I
!     ECPT(13) X-COOR. OF GRID POINT B (IN BASIC COOR.)      R
!     ECPT(14) Y-COOR. OF GRID POINT B (IN BASIC COOR.)      R
!     ECPT(15) Z-COOR. OF GRID POINT B (IN BASIC COOR.)      R
!     ECPT(16) COOR. SYS. ID. FOR GRID POINT C               I
!     ECPT(17) X-COOR. OF GRID POINT C (IN BASIC COOR.)      R
!     ECPT(18) Y-COOR. OF GRID POINT C (IN BASIC COOR.)      R
!     ECPT(19) Z-COOR. OF GRID POINT C (IN BASIC COOR.)      R
!     ECPT(20) COOR. SYS. ID. FOR GRID POINT D               I
!     ECPT(21) X-COOR. OF GRID POINT D (IN BASIC COOR.)      R
!     ECPT(22) Y-COOR. OF GRID POINT D (IN BASIC COOR.)      R
!     ECPT(23) Z-COOR. OF GRID POINT D (IN BASIC COOR.)      R
!     ECPT(24) EL. TEMPERATURE FOR MATERIAL PROPERTIES       R
!
   EQUIVALENCE (Consts(2),Twopi) , (Consts(4),Degra) , (Iecpt(1),Ecpt(1)) , (R(1),R1) , (R(2),R2) , (R(3),R3) , (R(4),R4) ,         &
    & (Z(1),Z1) , (Z(2),Z2) , (Z(3),Z3) , (Z(4),Z4) , (Gambl(1),Sp(1)) , (Gambl(1),Teo(1)) , (Gambl(17),Delint(1))
!
!     STORE ECPT PARAMETERS IN LOCAL VARIABLES
!
   Idel = Iecpt(1)
   Igp(1) = Iecpt(2)
   Igp(2) = Iecpt(3)
   Igp(3) = Iecpt(4)
   Igp(4) = Iecpt(5)
   matid = Iecpt(7)
   ics(1) = Iecpt(8)
   ics(2) = Iecpt(12)
   ics(3) = Iecpt(16)
   ics(4) = Iecpt(20)
   R(1) = Ecpt(9)
   D(1) = Ecpt(10)
   Z(1) = Ecpt(11)
   R(2) = Ecpt(13)
   D(2) = Ecpt(14)
   Z(2) = Ecpt(15)
   R(3) = Ecpt(17)
   D(3) = Ecpt(18)
   Z(3) = Ecpt(19)
   R(4) = Ecpt(21)
   D(4) = Ecpt(22)
   Z(4) = Ecpt(23)
   tempe = Ecpt(24)
   dgama = Ecpt(6)
!
!     TEST THE VALIDITY OF THE GRID POINT COORDINATES
!
   DO i = 1 , 4
      IF ( R(i)<0.0 ) CALL mesage(-30,37,Idel)
      IF ( D(i)/=0.0 ) CALL mesage(-30,37,Idel)
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
   IF ( rmin/=0. ) THEN
      IF ( rmax/rmin>10. ) THEN
!
!     RATIO OF RADII IS TOO LARGE FOR GAUSS QUADRATURE FOR IP=-1
!
         WRITE (Iout,99001) Ufm , Idel
99001    FORMAT (A23,', TRAPRG ELEMENT',I9,' HAS A MAXIMUM TO MINIMUM ','RADIUS RATIO EXCEEDING 10.',/5X,                           &
                &'ACCURACY OF NUMERICAL INTEGRATION WOULD BE IN DOUBT.')
         CALL mesage(-30,37,Idel)
      ENDIF
   ENDIF
   icore = 0
   j = 1
   DO i = 1 , 4
      IF ( R(i)==0. ) THEN
         icore = icore + 1
         jrz(j) = i
         j = 2
      ENDIF
   ENDDO
   IF ( icore/=0 .AND. icore/=2 ) CALL mesage(-30,37,Idel)
!
!     FORM THE TRANSFORMATION MATRIX (8X8) FROM FIELD COORDINATES TO
!     GRID POINT DEGREES OF FREEDOM
!
   DO i = 1 , 64
      gambq(i) = 0.0
   ENDDO
   gambq(1) = 1.0
   gambq(2) = R1
   gambq(3) = Z1
   gambq(4) = R1*Z1
   gambq(13) = 1.0
   gambq(14) = R1
   gambq(15) = Z1
   gambq(16) = gambq(4)
   gambq(17) = 1.0
   gambq(18) = R2
   gambq(19) = Z2
   gambq(20) = R2*Z2
   gambq(29) = 1.0
   gambq(30) = R2
   gambq(31) = Z2
   gambq(32) = gambq(20)
   gambq(33) = 1.0
   gambq(34) = R3
   gambq(35) = Z3
   gambq(36) = R3*Z3
   gambq(45) = 1.0
   gambq(46) = R3
   gambq(47) = Z3
   gambq(48) = gambq(36)
   gambq(49) = 1.0
   gambq(50) = R4
   gambq(51) = Z4
   gambq(52) = R4*Z4
   gambq(61) = 1.0
   gambq(62) = R4
   gambq(63) = Z4
   gambq(64) = gambq(52)
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
   ising = -1
   CALL invers(8,gambq(1),8,D(10),0,D(11),ising,Sp)
!
   IF ( ising==2 ) CALL mesage(-30,26,Idel)
!
!     MODIFY THE TRANSFORMATION MATRIX IF ELEMENT IS A CORE ELEMENT
!
   IF ( icore/=0 ) THEN
      jj1 = 2*jrz(1) - 1
      jj2 = 2*jrz(2) - 1
!
      DO i = 1 , 8
         j = 8*(i-1)
         gambq(i) = 0.0
         gambq(i+16) = 0.0
         gambq(j+jj1) = 0.
         gambq(j+jj2) = 0.
      ENDDO
   ENDIF
!
!     CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT WHERE THE ORDER IS
!     INDICATED BY THE FOLLOWING TABLE
!
!       DELINT( 1) - (-1,0)
!       DELINT( 2) - (-1,1)
!       DELINT( 3) - (-1,2)
!       DELINT( 4) - ( 0,0)
!       DELINT( 5) - ( 0,1)
!       DELINT( 6) - ( 0,2)
!       DELINT( 7) - ( 1,0)
!       DELINT( 8) - ( 1,1)
!       DELINT( 9) - ( 1,2)
!       DELINT(10) - ( 2,0)
!       DELINT(11) - ( 2,1)
!       DELINT(12) - ( 3,0)
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
         IF ( icore/=0 ) THEN
            IF ( i1<=3 ) THEN
               Delint(i1) = 0.0
               CYCLE
            ENDIF
         ENDIF
         Delint(i1) = rzints(ip,iq,R,Z,4)
      ENDDO
   ENDDO
!
!     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3 TABLE
!
   Matidc = matid
   Matflg = 7
   Eltemp = tempe
   CALL mat(Idel)
!
!     SET MATERIAL PROPERTIES IN LOCAL VARIABLES
!
   er = E(1)
   et = E(2)
   ez = E(3)
   vrt = Anu(1)
   vtz = Anu(2)
   vzr = Anu(3)
   grz = G(3)
   Tz = Tzero
   vtr = vrt*et/er
   vzt = vtz*ez/et
   vrz = vzr*er/ez
   del = 1.0 - vrt*vtr - vtz*vzt - vzr*vrz - vrt*vtz*vzr - vrz*vtr*vzt
!
!     GENERATE ELASTIC CONSTANTS MATRIX (4X4)
!
   ee(1) = er*(1.0-vtz*vzt)/del
   ee(2) = er*(vtr+vzr*vtz)/del
   ee(3) = er*(vzr+vtr*vzt)/del
   ee(4) = 0.0
   ee(5) = ee(2)
   ee(6) = et*(1.0-vrz*vzr)/del
   ee(7) = et*(vzt+vrt*vzr)/del
   ee(8) = 0.0
   ee(9) = ee(3)
   ee(10) = ee(7)
   ee(11) = ez*(1.0-vrt*vtr)/del
   ee(12) = 0.0
   ee(13) = 0.0
   ee(14) = 0.0
   ee(15) = 0.0
   ee(16) = grz
!
!     FORM TRANSFORMATION MATRIX (4X4) FROM MATERIAL AXIS TO ELEMENT
!     GEOMETRIC AXIS
!
   dgamr = dgama*Degra
   cosg = cos(dgamr)
   sing = sin(dgamr)
   Teo(1) = cosg**2
   Teo(2) = 0.0
   Teo(3) = sing**2
   Teo(4) = sing*cosg
   Teo(5) = 0.0
   Teo(6) = 1.0
   Teo(7) = 0.0
   Teo(8) = 0.0
   Teo(9) = Teo(3)
   Teo(10) = 0.0
   Teo(11) = Teo(1)
   Teo(12) = -Teo(4)
   Teo(13) = -2.0*Teo(4)
   Teo(14) = 0.0
   Teo(15) = -Teo(13)
   Teo(16) = Teo(1) - Teo(3)
!
!     TRANSFORM THE ELASTIC CONSTANTS MATRIX FROM MATERIAL
!     TO ELEMENT GEOMETRIC AXIS
!
   CALL gmmats(Teo,4,4,1,ee,4,4,0,D)
   CALL gmmats(D,4,4,0,Teo,4,4,0,ee)
!
!     FORM THE ELEMENT STIFFNESS MATRIX IN FIELD COORDINATES
!
   ee48 = ee(4) + ee(8)
   D(1) = ee(1) + 2.0*ee(2) + ee(6)
   Ak(1) = ee(6)*Delint(1)
   Ak(2) = (ee(2)+ee(6))*Delint(4)
   Ak(3) = ee(6)*Delint(2) + ee(8)*Delint(4)
   Ak(4) = (ee(2)+ee(6))*Delint(5) + ee(8)*Delint(7)
   Ak(5) = 0.0
   Ak(6) = ee(8)*Delint(4)
   Ak(7) = ee(7)*Delint(4)
   Ak(8) = ee(7)*Delint(7) + ee(8)*Delint(5)
   Ak(9) = Ak(2)
   Ak(10) = D(1)*Delint(7)
   Ak(11) = (ee(2)+ee(6))*Delint(5) + ee48*Delint(7)
   Ak(12) = D(1)*Delint(8) + ee48*Delint(10)
   Ak(13) = 0.0
   Ak(14) = ee48*Delint(7)
   Ak(15) = (ee(3)+ee(7))*Delint(7)
   Ak(16) = (ee(3)+ee(7))*Delint(10) + ee48*Delint(8)
   Ak(17) = Ak(3)
   Ak(18) = Ak(11)
   Ak(19) = ee(6)*Delint(3) + ee(16)*Delint(7) + (ee(8)+ee(14))*Delint(5)
   Ak(20) = (ee(2)+ee(6))*Delint(6) + ee(16)*Delint(10) + (ee(8)+ee(13)+ee(14))*Delint(8)
   Ak(21) = 0.0
   Ak(22) = ee(16)*Delint(7) + ee(8)*Delint(5)
   Ak(23) = ee(7)*Delint(5) + ee(15)*Delint(7)
   Ak(24) = (ee(7)+ee(16))*Delint(8) + ee(8)*Delint(6) + ee(15)*Delint(10)
   Ak(25) = Ak(4)
   Ak(26) = Ak(12)
   Ak(27) = Ak(20)
   Ak(28) = D(1)*Delint(9) + ee(16)*Delint(12) + (ee48+ee(13)+ee(14))*Delint(11)
   Ak(29) = 0.0
   Ak(30) = ee(16)*Delint(10) + ee48*Delint(8)
   Ak(31) = (ee(3)+ee(7))*Delint(8) + ee(15)*Delint(10)
   Ak(32) = (ee(3)+ee(7)+ee(16))*Delint(11) + ee(15)*Delint(12) + ee48*Delint(9)
   Ak(33) = 0.0
   Ak(34) = 0.0
   Ak(35) = 0.0
   Ak(36) = 0.0
   Ak(37) = 0.0
   Ak(38) = 0.0
   Ak(39) = 0.0
   Ak(40) = 0.0
   Ak(41) = Ak(6)
   Ak(42) = Ak(14)
   Ak(43) = Ak(22)
   Ak(44) = Ak(30)
   Ak(45) = 0.0
   Ak(46) = ee(16)*Delint(7)
   Ak(47) = ee(15)*Delint(7)
   Ak(48) = ee(16)*Delint(8) + ee(15)*Delint(10)
   Ak(49) = Ak(7)
   Ak(50) = Ak(15)
   Ak(51) = Ak(23)
   Ak(52) = Ak(31)
   Ak(53) = 0.0
   Ak(54) = Ak(47)
   Ak(55) = ee(11)*Delint(7)
   Ak(56) = ee(11)*Delint(10) + ee(12)*Delint(8)
   Ak(57) = Ak(8)
   Ak(58) = Ak(16)
   Ak(59) = Ak(24)
   Ak(60) = Ak(32)
   Ak(61) = 0.0
   Ak(62) = Ak(48)
   Ak(63) = Ak(56)
   Ak(64) = ee(11)*Delint(12) + ee(16)*Delint(9) + (ee(12)+ee(13))*Delint(11)
!
   DO i = 1 , 64
      Ak(i) = Twopi*Ak(i)
   ENDDO
!
!     TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD COORDINATES
!     TO GRID POINT DEGREES OF FREEDOM
!
   CALL gmmats(gambq,8,8,1,Ak,8,8,0,D)
   CALL gmmats(D,8,8,0,gambq,8,8,0,Ak)
!
!     GENERATE THE TRANSFORMATION MATRIX FROM TWO TO THREE DEGREES OF
!     FREEDOM PER POINT
!
   DO i = 1 , 96
      gamqs(i) = 0.0
   ENDDO
   gamqs(1) = 1.0
   gamqs(15) = 1.0
   gamqs(28) = 1.0
   gamqs(42) = 1.0
   gamqs(55) = 1.0
   gamqs(69) = 1.0
   gamqs(82) = 1.0
   gamqs(96) = 1.0
!
!     TRANSFORM THE STIFFNESS MATRIX FROM TWO TO THREE DEGREES OF
!     FREEDOM PER POINT
!
   CALL gmmats(gamqs(1),8,12,1,Ak(1),8,8,0,D(1))
   CALL gmmats(D(1),12,8,0,gamqs(1),8,12,0,Ak(1))
!
!     LOCATE THE TRANSFORMATION MATRICES FOR THE FOUR  GRID POINTS
!
   DO i = 1 , 144
      Gambl(i) = 0.0
   ENDDO
   DO i = 1 , 4
      CALL transs(ics(i),D(1))
      k = 39*(i-1) + 1
      DO j = 1 , 3
         kk = k + 12*(j-1)
         jj = 3*(j-1) + 1
         Gambl(kk) = D(jj)
         Gambl(kk+1) = D(jj+1)
         Gambl(kk+2) = D(jj+2)
      ENDDO
   ENDDO
!
!     TRANSFORM THE STIFFNESS MATRIX FROM BASIC TO LOCAL COORDINATES
!
   CALL gmmats(Gambl(1),12,12,1,Ak(1),12,12,0,D(1))
   CALL gmmats(D(1),12,12,0,Gambl(1),12,12,0,Ak(1))
!
!     COMPUTE THE FIFTH GRID POINT TO BE THE AVERAGE OF THE FOUR
!     CORNER POINTS
!
   R(5) = (R1+R2+R3+R4)/4.0
   Z(5) = (Z1+Z2+Z3+Z4)/4.0
!
!     INITIALIZE THE CONSTANT PORTION OF THE D SUB 0 MATRIX
!
   DO i = 1 , 32
      dzero(i) = 0.0
   ENDDO
   dzero(2) = 1.0
   dzero(10) = 1.0
   dzero(23) = 1.0
   dzero(27) = 1.0
   dzero(30) = 1.0
!
!     START THE LOOP TO COMPUTE THE STRESS MATRIX FOR EACH GRID POINT
!
   DO j = 1 , 5
!
!     COMPUTE THE VARIABLE PORTION OF THE D SUB 0 MATRIX
!
      dzero(4) = Z(j)
      IF ( icore==0 ) THEN
         dzero(9) = 1.00/R(j)
         dzero(11) = Z(j)/R(j)
      ENDIF
      dzero(12) = Z(j)
      dzero(24) = R(j)
      dzero(28) = R(j)
      dzero(32) = Z(j)
!
!     COMPUTE THE STRESS MATRIX IN FIELD COORDINATES
!
      CALL gmmats(ee(1),4,4,0,dzero(1),4,8,0,D(1))
!
!     TRANSFORM THE STRESS MATRIX TO GRID POINT DEGREES OF FREEDOM
!
      CALL gmmats(D(1),4,8,0,gambq(1),8,8,0,D(37))
!
!     TRANSFORM THE STRESS MATRIX FROM TWO TO THREE DEGREES OF FREEDOM
!     PER POINT
!
      CALL gmmats(D(37),4,8,0,gamqs(1),8,12,0,D(73))
!
!     TRANSFORM THE STRESS MATRIX FROM BASIC TO LOCAL COORDINATES
!
      k = 48*(j-1) + 1
      CALL gmmats(D(73),4,12,0,Gambl(1),12,12,0,Sel(k))
!
   ENDDO
!
!     COMPUTE THE THERMAL STRAIN VECTOR
!
   DO i = 1 , 3
      alfb(i) = Alf(i)
   ENDDO
   alfb(4) = 0.0
!
!     COMPUTE THE THERMAL STRESS VECTOR
!
   CALL gmmats(ee(1),4,4,0,alfb(1),4,1,0,Ts(1))
END SUBROUTINE strap1
