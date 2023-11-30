
SUBROUTINE ttrapr(Ti,Pg)
   IMPLICIT NONE
   REAL Alf(3) , Anu(3) , Consts(5) , Costh , Degra , E(3) , Ecpt(24) , Eltemp , G(3) , Rho , Sinth , Stress , Twopi , Tzero
   INTEGER Ibuf , Iecpt(24) , Iout , Matflg , Matidc
   CHARACTER*23 Ufm
   COMMON /condas/ Consts
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero
   COMMON /system/ Ibuf , Iout
   COMMON /trimex/ Ecpt
   COMMON /xmssg / Ufm
   REAL Pg(1) , Ti(4)
   REAL alfb(4) , cosg , d(22) , del , delint(12) , dgama , dgamr , ee(16) , er , et , ez , gambl(144) , gambq(64) , gamqs(96) ,    &
      & grz , hprim(16) , q(32) , r(4) , r1 , r2 , r3 , r4 , rmax , rmin , sing , sp(24) , tempe , teo(16) , tl(12) , ts(4) , tz ,  &
      & vrt , vrz , vtr , vtz , vzr , vzt , z(4) , z1 , z2 , z3 , z4 , zmin
   INTEGER i , i1 , icore , ics(4) , idel , igp(4) , ip , iq , ising , j , jj , jj1 , jj2 , jrz(2) , k , kk , l , matid
   REAL rzints
!
!     THIS ROUTINE COMPUTES THE THERMAL LOAD FOR THE TRAPEZOIDAL
!     CROSS SECTION RING
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
!
   EQUIVALENCE (Consts(2),Twopi) , (Consts(4),Degra) , (Iecpt(1),Ecpt(1)) , (r(1),r1) , (r(2),r2) , (r(3),r3) , (r(4),r4) ,         &
    & (z(1),z1) , (z(2),z2) , (z(3),z3) , (z(4),z4) , (gambl(1),ee(1)) , (gambl(17),teo(1)) , (gambl(33),alfb(1)) ,                 &
    & (gambl(37),ts(1)) , (gambl(41),delint(1)) , (gambl(1),gambq(1)) , (gambl(65),q(1)) , (gambl(97),hprim(1)) , (gambl(113),sp(1))&
    & , (gambl(1),gamqs(1))
!
!     STORE ECPT PARAMETERS IN LOCAL VARIABLES
!
   idel = Iecpt(1)
   igp(1) = Iecpt(2)
   igp(2) = Iecpt(3)
   igp(3) = Iecpt(4)
   igp(4) = Iecpt(5)
   matid = Iecpt(7)
   ics(1) = Iecpt(8)
   ics(2) = Iecpt(12)
   ics(3) = Iecpt(16)
   ics(4) = Iecpt(20)
   r(1) = Ecpt(9)
   d(1) = Ecpt(10)
   z(1) = Ecpt(11)
   r(2) = Ecpt(13)
   d(2) = Ecpt(14)
   z(2) = Ecpt(15)
   r(3) = Ecpt(17)
   d(3) = Ecpt(18)
   z(3) = Ecpt(19)
   r(4) = Ecpt(21)
   d(4) = Ecpt(22)
   z(4) = Ecpt(23)
   tempe = Ecpt(24)
   dgama = Ecpt(6)
!
!     TEST THE VALIDITY OF THE GRID POINT COORDINATES
!
   DO i = 1 , 4
      IF ( r(i)<0.0 ) CALL mesage(-30,37,idel)
      IF ( d(i)/=0.0 ) CALL mesage(-30,37,idel)
   ENDDO
!
!     COMPUTE THE ELEMENT COORDINATES
!
   zmin = amin1(z1,z2,z3,z4)
   z1 = z1 - zmin
   z2 = z2 - zmin
   z3 = z3 - zmin
   z4 = z4 - zmin
   rmin = amin1(r1,r2,r3,r4)
   rmax = amax1(r1,r2,r3,r4)
   IF ( rmin/=0. ) THEN
      IF ( rmax/rmin>10. ) THEN
!
!     RATIO OF RADII IS TOO LARGE FOR GAUSS QUADRATURE FOR IP=-1
!
         WRITE (Iout,99001) Ufm , idel
99001    FORMAT (A23,', TRAPRG ELEMENT',I9,' HAS A MAXIMUM TO MINIMUM ','RADIUS RATIO EXCEEDING 10.'/5X,'ACCURACY OF NUMERICAL ',   &
                &'INTEGRATION WOULD BE IN DOUBT.')
         CALL mesage(-61,0,0)
      ENDIF
   ENDIF
   icore = 0
   j = 1
   DO i = 1 , 4
      IF ( r(i)==0. ) THEN
         icore = icore + 1
         jrz(j) = i
         j = 2
      ENDIF
   ENDDO
   IF ( icore/=0 .AND. icore/=2 ) CALL mesage(-61,0,0)
!
!     CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT WHERE THE ORDER IS
!     INDICATED BY THE FOLLOWING TABLE
!
!       DELINT( 1) - ( 0,0)
!       DELINT( 2) - ( 0,1)
!       DELINT( 3) - ( 0,2)
!       DELINT( 4) - ( 1,0)
!       DELINT( 5) - ( 1,1)
!       DELINT( 6) - ( 1,2)
!       DELINT( 7) - ( 2,0)
!       DELINT( 8) - ( 2,1)
!       DELINT( 9) - ( 2,2)
!       DELINT(10) - ( 3,0)
!       DELINT(11) - ( 3,1)
!
   i1 = 0
   DO i = 1 , 4
      ip = i - 1
      DO j = 1 , 3
         iq = j - 1
         i1 = i1 + 1
         IF ( i1/=12 ) delint(i1) = rzints(ip,iq,r,z,4)
      ENDDO
   ENDDO
!
!     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3 TABLE
!
   Matidc = matid
   Matflg = 7
   Eltemp = tempe
   CALL mat(idel)
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
   tz = Tzero
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
   teo(1) = cosg**2
   teo(2) = 0.0
   teo(3) = sing**2
   teo(4) = sing*cosg
   teo(5) = 0.0
   teo(6) = 1.0
   teo(7) = 0.0
   teo(8) = 0.0
   teo(9) = teo(3)
   teo(10) = 0.0
   teo(11) = teo(1)
   teo(12) = -teo(4)
   teo(13) = -2.0*teo(4)
   teo(14) = 0.0
   teo(15) = -teo(13)
   teo(16) = teo(1) - teo(3)
!
!     TRANSFORM THE ELASTIC CONSTANTS MATRIX FROM MATERIAL
!     TO ELEMENT GEOMETRIC AXIS
!
   CALL gmmats(teo,4,4,1,ee,4,4,0,d)
   CALL gmmats(d,4,4,0,teo,4,4,0,ee)
!
!     COMPUTE THE THERMAL STRAIN VECTOR
!
   DO i = 1 , 3
      alfb(i) = Alf(i)
   ENDDO
   alfb(4) = 0.0
!
   CALL gmmats(ee(1),4,4,0,alfb(1),4,1,0,ts(1))
!
!     FORM THE Q MATRIX (8X4)
!
   d(1) = ts(1) + ts(2)
   q(1) = ts(2)*delint(1)
   q(2) = ts(2)*delint(4)
   q(3) = ts(2)*delint(2)
   q(4) = ts(2)*delint(5)
   q(5) = d(1)*delint(4)
   q(6) = d(1)*delint(7)
   q(7) = d(1)*delint(5)
   q(8) = d(1)*delint(8)
   q(9) = ts(2)*delint(2)
   q(10) = ts(2)*delint(5)
   q(11) = ts(2)*delint(3)
   q(12) = ts(2)*delint(6)
   q(13) = d(1)*delint(5)
   q(14) = d(1)*delint(8)
   q(15) = d(1)*delint(6)
   q(16) = d(1)*delint(9)
   DO i = 17 , 24
      q(i) = 0.0
   ENDDO
   q(25) = ts(3)*delint(4)
   q(26) = ts(3)*delint(7)
   q(27) = ts(3)*delint(5)
   q(28) = ts(3)*delint(8)
   q(29) = ts(3)*delint(7)
   q(30) = ts(3)*delint(10)
   q(31) = ts(3)*delint(8)
   q(32) = ts(3)*delint(11)
!
!     FORM THE TRANSFORMATION MATRIX (8X8) FROM FIELD COORDINATES TO
!     GRID POINT DEGREES OF FREEDOM
!
   DO i = 1 , 64
      gambq(i) = 0.0
   ENDDO
   gambq(1) = 1.0
   gambq(2) = r1
   gambq(3) = z1
   gambq(4) = r1*z1
   gambq(13) = 1.0
   gambq(14) = r1
   gambq(15) = z1
   gambq(16) = gambq(4)
   gambq(17) = 1.0
   gambq(18) = r2
   gambq(19) = z2
   gambq(20) = r2*z2
   gambq(29) = 1.0
   gambq(30) = r2
   gambq(31) = z2
   gambq(32) = gambq(20)
   gambq(33) = 1.0
   gambq(34) = r3
   gambq(35) = z3
   gambq(36) = r3*z3
   gambq(45) = 1.0
   gambq(46) = r3
   gambq(47) = z3
   gambq(48) = gambq(36)
   gambq(49) = 1.0
   gambq(50) = r4
   gambq(51) = z4
   gambq(52) = r4*z4
   gambq(61) = 1.0
   gambq(62) = r4
   gambq(63) = z4
   gambq(64) = gambq(52)
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
   ising = -1
   CALL invers(8,gambq(1),8,d(10),0,d(11),ising,sp)
!
   IF ( ising==2 ) CALL mesage(-30,26,idel)
!
!     FORM THE HPRIM MATRIX (4X4)
!
   k = 0
   DO i = 1 , 4
      kk = 8*(i-1) - 1
      DO j = 1 , 4
         k = k + 1
         kk = kk + 2
         hprim(k) = gambq(kk)
      ENDDO
   ENDDO
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
!     FORM THE TEMPERATURE VECTOR
!
   DO i = 1 , 4
      Ti(i) = Ti(i) - Tzero
   ENDDO
!
!     COMPUTE THE THERMAL LOAD IN FIELD COORDINATES
!
   CALL gmmats(hprim(1),4,4,0,Ti(1),4,1,0,tl(1))
   CALL gmmats(q(1),8,4,0,tl(1),4,1,0,d(1))
!
!     TRANSFORM THE THERMAL LOAD TO GRID POINT DEGREES OF FREEDOM
!
   CALL gmmats(gambq(1),8,8,1,d(1),8,1,0,tl(1))
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
!     TRANSFORM THE   THERMAL LOAD   FROM TWO TO THREE DEGREES OF
!     FREEDOM PER POINT
!
   CALL gmmats(gamqs(1),8,12,1,tl(1),8,1,0,d(10))
!
!     LOCATE THE TRANSFORMATION MATRICES FOR THE FOUR  GRID POINTS
!
   DO i = 1 , 144
      gambl(i) = 0.0
   ENDDO
   DO i = 1 , 4
      CALL gbtran(ics(i),Ecpt(4*i+4),d(1))
      k = 39*(i-1) + 1
      DO j = 1 , 3
         kk = k + 12*(j-1)
         jj = 3*(j-1) + 1
         gambl(kk) = d(jj)
         gambl(kk+1) = d(jj+1)
         gambl(kk+2) = d(jj+2)
      ENDDO
   ENDDO
!
!     TRANSFORM THE   THERMAL LOAD   FROM BASIC TO LOCAL COORDINATES
!
   CALL gmmats(gambl(1),12,12,1,d(10),12,1,0,tl(1))
   DO i = 1 , 12
      tl(i) = Twopi*tl(i)
   ENDDO
!
!     ADD THE ELEMENT THERMAL LOAD TO THE STRUCTURE THERMAL LOAD
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
!
END SUBROUTINE ttrapr
