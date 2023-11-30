
SUBROUTINE ttrirg(Ti,Pg)
   IMPLICIT NONE
   REAL Alf(3) , Anu(3) , Consts(5) , Costh , Degra , E(3) , Ecpt(19) , Eltemp , G(3) , Rho , Sinth , Stress , Twopi , Tzero
   INTEGER Iecpt(19) , Matflg , Matidc
   COMMON /condas/ Consts
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero
   COMMON /trimex/ Ecpt
   REAL Pg(1) , Ti(3)
   REAL ai
   REAL alfb(4) , area , cosg , d(18) , del , delint(8) , dgama , dgamr , dr , dz , dzero(24) , ee(16) , er , et , ez , gambl(81) , &
      & gambq(36) , gamqs(54) , grz , r(3) , r1 , r2 , r3 , ra , rh , sing , sp(18) , tempe , teo(16) , tl(9) , tz , vrt , vrz ,    &
      & vtr , vtz , vzr , vzt , z(3) , z1 , z2 , z3 , za , zh , zmin
   INTEGER i , i1 , ics(3) , idel , igp(3) , ip , iq , ising , j , jj , k , kk , kode , l , matid
!
!
!*****
! THIS ROUTINECOMPUTES THE THERMAL LOAD FOR A TRIANGULAR CROSS
! SECTION RING
!*****
!
!
!                        ECPT FOR THE TRIANGULAR RING
!
!
!                                                      TYPE
! ECPT( 1) ELEMENT IDENTIFICATION                        I
! ECPT( 2) SCALAR INDEX NO. FOR GRID POINT A             I
! ECPT( 3) SCALAR INDEX NO. FOR GRID POINT B             I
! ECPT( 4) SCALAR INDEX NO. FOR GRID POINT C             I
! ECPT( 5) MATERIAL ORIENTATION ANGLE(DEGREES)           R
! ECPT( 6) MATERIAL IDENTIFICATION                       I
! ECPT( 7) COOR. SYS. ID. FOR GRID POINT A               I
! ECPT( 8) X-COOR. OF GRID POINT A (IN BASIC COOR.)      R
! ECPT( 9) Y-COOR. OF GRID POINT A (IN BASIC COOR.)      R
! ECPT(10) Z-COOR. OF GRID POINT A (IN BASIC COOR.)      R
! ECPT(11) COOR. SYS. ID. FOR GRID POINT B               I
! ECPT(12) X-COOR. OF GRID POINT B (IN BASIC COOR.)      R
! ECPT(13) Y-COOR. OF GRID POINT B (IN BASIC COOR.)      R
! ECPT(14) Z-COOR. OF GRID POINT B (IN BASIC COOR.)      R
! ECPT(15) COOR. SYS. ID. FOR GRID POINT C               I
! ECPT(16) X-COOR. OF GRID POINT C (IN BASIC COOR.)      R
! ECPT(17) Y-COOR. OF GRID POINT C (IN BASIC COOR.)      R
! ECPT(18) Z-COOR. OF GRID POINT C (IN BASIC COOR.)      R
! ECPT(19) EL. TEMPERATURE FOR MATERIAL PROPERTIES       R
!
!
!
!
   EQUIVALENCE (Consts(2),Twopi)
   EQUIVALENCE (Consts(4),Degra)
   EQUIVALENCE (Iecpt(1),Ecpt(1))
   EQUIVALENCE (r(1),r1) , (r(2),r2) , (r(3),r3) , (z(1),z1) , (z(2),z2) , (z(3),z3)
   EQUIVALENCE (gambl(1),ee(1))
   EQUIVALENCE (gambl(17),teo(1))
   EQUIVALENCE (gambl(33),dzero(1))
   EQUIVALENCE (gambl(57),alfb(1))
   EQUIVALENCE (gambl(61),delint(1))
   EQUIVALENCE (gambl(37),sp(1))
   EQUIVALENCE (gambl(1),gambq(1))
   EQUIVALENCE (gambl(1),gamqs(1))
!
! ----------------------------------------------------------------------
!
! STORE ECPT PARAMETERS IN LOCAL VARIABLES
!
   idel = Iecpt(1)
   igp(1) = Iecpt(2)
   igp(2) = Iecpt(3)
   igp(3) = Iecpt(4)
   matid = Iecpt(6)
   ics(1) = Iecpt(7)
   ics(2) = Iecpt(11)
   ics(3) = Iecpt(15)
   r(1) = Ecpt(8)
   d(1) = Ecpt(9)
   z(1) = Ecpt(10)
   r(2) = Ecpt(12)
   d(2) = Ecpt(13)
   z(2) = Ecpt(14)
   r(3) = Ecpt(16)
   d(3) = Ecpt(17)
   z(3) = Ecpt(18)
   tempe = Ecpt(19)
   dgama = Ecpt(5)
!
!
! TEST THE VALIDITY OF THE GRID POINT COORDINATES
!
   DO i = 1 , 3
      IF ( r(i)<0.0E0 ) CALL mesage(-30,37,idel)
      IF ( d(i)/=0.0E0 ) CALL mesage(-30,37,idel)
   ENDDO
!
!
! COMPUTE THE ELEMENT COORDINATES
!
   zmin = amin1(z1,z2,z3)
   z1 = z1 - zmin
   z2 = z2 - zmin
   z3 = z3 - zmin
!
! CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT WHERE THE ORDER IS
! INDICATED BY THE FOLLOWING TABLE
!
!              DELINT( 1) - (-1,0)
!              DELINT( 2) - (-1,1)
!              DELINT( 3) - (-1,2)
!              DELINT( 4) - ( 0,0)
!              DELINT( 5) - ( 0,1)
!              DELINT( 6) - ( 1,0)
!              DELINT( 7) - ( 0,2)
!              DELINT( 8) - ( 1,2)
!
!
! TEST FOR RELATIVE SMALL AREA OF INTEGRATION
! AND IF AREA IS SMALL THEN APPROXIMATE INTEGRALS
!
   dr = amax1(abs(r1-r2),abs(r2-r3),abs(r3-r1))
   rh = amin1(r1,r2,r3)/10.0E0
   dz = amax1(abs(z1-z2),abs(z2-z3),abs(z3-z1))
   zh = amin1(z1,z2,z3)/10.0E0
   ra = (r1+r2+r3)/3.0E0
   za = (z1+z2+z3)/3.0E0
   area = (r1*(z2-z3)+r2*(z3-z1)+r3*(z1-z2))/2.0E0
   kode = 0
   IF ( abs((r2-r1)/r2)<1.0E-5 ) kode = 1
   IF ( dr<=rh .OR. dz<=zh ) kode = -1
   DO
!
!
      i1 = 0
      DO i = 1 , 3
         ip = i - 2
         DO j = 1 , 3
            iq = j - 1
            IF ( ip/=1 .OR. iq/=1 ) THEN
               i1 = i1 + 1
               IF ( kode<0 ) THEN
                  delint(i1) = ((ra)**ip)*((za)**iq)*area
               ELSEIF ( kode==0 ) THEN
                  delint(i1) = ai(1,3,1,2,1,3,ip,iq,r,z) + ai(3,2,1,2,3,2,ip,iq,r,z)
               ELSE
                  delint(i1) = ai(1,3,3,2,1,3,ip,iq,r,z)
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      d(1) = delint(6)
      delint(6) = delint(7)
      delint(7) = d(1)
!
!
! TEST FOR EXCESSIVE ROUND-OFF ERROR IN INTEGRAL CALCULATIONS
! AND IF IT EXIST APPROXIMATE INTEGRALS
!
      IF ( kode<0 ) EXIT
      DO i = 1 , 8
         IF ( delint(i)<0.0E0 ) GOTO 50
      ENDDO
      IF ( delint(8)>delint(7) ) THEN
         IF ( delint(3)<delint(8) ) THEN
            IF ( delint(3)<=delint(7) ) EXIT
         ENDIF
      ENDIF
 50   kode = -1
   ENDDO
!
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
   del = 1.0E0 - vrt*vtr - vtz*vzt - vzr*vrz - vrt*vtz*vzr - vrz*vtr*vzt
!
!
! GENERATE ELASTIC CONSTANTS MATRIX (4X4)
!
   ee(1) = er*(1.0E0-vtz*vzt)/del
   ee(2) = er*(vtr+vzr*vtz)/del
   ee(3) = er*(vzr+vtr*vzt)/del
   ee(4) = 0.0E0
   ee(5) = ee(2)
   ee(6) = et*(1.0E0-vrz*vzr)/del
   ee(7) = et*(vzt+vrt*vzr)/del
   ee(8) = 0.0E0
   ee(9) = ee(3)
   ee(10) = ee(7)
   ee(11) = ez*(1.0E0-vrt*vtr)/del
   ee(12) = 0.0E0
   ee(13) = 0.0E0
   ee(14) = 0.0E0
   ee(15) = 0.0E0
   ee(16) = grz
!
!
! FORM TRANSFORMATION MATRIX (4X4) FROM MATERIAL AXIS TO ELEMENT
! GEOMETRIC AXIS
!
   dgamr = dgama*Degra
   cosg = cos(dgamr)
   sing = sin(dgamr)
   teo(1) = cosg**2
   teo(2) = 0.0E0
   teo(3) = sing**2
   teo(4) = sing*cosg
   teo(5) = 0.0E0
   teo(6) = 1.0E0
   teo(7) = 0.0E0
   teo(8) = 0.0E0
   teo(9) = teo(3)
   teo(10) = 0.0E0
   teo(11) = teo(1)
   teo(12) = -teo(4)
   teo(13) = -2.0E0*teo(4)
   teo(14) = 0.0E0
   teo(15) = -teo(13)
   teo(16) = teo(1) - teo(3)
!
!
! TRANSFORM THE ELASTIC CONSTANTS MATRIX FROM MATERIAL
! TO ELEMENT GEOMETRIC AXIS
!
   CALL gmmats(teo,4,4,1,ee,4,4,0,d)
   CALL gmmats(d,4,4,0,teo,4,4,0,ee)
!
!
!
! FORM THE D-CURL MATRIX
!
   DO i = 1 , 24
      dzero(i) = 0.0E0
   ENDDO
   dzero(2) = delint(6)*Twopi
   dzero(7) = delint(4)*Twopi
   dzero(8) = dzero(2)
   dzero(9) = delint(5)*Twopi
   dzero(18) = dzero(2)
   dzero(21) = dzero(2)
   dzero(23) = dzero(2)
!
!
! COMPUTE THE THERMAL STRAIN VECTOR
!
   d(1) = (Ti(1)+Ti(2)+Ti(3))/3.0E0
   d(1) = d(1) - tz
   DO i = 1 , 3
      alfb(i) = Alf(i)*d(1)
   ENDDO
   alfb(4) = 0.0E0
!
!
! COMPUTE THE THERMAL LOAD IN FIELD COORDINATES
!
   CALL gmmats(ee(1),4,4,0,alfb(1),4,1,0,tl(1))
   CALL gmmats(dzero(1),4,6,1,tl(1),4,1,0,d(1))
!
!
! FORM THE TRANSFORMATION MATRIX (6X6) FROM FIELD COORDINATES TO GRID
! POINT DEGREES OF FREEDOM
!
   DO i = 1 , 36
      gambq(i) = 0.0E0
   ENDDO
   gambq(1) = 1.0E0
   gambq(2) = r1
   gambq(3) = z1
   gambq(10) = 1.0E0
   gambq(11) = r1
   gambq(12) = z1
   gambq(13) = 1.0E0
   gambq(14) = r2
   gambq(15) = z2
   gambq(22) = 1.0E0
   gambq(23) = r2
   gambq(24) = z2
   gambq(25) = 1.0E0
   gambq(26) = r3
   gambq(27) = z3
   gambq(34) = 1.0E0
   gambq(35) = r3
   gambq(36) = z3
!
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
   ising = -1
   CALL invers(6,gambq(1),6,d(10),0,d(11),ising,sp)
!
   IF ( ising==2 ) CALL mesage(-30,26,idel)
!
!
!
! TRANSFORM THE THERMAL LOAD TO GRID POINT DEGREES OF FREEDOM
!
   CALL gmmats(gambq(1),6,6,1,d(1),6,1,0,tl(1))
!
!
! GENERATE THE TRANSFORMATION MATRIX FROM TWO TO THREE DEGREES OF
! FREEDOM PER POINT
!
   DO i = 1 , 54
      gamqs(i) = 0.0E0
   ENDDO
   gamqs(1) = 1.0E0
   gamqs(12) = 1.0E0
   gamqs(22) = 1.0E0
   gamqs(33) = 1.0E0
   gamqs(43) = 1.0E0
   gamqs(54) = 1.0E0
!
!
! TRANSFORM THE THERMAL LOAD FROM TWO TO THREE DEGREES OF FREEDOM
!
   CALL gmmats(gamqs(1),6,9,1,tl(1),6,1,0,d(10))
!
!
! LOCATE THE TRANSFORMATION MATRICES FOR THE THREE GRID POINTS
!
   DO i = 1 , 81
      gambl(i) = 0.0E0
   ENDDO
   DO i = 1 , 3
      CALL gbtran(ics(i),Ecpt(4*i+7),d(1))
      k = 30*(i-1) + 1
      DO j = 1 , 3
         kk = k + 9*(j-1)
         jj = 3*(j-1) + 1
         gambl(kk) = d(jj)
         gambl(kk+1) = d(jj+1)
         gambl(kk+2) = d(jj+2)
      ENDDO
   ENDDO
!
!
! TRANSFORM THE THERMAL LOAD FROM BASIC TO LOCAL COORDINATES
!
   CALL gmmats(gambl(1),9,9,1,d(10),9,1,0,tl(1))
!
!
! ADD THE ELEMENT THERMAL LOAD TO THE STRUCTURE THERMAL LOAD
!
   k = 0
   DO i = 1 , 3
      l = igp(i) - 1
      DO j = 1 , 3
         k = k + 1
         l = l + 1
         Pg(l) = Pg(l) + tl(k)
      ENDDO
   ENDDO
!
!
!
END SUBROUTINE ttrirg
