
SUBROUTINE strir1
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Ak(81) , Alf(3) , Alfb(4) , Anu(3) , Consts(5) , Costh , D(81) , Degra , Delint(8) , Dum5(81) , Dzero(24) , E(3) , Ecpt(19) &
      & , Ee(16) , Eltemp , G(3) , Gambl(81) , Gambq(36) , Gamqs(54) , Rho , Sel(36) , Sinth , Sp(18) , Stress , Teo(16) , Ts(4) ,  &
      & Twopi , Tz , Tzero
   INTEGER Idel , Iecpt(19) , Igp(3) , Matflg , Matidc
   COMMON /condas/ Consts
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero
   COMMON /sdr2x5/ Ecpt , Dum5 , Idel , Igp , Tz , Sel , Ts , Ak
   COMMON /sdr2x6/ D , Gambq , Ee , Gamqs , Dzero , Gambl , Alfb
!
! Local variable declarations
!
   REAL ai
   REAL area , cosg , del , dgama , dgamr , dr , dz , er , et , ez , grz , r(3) , r1 , r2 , r3 , ra , rh , sing , tempe , vrt ,     &
      & vrz , vtr , vtz , vzr , vzt , z(3) , z1 , z2 , z3 , za , zh , zmin
   INTEGER i , i1 , ics(3) , ip , iq , ising , j , jj , k , kk , kode , matid
!
! End of declarations
!
!
!
!*****
! THIS ROUTINE IS PHASE I OF STRESS DATA RECOVERY FOR THE TRIANGULAR
! CROSS SECTION RING
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
   EQUIVALENCE (Gambl(1),Sp(1))
   EQUIVALENCE (Gambl(1),Teo(1))
   EQUIVALENCE (Gambl(17),Delint(1))
!
! ----------------------------------------------------------------------
!
! STORE ECPT PARAMETERS IN LOCAL VARIABLES
!
   Idel = Iecpt(1)
   Igp(1) = Iecpt(2)
   Igp(2) = Iecpt(3)
   Igp(3) = Iecpt(4)
   matid = Iecpt(6)
   ics(1) = Iecpt(7)
   ics(2) = Iecpt(11)
   ics(3) = Iecpt(15)
   r(1) = Ecpt(8)
   D(1) = Ecpt(9)
   z(1) = Ecpt(10)
   r(2) = Ecpt(12)
   D(2) = Ecpt(13)
   z(2) = Ecpt(14)
   r(3) = Ecpt(16)
   D(3) = Ecpt(17)
   z(3) = Ecpt(18)
   tempe = Ecpt(19)
   dgama = Ecpt(5)
!
!
! TEST THE VALIDITY OF THE GRID POINT COORDINATES
!
   DO i = 1 , 3
      IF ( r(i)<0.0E0 ) CALL mesage(-30,37,Idel)
      IF ( D(i)/=0.0E0 ) CALL mesage(-30,37,Idel)
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
!
!
! FORM THE TRANSFORMATION MATRIX (6X6) FROM FIELD COORDINATES TO GRID
! POINT DEGREES OF FREEDOM
!
   DO i = 1 , 36
      Gambq(i) = 0.0E0
   ENDDO
   Gambq(1) = 1.0E0
   Gambq(2) = r1
   Gambq(3) = z1
   Gambq(10) = 1.0E0
   Gambq(11) = r1
   Gambq(12) = z1
   Gambq(13) = 1.0E0
   Gambq(14) = r2
   Gambq(15) = z2
   Gambq(22) = 1.0E0
   Gambq(23) = r2
   Gambq(24) = z2
   Gambq(25) = 1.0E0
   Gambq(26) = r3
   Gambq(27) = z3
   Gambq(34) = 1.0E0
   Gambq(35) = r3
   Gambq(36) = z3
!
!
!     NO NEED TO COMPUTR DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
   ising = -1
   CALL invers(6,Gambq(1),6,D(10),0,D(11),ising,Sp)
!
   IF ( ising==2 ) CALL mesage(-30,26,Idel)
!
!
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
                  Delint(i1) = ((ra)**ip)*((za)**iq)*area
               ELSEIF ( kode==0 ) THEN
                  Delint(i1) = ai(1,3,1,2,1,3,ip,iq,r,z) + ai(3,2,1,2,3,2,ip,iq,r,z)
               ELSE
                  Delint(i1) = ai(1,3,3,2,1,3,ip,iq,r,z)
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      D(1) = Delint(6)
      Delint(6) = Delint(7)
      Delint(7) = D(1)
!
!
! TEST FOR EXCESSIVE ROUND-OFF ERROR IN INTEGRAL CALCULATIONS
! AND IF IT EXIST APPROXIMATE INTEGRALS
!
      IF ( kode<0 ) EXIT
      DO i = 1 , 8
         IF ( Delint(i)<0.0E0 ) GOTO 50
      ENDDO
      IF ( Delint(8)>Delint(7) ) THEN
         IF ( Delint(3)<Delint(8) ) THEN
            IF ( Delint(3)<=Delint(7) ) EXIT
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
   CALL mat(Idel)
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
   Tz = Tzero
   vtr = vrt*et/er
   vzt = vtz*ez/et
   vrz = vzr*er/ez
   del = 1.0E0 - vrt*vtr - vtz*vzt - vzr*vrz - vrt*vtz*vzr - vrz*vtr*vzt
!
!
! GENERATE ELASTIC CONSTANTS MATRIX (4X4)
!
   Ee(1) = er*(1.0E0-vtz*vzt)/del
   Ee(2) = er*(vtr+vzr*vtz)/del
   Ee(3) = er*(vzr+vtr*vzt)/del
   Ee(4) = 0.0E0
   Ee(5) = Ee(2)
   Ee(6) = et*(1.0E0-vrz*vzr)/del
   Ee(7) = et*(vzt+vrt*vzr)/del
   Ee(8) = 0.0E0
   Ee(9) = Ee(3)
   Ee(10) = Ee(7)
   Ee(11) = ez*(1.0E0-vrt*vtr)/del
   Ee(12) = 0.0E0
   Ee(13) = 0.0E0
   Ee(14) = 0.0E0
   Ee(15) = 0.0E0
   Ee(16) = grz
!
!
! FORM TRANSFORMATION MATRIX (4X4) FROM MATERIAL AXIS TO ELEMENT
! GEOMETRIC AXIS
!
   dgamr = dgama*Degra
   cosg = cos(dgamr)
   sing = sin(dgamr)
   Teo(1) = cosg**2
   Teo(2) = 0.0E0
   Teo(3) = sing**2
   Teo(4) = sing*cosg
   Teo(5) = 0.0E0
   Teo(6) = 1.0E0
   Teo(7) = 0.0E0
   Teo(8) = 0.0E0
   Teo(9) = Teo(3)
   Teo(10) = 0.0E0
   Teo(11) = Teo(1)
   Teo(12) = -Teo(4)
   Teo(13) = -2.0E0*Teo(4)
   Teo(14) = 0.0E0
   Teo(15) = -Teo(13)
   Teo(16) = Teo(1) - Teo(3)
!
!
! TRANSFORM THE ELASTIC CONSTANTS MATRIX FROM MATERIAL
! TO ELEMENT GEOMETRIC AXIS
!
   CALL gmmats(Teo,4,4,1,Ee,4,4,0,D)
   CALL gmmats(D,4,4,0,Teo,4,4,0,Ee)
!
!
!
! FORM THE ELEMENT STIFFNESS MATRIX IN FIELD COORDINATES
!
   Ak(1) = Ee(6)*Delint(1)
   Ak(2) = (Ee(2)+Ee(6))*Delint(4)
   Ak(3) = Ee(6)*Delint(2) + Ee(8)*Delint(4)
   Ak(4) = 0.0E0
   Ak(5) = Ee(8)*Delint(4)
   Ak(6) = Ee(7)*Delint(4)
   Ak(7) = Ak(2)
   Ak(8) = (Ee(1)+2.0E0*Ee(2)+Ee(6))*Delint(6)
   Ak(9) = (Ee(2)+Ee(6))*Delint(5) + (Ee(4)+Ee(8))*Delint(6)
   Ak(10) = 0.0E0
   Ak(11) = (Ee(4)+Ee(8))*Delint(6)
   Ak(12) = (Ee(3)+Ee(7))*Delint(6)
   Ak(13) = Ak(3)
   Ak(14) = Ak(9)
   Ak(15) = Ee(6)*Delint(3) + 2.0E0*Ee(8)*Delint(5) + Ee(16)*Delint(6)
   Ak(16) = 0.0E0
   Ak(17) = Ee(8)*Delint(5) + Ee(16)*Delint(6)
   Ak(18) = Ee(7)*Delint(5) + Ee(12)*Delint(6)
   Ak(19) = 0.0E0
   Ak(20) = 0.0E0
   Ak(21) = 0.0E0
   Ak(22) = 0.0E0
   Ak(23) = 0.0E0
   Ak(24) = 0.0E0
   Ak(25) = Ak(5)
   Ak(26) = Ak(11)
   Ak(27) = Ak(17)
   Ak(28) = 0.0E0
   Ak(29) = Ee(16)*Delint(6)
   Ak(30) = Ee(12)*Delint(6)
   Ak(31) = Ak(6)
   Ak(32) = Ak(12)
   Ak(33) = Ak(18)
   Ak(34) = 0.0E0
   Ak(35) = Ak(30)
   Ak(36) = Ee(11)*Delint(6)
!
   DO i = 1 , 36
      Ak(i) = Twopi*Ak(i)
   ENDDO
!
! TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD COORDINATES
! TO GRID POINT DEGREES OF FREEDOM
!
   CALL gmmats(Gambq,6,6,1,Ak,6,6,0,D)
   CALL gmmats(D,6,6,0,Gambq,6,6,0,Ak)
!
!
!
! GENERATE THE TRANSFORMATION MATRIX FROM TWO TO THREE DEGREES OF
! FREEDOM PER POINT
!
   DO i = 1 , 54
      Gamqs(i) = 0.0E0
   ENDDO
   Gamqs(1) = 1.0E0
   Gamqs(12) = 1.0E0
   Gamqs(22) = 1.0E0
   Gamqs(33) = 1.0E0
   Gamqs(43) = 1.0E0
   Gamqs(54) = 1.0E0
!
!
! TRANSFORM THE STIFFNESS MATRIX FROM TWO TO THREE DEGREES OF
! FREEDOM PER POINT
!
   CALL gmmats(Gamqs(1),6,9,1,Ak(1),6,6,0,D(1))
   CALL gmmats(D(1),9,6,0,Gamqs(1),6,9,0,Ak(1))
!
!
! LOCATE THE TRANSFORMATION MATRICES FOR THE THREE GRID POINTS
!
   DO i = 1 , 81
      Gambl(i) = 0.0E0
   ENDDO
   DO i = 1 , 3
      CALL transs(ics(i),D(1))
      k = 30*(i-1) + 1
      DO j = 1 , 3
         kk = k + 9*(j-1)
         jj = 3*(j-1) + 1
         Gambl(kk) = D(jj)
         Gambl(kk+1) = D(jj+1)
         Gambl(kk+2) = D(jj+2)
      ENDDO
   ENDDO
!
!
! TRANSFORM THE STIFFNESS MATRIX FROM BASIC TO LOCAL COORDINATES
!
   CALL gmmats(Gambl(1),9,9,1,Ak(1),9,9,0,D(1))
   CALL gmmats(D(1),9,9,0,Gambl(1),9,9,0,Ak(1))
!
!
! FORM THE D SUB 0 MATRIX
!
   DO i = 1 , 24
      Dzero(i) = 0.0E0
   ENDDO
   Dzero(2) = 1.0E0
   Dzero(7) = 1.0E0/ra
   Dzero(8) = 1.0E0
   Dzero(9) = za/ra
   Dzero(18) = 1.0E0
   Dzero(21) = 1.0E0
   Dzero(23) = 1.0E0
!
!
! COMPUTE THE STRESS MATRIX IN FIELD COORDINATES
!
   CALL gmmats(Ee(1),4,4,0,Dzero(1),4,6,0,D(1))
!
!
! TRANSFORM THE STRESS MATRIX TO GRID POINT DEGREES OF FREEDOM
!
   CALL gmmats(D(1),4,6,0,Gambq(1),6,6,0,Sel(1))
!
!
! TRANSFORM THE STRESS MATRIX FROM TWO TO THREE DEGREES OF FREEDOM
! PER POINT
!
   CALL gmmats(Sel(1),4,6,0,Gamqs(1),6,9,0,D(1))
!
!
! TRANSFORM THE STRESS MATRIX FROM BASIC TO LOCAL COORDINATES
!
   CALL gmmats(D(1),4,9,0,Gambl(1),9,9,0,Sel(1))
!
!
! COMPUTE THE THERMAL STRAIN VECTOR
!
   DO i = 1 , 3
      Alfb(i) = Alf(i)
   ENDDO
   Alfb(4) = 0.0E0
!
!
! COMPUTE THE THERMAL STRESS VECTOR
!
   CALL gmmats(Ee(1),4,4,0,Alfb(1),4,1,0,Ts(1))
!
!
END SUBROUTINE strir1
