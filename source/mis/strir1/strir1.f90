!*==strir1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE strir1
   USE c_condas
   USE c_matin
   USE c_matout
   USE c_sdr2x5
   USE c_sdr2x6
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: area , cosg , degra , del , dgama , dgamr , dr , dz , er , et , ez , grz , r1 , r2 , r3 , ra , rh , sing , tempe ,       &
         & twopi , vrt , vrz , vtr , vtz , vzr , vzt , z1 , z2 , z3 , za , zh , zmin
   REAL , DIMENSION(8) :: delint
   INTEGER :: i , i1 , ip , iq , ising , j , jj , k , kk , kode , matid
   INTEGER , DIMENSION(3) :: ics
   INTEGER , DIMENSION(19) :: iecpt
   REAL , DIMENSION(3) :: r , z
   REAL , DIMENSION(18) :: sp
   REAL , DIMENSION(16) :: teo
   EXTERNAL ai , gmmats , invers , mat , mesage , transs
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Consts(2),Twopi)
   !>>>>EQUIVALENCE (Consts(4),Degra)
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1))
   !>>>>EQUIVALENCE (r(1),r1) , (r(2),r2) , (r(3),r3) , (z(1),z1) , (z(2),z2) , (z(3),z3)
   !>>>>EQUIVALENCE (Gambl(1),Sp(1))
   !>>>>EQUIVALENCE (Gambl(1),Teo(1))
   !>>>>EQUIVALENCE (Gambl(17),Delint(1))
!
! ----------------------------------------------------------------------
!
! STORE ECPT PARAMETERS IN LOCAL VARIABLES
!
   idel = iecpt(1)
   igp(1) = iecpt(2)
   igp(2) = iecpt(3)
   igp(3) = iecpt(4)
   matid = iecpt(6)
   ics(1) = iecpt(7)
   ics(2) = iecpt(11)
   ics(3) = iecpt(15)
   r(1) = ecpt(8)
   d(1) = ecpt(9)
   z(1) = ecpt(10)
   r(2) = ecpt(12)
   d(2) = ecpt(13)
   z(2) = ecpt(14)
   r(3) = ecpt(16)
   d(3) = ecpt(17)
   z(3) = ecpt(18)
   tempe = ecpt(19)
   dgama = ecpt(5)
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
!     NO NEED TO COMPUTR DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
   ising = -1
   CALL invers(6,gambq(1),6,d(10),0,d(11),ising,sp)
!
   IF ( ising==2 ) CALL mesage(-30,26,idel)
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
   SPAG_Loop_1_1: DO
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
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
            IF ( kode<0 ) EXIT SPAG_Loop_1_1
            DO i = 1 , 8
               IF ( delint(i)<0.0E0 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            IF ( delint(8)>delint(7) ) THEN
               IF ( delint(3)<delint(8) ) THEN
                  IF ( delint(3)<=delint(7) ) EXIT SPAG_Loop_1_1
               ENDIF
            ENDIF
            spag_nextblock_1 = 2
         CASE (2)
            kode = -1
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO SPAG_Loop_1_1
!
!
!
! LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3 TABLE
!
   matidc = matid
   matflg = 7
   eltemp = tempe
   CALL mat(idel)
!
!
! SET MATERIAL PROPERTIES IN LOCAL VARIABLES
!
   er = e(1)
   et = e(2)
   ez = e(3)
   vrt = anu(1)
   vtz = anu(2)
   vzr = anu(3)
   grz = g(3)
   tz = tzero
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
   dgamr = dgama*degra
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
! FORM THE ELEMENT STIFFNESS MATRIX IN FIELD COORDINATES
!
   ak(1) = ee(6)*delint(1)
   ak(2) = (ee(2)+ee(6))*delint(4)
   ak(3) = ee(6)*delint(2) + ee(8)*delint(4)
   ak(4) = 0.0E0
   ak(5) = ee(8)*delint(4)
   ak(6) = ee(7)*delint(4)
   ak(7) = ak(2)
   ak(8) = (ee(1)+2.0E0*ee(2)+ee(6))*delint(6)
   ak(9) = (ee(2)+ee(6))*delint(5) + (ee(4)+ee(8))*delint(6)
   ak(10) = 0.0E0
   ak(11) = (ee(4)+ee(8))*delint(6)
   ak(12) = (ee(3)+ee(7))*delint(6)
   ak(13) = ak(3)
   ak(14) = ak(9)
   ak(15) = ee(6)*delint(3) + 2.0E0*ee(8)*delint(5) + ee(16)*delint(6)
   ak(16) = 0.0E0
   ak(17) = ee(8)*delint(5) + ee(16)*delint(6)
   ak(18) = ee(7)*delint(5) + ee(12)*delint(6)
   ak(19) = 0.0E0
   ak(20) = 0.0E0
   ak(21) = 0.0E0
   ak(22) = 0.0E0
   ak(23) = 0.0E0
   ak(24) = 0.0E0
   ak(25) = ak(5)
   ak(26) = ak(11)
   ak(27) = ak(17)
   ak(28) = 0.0E0
   ak(29) = ee(16)*delint(6)
   ak(30) = ee(12)*delint(6)
   ak(31) = ak(6)
   ak(32) = ak(12)
   ak(33) = ak(18)
   ak(34) = 0.0E0
   ak(35) = ak(30)
   ak(36) = ee(11)*delint(6)
!
   DO i = 1 , 36
      ak(i) = twopi*ak(i)
   ENDDO
!
! TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD COORDINATES
! TO GRID POINT DEGREES OF FREEDOM
!
   CALL gmmats(gambq,6,6,1,ak,6,6,0,d)
   CALL gmmats(d,6,6,0,gambq,6,6,0,ak)
!
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
! TRANSFORM THE STIFFNESS MATRIX FROM TWO TO THREE DEGREES OF
! FREEDOM PER POINT
!
   CALL gmmats(gamqs(1),6,9,1,ak(1),6,6,0,d(1))
   CALL gmmats(d(1),9,6,0,gamqs(1),6,9,0,ak(1))
!
!
! LOCATE THE TRANSFORMATION MATRICES FOR THE THREE GRID POINTS
!
   DO i = 1 , 81
      gambl(i) = 0.0E0
   ENDDO
   DO i = 1 , 3
      CALL transs(ics(i),d(1))
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
! TRANSFORM THE STIFFNESS MATRIX FROM BASIC TO LOCAL COORDINATES
!
   CALL gmmats(gambl(1),9,9,1,ak(1),9,9,0,d(1))
   CALL gmmats(d(1),9,9,0,gambl(1),9,9,0,ak(1))
!
!
! FORM THE D SUB 0 MATRIX
!
   DO i = 1 , 24
      dzero(i) = 0.0E0
   ENDDO
   dzero(2) = 1.0E0
   dzero(7) = 1.0E0/ra
   dzero(8) = 1.0E0
   dzero(9) = za/ra
   dzero(18) = 1.0E0
   dzero(21) = 1.0E0
   dzero(23) = 1.0E0
!
!
! COMPUTE THE STRESS MATRIX IN FIELD COORDINATES
!
   CALL gmmats(ee(1),4,4,0,dzero(1),4,6,0,d(1))
!
!
! TRANSFORM THE STRESS MATRIX TO GRID POINT DEGREES OF FREEDOM
!
   CALL gmmats(d(1),4,6,0,gambq(1),6,6,0,sel(1))
!
!
! TRANSFORM THE STRESS MATRIX FROM TWO TO THREE DEGREES OF FREEDOM
! PER POINT
!
   CALL gmmats(sel(1),4,6,0,gamqs(1),6,9,0,d(1))
!
!
! TRANSFORM THE STRESS MATRIX FROM BASIC TO LOCAL COORDINATES
!
   CALL gmmats(d(1),4,9,0,gambl(1),9,9,0,sel(1))
!
!
! COMPUTE THE THERMAL STRAIN VECTOR
!
   DO i = 1 , 3
      alfb(i) = alf(i)
   ENDDO
   alfb(4) = 0.0E0
!
!
! COMPUTE THE THERMAL STRESS VECTOR
!
   CALL gmmats(ee(1),4,4,0,alfb(1),4,1,0,ts(1))
!
!
END SUBROUTINE strir1
