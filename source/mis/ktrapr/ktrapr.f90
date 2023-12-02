!*==ktrapr.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ktrapr
   USE c_condad
   USE c_matin
   USE c_matout
   USE c_msgx
   USE c_sma1cl
   USE c_sma1dp
   USE c_sma1et
   USE c_sma1io
   USE c_system
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(36) :: aki
   REAL(REAL64) , DIMENSION(9) :: akt
   REAL(REAL64) :: dampc , degrad , ee48 , r1 , r2 , r3 , r4 , rmax , rmin , twopi , z1 , z2 , z3 , z4
   INTEGER :: i , i1 , iai , iapp , ic1 , icore , idel , ip , ipp , iq , ir1 , irc , ising , j , j1 , j2 , jj1 , jj2 , k , matid
   INTEGER , DIMENSION(24) :: iecpt
   INTEGER , SAVE :: irg
   INTEGER , DIMENSION(2) :: jrz
   EXTERNAL gmmatd , inverd , mat , mesage , rzintd , sma1b , transd
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE COMPUTES THE STIFFNESS MATRIX FOR A AXI-SYMMETRIC
!     RING WITH A TRAPEZOIDAL CROSS SECTION
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
   !>>>>EQUIVALENCE (Constd(2),Twopi) , (Constd(4),Degrad) , (Iecpt(1),Ecpt(1)) , (R(1),R1) , (R(2),R2) , (R(3),R3) , (R(4),R4) ,        &
!>>>>    & (Z(1),Z1) , (Z(2),Z2) , (Z(3),Z3) , (Z(4),Z4) , (Aki(1),Gambq(1)) , (Akt(1),Gambq(37))
   DATA irg/4HTRAP/
!
!
!     STORE ECPT PARAMETERS IN LOCAL VARIABLES
!
   idel = iecpt(1)
   igp(1) = iecpt(2)
   igp(2) = iecpt(3)
   igp(3) = iecpt(4)
   igp(4) = iecpt(5)
   matid = iecpt(7)
   ics(1) = iecpt(8)
   ics(2) = iecpt(12)
   ics(3) = iecpt(16)
   ics(4) = iecpt(20)
   r(1) = ecpt(9)
   d(1) = ecpt(10)
   z(1) = ecpt(11)
   r(2) = ecpt(13)
   d(2) = ecpt(14)
   z(2) = ecpt(15)
   r(3) = ecpt(17)
   d(3) = ecpt(18)
   z(3) = ecpt(19)
   r(4) = ecpt(21)
   d(4) = ecpt(22)
   z(4) = ecpt(23)
   tempe = ecpt(24)
   dgama = ecpt(6)
!
!     CHECK INTERNAL GRID POINTS FOR PIVOT POINT
!
   ipp = 0
   DO i = 1 , 4
      IF ( npvt==igp(i) ) ipp = i
   ENDDO
   IF ( ipp==0 ) CALL mesage(-30,34,idel)
!
!     TEST THE VALIDITY OF THE GRID POINT COORDINATES
!
   DO i = 1 , 4
      IF ( r(i)<0.0D0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      IF ( d(i)/=0.0D0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
!
!     COMPUTE THE ELEMENT COORDINATES
!
   zmin = dmin1(z1,z2,z3,z4)
   z1 = z1 - zmin
   z2 = z2 - zmin
   z3 = z3 - zmin
   z4 = z4 - zmin
!
!     FATAL IF RATIO OF RADII IS TO LARGE FOR GUASS QUADRATURE FOR
!     IP =-1
!
   rmin = dmin1(r1,r2,r3,r4)
   rmax = dmax1(r1,r2,r3,r4)
   IF ( rmin/=0.D0 ) THEN
      IF ( rmax/rmin>10.D0 ) THEN
         i = 221
         CALL spag_block_2
         RETURN
      ENDIF
   ENDIF
!
   IF ( r1<r2 .AND. r4<r3 .AND. z4>z1 ) THEN
      IF ( dabs(z1-z2)<=1.0D-3 ) THEN
         IF ( dabs(z3-z4)<=1.0D-3 ) THEN
            d(5) = (r1+r4)/2.0D0
            d(6) = (r2+r3)/2.0D0
            IF ( d(5)/=0.0D0 ) THEN
               IF ( dabs((r1-r4)/d(5))<=0.5D-2 ) THEN
                  r1 = d(5)
                  r4 = d(5)
               ENDIF
            ENDIF
            IF ( d(6)/=0.0D0 ) THEN
               IF ( dabs((r2-r3)/d(6))<=0.5D-2 ) THEN
                  r(2) = d(6)
                  r(3) = d(6)
               ENDIF
            ENDIF
!
            icore = 0
            j = 1
            DO i = 1 , 4
               IF ( r(i)==0.D0 ) THEN
                  icore = icore + 1
                  jrz(j) = i
                  j = 2
               ENDIF
            ENDDO
            IF ( icore==0 .OR. icore==2 ) THEN
!
!     FORM THE TRANSFORMATION MATRIX (8X8) FROM FIELD COORDINATES TO
!     GRID POINT DEGREES OF FREEDOM
!
               DO i = 1 , 64
                  gambq(i) = 0.0D0
               ENDDO
               gambq(1) = 1.0D0
               gambq(2) = r1
               gambq(3) = z1
               gambq(4) = r1*z1
               gambq(13) = 1.0D0
               gambq(14) = r1
               gambq(15) = z1
               gambq(16) = gambq(4)
               gambq(17) = 1.0D0
               gambq(18) = r2
               gambq(19) = z2
               gambq(20) = r2*z2
               gambq(29) = 1.0D0
               gambq(30) = r2
               gambq(31) = z2
               gambq(32) = gambq(20)
               gambq(33) = 1.0D0
               gambq(34) = r3
               gambq(35) = z3
               gambq(36) = r3*z3
               gambq(45) = 1.0D0
               gambq(46) = r3
               gambq(47) = z3
               gambq(48) = gambq(36)
               gambq(49) = 1.0D0
               gambq(50) = r4
               gambq(51) = z4
               gambq(52) = r4*z4
               gambq(61) = 1.0D0
               gambq(62) = r4
               gambq(63) = z4
               gambq(64) = gambq(52)
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
               ising = -1
               CALL inverd(8,gambq(1),8,d(10),0,d(11),ising,sp)
               IF ( ising==2 ) THEN
                  i = 26
                  CALL spag_block_2
                  RETURN
               ELSE
!
!     MODIFY THE TRANSFORMATION MATRIX IF ELEMENT IS A CORE ELEMENT
!
                  IF ( icore/=0 ) THEN
                     jj1 = 2*jrz(1) - 1
                     jj2 = 2*jrz(2) - 1
!
                     DO i = 1 , 8
                        j = 8*(i-1)
                        gambq(i) = 0.0D0
                        gambq(i+16) = 0.0D0
                        gambq(j+jj1) = 0.D0
                        gambq(j+jj2) = 0.D0
                     ENDDO
                  ENDIF
!
!     CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT WHERE THE ORDER IS
!     INDICATED BY THE FOLLOWING TABLE
!
!        DELINT( 1) - (-1,0)
!        DELINT( 2) - (-1,1)
!        DELINT( 3) - (-1,2)
!        DELINT( 4) - ( 0,0)
!        DELINT( 5) - ( 0,1)
!        DELINT( 6) - ( 0,2)
!        DELINT( 7) - ( 1,0)
!        DELINT( 8) - ( 1,1)
!        DELINT( 9) - ( 1,2)
!        DELINT(10) - ( 2,0)
!        DELINT(11) - ( 2,1)
!        DELINT(12) - ( 3,0)
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
                              delint(i1) = 0.0D0
                              CYCLE
                           ENDIF
                        ENDIF
                        delint(i1) = rzintd(ip,iq,r,z,4)
                     ENDDO
                  ENDDO
!
!     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3 TABLE
!
                  matidc = matid
                  matflg = 7
                  eltemp = tempe
                  CALL mat(idel)
!
!     SET MATERIAL PROPERTIES IN DOUBLE PRECISION VARIABLES
!
                  er = e(1)
                  et = e(2)
                  ez = e(3)
                  vrt = anu(1)
                  vtz = anu(2)
                  vzr = anu(3)
                  grz = g(3)
                  vtr = vrt*et/er
                  vzt = vtz*ez/et
                  vrz = vzr*er/ez
                  del = 1.0D0 - vrt*vtr - vtz*vzt - vzr*vrz - vrt*vtz*vzr - vrz*vtr*vzt
!
!     GENERATE ELASTIC CONSTANTS MATRIX (4X4)
!
                  ee(1) = er*(1.0D0-vtz*vzt)/del
                  ee(2) = er*(vtr+vzr*vtz)/del
                  ee(3) = er*(vzr+vtr*vzt)/del
                  ee(4) = 0.0D0
                  ee(5) = ee(2)
                  ee(6) = et*(1.0D0-vrz*vzr)/del
                  ee(7) = et*(vzt+vrt*vzr)/del
                  ee(8) = 0.0D0
                  ee(9) = ee(3)
                  ee(10) = ee(7)
                  ee(11) = ez*(1.0D0-vrt*vtr)/del
                  ee(12) = 0.0D0
                  ee(13) = 0.0D0
                  ee(14) = 0.0D0
                  ee(15) = 0.0D0
                  ee(16) = grz
!
!     FORM TRANSFORMATION MATRIX (4X4) FROM MATERIAL AXIS TO ELEMENT
!     GEOMETRIC AXIS
!
                  dgamr = dgama*degrad
                  cosg = dcos(dgamr)
                  sing = dsin(dgamr)
                  teo(1) = cosg**2
                  teo(2) = 0.0D0
                  teo(3) = sing**2
                  teo(4) = sing*cosg
                  teo(5) = 0.0D0
                  teo(6) = 1.0D0
                  teo(7) = 0.0D0
                  teo(8) = 0.0D0
                  teo(9) = teo(3)
                  teo(10) = 0.0D0
                  teo(11) = teo(1)
                  teo(12) = -teo(4)
                  teo(13) = -2.0D0*teo(4)
                  teo(14) = 0.0D0
                  teo(15) = -teo(13)
                  teo(16) = teo(1) - teo(3)
!
!     TRANSFORM THE ELASTIC CONSTANTS MATRIX FROM MATERIAL
!     TO ELEMENT GEOMETRIC AXIS
!
                  CALL gmmatd(teo,4,4,1,ee,4,4,0,d)
                  CALL gmmatd(d,4,4,0,teo,4,4,0,ee)
!
!     FORM THE ELEMENT STIFFNESS MATRIX IN FIELD COORDINATES
!
                  ee48 = ee(4) + ee(8)
                  d(1) = ee(1) + 2.0D0*ee(2) + ee(6)
                  ak(1) = ee(6)*delint(1)
                  ak(2) = (ee(2)+ee(6))*delint(4)
                  ak(3) = ee(6)*delint(2) + ee(8)*delint(4)
                  ak(4) = (ee(2)+ee(6))*delint(5) + ee(8)*delint(7)
                  ak(5) = 0.0D0
                  ak(6) = ee(8)*delint(4)
                  ak(7) = ee(7)*delint(4)
                  ak(8) = ee(7)*delint(7) + ee(8)*delint(5)
                  ak(9) = ak(2)
                  ak(10) = d(1)*delint(7)
                  ak(11) = (ee(2)+ee(6))*delint(5) + ee48*delint(7)
                  ak(12) = d(1)*delint(8) + ee48*delint(10)
                  ak(13) = 0.0D0
                  ak(14) = ee48*delint(7)
                  ak(15) = (ee(3)+ee(7))*delint(7)
                  ak(16) = (ee(3)+ee(7))*delint(10) + ee48*delint(8)
                  ak(17) = ak(3)
                  ak(18) = ak(11)
                  ak(19) = ee(6)*delint(3) + ee(16)*delint(7) + (ee(8)+ee(14))*delint(5)
                  ak(20) = (ee(2)+ee(6))*delint(6) + ee(16)*delint(10) + (ee(8)+ee(13)+ee(14))*delint(8)
                  ak(21) = 0.0D0
                  ak(22) = ee(16)*delint(7) + ee(8)*delint(5)
                  ak(23) = ee(7)*delint(5) + ee(15)*delint(7)
                  ak(24) = (ee(7)+ee(16))*delint(8) + ee(8)*delint(6) + ee(15)*delint(10)
                  ak(25) = ak(4)
                  ak(26) = ak(12)
                  ak(27) = ak(20)
                  ak(28) = d(1)*delint(9) + ee(16)*delint(12) + (ee48+ee(13)+ee(14))*delint(11)
                  ak(29) = 0.0D0
                  ak(30) = ee(16)*delint(10) + ee48*delint(8)
                  ak(31) = (ee(3)+ee(7))*delint(8) + ee(15)*delint(10)
                  ak(32) = (ee(3)+ee(7)+ee(16))*delint(11) + ee(15)*delint(12) + ee48*delint(9)
                  ak(33) = 0.0D0
                  ak(34) = 0.0D0
                  ak(35) = 0.0D0
                  ak(36) = 0.0D0
                  ak(37) = 0.0D0
                  ak(38) = 0.0D0
                  ak(39) = 0.0D0
                  ak(40) = 0.0D0
                  ak(41) = ak(6)
                  ak(42) = ak(14)
                  ak(43) = ak(22)
                  ak(44) = ak(30)
                  ak(45) = 0.0D0
                  ak(46) = ee(16)*delint(7)
                  ak(47) = ee(15)*delint(7)
                  ak(48) = ee(16)*delint(8) + ee(15)*delint(10)
                  ak(49) = ak(7)
                  ak(50) = ak(15)
                  ak(51) = ak(23)
                  ak(52) = ak(31)
                  ak(53) = 0.0D0
                  ak(54) = ak(47)
                  ak(55) = ee(11)*delint(7)
                  ak(56) = ee(11)*delint(10) + ee(12)*delint(8)
                  ak(57) = ak(8)
                  ak(58) = ak(16)
                  ak(59) = ak(24)
                  ak(60) = ak(32)
                  ak(61) = 0.0D0
                  ak(62) = ak(48)
                  ak(63) = ak(56)
                  ak(64) = ee(11)*delint(12) + ee(16)*delint(9) + (ee(12)+ee(15))*delint(11)
!
                  DO i = 1 , 64
                     ak(i) = twopi*ak(i)
                  ENDDO
!
!     TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD COORDINATES
!     TO GRID POINT DEGREES OF FREEDOM
!
                  CALL gmmatd(gambq,8,8,1,ak,8,8,0,d)
                  CALL gmmatd(d,8,8,0,gambq,8,8,0,ak)
!
!     ZERO OUT THE (6X6) MATRIX USED AS INPUT TO THE INSERTION ROUTINE
!
                  DO i = 1 , 36
                     aki(i) = 0.0D0
                  ENDDO
!
!     LOCATE THE TRANSFORMATION MATRICES FOR THE FOUR  GRID POINTS
!
                  DO i = 1 , 4
                     IF ( ics(i)/=0 ) THEN
                        k = 9*(i-1) + 1
                        CALL transd(ics(i),d(k))
                     ENDIF
                  ENDDO
!
!     START THE LOOP FOR INSERTION OF THE FOUR  (6X6) MATRICES
!     INTO THE MASTER STIFFNESS MATRIX
!
                  ir1 = 2*ipp - 1
                  iapp = 9*(ipp-1) + 1
                  DO i = 1 , 4
!
!     PLACE THE APPROIATE (2X2) SUBMATRIX OF THE STIFFNESS MATRIX
!     IN A (3X3) MATRIX FOR TRANSFORMATION
!
                     ic1 = 2*i - 1
                     irc = (ir1-1)*8 + ic1
                     akt(1) = ak(irc)
                     akt(2) = 0.0D0
                     akt(3) = ak(irc+1)
                     akt(4) = 0.0D0
                     akt(5) = 0.0D0
                     akt(6) = 0.0D0
                     akt(7) = ak(irc+8)
                     akt(8) = 0.0D0
                     akt(9) = ak(irc+9)
!
!     TRANSFORM THE (3X3) STIFFNESS MATRIX
!
                     IF ( ics(ipp)/=0 ) THEN
                        CALL gmmatd(d(iapp),3,3,1,akt(1),3,3,0,d(37))
                        DO j = 1 , 9
                           akt(j) = d(j+36)
                        ENDDO
                     ENDIF
                     IF ( ics(i)/=0 ) THEN
                        iai = 9*(i-1) + 1
                        CALL gmmatd(akt(1),3,3,0,d(iai),3,3,0,d(37))
                        DO j = 1 , 9
                           akt(j) = d(j+36)
                        ENDDO
                     ENDIF
!
!     PLACE THE TRANSFORMED (3X3) MATRIX INTO A (6X6) MATRIX FOR
!     THE INSERTION ROUTINE
!
                     j = 0
                     DO j1 = 1 , 18 , 6
                        DO j2 = 1 , 3
                           j = j + 1
                           k = j1 + j2 - 1
                           aki(k) = akt(j)
                        ENDDO
                     ENDDO
!
!     CALL THE INSERTION ROUTINE
!
                     CALL sma1b(aki(1),igp(i),-1,ifkgg,0.0D0)
                     IF ( iopt4/=0 .AND. gsube/=0.0 ) THEN
                        k4ggsw = 1
                        dampc = gsube
                        CALL sma1b(aki(1),igp(i),-1,if4gg,dampc)
                     ENDIF
                  ENDDO
                  RETURN
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      USE ISO_FORTRAN_ENV                 
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
      I = 37
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      USE ISO_FORTRAN_ENV                 
! ...     221 WILL PRINT USER MESSAGE 2218
!
      IF ( nmsg/=0 ) THEN
         IF ( nmsg>=mmsg ) RETURN
         DO J = 1 , nmsg
            IF ( msg(3,J)==Idel .AND. msg(2,J)==I ) RETURN
         ENDDO
      ENDIF
      ics(1) = Idel
      ics(2) = Irg
      CALL mesage(30,I,ics)
      nogo = 1
   END SUBROUTINE spag_block_2
!
END SUBROUTINE ktrapr
