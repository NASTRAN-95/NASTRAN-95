!*==mtrapr.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mtrapr
   USE c_condad
   USE c_matin
   USE c_matout
   USE c_sma2cl
   USE c_sma2dp
   USE c_sma2et
   USE c_sma2io
   USE c_system
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(64) :: am
   REAL(REAL64) :: d2pi , r1 , r2 , r3 , r4 , rmax , rmin , z1 , z2 , z3 , z4
   INTEGER :: i , i1 , iai , iapp , ic1 , icore , idel , ip , ipp , iq , ir1 , irc , ising , j , j1 , j2 , jj1 , jj2 , k , matid
   INTEGER , DIMENSION(24) :: iecpt
   INTEGER , DIMENSION(2) :: jrz
   EXTERNAL gmmatd , inverd , mat , mesage , rzintd , sma2b , transd
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE COMPUTES THE MASS MATRIX FOR A AXI-SYMMETRIC RING
!     WITH A TRAPEZOIDAL CROSS SECTION
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
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1)) , (R(1),R1) , (R(2),R2) , (R(3),R3) , (R(4),R4) , (Z(1),Z1) , (Z(2),Z2) , (Z(3),Z3) , (Z(4),Z4) , &
!>>>>    & (Am(1),Ak(1)) , (Constd(2),D2pi)
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
!     FATAL IF RATIO OF RADII IS TO LARGE FOR GUASS QUADRATURE FOR IP=-1
!
   rmin = dmin1(r1,r2,r3,r4)
   rmax = dmax1(r1,r2,r3,r4)
   IF ( rmin/=0.D0 ) THEN
      IF ( rmax/rmin>10.D0 ) THEN
         i = 218
         nogo = 1
         RETURN
      ENDIF
   ENDIF
!
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
         r2 = d(6)
         r3 = d(6)
      ENDIF
   ENDIF
!
   icore = 0
   j = 1
   DO i = 1 , 4
      IF ( r(i)==0.0D0 ) THEN
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
!
!     ERROR TYPE 218 HAD BEEN ISSUED BY KTRAPR ALREADY.
!
         CALL mesage(30,i,idel)
         nogo = 1
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
               gambq(j+jj1) = 0.0D0
               gambq(j+jj2) = 0.0D0
            ENDDO
         ENDIF
!
!     CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT WHERE THE ORDER IS
!     INDICATED BY THE FOLLOWING TABLE
!
!     DELINT(1) - (1,0)
!     DELINT(2) - (1,1)
!     DELINT(3) - (1,2)
!     DELINT(4) - (2,0)
!     DELINT(5) - (2,1)
!     DELINT(6) - (2,2)
!     DELINT(7) - (3,0)
!     DELINT(8) - (3,1)
!     DELINT(9) - (3,2)
!
         i1 = 0
         DO i = 1 , 3
            ip = i
            DO j = 1 , 3
               iq = j - 1
               i1 = i1 + 1
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
!    SET MATERIAL PROPERTIES IN DOUBLE PRECISION VARIABLES
!
         rhod = rho
!
!     GENERATE THE CONSISTENT MASS MATRIX IN FIELD COORDINATES
!
         DO i = 1 , 64
            am(i) = 0.0D0
         ENDDO
         twopi = d2pi*rhod
         am(1) = twopi*delint(1)
         am(2) = twopi*delint(4)
         am(3) = twopi*delint(2)
         am(4) = twopi*delint(5)
         am(9) = am(2)
         am(10) = twopi*delint(7)
         am(11) = twopi*delint(5)
         am(12) = twopi*delint(8)
         am(17) = am(3)
         am(18) = am(11)
         am(19) = twopi*delint(3)
         am(20) = twopi*delint(6)
         am(25) = am(4)
         am(26) = am(12)
         am(27) = am(20)
         am(28) = twopi*delint(9)
         DO i = 1 , 4
            k = (i-1)*8
            DO j = 1 , 4
               k = k + 1
               am(k+36) = am(k)
            ENDDO
         ENDDO
!
!     TRANSFORM THE ELEMENT MASS MATRIX FROM FIELD COORDINATES TO GRID
!     POINT DEGREES OF FREEDOM
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
!     START THE LOOP FOR INSERTION OF THE FOUR  (6X6) MATRICES INTO THE
!     MASTER MASS MATRIX
!
         ir1 = 2*ipp - 1
         iapp = 9*(ipp-1) + 1
         DO i = 1 , 4
!
!     PLACE THE APPROIATE (2X2) SUBMATRIX OF THE MASS MATRIX IN A (3X3)
!     MATRIX FOR TRANSFORMATION
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
!     TRANSFORM THE (3X3) MASS MATRIX
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
!     PLACE THE TRANSFORMED (3X3) MATRIX INTO A (6X6) MATRIX FOR THE
!     INSERTION ROUTINE
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
            CALL sma2b(aki(1),igp(i),-1,ifmgg,0.0D0)
         ENDDO
         RETURN
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
      CALL mesage(30,I,Idel)
      Nogo = 1
   END SUBROUTINE spag_block_1
!
END SUBROUTINE mtrapr
