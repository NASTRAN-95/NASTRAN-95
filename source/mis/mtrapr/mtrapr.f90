!*==mtrapr.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mtrapr
USE C_CONDAD
USE C_MATIN
USE C_MATOUT
USE C_SMA2CL
USE C_SMA2DP
USE C_SMA2ET
USE C_SMA2IO
USE C_SYSTEM
USE ISO_FORTRAN_ENV                 
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
   Igp(1) = iecpt(2)
   Igp(2) = iecpt(3)
   Igp(3) = iecpt(4)
   Igp(4) = iecpt(5)
   matid = iecpt(7)
   Ics(1) = iecpt(8)
   Ics(2) = iecpt(12)
   Ics(3) = iecpt(16)
   Ics(4) = iecpt(20)
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
   Tempe = Ecpt(24)
   Dgama = Ecpt(6)
!
!     CHECK INTERNAL GRID POINTS FOR PIVOT POINT
!
   ipp = 0
   DO i = 1 , 4
      IF ( Npvt==Igp(i) ) ipp = i
   ENDDO
   IF ( ipp==0 ) CALL mesage(-30,34,idel)
!
!     TEST THE VALIDITY OF THE GRID POINT COORDINATES
!
   DO i = 1 , 4
      IF ( R(i)<0.0D0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      IF ( D(i)/=0.0D0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
!
!     COMPUTE THE ELEMENT COORDINATES
!
   Zmin = dmin1(z1,z2,z3,z4)
   z1 = z1 - Zmin
   z2 = z2 - Zmin
   z3 = z3 - Zmin
   z4 = z4 - Zmin
!
!     FATAL IF RATIO OF RADII IS TO LARGE FOR GUASS QUADRATURE FOR IP=-1
!
   rmin = dmin1(r1,r2,r3,r4)
   rmax = dmax1(r1,r2,r3,r4)
   IF ( rmin/=0.D0 ) THEN
      IF ( rmax/rmin>10.D0 ) THEN
         i = 218
         Nogo = 1
         RETURN
      ENDIF
   ENDIF
!
   D(5) = (r1+r4)/2.0D0
   D(6) = (r2+r3)/2.0D0
   IF ( D(5)/=0.0D0 ) THEN
      IF ( dabs((r1-r4)/D(5))<=0.5D-2 ) THEN
         r1 = D(5)
         r4 = D(5)
      ENDIF
   ENDIF
   IF ( D(6)/=0.0D0 ) THEN
      IF ( dabs((r2-r3)/D(6))<=0.5D-2 ) THEN
         r2 = D(6)
         r3 = D(6)
      ENDIF
   ENDIF
!
   icore = 0
   j = 1
   DO i = 1 , 4
      IF ( R(i)==0.0D0 ) THEN
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
         Gambq(i) = 0.0D0
      ENDDO
      Gambq(1) = 1.0D0
      Gambq(2) = r1
      Gambq(3) = z1
      Gambq(4) = r1*z1
      Gambq(13) = 1.0D0
      Gambq(14) = r1
      Gambq(15) = z1
      Gambq(16) = Gambq(4)
      Gambq(17) = 1.0D0
      Gambq(18) = r2
      Gambq(19) = z2
      Gambq(20) = r2*z2
      Gambq(29) = 1.0D0
      Gambq(30) = r2
      Gambq(31) = z2
      Gambq(32) = Gambq(20)
      Gambq(33) = 1.0D0
      Gambq(34) = r3
      Gambq(35) = z3
      Gambq(36) = r3*z3
      Gambq(45) = 1.0D0
      Gambq(46) = r3
      Gambq(47) = z3
      Gambq(48) = Gambq(36)
      Gambq(49) = 1.0D0
      Gambq(50) = r4
      Gambq(51) = z4
      Gambq(52) = r4*z4
      Gambq(61) = 1.0D0
      Gambq(62) = r4
      Gambq(63) = z4
      Gambq(64) = Gambq(52)
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
      ising = -1
      CALL inverd(8,Gambq(1),8,D(10),0,D(11),ising,Sp)
      IF ( ising==2 ) THEN
         i = 26
!
!     ERROR TYPE 218 HAD BEEN ISSUED BY KTRAPR ALREADY.
!
         CALL mesage(30,i,idel)
         Nogo = 1
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
               Gambq(i) = 0.0D0
               Gambq(i+16) = 0.0D0
               Gambq(j+jj1) = 0.0D0
               Gambq(j+jj2) = 0.0D0
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
               Delint(i1) = rzintd(ip,iq,R,Z,4)
            ENDDO
         ENDDO
!
!     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3 TABLE
!
         Matidc = matid
         Matflg = 7
         Eltemp = Tempe
         CALL mat(idel)
!
!    SET MATERIAL PROPERTIES IN DOUBLE PRECISION VARIABLES
!
         Rhod = Rho
!
!     GENERATE THE CONSISTENT MASS MATRIX IN FIELD COORDINATES
!
         DO i = 1 , 64
            am(i) = 0.0D0
         ENDDO
         Twopi = d2pi*Rhod
         am(1) = Twopi*Delint(1)
         am(2) = Twopi*Delint(4)
         am(3) = Twopi*Delint(2)
         am(4) = Twopi*Delint(5)
         am(9) = am(2)
         am(10) = Twopi*Delint(7)
         am(11) = Twopi*Delint(5)
         am(12) = Twopi*Delint(8)
         am(17) = am(3)
         am(18) = am(11)
         am(19) = Twopi*Delint(3)
         am(20) = Twopi*Delint(6)
         am(25) = am(4)
         am(26) = am(12)
         am(27) = am(20)
         am(28) = Twopi*Delint(9)
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
         CALL gmmatd(Gambq,8,8,1,Ak,8,8,0,D)
         CALL gmmatd(D,8,8,0,Gambq,8,8,0,Ak)
!
!     ZERO OUT THE (6X6) MATRIX USED AS INPUT TO THE INSERTION ROUTINE
!
         DO i = 1 , 36
            Aki(i) = 0.0D0
         ENDDO
!
!     LOCATE THE TRANSFORMATION MATRICES FOR THE FOUR  GRID POINTS
!
         DO i = 1 , 4
            IF ( Ics(i)/=0 ) THEN
               k = 9*(i-1) + 1
               CALL transd(Ics(i),D(k))
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
            Akt(1) = Ak(irc)
            Akt(2) = 0.0D0
            Akt(3) = Ak(irc+1)
            Akt(4) = 0.0D0
            Akt(5) = 0.0D0
            Akt(6) = 0.0D0
            Akt(7) = Ak(irc+8)
            Akt(8) = 0.0D0
            Akt(9) = Ak(irc+9)
!
!     TRANSFORM THE (3X3) MASS MATRIX
!
            IF ( Ics(ipp)/=0 ) THEN
               CALL gmmatd(D(iapp),3,3,1,Akt(1),3,3,0,D(37))
               DO j = 1 , 9
                  Akt(j) = D(j+36)
               ENDDO
            ENDIF
            IF ( Ics(i)/=0 ) THEN
               iai = 9*(i-1) + 1
               CALL gmmatd(Akt(1),3,3,0,D(iai),3,3,0,D(37))
               DO j = 1 , 9
                  Akt(j) = D(j+36)
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
                  Aki(k) = Akt(j)
               ENDDO
            ENDDO
!
!     CALL THE INSERTION ROUTINE
!
            CALL sma2b(Aki(1),Igp(i),-1,Ifmgg,0.0D0)
         ENDDO
         RETURN
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
      i = 37
      CALL mesage(30,i,idel)
      Nogo = 1
   END SUBROUTINE spag_block_1
!
END SUBROUTINE mtrapr
