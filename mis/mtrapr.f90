
SUBROUTINE mtrapr
   IMPLICIT NONE
   DOUBLE PRECISION Ak(64) , Aki(36) , Akt(9) , Am(64) , Constd(5) , D(64) , D2pi , Delint(12) , Dgama , Gambq(64) , R(4) , R1 ,    &
                  & R2 , R3 , R4 , Rhod , Twopi , Z(4) , Z1 , Z2 , Z3 , Z4 , Zmin
   REAL Alf(3) , Anu(3) , Costh , Dum1(10) , Dum2(25) , Dum3(2) , Dum4(7) , Dum5(76) , E(3) , Ecpt(24) , Eltemp , G(3) , Rho ,      &
      & Sinth , Sp(24) , Stress , Tempe , Tzero
   INTEGER Ibuf , Ics(4) , Iecpt(24) , Ifmgg , Igp(4) , Iout , Link(10) , Matflg , Matidc , Nogo , Npvt
   COMMON /condad/ Constd
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero
   COMMON /sma2cl/ Dum3 , Npvt , Dum4 , Link , Nogo
   COMMON /sma2dp/ D , Gambq , R , Z , Delint , Ak , Aki , Akt , Dgama , Zmin , Rhod , Twopi , Igp , Ics , Sp , Tempe
   COMMON /sma2et/ Ecpt , Dum5
   COMMON /sma2io/ Dum1 , Ifmgg , Dum2
   COMMON /system/ Ibuf , Iout
   INTEGER i , i1 , iai , iapp , ic1 , icore , idel , ip , ipp , iq , ir1 , irc , ising , j , j1 , j2 , jj1 , jj2 , jrz(2) , k ,    &
         & matid
   DOUBLE PRECISION rmax , rmin
   DOUBLE PRECISION rzintd
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
   idel = Iecpt(1)
   Igp(1) = Iecpt(2)
   Igp(2) = Iecpt(3)
   Igp(3) = Iecpt(4)
   Igp(4) = Iecpt(5)
   matid = Iecpt(7)
   Ics(1) = Iecpt(8)
   Ics(2) = Iecpt(12)
   Ics(3) = Iecpt(16)
   Ics(4) = Iecpt(20)
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
      IF ( R(i)<0.0D0 ) GOTO 100
      IF ( D(i)/=0.0D0 ) GOTO 100
   ENDDO
!
!     COMPUTE THE ELEMENT COORDINATES
!
   Zmin = dmin1(Z1,Z2,Z3,Z4)
   Z1 = Z1 - Zmin
   Z2 = Z2 - Zmin
   Z3 = Z3 - Zmin
   Z4 = Z4 - Zmin
!
!     FATAL IF RATIO OF RADII IS TO LARGE FOR GUASS QUADRATURE FOR IP=-1
!
   rmin = dmin1(R1,R2,R3,R4)
   rmax = dmax1(R1,R2,R3,R4)
   IF ( rmin/=0.D0 ) THEN
      IF ( rmax/rmin>10.D0 ) THEN
         i = 218
         Nogo = 1
         GOTO 99999
      ENDIF
   ENDIF
!
   D(5) = (R1+R4)/2.0D0
   D(6) = (R2+R3)/2.0D0
   IF ( D(5)/=0.0D0 ) THEN
      IF ( dabs((R1-R4)/D(5))<=0.5D-2 ) THEN
         R1 = D(5)
         R4 = D(5)
      ENDIF
   ENDIF
   IF ( D(6)/=0.0D0 ) THEN
      IF ( dabs((R2-R3)/D(6))<=0.5D-2 ) THEN
         R2 = D(6)
         R3 = D(6)
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
      Gambq(2) = R1
      Gambq(3) = Z1
      Gambq(4) = R1*Z1
      Gambq(13) = 1.0D0
      Gambq(14) = R1
      Gambq(15) = Z1
      Gambq(16) = Gambq(4)
      Gambq(17) = 1.0D0
      Gambq(18) = R2
      Gambq(19) = Z2
      Gambq(20) = R2*Z2
      Gambq(29) = 1.0D0
      Gambq(30) = R2
      Gambq(31) = Z2
      Gambq(32) = Gambq(20)
      Gambq(33) = 1.0D0
      Gambq(34) = R3
      Gambq(35) = Z3
      Gambq(36) = R3*Z3
      Gambq(45) = 1.0D0
      Gambq(46) = R3
      Gambq(47) = Z3
      Gambq(48) = Gambq(36)
      Gambq(49) = 1.0D0
      Gambq(50) = R4
      Gambq(51) = Z4
      Gambq(52) = R4*Z4
      Gambq(61) = 1.0D0
      Gambq(62) = R4
      Gambq(63) = Z4
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
         GOTO 99999
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
            Am(i) = 0.0D0
         ENDDO
         Twopi = D2pi*Rhod
         Am(1) = Twopi*Delint(1)
         Am(2) = Twopi*Delint(4)
         Am(3) = Twopi*Delint(2)
         Am(4) = Twopi*Delint(5)
         Am(9) = Am(2)
         Am(10) = Twopi*Delint(7)
         Am(11) = Twopi*Delint(5)
         Am(12) = Twopi*Delint(8)
         Am(17) = Am(3)
         Am(18) = Am(11)
         Am(19) = Twopi*Delint(3)
         Am(20) = Twopi*Delint(6)
         Am(25) = Am(4)
         Am(26) = Am(12)
         Am(27) = Am(20)
         Am(28) = Twopi*Delint(9)
         DO i = 1 , 4
            k = (i-1)*8
            DO j = 1 , 4
               k = k + 1
               Am(k+36) = Am(k)
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
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
 100  i = 37
   CALL mesage(30,i,idel)
   Nogo = 1
!
99999 RETURN
END SUBROUTINE mtrapr