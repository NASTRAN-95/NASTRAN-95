
SUBROUTINE ktrapr
   IMPLICIT NONE
   DOUBLE PRECISION Ak(64) , Aki(36) , Akt(9) , Constd(5) , Cosg , D(64) , Degrad , Del , Delint(12) , Dgama , Dgamr , Ee(16) , Er ,&
                  & Et , Ez , Gambq(64) , Grz , R(4) , R1 , R2 , R3 , R4 , Sing , Teo(16) , Twopi , Vrt , Vrz , Vtr , Vtz , Vzr ,   &
                  & Vzt , Z(4) , Z1 , Z2 , Z3 , Z4 , Zmin
   REAL Alf(3) , Anu(3) , Costh , Dodet , Dum1(10) , Dum2(21) , Dum4(7) , Dum5(76) , E(3) , Ecpt(24) , Eltemp , G(3) , Gsube , Rho ,&
      & Sinth , Sp(24) , Stress , Tempe , Tzero
   INTEGER Ibuf , Ics(4) , Idetck , Iecpt(24) , If4gg , Ifkgg , Igkgg , Igp(4) , Iopt4 , Iout , K4ggsw , Link(10) , Matflg ,        &
         & Matidc , Mmsg , Msg(4,1) , Nmsg , Nogo , Npvt
   COMMON /condad/ Constd
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero , Gsube
   COMMON /msgx  / Nmsg , Mmsg , Msg
   COMMON /sma1cl/ Iopt4 , K4ggsw , Npvt , Dum4 , Link , Idetck , Dodet , Nogo
   COMMON /sma1dp/ D , Gambq , R , Z , Teo , Ee , Delint , Ak , Dgama , Zmin , Er , Et , Ez , Vrt , Vtr , Vtz , Vzt , Vzr , Vrz ,   &
                 & Grz , Del , Cosg , Sing , Dgamr , Igp , Ics , Sp , Tempe
   COMMON /sma1et/ Ecpt , Dum5
   COMMON /sma1io/ Dum1 , Ifkgg , Igkgg , If4gg , Dum2
   COMMON /system/ Ibuf , Iout
   DOUBLE PRECISION dampc , ee48 , rmax , rmin
   INTEGER i , i1 , iai , iapp , ic1 , icore , idel , ip , ipp , iq , ir1 , irc , irg , ising , j , j1 , j2 , jj1 , jj2 , jrz(2) ,  &
         & k , matid
   DOUBLE PRECISION rzintd
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
!     FATAL IF RATIO OF RADII IS TO LARGE FOR GUASS QUADRATURE FOR
!     IP =-1
!
   rmin = dmin1(R1,R2,R3,R4)
   rmax = dmax1(R1,R2,R3,R4)
   IF ( rmin/=0.D0 ) THEN
      IF ( rmax/rmin>10.D0 ) THEN
         i = 221
         GOTO 200
      ENDIF
   ENDIF
!
   IF ( R1<R2 .AND. R4<R3 .AND. Z4>Z1 ) THEN
      IF ( dabs(Z1-Z2)<=1.0D-3 ) THEN
         IF ( dabs(Z3-Z4)<=1.0D-3 ) THEN
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
                  R(2) = D(6)
                  R(3) = D(6)
               ENDIF
            ENDIF
!
            icore = 0
            j = 1
            DO i = 1 , 4
               IF ( R(i)==0.D0 ) THEN
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
                  GOTO 200
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
                        Gambq(j+jj1) = 0.D0
                        Gambq(j+jj2) = 0.D0
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
                              Delint(i1) = 0.0D0
                              CYCLE
                           ENDIF
                        ENDIF
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
!     SET MATERIAL PROPERTIES IN DOUBLE PRECISION VARIABLES
!
                  Er = E(1)
                  Et = E(2)
                  Ez = E(3)
                  Vrt = Anu(1)
                  Vtz = Anu(2)
                  Vzr = Anu(3)
                  Grz = G(3)
                  Vtr = Vrt*Et/Er
                  Vzt = Vtz*Ez/Et
                  Vrz = Vzr*Er/Ez
                  Del = 1.0D0 - Vrt*Vtr - Vtz*Vzt - Vzr*Vrz - Vrt*Vtz*Vzr - Vrz*Vtr*Vzt
!
!     GENERATE ELASTIC CONSTANTS MATRIX (4X4)
!
                  Ee(1) = Er*(1.0D0-Vtz*Vzt)/Del
                  Ee(2) = Er*(Vtr+Vzr*Vtz)/Del
                  Ee(3) = Er*(Vzr+Vtr*Vzt)/Del
                  Ee(4) = 0.0D0
                  Ee(5) = Ee(2)
                  Ee(6) = Et*(1.0D0-Vrz*Vzr)/Del
                  Ee(7) = Et*(Vzt+Vrt*Vzr)/Del
                  Ee(8) = 0.0D0
                  Ee(9) = Ee(3)
                  Ee(10) = Ee(7)
                  Ee(11) = Ez*(1.0D0-Vrt*Vtr)/Del
                  Ee(12) = 0.0D0
                  Ee(13) = 0.0D0
                  Ee(14) = 0.0D0
                  Ee(15) = 0.0D0
                  Ee(16) = Grz
!
!     FORM TRANSFORMATION MATRIX (4X4) FROM MATERIAL AXIS TO ELEMENT
!     GEOMETRIC AXIS
!
                  Dgamr = Dgama*Degrad
                  Cosg = dcos(Dgamr)
                  Sing = dsin(Dgamr)
                  Teo(1) = Cosg**2
                  Teo(2) = 0.0D0
                  Teo(3) = Sing**2
                  Teo(4) = Sing*Cosg
                  Teo(5) = 0.0D0
                  Teo(6) = 1.0D0
                  Teo(7) = 0.0D0
                  Teo(8) = 0.0D0
                  Teo(9) = Teo(3)
                  Teo(10) = 0.0D0
                  Teo(11) = Teo(1)
                  Teo(12) = -Teo(4)
                  Teo(13) = -2.0D0*Teo(4)
                  Teo(14) = 0.0D0
                  Teo(15) = -Teo(13)
                  Teo(16) = Teo(1) - Teo(3)
!
!     TRANSFORM THE ELASTIC CONSTANTS MATRIX FROM MATERIAL
!     TO ELEMENT GEOMETRIC AXIS
!
                  CALL gmmatd(Teo,4,4,1,Ee,4,4,0,D)
                  CALL gmmatd(D,4,4,0,Teo,4,4,0,Ee)
!
!     FORM THE ELEMENT STIFFNESS MATRIX IN FIELD COORDINATES
!
                  ee48 = Ee(4) + Ee(8)
                  D(1) = Ee(1) + 2.0D0*Ee(2) + Ee(6)
                  Ak(1) = Ee(6)*Delint(1)
                  Ak(2) = (Ee(2)+Ee(6))*Delint(4)
                  Ak(3) = Ee(6)*Delint(2) + Ee(8)*Delint(4)
                  Ak(4) = (Ee(2)+Ee(6))*Delint(5) + Ee(8)*Delint(7)
                  Ak(5) = 0.0D0
                  Ak(6) = Ee(8)*Delint(4)
                  Ak(7) = Ee(7)*Delint(4)
                  Ak(8) = Ee(7)*Delint(7) + Ee(8)*Delint(5)
                  Ak(9) = Ak(2)
                  Ak(10) = D(1)*Delint(7)
                  Ak(11) = (Ee(2)+Ee(6))*Delint(5) + ee48*Delint(7)
                  Ak(12) = D(1)*Delint(8) + ee48*Delint(10)
                  Ak(13) = 0.0D0
                  Ak(14) = ee48*Delint(7)
                  Ak(15) = (Ee(3)+Ee(7))*Delint(7)
                  Ak(16) = (Ee(3)+Ee(7))*Delint(10) + ee48*Delint(8)
                  Ak(17) = Ak(3)
                  Ak(18) = Ak(11)
                  Ak(19) = Ee(6)*Delint(3) + Ee(16)*Delint(7) + (Ee(8)+Ee(14))*Delint(5)
                  Ak(20) = (Ee(2)+Ee(6))*Delint(6) + Ee(16)*Delint(10) + (Ee(8)+Ee(13)+Ee(14))*Delint(8)
                  Ak(21) = 0.0D0
                  Ak(22) = Ee(16)*Delint(7) + Ee(8)*Delint(5)
                  Ak(23) = Ee(7)*Delint(5) + Ee(15)*Delint(7)
                  Ak(24) = (Ee(7)+Ee(16))*Delint(8) + Ee(8)*Delint(6) + Ee(15)*Delint(10)
                  Ak(25) = Ak(4)
                  Ak(26) = Ak(12)
                  Ak(27) = Ak(20)
                  Ak(28) = D(1)*Delint(9) + Ee(16)*Delint(12) + (ee48+Ee(13)+Ee(14))*Delint(11)
                  Ak(29) = 0.0D0
                  Ak(30) = Ee(16)*Delint(10) + ee48*Delint(8)
                  Ak(31) = (Ee(3)+Ee(7))*Delint(8) + Ee(15)*Delint(10)
                  Ak(32) = (Ee(3)+Ee(7)+Ee(16))*Delint(11) + Ee(15)*Delint(12) + ee48*Delint(9)
                  Ak(33) = 0.0D0
                  Ak(34) = 0.0D0
                  Ak(35) = 0.0D0
                  Ak(36) = 0.0D0
                  Ak(37) = 0.0D0
                  Ak(38) = 0.0D0
                  Ak(39) = 0.0D0
                  Ak(40) = 0.0D0
                  Ak(41) = Ak(6)
                  Ak(42) = Ak(14)
                  Ak(43) = Ak(22)
                  Ak(44) = Ak(30)
                  Ak(45) = 0.0D0
                  Ak(46) = Ee(16)*Delint(7)
                  Ak(47) = Ee(15)*Delint(7)
                  Ak(48) = Ee(16)*Delint(8) + Ee(15)*Delint(10)
                  Ak(49) = Ak(7)
                  Ak(50) = Ak(15)
                  Ak(51) = Ak(23)
                  Ak(52) = Ak(31)
                  Ak(53) = 0.0D0
                  Ak(54) = Ak(47)
                  Ak(55) = Ee(11)*Delint(7)
                  Ak(56) = Ee(11)*Delint(10) + Ee(12)*Delint(8)
                  Ak(57) = Ak(8)
                  Ak(58) = Ak(16)
                  Ak(59) = Ak(24)
                  Ak(60) = Ak(32)
                  Ak(61) = 0.0D0
                  Ak(62) = Ak(48)
                  Ak(63) = Ak(56)
                  Ak(64) = Ee(11)*Delint(12) + Ee(16)*Delint(9) + (Ee(12)+Ee(15))*Delint(11)
!
                  DO i = 1 , 64
                     Ak(i) = Twopi*Ak(i)
                  ENDDO
!
!     TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD COORDINATES
!     TO GRID POINT DEGREES OF FREEDOM
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
!     TRANSFORM THE (3X3) STIFFNESS MATRIX
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
!     PLACE THE TRANSFORMED (3X3) MATRIX INTO A (6X6) MATRIX FOR
!     THE INSERTION ROUTINE
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
                     CALL sma1b(Aki(1),Igp(i),-1,Ifkgg,0.0D0)
                     IF ( Iopt4/=0 .AND. Gsube/=0.0 ) THEN
                        K4ggsw = 1
                        dampc = Gsube
                        CALL sma1b(Aki(1),Igp(i),-1,If4gg,dampc)
                     ENDIF
                  ENDDO
                  RETURN
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
 100  i = 37
! ...     221 WILL PRINT USER MESSAGE 2218
!
 200  IF ( Nmsg/=0 ) THEN
      IF ( Nmsg>=Mmsg ) RETURN
      DO j = 1 , Nmsg
         IF ( Msg(3,j)==idel .AND. Msg(2,j)==i ) RETURN
      ENDDO
   ENDIF
   Ics(1) = idel
   Ics(2) = irg
   CALL mesage(30,i,Ics)
   Nogo = 1
!
END SUBROUTINE ktrapr