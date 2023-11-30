
SUBROUTINE mtrirg
   IMPLICIT NONE
   DOUBLE PRECISION Ak(36) , Aki(36) , Akt(9) , Am(36) , Area , Constd(5) , D(36) , D2pi , Delint(8) , Dgama , Dr , Dz , Gambq(36) ,&
                  & R(3) , R1 , R2 , R3 , Ra , Rh , Rhod , Twopi , Z(3) , Z1 , Z2 , Z3 , Za , Zh , Zmin
   REAL Alf(3) , Anu(3) , Costh , Dum1(10) , Dum2(25) , Dum3(2) , Dum4(7) , Dum5(81) , E(3) , Ecpt(19) , Eltemp , G(3) , Rho ,      &
      & Sinth , Sp(18) , Stress , Tempe , Tzero
   INTEGER Ics(3) , Iecpt(19) , Ifmgg , Igp(3) , Link(10) , Matflg , Matidc , Nogo , Npvt
   COMMON /condad/ Constd
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , Anu , Rho , G , Alf , Tzero
   COMMON /sma2cl/ Dum3 , Npvt , Dum4 , Link , Nogo
   COMMON /sma2dp/ D , Gambq , R , Z , Delint , Ak , Aki , Akt , Dgama , Zmin , Rhod , Twopi , Dr , Rh , Dz , Zh , Ra , Za , Area , &
                 & Igp , Ics , Sp , Tempe
   COMMON /sma2et/ Ecpt , Dum5
   COMMON /sma2io/ Dum1 , Ifmgg , Dum2
   DOUBLE PRECISION dki
   INTEGER i , i1 , iai , iapp , ic1 , idel , ieror1 , ieror2 , ip , ipp , iq , ir1 , irc , ising , j , j1 , j2 , k , kode , matid
!
!
!*****
! THIS ROUTINE COMPUTES THE   MASS    MATRIX FOR A AXI-SYMMETRIC RING
! WITH A TRIANGULAR CROSS SECTION
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
!
   EQUIVALENCE (Iecpt(1),Ecpt(1))
   EQUIVALENCE (R(1),R1) , (R(2),R2) , (R(3),R3) , (Z(1),Z1) , (Z(2),Z2) , (Z(3),Z3)
   EQUIVALENCE (Am(1),Ak(1))
   EQUIVALENCE (Constd(2),D2pi)
!
! ----------------------------------------------------------------------
!
! STORE ECPT PARAMETERS IN LOCAL VARIABLES
!
   idel = Iecpt(1)
   Igp(1) = Iecpt(2)
   Igp(2) = Iecpt(3)
   Igp(3) = Iecpt(4)
   matid = Iecpt(6)
   Ics(1) = Iecpt(7)
   Ics(2) = Iecpt(11)
   Ics(3) = Iecpt(15)
   R(1) = Ecpt(8)
   D(1) = Ecpt(9)
   Z(1) = Ecpt(10)
   R(2) = Ecpt(12)
   D(2) = Ecpt(13)
   Z(2) = Ecpt(14)
   R(3) = Ecpt(16)
   D(3) = Ecpt(17)
   Z(3) = Ecpt(18)
   Tempe = Ecpt(19)
   Dgama = Ecpt(5)
!
!
! CHECK INTERNAL GRID POINTS FOR PIVOT POINT
!
   ipp = 0
   DO i = 1 , 3
      IF ( Npvt==Igp(i) ) ipp = i
   ENDDO
   IF ( ipp==0 ) CALL mesage(-30,34,idel)
!
!
! TEST THE VALIDITY OF THE GRID POINT COORDINATES
!
   ieror1 = 0
   DO i = 1 , 3
      IF ( R(i)<=0.0D0 ) THEN
         IF ( ieror1==0 ) THEN
            CALL mesage(30,211,idel)
            ieror1 = 1
         ENDIF
      ENDIF
   ENDDO
   ieror2 = 0
   DO i = 1 , 3
      IF ( D(i)/=0.0D0 ) THEN
         IF ( ieror2==0 ) THEN
            CALL mesage(30,212,idel)
            ieror2 = 1
         ENDIF
      ENDIF
   ENDDO
!
!
! COMPUTE THE ELEMENT COORDINATES
!
   IF ( ieror1==0 .AND. ieror2==0 ) THEN
      Zmin = dmin1(Z1,Z2,Z3)
      Z1 = Z1 - Zmin
      Z2 = Z2 - Zmin
      Z3 = Z3 - Zmin
!
!
!
! FORM THE TRANSFORMATION MATRIX (6X6) FROM FIELD COORDINATES TO GRID
! POINT DEGREES OF FREEDOM
!
      DO i = 1 , 36
         Gambq(i) = 0.0D0
      ENDDO
      Gambq(1) = 1.0D0
      Gambq(2) = R1
      Gambq(3) = Z1
      Gambq(10) = 1.0D0
      Gambq(11) = R1
      Gambq(12) = Z1
      Gambq(13) = 1.0D0
      Gambq(14) = R2
      Gambq(15) = Z2
      Gambq(22) = 1.0D0
      Gambq(23) = R2
      Gambq(24) = Z2
      Gambq(25) = 1.0D0
      Gambq(26) = R3
      Gambq(27) = Z3
      Gambq(34) = 1.0D0
      Gambq(35) = R3
      Gambq(36) = Z3
!
!
!     NONEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      ising = -1
      CALL inverd(6,Gambq(1),6,D(10),0,D(11),ising,Sp)
!
      IF ( ising==2 ) THEN
         CALL mesage(30,37,idel)
      ELSE
!
!
!
! CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT WHERE THE ORDER IS
! INDICATED BY THE FOLLOWING TABLE
!
!              DELINT( 1) - ( 1,0)
!              DELINT( 2) - ( 1,1)
!              DELINT( 3) - ( 1,2)
!              DELINT( 4) - ( 2,0)
!              DELINT( 5) - ( 2,1)
!              DELINT( 6) - ( 0,2)
!              DELINT( 7) - ( 3,0)
!              DELINT( 8) - (-1,2)
!
!
! TEST FOR RELATIVE SMALL AREA OF INTEGRATION
! AND IF AREA IS SMALL THEN APPROXIMATE INTEGRALS
!
         Dr = dmax1(dabs(R1-R2),dabs(R2-R3),dabs(R3-R1))
         Rh = dmin1(R1,R2,R3)/10.0D0
         Dz = dmax1(dabs(Z1-Z2),dabs(Z2-Z3),dabs(Z3-Z1))
         Zh = dmin1(Z1,Z2,Z3)/10.0D0
         Ra = (R1+R2+R3)/3.0D0
         Za = (Z1+Z2+Z3)/3.0D0
         Area = (R1*(Z2-Z3)+R2*(Z3-Z1)+R3*(Z1-Z2))/2.0D0
         kode = 0
         IF ( dabs((R2-R1)/R2)<1.0D-5 ) kode = 1
         IF ( Dr<=Rh .OR. Dz<=Zh ) kode = -1
         DO
!
!
            i1 = 0
            DO i = 1 , 3
               ip = i
               DO j = 1 , 3
                  iq = j - 1
                  IF ( ip==2 .AND. iq==2 ) ip = 0
                  IF ( ip==3 .AND. iq==2 ) ip = -1
                  IF ( ip/=3 .OR. iq/=1 ) THEN
                     i1 = i1 + 1
                     IF ( kode<0 ) THEN
                        Delint(i1) = ((Ra)**ip)*((Za)**iq)*Area
                     ELSEIF ( kode==0 ) THEN
                        Delint(i1) = dki(1,3,1,2,1,3,ip,iq,R,Z) + dki(3,2,1,2,3,2,ip,iq,R,Z)
                     ELSE
                        Delint(i1) = dki(1,3,3,2,1,3,ip,iq,R,Z)
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
!
!
! TEST FOR EXCESSIVE ROUND-OFF ERROR IN INTEGRAL CALCULATIONS
! AND IF IT EXIST APPROXIMATE INTEGRALS
!
            IF ( kode<0 ) EXIT
            DO i = 1 , 8
               IF ( Delint(i)<0.0D0 ) GOTO 10
            ENDDO
            IF ( Delint(3)>Delint(6) ) THEN
               IF ( Delint(8)<Delint(3) ) THEN
                  IF ( Delint(8)<=Delint(6) ) EXIT
               ENDIF
            ENDIF
 10         kode = -1
         ENDDO
!
!
!
! LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3 TABLE
!
         Matidc = matid
         Matflg = 7
         Eltemp = Tempe
         CALL mat(idel)
!
!
! SET MATERIAL PROPERTIES IN DOUBLE PRECISION VARIABLES
!
         Rhod = Rho
!
! GENERATE THE CONSISTENT MASS MATRIX IN FIELD COORDINATES
!
         DO i = 1 , 36
            Am(i) = 0.0D0
         ENDDO
         Twopi = D2pi*Rhod
         Am(1) = Twopi*Delint(1)
         Am(2) = Twopi*Delint(4)
         Am(3) = Twopi*Delint(2)
         Am(7) = Am(2)
         Am(8) = Twopi*Delint(7)
         Am(9) = Twopi*Delint(5)
         Am(13) = Am(3)
         Am(14) = Am(9)
         Am(15) = Twopi*Delint(3)
         Am(22) = Am(1)
         Am(23) = Am(2)
         Am(24) = Am(3)
         Am(28) = Am(23)
         Am(29) = Am(8)
         Am(30) = Am(9)
         Am(34) = Am(24)
         Am(35) = Am(30)
         Am(36) = Am(15)
!
! TRANSFORM THE ELEMENT   MASS    MATRIX FROM FIELD COORDINATES
! TO GRID POINT DEGREES OF FREEDOM
!
         CALL gmmatd(Gambq,6,6,1,Ak,6,6,0,D)
         CALL gmmatd(D,6,6,0,Gambq,6,6,0,Ak)
!
!
!
! ZERO OUT THE (6X6) MATRIX USED AS INPUT TO THE INSERTION ROUTINE
!
         DO i = 1 , 36
            Aki(i) = 0.0D0
         ENDDO
!
!
! LOCATE THE TRANSFORMATION MATRICES FOR THE THREE GRID POINTS
!
         DO i = 1 , 3
            IF ( Ics(i)/=0 ) THEN
               k = 9*(i-1) + 1
               CALL transd(Ics(i),D(k))
            ENDIF
         ENDDO
!
!
!
! START THE LOOP FOR INSERTION OF THE THREE (6X6) MATRICES
! INTO THE MASTER MASS MATRIX
!
         ir1 = 2*ipp - 1
         iapp = 9*(ipp-1) + 1
         DO i = 1 , 3
!
! PLACE THE APPROIATE (2X2) SUBMATRIX OF THE MASS MATRIX
! IN A (3X3) MATRIX FOR TRANSFORMATION
!
            ic1 = 2*i - 1
            irc = (ir1-1)*6 + ic1
            Akt(1) = Ak(irc)
            Akt(2) = 0.0D0
            Akt(3) = Ak(irc+1)
            Akt(4) = 0.0D0
            Akt(5) = 0.0D0
            Akt(6) = 0.0D0
            Akt(7) = Ak(irc+6)
            Akt(8) = 0.0D0
            Akt(9) = Ak(irc+7)
!
! TRANSFORM THE (3X3) MASS MATRIX
!
            IF ( Ics(ipp)/=0 ) THEN
               CALL gmmatd(D(iapp),3,3,1,Akt(1),3,3,0,D(28))
               DO j = 1 , 9
                  Akt(j) = D(j+27)
               ENDDO
            ENDIF
            IF ( Ics(i)/=0 ) THEN
               iai = 9*(i-1) + 1
               CALL gmmatd(Akt(1),3,3,0,D(iai),3,3,0,D(28))
               DO j = 1 , 9
                  Akt(j) = D(j+27)
               ENDDO
            ENDIF
!
! PLACE THE TRANSFORMED (3X3) MATRIX INTO A (6X6) MATRIX FOR
! THE INSERTION ROUTINE
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
! CALL THE INSERTION ROUTINE
!
            CALL sma2b(Aki(1),Igp(i),-1,Ifmgg,0.0D0)
         ENDDO
         RETURN
      ENDIF
   ELSE
      Nogo = 2
      RETURN
   ENDIF
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
   Nogo = 1
   RETURN
!
END SUBROUTINE mtrirg
