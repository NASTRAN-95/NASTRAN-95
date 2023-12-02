!*==ktrirg.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ktrirg
USE C_CONDAD
USE C_MATIN
USE C_MATOUT
USE C_SMA1CL
USE C_SMA1DP
USE C_SMA1ET
USE C_SMA1IO
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(36) :: aki
   REAL(REAL64) , DIMENSION(9) :: akt
   REAL(REAL64) :: dampc , degrad , r1 , r2 , r3 , twopi , z1 , z2 , z3
   INTEGER :: i , i1 , iai , iapp , ic1 , idel , ieror1 , ieror2 , ip , ipp , iq , ir1 , irc , ising , j , j1 , j2 , k , kode ,     &
            & matid
   INTEGER , DIMENSION(19) :: iecpt
   EXTERNAL dki , gmmatd , inverd , mat , mesage , sma1b , transd
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!
!*****
! THIS ROUTINE COMPUTES THE STIFFNESS MATRIX FOR A AXI-SYMMETRIC RING
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
   !>>>>EQUIVALENCE (Constd(2),Twopi)
   !>>>>EQUIVALENCE (Constd(4),Degrad)
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1))
   !>>>>EQUIVALENCE (R(1),R1) , (R(2),R2) , (R(3),R3) , (Z(1),Z1) , (Z(2),Z2) , (Z(3),Z3)
   !>>>>EQUIVALENCE (Aki(1),Gambq(1))
   !>>>>EQUIVALENCE (Akt(1),Teo(1))
!
! ----------------------------------------------------------------------
!
! STORE ECPT PARAMETERS IN LOCAL VARIABLES
!
   idel = iecpt(1)
   Igp(1) = iecpt(2)
   Igp(2) = iecpt(3)
   Igp(3) = iecpt(4)
   matid = iecpt(6)
   Ics(1) = iecpt(7)
   Ics(2) = iecpt(11)
   Ics(3) = iecpt(15)
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
   IF ( ieror1/=0 .OR. ieror2/=0 ) THEN
      Nogo = 2
      RETURN
   ELSEIF ( (r2-r1)*(z3-z1)-(r3-r1)*(z2-z1)>=0.0D0 ) THEN
!
!
! COMPUTE THE ELEMENT COORDINATES
!
      Zmin = dmin1(z1,z2,z3)
      z1 = z1 - Zmin
      z2 = z2 - Zmin
      z3 = z3 - Zmin
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
      Gambq(2) = r1
      Gambq(3) = z1
      Gambq(10) = 1.0D0
      Gambq(11) = r1
      Gambq(12) = z1
      Gambq(13) = 1.0D0
      Gambq(14) = r2
      Gambq(15) = z2
      Gambq(22) = 1.0D0
      Gambq(23) = r2
      Gambq(24) = z2
      Gambq(25) = 1.0D0
      Gambq(26) = r3
      Gambq(27) = z3
      Gambq(34) = 1.0D0
      Gambq(35) = r3
      Gambq(36) = z3
!
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      ising = -1
      CALL inverd(6,Gambq(1),6,D(10),0,D(11),ising,Sp)
!
      IF ( ising/=2 ) THEN
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
         Dr = dmax1(dabs(r1-r2),dabs(r2-r3),dabs(r3-r1))
         Rh = dmin1(r1,r2,r3)/10.0D0
         Dz = dmax1(dabs(z1-z2),dabs(z2-z3),dabs(z3-z1))
         Zh = dmin1(z1,z2,z3)/10.0D0
         Ra = (r1+r2+r3)/3.0D0
         Za = (z1+z2+z3)/3.0D0
         Area = (r1*(z2-z3)+r2*(z3-z1)+r3*(z1-z2))/2.0D0
         kode = 0
         IF ( dabs((r2-r1)/r2)<1.0D-5 ) kode = 1
         IF ( Dr<=Rh .OR. Dz<=Zh ) kode = -1
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
                              Delint(i1) = ((Ra)**ip)*((Za)**iq)*Area
                           ELSEIF ( kode==0 ) THEN
                              Delint(i1) = dki(1,3,1,2,1,3,ip,iq,R,Z) + dki(3,2,1,2,3,2,ip,iq,R,Z)
                           ELSE
                              Delint(i1) = dki(1,3,3,2,1,3,ip,iq,R,Z)
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
                  IF ( kode<0 ) EXIT SPAG_Loop_1_1
                  DO i = 1 , 8
                     IF ( Delint(i)<0.0D0 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  IF ( Delint(8)>Delint(7) ) THEN
                     IF ( Delint(3)<Delint(8) ) THEN
                        IF ( Delint(3)<=Delint(7) ) EXIT SPAG_Loop_1_1
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
         Matidc = matid
         Matflg = 7
         Eltemp = Tempe
         CALL mat(idel)
!
!
! SET MATERIAL PROPERTIES IN DOUBLE PRECISION VARIABLES
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
!
! GENERATE ELASTIC CONSTANTS MATRIX (4X4)
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
!
! FORM TRANSFORMATION MATRIX (4X4) FROM MATERIAL AXIS TO ELEMENT
! GEOMETRIC AXIS
!
         Dgamr = Dgama*degrad
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
!
! TRANSFORM THE ELASTIC CONSTANTS MATRIX FROM MATERIAL
! TO ELEMENT GEOMETRIC AXIS
!
         CALL gmmatd(Teo,4,4,1,Ee,4,4,0,D)
         CALL gmmatd(D,4,4,0,Teo,4,4,0,Ee)
!
!
!
! FORM THE ELEMENT STIFFNESS MATRIX IN FIELD COORDINATES
!
         Ak(1) = Ee(6)*Delint(1)
         Ak(2) = (Ee(2)+Ee(6))*Delint(4)
         Ak(3) = Ee(6)*Delint(2) + Ee(8)*Delint(4)
         Ak(4) = 0.0D0
         Ak(5) = Ee(8)*Delint(4)
         Ak(6) = Ee(7)*Delint(4)
         Ak(7) = Ak(2)
         Ak(8) = (Ee(1)+2.0D0*Ee(2)+Ee(6))*Delint(6)
         Ak(9) = (Ee(2)+Ee(6))*Delint(5) + (Ee(4)+Ee(8))*Delint(6)
         Ak(10) = 0.0D0
         Ak(11) = (Ee(4)+Ee(8))*Delint(6)
         Ak(12) = (Ee(3)+Ee(7))*Delint(6)
         Ak(13) = Ak(3)
         Ak(14) = Ak(9)
         Ak(15) = Ee(6)*Delint(3) + 2.0D0*Ee(8)*Delint(5) + Ee(16)*Delint(6)
         Ak(16) = 0.0D0
         Ak(17) = Ee(8)*Delint(5) + Ee(16)*Delint(6)
         Ak(18) = Ee(7)*Delint(5) + Ee(12)*Delint(6)
         Ak(19) = 0.0D0
         Ak(20) = 0.0D0
         Ak(21) = 0.0D0
         Ak(22) = 0.0D0
         Ak(23) = 0.0D0
         Ak(24) = 0.0D0
         Ak(25) = Ak(5)
         Ak(26) = Ak(11)
         Ak(27) = Ak(17)
         Ak(28) = 0.0D0
         Ak(29) = Ee(16)*Delint(6)
         Ak(30) = Ee(12)*Delint(6)
         Ak(31) = Ak(6)
         Ak(32) = Ak(12)
         Ak(33) = Ak(18)
         Ak(34) = 0.0D0
         Ak(35) = Ak(30)
         Ak(36) = Ee(11)*Delint(6)
!
         DO i = 1 , 36
            Ak(i) = twopi*Ak(i)
         ENDDO
!
! TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD COORDINATES
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
            aki(i) = 0.0D0
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
! INTO THE MASTER STIFFNESS MATRIX
!
         ir1 = 2*ipp - 1
         iapp = 9*(ipp-1) + 1
         DO i = 1 , 3
!
! PLACE THE APPROIATE (2X2) SUBMATRIX OF THE STIFFNESS MATRIX
! IN A (3X3) MATRIX FOR TRANSFORMATION
!
            ic1 = 2*i - 1
            irc = (ir1-1)*6 + ic1
            akt(1) = Ak(irc)
            akt(2) = 0.0D0
            akt(3) = Ak(irc+1)
            akt(4) = 0.0D0
            akt(5) = 0.0D0
            akt(6) = 0.0D0
            akt(7) = Ak(irc+6)
            akt(8) = 0.0D0
            akt(9) = Ak(irc+7)
!
! TRANSFORM THE (3X3) STIFFNESS MATRIX
!
            IF ( Ics(ipp)/=0 ) THEN
               CALL gmmatd(D(iapp),3,3,1,akt(1),3,3,0,D(28))
               DO j = 1 , 9
                  akt(j) = D(j+27)
               ENDDO
            ENDIF
            IF ( Ics(i)/=0 ) THEN
               iai = 9*(i-1) + 1
               CALL gmmatd(akt(1),3,3,0,D(iai),3,3,0,D(28))
               DO j = 1 , 9
                  akt(j) = D(j+27)
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
                  aki(k) = akt(j)
               ENDDO
            ENDDO
!
! CALL THE INSERTION ROUTINE
!
            CALL sma1b(aki(1),Igp(i),-1,Ifkgg,0.0D0)
            IF ( Iopt4/=0 .AND. Gsube/=0.0 ) THEN
               K4ggsw = 1
               dampc = Gsube
               CALL sma1b(aki(1),Igp(i),-1,If4gg,dampc)
            ENDIF
         ENDDO
         RETURN
      ENDIF
   ENDIF
   CALL mesage(30,26,idel)
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
   Nogo = 1
!
END SUBROUTINE ktrirg
