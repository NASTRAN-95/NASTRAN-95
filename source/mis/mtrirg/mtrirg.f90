!*==mtrirg.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mtrirg
USE C_CONDAD
USE C_MATIN
USE C_MATOUT
USE C_SMA2CL
USE C_SMA2DP
USE C_SMA2ET
USE C_SMA2IO
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(36) :: am
   REAL(REAL64) :: d2pi , r1 , r2 , r3 , z1 , z2 , z3
   INTEGER :: i , i1 , iai , iapp , ic1 , idel , ieror1 , ieror2 , ip , ipp , iq , ir1 , irc , ising , j , j1 , j2 , k , kode ,     &
            & matid
   INTEGER , DIMENSION(19) :: iecpt
   EXTERNAL dki , gmmatd , inverd , mat , mesage , sma2b , transd
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1))
   !>>>>EQUIVALENCE (R(1),R1) , (R(2),R2) , (R(3),R3) , (Z(1),Z1) , (Z(2),Z2) , (Z(3),Z3)
   !>>>>EQUIVALENCE (Am(1),Ak(1))
   !>>>>EQUIVALENCE (Constd(2),D2pi)
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
!
!
! COMPUTE THE ELEMENT COORDINATES
!
   IF ( ieror1==0 .AND. ieror2==0 ) THEN
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
                  IF ( kode<0 ) EXIT SPAG_Loop_1_1
                  DO i = 1 , 8
                     IF ( Delint(i)<0.0D0 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  IF ( Delint(3)>Delint(6) ) THEN
                     IF ( Delint(8)<Delint(3) ) THEN
                        IF ( Delint(8)<=Delint(6) ) EXIT SPAG_Loop_1_1
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
         Rhod = Rho
!
! GENERATE THE CONSISTENT MASS MATRIX IN FIELD COORDINATES
!
         DO i = 1 , 36
            am(i) = 0.0D0
         ENDDO
         Twopi = d2pi*Rhod
         am(1) = Twopi*Delint(1)
         am(2) = Twopi*Delint(4)
         am(3) = Twopi*Delint(2)
         am(7) = am(2)
         am(8) = Twopi*Delint(7)
         am(9) = Twopi*Delint(5)
         am(13) = am(3)
         am(14) = am(9)
         am(15) = Twopi*Delint(3)
         am(22) = am(1)
         am(23) = am(2)
         am(24) = am(3)
         am(28) = am(23)
         am(29) = am(8)
         am(30) = am(9)
         am(34) = am(24)
         am(35) = am(30)
         am(36) = am(15)
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
!
END SUBROUTINE mtrirg
