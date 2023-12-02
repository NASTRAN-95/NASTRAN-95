!*==shcsgd.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE shcsgd(Cflag,Ccsid,Ctheta,Pflag,Pcsid,Ptheta,Necpt,Tubd,Csid,Thetad,Tumsd) !HIDESTARS (*,Cflag,Ccsid,Ctheta,Pflag,Pcsid,Ptheta,Necpt,Tubd,Csid,Thetad,Tumsd)
   USE c_condad
   USE c_condas
   USE C_CONDAD
   USE C_CONDAS
   IMPLICIT NONE
   DOUBLE PRECISION Degrdd , Pid , Raddgd , Twopid
   REAL Degrds , Pis , Raddgs , Twopis
   COMMON /condad/ Pid , Twopid , Raddgd , Degrdd
   COMMON /condas/ Pis , Twopis , Raddgs , Degrds
   INTEGER Ccsid , Cflag , Csid , Pcsid , Pflag
   REAL Ctheta , Ptheta , Thetas
   DOUBLE PRECISION Thetad
   INTEGER Necpt(4)
   DOUBLE PRECISION Tubd(9) , Tumsd(9)
   REAL Tubs(9) , Tumss(9)
   DOUBLE PRECISION eps1d , flipd , tbmsd(9) , xmd , ymd
   REAL eps1s , flips , tbmss(9) , xms , yms
   INTEGER i
!
!     WITH ENTRY SHCSGS (*,CFLAG,CCSID,CTHETA,PFLAG,PCSID,PTHETA,
!    1                   NECPT,TUBS,CSID,THETAS,TUMSS)
!
!
!     'COORDINATE SYSTEM GENERATOR' ROUTINE FOR SHELL ELEMENTS.
!
!     THIS ROUTINE USES THE VALUES IN THE EST TABLE TO CREATE
!     APPROPRIATE MATERIAL/STRESS COORDINATE SYSTEM TRANSFORMATIONS.
!
!     INPUT:
!            CFLAG    - INDICATOR FLAG FROM CONNECTION
!            CCSID    - CSID  FROM CONNECTION
!            CTHETA   - ANGLE FROM CONNECTION
!            PFLAG    - INDICATOR FLAG FROM PROPERTY
!            PCSID    - CSID  FROM PROPERTY
!            PTHETA   - ANGLE FROM PROPERTY
!            NECPT    - ARRAY OF LENGTH 4, WORDS 2-4 ARE THE LOCATION
!                       WHERE THE TRANSFORMATION NEEDS TO BE CALCULATED
!            TUBD/S   - USER TO BASIC TRANSFORMATION
!     OUTPUT:
!            TUMSD/S  - USER TO MATERIAL/STRESS TRANSFORMATION
!            CSID     - CSID  USED FOR CALCULATIONS
!            THETAD/S - THETA USED FOR CALCULATIONS
!
!     NOTES:
!     1- IF CSID HAS BEEN SPECIFIED, SUBROUTINE TRANSD IS CALLED TO
!        CALCULATE [TBMS] (MATERIAL/STRESS TO BASIC TRANSFORMATION).
!        [TBMS] IS THEN PREMULTIPLIED BY [TUB] TO OBTAIN [TUMS].
!        THEN USING THE PROJECTION OF X-AXIS, AN ANGLE IS CALCULATED
!        UPON WHICH STEP 2 IS TAKEN.
!     2- IF THETA HAS BEEN SPECIFIED, INPLANE TRANSFORMATION IS USED TO
!        CALCULATE [TUMS] (MATERIAL/STRESS TO USER TRANSFORMATION).
!     3- IF THE CONNECTION VALUE IS LEFT BLANK, THE PROPERTY VALUE IS
!        USED.
!     4- NON-STANDARD RETURN IS TAKEN WHEN THE X-AXIS OF THE SPECIFIED
!        COORDINATE SYSTEM DOES NOT HAVE A PROJECTION ON THE X-Y PLANE
!        OF THE ELEMENT COORD. SYSTEM
!
!
   !>>>>EQUIVALENCE (tbmss(1),tbmsd(1))
   DATA eps1d , eps1s/1.0D-7 , 1.0E-7/
!
!
!     DOUBLE PRECISION VERSION
!
   flipd = 1.0D0
   IF ( Cflag/=0 ) THEN
!
!     DETERMINE THETA FROM THE PROJECTION OF THE X-AXIS OF THE MATERIAL/
!     STRESS COORD. SYSTEM, DETERMINED BASED ON CCSID, ONTO THE XY-PLANE
!     OF THE ELEMENT COORD. SYSTEM.
!
      Csid = Ccsid
      IF ( Ccsid>0 ) THEN
!
!     [TUMS] = [TUB] [TBMS]
!
         Necpt(1) = Ccsid
         CALL transd(Necpt,tbmsd)
         CALL gmmatd(Tubd,3,3,0,tbmsd,3,3,0,Tumsd)
      ELSE
!
!     [TUMS] = [TUB]
!
         DO i = 1 , 9
            Tumsd(i) = Tubd(i)
         ENDDO
      ENDIF
!
      xmd = Tumsd(1)
      ymd = Tumsd(4)
      IF ( dabs(xmd)<=eps1d .AND. dabs(ymd)<=eps1d ) RETURN 1
      Thetad = datan2(ymd,xmd)
      IF ( Tumsd(9)<0.0D0 ) flipd = -1.0D0
!
   ELSEIF ( Ctheta/=0.0 ) THEN
!
!     DETERMINE THETA FROM CTHETA
!
      Thetad = dble(Ctheta)*Degrdd
!
!     DEFAULT IS CHOSEN, LOOK FOR VALUES OF PCSID AND/OR PTHETA ON THE
!     PSHELL CARD.
!
   ELSEIF ( Pflag==0 ) THEN
!
!     DETERMINE THETA FROM PTHETA
!
      Thetad = dble(Ptheta)*Degrdd
   ELSE
!
!     DETERMINE THETA FROM THE PROJECTION OF THE X-AXIS OF THE MATERIAL/
!     STRESS COORD. SYSTEM, DETERMINED BASED ON PCSID, ONTO THE XY-PLANE
!     OF THE ELEMENT COORD. SYSTEM.
!
      Csid = Pcsid
      IF ( Pcsid>0 ) THEN
!
!     [TUMS] = [TUB] [TBMS]
!
         Necpt(1) = Pcsid
         CALL transd(Necpt,tbmsd)
         CALL gmmatd(Tubd,3,3,0,tbmsd,3,3,0,Tumsd)
      ELSE
!
!     [TUMS] = [TUB]
!
         DO i = 1 , 9
            Tumsd(i) = Tubd(i)
         ENDDO
      ENDIF
!
      xmd = Tumsd(1)
      ymd = Tumsd(4)
      IF ( dabs(xmd)<=eps1d .AND. dabs(ymd)<=eps1d ) RETURN 1
      Thetad = datan2(ymd,xmd)
      IF ( Tumsd(9)<0.0D0 ) flipd = -1.0D0
   ENDIF
!
!     IF THE Z-AXIS OF THE TARGET MATERIAL/STRESS COORD. SYSTEM WAS NOT
!     POINTING IN THE SAME GENERAL DIRECTION AS THE Z-AXIS OF THE USER
!     COORD. SYSTEM, FLIP THE Y- AND Z-AXES OF THE FINAL COORDINATE
!     SYSTEM TO ACCOUNT FOR IT.
!
   Tumsd(1) = dcos(Thetad)
   Tumsd(2) = -flipd*dsin(Thetad)
   Tumsd(3) = 0.0D0
   Tumsd(4) = dsin(Thetad)
   Tumsd(5) = flipd*dcos(Thetad)
   Tumsd(6) = 0.0D0
   Tumsd(7) = 0.0D0
   Tumsd(8) = 0.0D0
   Tumsd(9) = flipd
!
   RETURN
!
!
   ENTRY shcsgs(Cflag,Ccsid,Ctheta,Pflag,Pcsid,Ptheta,Necpt,Tubs,Csid,Thetas,Tumss) !HIDESTARS (*,Cflag,Ccsid,Ctheta,Pflag,Pcsid,Ptheta,Necpt,Tubs,Csid,Thetas,Tumss)
!     ======================================================
!
!     SINGLE PRECISION VERSION
!
   flips = 1.0
   IF ( Cflag/=0 ) THEN
!
!     DETERMINE THETA FROM THE PROJECTION OF THE X-AXIS OF THE MATERIAL/
!     STRESS COORD. SYSTEM, DETERMINED BASED ON CCSID, ONTO THE XY-PLANE
!     OF THE ELEMENT COORD. SYSTEM.
!
      Csid = Ccsid
      IF ( Ccsid>0 ) THEN
!
!     [TUMS] = [TUB] [TBMS]
!
         Necpt(1) = Ccsid
         CALL transs(Necpt,tbmss)
         CALL gmmats(Tubs,3,3,0,tbmss,3,3,0,Tumss)
      ELSE
!
!     [TUMS] = [TUB]
!
         DO i = 1 , 9
            Tumss(i) = Tubs(i)
         ENDDO
      ENDIF
!
      xms = Tumss(1)
      yms = Tumss(4)
      IF ( abs(xms)<=eps1s .AND. abs(yms)<=eps1s ) RETURN 1
      Thetas = atan2(yms,xms)
      IF ( Tumss(9)<0.0 ) flips = -1.0
!
   ELSEIF ( Ctheta/=0.0 ) THEN
!
!     DETERMINE THETA FROM CTHETA
!
      Thetas = Ctheta*Degrds
!
!     DEFAULT IS CHOSEN, LOOK FOR VALUES OF PCSID AND/OR PTHETA ON THE
!     PSHELL CARD.
!
   ELSEIF ( Pflag==0 ) THEN
!
!     DETERMINE THETA FROM PTHETA
!
      Thetas = Ptheta*Degrds
   ELSE
!
!     DETERMINE THETA FROM THE PROJECTION OF THE X-AXIS OF THE MATERIAL/
!     STRESS COORD. SYSTEM, DETERMINED BASED ON PCSID, ONTO THE XY-PLANE
!     OF THE ELEMENT COORD. SYSTEM.
!
      Csid = Pcsid
      IF ( Pcsid>0 ) THEN
!
!     [TUMS] = [TUB] [TBMS]
!
         Necpt(1) = Pcsid
         CALL transs(Necpt,tbmss)
         CALL gmmats(Tubs,3,3,0,tbmss,3,3,0,Tumss)
      ELSE
!
!     [TUMS] = [TUB]
!
         DO i = 1 , 9
            Tumss(i) = Tubs(i)
         ENDDO
      ENDIF
!
      xms = Tumss(1)
      yms = Tumss(4)
      IF ( abs(xms)<=eps1s .AND. abs(yms)<=eps1s ) RETURN 1
      Thetas = atan2(yms,xms)
      IF ( Tumss(9)<0.0 ) flips = -1.0
   ENDIF
!
!     IF THE Z-AXIS OF THE TARGET MATERIAL/STRESS COORD. SYSTEM WAS NOT
!     POINTING IN THE SAME GENERAL DIRECTION AS THE Z-AXIS OF THE USER
!     COORD. SYSTEM, FLIP THE Y- AND Z-AXES OF THE FINAL COORDINATE
!     SYSTEM TO ACCOUNT FOR IT.
!
   Tumss(1) = cos(Thetas)
   Tumss(2) = -flips*sin(Thetas)
   Tumss(3) = 0.0
   Tumss(4) = sin(Thetas)
   Tumss(5) = flips*cos(Thetas)
   Tumss(6) = 0.0
   Tumss(7) = 0.0
   Tumss(8) = 0.0
   Tumss(9) = flips
!
END SUBROUTINE shcsgd
