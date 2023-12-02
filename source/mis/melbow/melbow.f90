!*==melbow.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE melbow
   IMPLICIT NONE
   USE c_hmtout
   USE c_matin
   USE c_matout
   USE c_sma2bk
   USE c_sma2cl
   USE c_sma2dp
   USE c_sma2et
   USE c_sma2ht
   USE c_sma2io
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   REAL , SAVE :: dcr
   REAL , DIMENSION(9) :: ecpt
   REAL*8 :: fm
   INTEGER :: i
   INTEGER , DIMENSION(1) :: iecpt
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE COMPUTES THE MASS MATRIX M(NPVT,NPVT) FOR AN ELBOW.
!
!     ECPT FOR THE ELBOW
!
!     ECPT( 1)  -  IELID        ELEMENT ID. NUMBER
!     ECPT( 2)  -  ISILNO(2)    * SCALAR INDEX NOS. OF THE GRID POINTS
!     ECPT( 3)  -    ...        *
!     ECPT( 9)  -  A            CROSS-SECTIONAL AREA
!     ECPT(13)  -  NSM          NON-STRUCTURAL MASS
!     ECPT(29)  -  R            RADIUS OF CURVATURE
!     ECPT(30)  -  BETAR        ANGLE FROM GA TO GB
!
!
!     SMA2 VARIABLE CORE BOOKKEEPING PARAMETERS
!
!
!     SMA2 PROGRAM CONTROL PARAMETERS
!
!
!     ECPT COMMON BLOCK
!
!
!     SMA2 LOCAL VARIABLES
!
!
!     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
   !>>>>EQUIVALENCE (Z(1),Iz(1),Dz) , (Ecpt(1),Iecpt(1),Ielid)
   DATA dcr/0.01745329/
!
!     COMPUTE LENGTH OF ELBOW, FL
!
   dp(1) = r
   dp(2) = betar
   dp(3) = dcr
   fl = dp(1)*dp(2)*dp(3)
   IF ( fl==0.0D0 ) THEN
!
      CALL mesage(30,26,iecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
      nogo = 1
      RETURN
   ELSEIF ( heat ) THEN
!
!     HEAT FORMULATION
!
!     GET CP USING -HMAT- ROUTINE.
!
      matidc = imatid
      matflg = 4
      CALL hmat(ielid)
      m(1) = fl*dble(ecpt(9))*dble(cp)/2.0D0
!
!     OUTPUT THE MASS FOR HEAT PROBLEM.
!
      CALL sma2b(m(1),npvt,npvt,ifbgg,dumdp)
      GOTO 99999
   ENDIF
!
!     GET RHO FROM MPT BY CALLING MAT
!
   matidc = imatid
   matflg = 4
   eltemp = tempel
   CALL mat(ecpt(1))
   DO i = 1 , 36
      m(i) = 0.0D0
   ENDDO
   fm = 0.5*fl*(rho*a+nsm)
!
!     PUT MASS IN M-ARRAY
!
   m(1) = fm
   m(8) = m(1)
   m(15) = m(1)
!
!     INSERT THE 6 X 6
!
   CALL sma2b(m,npvt,-1,ifmgg,dumdp)
99999 END SUBROUTINE melbow
