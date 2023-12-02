!*==melbow.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE melbow
   IMPLICIT NONE
   USE C_HMTOUT
   USE C_MATIN
   USE C_MATOUT
   USE C_SMA2BK
   USE C_SMA2CL
   USE C_SMA2DP
   USE C_SMA2ET
   USE C_SMA2HT
   USE C_SMA2IO
   USE C_ZZZZZZ
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
   Dp(1) = R
   Dp(2) = Betar
   Dp(3) = dcr
   Fl = Dp(1)*Dp(2)*Dp(3)
   IF ( Fl==0.0D0 ) THEN
!
      CALL mesage(30,26,iecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
      Nogo = 1
      RETURN
   ELSEIF ( Heat ) THEN
!
!     HEAT FORMULATION
!
!     GET CP USING -HMAT- ROUTINE.
!
      Matidc = Imatid
      Matflg = 4
      CALL hmat(Ielid)
      M(1) = Fl*dble(ecpt(9))*dble(Cp)/2.0D0
!
!     OUTPUT THE MASS FOR HEAT PROBLEM.
!
      CALL sma2b(M(1),Npvt,Npvt,Ifbgg,Dumdp)
      GOTO 99999
   ENDIF
!
!     GET RHO FROM MPT BY CALLING MAT
!
   Matidc = Imatid
   Matflg = 4
   Eltemp = Tempel
   CALL mat(ecpt(1))
   DO i = 1 , 36
      M(i) = 0.0D0
   ENDDO
   fm = 0.5*Fl*(Rho*A+Nsm)
!
!     PUT MASS IN M-ARRAY
!
   M(1) = fm
   M(8) = M(1)
   M(15) = M(1)
!
!     INSERT THE 6 X 6
!
   CALL sma2b(M,Npvt,-1,Ifmgg,Dumdp)
   RETURN
99999 END SUBROUTINE melbow
