!*==failur.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE failur(Fthr,Ultstn,Stresl,Findex)
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Fthr
   REAL , DIMENSION(6) :: Ultstn
   REAL , DIMENSION(3) :: Stresl
   REAL :: Findex
!
! Local variable declarations rewritten by SPAG
!
   REAL :: crit , eps1 , eps2 , f12 , fi1 , fi12 , fi2 , gama , s , sig1 , sig2 , tau12 , x , xc , xt , xx , y , yc , yt
   INTEGER :: i
!
! End of declarations rewritten by SPAG
!
!
!      THIS ROUTINE COMPUTES THE FAILURE INDEX OF A LAYER
!      IN A LAMINATED COMPOSITE ELEMENT USING ONE OF THE
!      FOLLOWING FIVE FAILURE THEORIES CURRENTLY AVAILABLE
!
!        1.   HILL
!        2.   HOFFMAN
!        3.   TSAI-WU
!        4.   MAX STRESS
!        5.   MAX STRAIN
!
!        DEFINITIONS
!        -----------
!        XT = ULTIMATE UNIAXIAL TENSILE STRENGTH IN THE FIBER
!             DIRECTION
!        XC = ULTIMATE UNIAXIAL COMPRESSIVE STRENGTH IN THE
!             FIBER DIRECTION
!        YT = ULTIMATE UNIAXIAL TENSILE STRENGTH PERPENDICULAR TO
!             THE FIBER DIRECTION
!        YC = ULTIMATE UNIAXIAL COMPRESSIVE STRENGTH PERPENDICULAR
!             TO THE FIBER DIRECTION
!        S  = ULTIMATE PLANAR SHEAR STRENGTH UNDER PURE SHEAR
!             LOADING
!        SIMILARILY FOR THE ULTIMATE STRAINS
!
!
!**** CHECK FOR ZERO STRENGTH VALUES
!
   DO i = 1 , 5
      IF ( Ultstn(i)==0.0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
!
!**** ULTIMATE STRENGTH VALUES
!
   xt = Ultstn(1)
   xc = Ultstn(2)
   yt = Ultstn(3)
   yc = Ultstn(4)
   s = Ultstn(5)
   f12 = Ultstn(6)
!
!**** LAYER STRESSES
!
   sig1 = Stresl(1)
   sig2 = Stresl(2)
   tau12 = Stresl(3)
!
!**** LAYER STRAINS
!
   eps1 = Stresl(1)
   eps2 = Stresl(2)
   gama = Stresl(3)
!
!
   IF ( Fthr==2 ) THEN
!
!
!     H O F F M A N  F A I L U R E  T H E O R Y
!     =========================================
!
      Findex = (1.0/xt-1.0/xc)*sig1
      Findex = Findex + (1.0/yt-1.0/yc)*sig2
      Findex = Findex + (sig1*sig1)/(xt*xc)
      Findex = Findex + (sig2*sig2)/(yt*yc)
      Findex = Findex + (tau12*tau12)/(s*s)
      Findex = Findex - (sig1*sig2)/(xt*xc)
   ELSEIF ( Fthr==3 ) THEN
!
!
!     T S A I-W U  F A I L U R E  T H E O R Y
!     =======================================
!
!**** CHECK STABILITY CRITERIA FOR THE INTERACTION TERM F12
      IF ( f12/=0.0 ) THEN
!
         crit = (1.0/(xt*xc))*(1.0/(yt*yc)) - (f12*f12)
!
!**** IF STABILITY CRITERIA IS VIOLATED THEN SET THE
!     F12 THE INTERACTION TERM TO ZERO
!
         IF ( crit<=0.0 ) f12 = 0.0
      ENDIF
!
!
      Findex = (1.0/xt-1.0/xc)*sig1
      Findex = Findex + (1.0/yt-1.0/yc)*sig2
      Findex = Findex + (sig1*sig1)/(xt*xc)
      Findex = Findex + (sig2*sig2)/(yt*yc)
      Findex = Findex + (tau12*tau12)/(s*s)
      IF ( f12/=0.0 ) Findex = Findex + (2.0*f12*sig1*sig2)
   ELSEIF ( Fthr==4 ) THEN
!
!
!     M A X  S T R E S S  F A I L U R E  T H E O R Y
!     ==============================================
!
      fi1 = sig1/xt
      IF ( sig1<0.0 ) fi1 = sig1/xc
!
      fi2 = sig2/yt
      IF ( sig2<0.0 ) fi2 = sig2/yc
!
      fi12 = abs(tau12)/s
!
      Findex = fi1
      IF ( fi2>Findex ) Findex = fi2
      IF ( fi12>Findex ) Findex = fi12
   ELSEIF ( Fthr==5 ) THEN
!
!
!     M A X  S T R A I N  F A I L U R E  T H E O R Y
!     ==============================================
!
      fi1 = eps1/xt
      IF ( eps1<0.0 ) fi1 = eps1/xc
!
      fi2 = eps2/yt
      IF ( eps2<0.0 ) fi2 = eps2/yc
!
      fi12 = abs(gama)/s
!
      Findex = fi1
      IF ( fi2>Findex ) Findex = fi2
      IF ( fi12>Findex ) Findex = fi12
   ELSE
!
!     H I L L   F A I L U R E  T H E O R Y
!     ====================================
!
      x = xt
      IF ( sig1<0.0 ) x = xc
!
      y = yt
      IF ( sig2<0.0 ) y = yc
!
      xx = xt
      IF ( (sig1*sig2)<0.0 ) xx = xc
!
      Findex = (sig1*sig1)/(x*x)
      Findex = Findex + (sig2*sig2)/(y*y)
      Findex = Findex - (sig1*sig2)/(xx*xx)
      Findex = Findex + (tau12*tau12)/(s*s)
   ENDIF
!
!
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
!
!     NON-FATAL ERROR
!
!
      Findex = 0.0
   END SUBROUTINE spag_block_1
END SUBROUTINE failur
