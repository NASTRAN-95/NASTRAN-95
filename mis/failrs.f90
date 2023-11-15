
SUBROUTINE failrs(Fthr,Ultstn,Stresl,Findex)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL Findex
   INTEGER Fthr
   REAL Stresl(3) , Ultstn(6)
!
! Local variable declarations
!
   REAL crit , eps1 , eps2 , f12 , fi1 , fi12 , fi2 , gama , s , sig1 , sig2 , tau12 , x , xc , xt , xx , y , yc , yt
   INTEGER i
!
! End of declarations
!
!
!     THIS ROUTINE COMPUTES THE FAILURE INDEX OF A LAYER IN A LAMINATED
!     COMPOSITE ELEMENT USING ONE OF THE FOLLOWING FIVE FAILURE THEORIES
!     CURRENTLY AVAILABLE
!        1.   HILL
!        2.   HOFFMAN
!        3.   TSAI-WU
!        4.   MAX STRESS
!        5.   MAX STRAIN
!
!
!     DEFINITIONS
!
!     XT = ULTIMATE UNIAXIAL TENSILE STRENGTH IN THE FIBER DIRECTION
!     XC = ULTIMATE UNIAXIAL COMPRESSIVE STRENGTH IN THE FIBER DIRECTION
!     YT = ULTIMATE UNIAXIAL TENSILE STRENGTH PERPENDICULAR TO THE FIBER
!          DIRECTION
!     YC = ULTIMATE UNIAXIAL COMPRESSIVE STRENGTH PERPENDICULAR TO THE
!          FIBER DIRECTION
!     S  = ULTIMATE PLANAR SHEAR STRENGTH UNDER PURE SHEAR LOADING
!
!     SIMILARILY FOR THE ULTIMATE STRAINS
!
!
!
!
!     CHECK FOR ZERO STRENGTH VALUES
!
   DO i = 1 , 5
      IF ( Ultstn(i)==0.0 ) GOTO 100
   ENDDO
!
!     ULTIMATE STRENGTH VALUES
!
   xt = Ultstn(1)
   xc = Ultstn(2)
   yt = Ultstn(3)
   yc = Ultstn(4)
   s = Ultstn(5)
   f12 = Ultstn(6)
!
!     LAYER STRESSES
!
   sig1 = Stresl(1)
   sig2 = Stresl(2)
   tau12 = Stresl(3)
!
!     LAYER STRAINS
!
   eps1 = Stresl(1)
   eps2 = Stresl(2)
   gama = Stresl(3)
!
   IF ( Fthr==2 ) THEN
!
!
!     HOFFMAN FAILURE THEORY
!     ----------------------
!
      Findex = (1.0/xt-1.0/xc)*sig1
      Findex = Findex + (1.0/yt-1.0/yc)*sig2
      Findex = Findex + (sig1*sig1)/(xt*xc)
      Findex = Findex + (sig2*sig2)/(yt*yc)
      Findex = Findex + (tau12*tau12)/(s*s)
      Findex = Findex + (sig1*sig2)/(xt*xc)
   ELSEIF ( Fthr==3 ) THEN
!
!
!     TSAI-WU FAILURE THEORY
!     ----------------------
!
!     CHECK STABILITY CRITERIA FOR THE INTERACTION TERM F12
!
      IF ( f12/=0.0 ) THEN
!
         crit = (1.0/(xt*xc))*(1.0/(yt*yc)) - f12*f12
!
!     IF STABILITY CRITERIA IS VIOLATED THEN SET THE F12 THE INTERACTION
!     TERM TO ZERO
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
      IF ( f12/=0.0 ) Findex = Findex + 2.0*f12*sig1*sig2
   ELSEIF ( Fthr==4 ) THEN
!
!
!     MAX STRESS FAILURE THEORY
!     -------------------------
!
      fi1 = sig1/xt
      IF ( sig1<0.0 ) fi1 = abs(sig1/xc)
!
      fi2 = sig2/yt
      IF ( sig2<0.0 ) fi2 = abs(sig2/yc)
!
      fi12 = abs(tau12)/s
!
      Findex = fi1
      IF ( fi2>Findex ) Findex = fi2
      IF ( fi12>Findex ) Findex = fi12
   ELSEIF ( Fthr==5 ) THEN
!
!
!     MAX STRAIN FAILURE THEORY
!     -------------------------
!
      fi1 = eps1/xt
      IF ( eps1<0.0 ) fi1 = abs(eps1/xc)
!
      fi2 = eps2/yt
      IF ( eps2<0.0 ) fi2 = abs(eps2/yc)
!
      fi12 = abs(gama)/s
!
      Findex = fi1
      IF ( fi2>Findex ) Findex = fi2
      IF ( fi12>Findex ) Findex = fi12
   ELSE
!
!     HILL FAILURE THEORY
!     -------------------
!
      x = xt
      IF ( sig1<0.0 ) x = xc
!
      y = yt
      IF ( sig2<0.0 ) y = yc
!
      xx = xt
      IF ( sig1*sig2<0.0 ) xx = xc
!
      Findex = (sig1*sig1)/(x*x)
      Findex = Findex + (sig2*sig2)/(y*y)
      Findex = Findex - (sig1*sig2)/(xx*xx)
      Findex = Findex + (tau12*tau12)/(s*s)
   ENDIF
!
!
   RETURN
!
!
!     NON-FATAL ERROR
!
 100  Findex = 0.0
END SUBROUTINE failrs
