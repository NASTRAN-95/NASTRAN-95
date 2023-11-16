
SUBROUTINE fwmw(Nd,Ne,Sgs,Cgs,Irb,A0,Arb,Xble,Xbte,Yb,Zb,Xs,Ys,Zs,Nas,Nasb,Kr,Beta2,Cbar,Avr,Fwz,Fwy)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL A0 , Beta2 , Cbar , Cgs , Sgs , Xble , Xbte , Xs , Ys , Zs
   COMPLEX Fwy , Fwz
   INTEGER Irb , Kr , Nas , Nd , Ne
   REAL Arb(1) , Avr(1) , Yb(1) , Zb(1)
   INTEGER Nasb(1)
!
! Local variable declarations
!
   REAL b , c , da , daib , darib , deleps , dfyyi , dfyyr , dfyzi , dfyzr , dfzyi , dfzyr , dfzzi , dfzzr , dmmy , dy , dyb , dz , &
      & dzb , eta , f , fwyi , fwyr , fwzi , fwzr , rho , rho2 , rhodb , s , sg , sy , sz , ybi , zbar , zbi , zeta
   INTEGER i , ib , infl , ioutfl , iret1 , itype , k
!
! End of declarations
!
!
!     CALCULATES THE EFFECT OF A DOUBLET PLUS ANY CONTRIBUTIONS DUE TO
!     IMAGES, SYMMETRY AND GROUND EFFECT ON BODY
!
!
!     ND        SYMMETRY FLAG
!     NE        GROUND EFFECTS FLAG
!     SGS       SINE   OF SENDING POINT DIHEDRAL ANGLE
!     CGS       COSINE OF SENDING POINT DIHEDRAL ANGLE
!     IRB       NUMBER OF THE RECEIVING BODY
!     A0        RADIUS OF THE BODY
!     ARB       ARRAY OF RATIOS OF BODY AXIS
!     XBLE      LEADING  EDGE COORDINATE OF SLENDER BODY ELEMENT
!     XBTE      TRAILING EDGE COORDINATE OF SLENDER BODY ELEMENT
!     YB        ARRAY CONTAINING THE Y-COORDINATES OF THE BODIES
!     ZB        ARRAY CONTAINING THE Y-COORDINATES OF THE BODIES
!     XS        1/4-CHORD  X-COORDINATE  OF SLENDER BODY ELEMENT
!     YS        Y-COORDINATE OF SENDING POINT
!     ZS        Z COORDINATE OF THE SENDING POINT
!     NAS       NUMBER OF ASSOCIATED BODIES
!     NASB      ARRAY CONTAINING THE ASSOCIATED BODY NOS.
!     KR        REDUCED FREQUENCY
!     BETA2     = 1 - MACH**2
!     CBAR      REFERENCE CHARD LENGTH
!     AVR       ARRAY OF BODY RADII
!     FWZ       OUTPUT Z-FORCE
!     FWY       OUTPUT Y FORCE
!
   Fwz = cmplx(0.0,0.0)
   Fwy = cmplx(0.0,0.0)
!
   dmmy = 0.0
   infl = 1
!
!     ARG-R  ARGUMENTS
!
   dyb = Yb(Irb)
   dzb = Zb(Irb)
   da = A0
   deleps = 1.0
   c = Cgs
   s = -Sgs
   dy = Ys
   dz = Zs
   itype = 1
   k = 1
   ASSIGN 100 TO iret1
   GOTO 900
 100  sy = 1.0
   sz = 1.0
   sg = Sgs
   ASSIGN 200 TO iret1
   GOTO 1200
!
!     CHECK SYMMETRY FLAG. BRANCH IF EQUAL TO ZERO
!
 200  IF ( Nd==0 ) THEN
!
!     SKIP GROUND EFFECTS CALCULATIONS IF FLAG IS ZERO
!
      IF ( Ne/=0 ) GOTO 600
      GOTO 99999
   ELSE
!
!     PORTION FOR SYMMETRIC CALCULATIONS
!
      deleps = Nd
      c = Cgs
      s = Sgs
      dy = -Ys
      dz = Zs
      itype = 1
      k = 2
      ASSIGN 300 TO iret1
      GOTO 900
   ENDIF
 300  sy = -1.0
   sz = 1.0
   sg = -Sgs
   ASSIGN 400 TO iret1
   GOTO 1200
!
!     CHECK GROUND EFFECTS FLAG. SKIP IF ZERO
!
 400  IF ( Ne==0 ) GOTO 99999
!
!     PORTION FOR COMBINATION OF SYMMETRY AND GROUND EFFECTS
!
   itype = 1
   k = 3
   deleps = Nd*Ne
   c = Cgs
   s = -Sgs
   dy = -Ys
   dz = -Zs
   ASSIGN 500 TO iret1
   GOTO 900
 500  sy = -1.0
   sg = Sgs
   sz = -1.0
   ASSIGN 600 TO iret1
   GOTO 1200
!
!     PORTION FOR GROUND EFFECTS ONLY
!
 600  deleps = Ne
   dy = Ys
   dz = -Zs
   c = Cgs
   s = Sgs
   itype = 1
   k = 4
   ASSIGN 700 TO iret1
   GOTO 900
 700  sy = 1.0
   sz = -1.0
   sg = -Sgs
   ASSIGN 800 TO iret1
   GOTO 1200
 800  RETURN
!
!     CALCULATION OF EFFECTIVE FORCES
!
 900  rho2 = (dy-dyb)**2 + (dz-dzb)**2
   rho = sqrt(rho2)
   b = Avr(Irb)*Arb(Irb)
   rhodb = rho/b
   f = 1.0
   IF ( rho>b ) f = rhodb/(Arb(Irb)*(rhodb-1.0)+1.0)
   zbar = (dz-dzb)/(f*Arb(Irb)) + dzb
   CALL fzy2(Xs,Xble,Xbte,dy,zbar,dyb,dzb,da,Beta2,Cbar,Kr,dfzzr,dfzzi,dfzyr,dfzyi,dfyzr,dfyzi,dfyyr,dfyyi)
!
   fwzr = c*dfzzr + s*dfzyr
   fwzi = c*dfzzi + s*dfzyi
   Fwz = Fwz + deleps*cmplx(fwzr,fwzi)
   fwyr = c*dfyzr + s*dfyyr
   fwyi = c*dfyzi + s*dfyyi
   Fwy = Fwy + deleps*cmplx(fwyr,fwyi)
 1000 IF ( itype==2 ) THEN
      i = i + 1
      IF ( i<=Nas ) GOTO 1300
   ENDIF
 1100 GOTO iret1
!
!     CALCULATION LOOP FOR ASSOCIATED BODIES
!
 1200 IF ( Nas<=0 ) GOTO 1100
   i = 1
   itype = 2
 1300 ib = Nasb(i)
!
!     CHECK TO SEE IF THE ASSOCIATED BODY IS THE RECEIVING BODY.
!
   IF ( ib==Irb ) THEN
!
!     IF IT IS DETERMINE IF THE SENDING POINT IS OUTSIDE OR INSIDE THE
!     BODY.
!
      IF ( k==1 ) GOTO 1400
      IF ( k==2 ) THEN
      ELSEIF ( k==3 ) THEN
         IF ( dyb/=0.0 ) GOTO 1400
      ELSE
         IF ( dyb/=0.0 ) THEN
         ENDIF
         GOTO 1400
      ENDIF
      IF ( dzb/=0.0 ) THEN
      ENDIF
   ENDIF
 1400 eta = sy*Ys
   zeta = sz*Zs
   zbi = sz*Zb(ib)
   ybi = sy*Yb(ib)
   darib = Arb(ib)
   daib = Avr(ib)
   CALL subi(daib,zbi,ybi,darib,eta,zeta,Cgs,sg,dmmy,dmmy,dmmy,dy,dz,dmmy,dmmy,dmmy,dmmy,s,c,infl,ioutfl)
   IF ( ioutfl==0 ) GOTO 1000
   GOTO 900
99999 RETURN
END SUBROUTINE fwmw
