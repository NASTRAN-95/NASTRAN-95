!*==fwmw.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fwmw(Nd,Ne,Sgs,Cgs,Irb,A0,Arb,Xble,Xbte,Yb,Zb,Xs,Ys,Zs,Nas,Nasb,Kr,Beta2,Cbar,Avr,Fwz,Fwy)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nd
   INTEGER :: Ne
   REAL :: Sgs
   REAL :: Cgs
   INTEGER :: Irb
   REAL :: A0
   REAL , DIMENSION(1) :: Arb
   REAL :: Xble
   REAL :: Xbte
   REAL , DIMENSION(1) :: Yb
   REAL , DIMENSION(1) :: Zb
   REAL :: Xs
   REAL :: Ys
   REAL :: Zs
   INTEGER :: Nas
   INTEGER , DIMENSION(1) :: Nasb
   INTEGER :: Kr
   REAL :: Beta2
   REAL :: Cbar
   REAL , DIMENSION(1) :: Avr
   COMPLEX :: Fwz
   COMPLEX :: Fwy
!
! Local variable declarations rewritten by SPAG
!
   REAL :: b , c , da , daib , darib , deleps , dfyyi , dfyyr , dfyzi , dfyzr , dfzyi , dfzyr , dfzzi , dfzzr , dmmy , dy , dyb ,   &
         & dz , dzb , eta , f , fwyi , fwyr , fwzi , fwzr , rho , rho2 , rhodb , s , sg , sy , sz , ybi , zbar , zbi , zeta
   INTEGER :: i , ib , infl , ioutfl , iret1 , itype , k , spag_nextblock_1
   EXTERNAL fzy2 , subi
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         ASSIGN 20 TO iret1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      sy = 1.0
         sz = 1.0
         sg = Sgs
         ASSIGN 40 TO iret1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     CHECK SYMMETRY FLAG. BRANCH IF EQUAL TO ZERO
!
 40      IF ( Nd==0 ) THEN
!
!     SKIP GROUND EFFECTS CALCULATIONS IF FLAG IS ZERO
!
            IF ( Ne==0 ) RETURN
            GOTO 120
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
            ASSIGN 60 TO iret1
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 60      sy = -1.0
         sz = 1.0
         sg = -Sgs
         ASSIGN 80 TO iret1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     CHECK GROUND EFFECTS FLAG. SKIP IF ZERO
!
 80      IF ( Ne==0 ) RETURN
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
         ASSIGN 100 TO iret1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 100     sy = -1.0
         sg = Sgs
         sz = -1.0
         ASSIGN 120 TO iret1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     PORTION FOR GROUND EFFECTS ONLY
!
 120     deleps = Ne
         dy = Ys
         dz = -Zs
         c = Cgs
         s = Sgs
         itype = 1
         k = 4
         ASSIGN 140 TO iret1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 140     sy = 1.0
         sz = -1.0
         sg = -Sgs
         ASSIGN 160 TO iret1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 160     RETURN
      CASE (2)
!
!     CALCULATION OF EFFECTIVE FORCES
!
         rho2 = (dy-dyb)**2 + (dz-dzb)**2
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
         spag_nextblock_1 = 3
      CASE (3)
         IF ( itype==2 ) THEN
            i = i + 1
            IF ( i<=Nas ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         GOTO iret1
      CASE (5)
!
!     CALCULATION LOOP FOR ASSOCIATED BODIES
!
         IF ( Nas<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = 1
         itype = 2
         spag_nextblock_1 = 6
      CASE (6)
         ib = Nasb(i)
!
!     CHECK TO SEE IF THE ASSOCIATED BODY IS THE RECEIVING BODY.
!
         IF ( ib==Irb ) THEN
!
!     IF IT IS DETERMINE IF THE SENDING POINT IS OUTSIDE OR INSIDE THE
!     BODY.
!
            IF ( k/=1 ) THEN
               IF ( k==2 ) THEN
               ELSEIF ( k==3 ) THEN
                  IF ( dyb/=0.0 ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSE
                  IF ( dyb/=0.0 ) THEN
                  ENDIF
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( dzb/=0.0 ) THEN
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         eta = sy*Ys
         zeta = sz*Zs
         zbi = sz*Zb(ib)
         ybi = sy*Yb(ib)
         darib = Arb(ib)
         daib = Avr(ib)
         CALL subi(daib,zbi,ybi,darib,eta,zeta,Cgs,sg,dmmy,dmmy,dmmy,dy,dz,dmmy,dmmy,dmmy,dmmy,s,c,infl,ioutfl)
         IF ( ioutfl/=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE fwmw
