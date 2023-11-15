
SUBROUTINE dzy(X,Y,Z,Sgr,Cgr,Xi1,Xi2,Eta,Zeta,Ar,Ao,Kr,Cbar,Beta,Fmach,Lsh,Idzdy,Dzdyr,Dzdyi)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL Ao , Ar , Beta , Cbar , Cgr , Dzdyi , Dzdyr , Eta , Fmach , Sgr , X , Xi1 , Xi2 , Y , Z , Zeta
   INTEGER Idzdy , Kr , Lsh
!
! Local variable declarations
!
   REAL cgs , cl1 , cl2 , e , kd1mi , kd1mr , kd1pi , kd1pr , kd2mi , kd2mr , kd2pi , kd2pr , pi16 , r1for , r1sqr , r2for , r2sqr ,&
      & sgs , sl1 , sl2 , tl1 , tl2 , x01 , x02 , y0 , y01 , y02 , z0 , z01 , z02
   INTEGER l
!
! End of declarations
!
!
!     CALCULATION OF THE DZ AND DY MATRICES USED IN SLENDER BODY FLOW
!
!     X        X- COORDINATE OF THE RECEIVING POINT
!     Y        Y - COORDINATE OF THE RECEIVING POINT
!     Z        Z - COORDINATE OF THE RECEIVING POINT
!     SGR      SINE OF THE RECEIVING POINT DIHEDRAL ANGLE
!     CGR      COSINE OF RECEIVING POINT DIHEDRAL ANGLE
!     XI1
!     XI2
!     ETA
!     ZETA
!     AR       ASPECT RATIO OF THE SENDING BODY
!     A0       RADIUS OF THE SENDING BODY
!     KR       REDUCED FREQUENCY
!     CBAR     REFERENCE CHORD LENGTH
!     BETA     SQRT(1.0-M**2)
!     FMACH    MACH NUMBER
!     IDZDY    FLAG INDICATING WHETHER DZ OF DY IS TO BE
!              CALCULATED.  =0  DZ, OTHERWISE DY
!     DZDYR    REAL PART OF DZ OR DY
!     DZDYI    IMAGINARY PART OF DZ OR DY
!
!
   DATA pi16/50.265482/
!
!
!     THE COMPLEX NUMBERS IN THIS ROUTINE ARE TREATED SEPERATLY AS
!     THE REAL PART,  NAME APPENDED BY AN  -R- ,  AND THE
!     IMAGINARY PART, NAME APPENDED BY AN  -I- .
!
   e = Ao*sqrt(abs(1.0-Ar**2))/2.0
   x01 = X - Xi1
   x02 = X - Xi2
!
!     CHECK ON INPUT FLAG,  = 0  DZ ,  = 1  DY
!
   IF ( Idzdy==1 ) THEN
!
!     **     **
!     *  D Y  *
!     **     **
!
      sgs = -1.0
      IF ( Lsh==1 ) sgs = 1.0
      cgs = 0.0
      IF ( Ar>1.0 ) GOTO 100
!
      z01 = Z - Zeta
      z02 = z01
      y01 = Y - (Eta+e)
      y02 = Y - (Eta-e)
   ELSE
!
!     **     **
!     *  D Z  *
!     **     **
!
      sgs = 0.0
      cgs = 1.0
      IF ( Ar<1.0 ) GOTO 100
!
      z01 = Z - (Zeta+e)
      z02 = Z - (Zeta-e)
      y01 = Y - Eta
      y02 = y01
   ENDIF
!
!     ****  DZ AR .GE. 1  ****
!     ****  DY AR .LE. 1  ****
!
   l = 0
   z0 = Z - Zeta
   y0 = Y - Eta
!
   r1sqr = y01**2 + z01**2
   r2sqr = y02**2 + z02**2
   r1for = r1sqr**2
   r2for = r2sqr**2
!
   CALL flld(x01,x02,y01,z01,Sgr,Cgr,sgs,cgs,Kr,Cbar,Fmach,e,l,kd1pr,kd1pi,kd2pr,kd2pi)
!
   IF ( Ar/=1.0 ) THEN
      CALL flld(x01,x02,y02,z02,Sgr,Cgr,sgs,cgs,Kr,Cbar,Fmach,e,l,kd1mr,kd1mi,kd2mr,kd2mi)
   ELSE
!
!     IDENTICAL RESULTS FROM FLLD, THEREFORE SKIP SECOND CALL
!
      kd1mr = kd1pr
      kd1mi = kd1pi
      kd2mr = kd2pr
      kd2mi = kd2pi
   ENDIF
   Dzdyr = 0.0
   Dzdyi = 0.0
   IF ( r1sqr>0.0001 .AND. r2sqr>0.0001 ) THEN
!
!     REAL
!
      Dzdyr = ((kd1pr/r1sqr+kd1mr/r2sqr)+(kd2pr/r1for+kd2mr/r2for))/pi16*(-1.0)
!
!     IMAGINARY
!
      Dzdyi = ((kd1pi/r1sqr+kd1mi/r2sqr)+(kd2pi/r1for+kd2mi/r2for))/pi16*(-1.0)
   ENDIF
!
   RETURN
!
!     ****   DZ-AR .LT. 1   ****
!     ****   DY-AR .GT. 1   ****
!
 100  sl1 = 0.0
   tl1 = 0.0
   sl2 = 0.0
   tl2 = 0.0
   cl1 = 1.0
   cl2 = 1.0
   e = 1.732051*e
   y0 = Y - Eta
   z0 = Z - Zeta
!
   CALL tvor(sl1,cl1,tl1,sl2,cl2,tl2,sgs,cgs,Sgr,Cgr,x01,x02,y0,z0,e,Beta,Cbar,Fmach,Kr,Dzdyr,Dzdyi)
END SUBROUTINE dzy
