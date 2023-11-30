
SUBROUTINE tvor(Sl1,Cl1,Tl1,Sl2,Cl2,Tl2,Sgs,Cgs,Sgr,Cgr,X01,X02,Y0,Z0,E,Beta,Cbar,Fmach,Kr,Bre,Bim)
   IMPLICIT NONE
   REAL Beta , Bim , Bre , Cbar , Cgr , Cgs , Cl1 , Cl2 , E , Fmach , Kr , Sgr , Sgs , Sl1 , Sl2 , Tl1 , Tl2 , X01 , X02 , Y0 , Z0
   REAL bs , cl , coef , cv , dij , dkc(2) , dki(2) , dko(2) , dx01 , dx02 , dy0 , dz0 , ecgs , ee , eps , esgs , etl1 , etl2 , fb ,&
      & fc , kd1(2) , kd2(2) , pi48 , r2 , r4 , sl , te , tl , x0
   INTEGER ib , iflld , isnp , l
!
!     NORMALWASH AT A POINT (X,Y,Z) - OF A SURFACE DIHEDRAL -
!     DUE TO A TRAPEZOIDAL UNSTEADY VORTEX RING OF UNIT STRENGTH.
!
!     THIS SUBROUTINE CALLS - SNPDF, IDF1, IDF2, FLLD
!
!     SL1, CL1, TL1  SIN(LAMBDA-1), COS(LAMBDA-1), TAN(LAMBDA-1)
!     SL2, CL2, TL2  SIN(LAMBDA-2), .....
!     SGS, CGS       SIN(GAMMA-S),  ....
!     SGR, CGR       SIN(GAMMA-R),  ....
!     X01            X-XI1
!     X02            X-XI2
!     Y0             Y - ETA
!     Z0             Z - ZETA
!     E
!     BETA           SQRT(1-FMACH**2)
!     CV
!     BR
!     FMACH          MACH NO.
!     BRE            REAL PART OF B      (RETURNED)
!     BIM            IMAGINARY PART OF B (RETURNED)
!
!
!     VARIABLES DIMENSIONED (2), FIRST WORD IS THE REAL PART OF THE
!     VALUE AND THE SECOND IS THE IMAGINARY PART
!
!
   DATA pi48/150.79644720/
!
!     CALCULATE  BS
!
   l = 1
   cv = X01 - X02
   sl = Sl1
   cl = Cl1
   tl = Tl1
   x0 = X01
   ee = E**2
   te = 2.0*E
   ASSIGN 100 TO isnp
!
!     CALL SNPDF
!
   GOTO 900
 100  bs = dij
   sl = Sl2
   cl = Cl2
   tl = Tl2
   x0 = X02
   ASSIGN 200 TO isnp
!
!     CALL SNPDF
!
   GOTO 900
 200  bs = bs - dij
!
!     CALCULATE   DELTA-B
!     LIMITS FOR SMALL VALUES OF RADII
!
   eps = 0.25*ee
   ib = 0
   fb = 1.0
   fc = 4.0
!
!     FIRST CALC.
!     DELTA-KD- 1I, 1C, AND 1O
!
   etl1 = E*Tl1
   etl2 = E*Tl2
   esgs = E*Sgs
   ecgs = E*Cgs
!
   dx01 = X01 + etl1
   dx02 = X02 + etl2
   dy0 = Y0 + ecgs
   dz0 = Z0 + esgs
   ASSIGN 300 TO iflld
!
!     CALCULATE  R-I  SQUARED AND CALL FLLD IF LARGE ENOUGH
!
   r2 = dy0**2 + dz0**2
   IF ( r2>=eps ) GOTO 1000
   ib = 1
   fc = 6.0
   fb = 0.0
   GOTO 400
 300  dki(1) = kd1(1)/r2 + kd2(1)/r4
   dki(2) = kd1(2)/r2 + kd2(2)/r4
!
!     KD1C AND KD2C
!
 400  dx01 = X01
   dx02 = X02
   dy0 = Y0
   dz0 = Z0
   ASSIGN 500 TO iflld
!
!     CALCULATE  R-C  SQUARED AND CALL FLLD IF LARGE ENOUGH
!
   r2 = dy0**2 + dz0**2
   IF ( r2>=eps ) GOTO 1000
   fc = 0.0
   fb = 3.0
   GOTO 600
 500  dkc(1) = kd1(1)/r2 + kd2(1)/r4
   dkc(2) = kd1(2)/r2 + kd2(2)/r4
!
!     KD1O AND KD2O
!     SKIP IF  R-I IS TOO SMALL
!
 600  IF ( ib/=0 ) GOTO 800
   dx01 = X01 - etl1
   dx02 = X02 - etl2
   dy0 = Y0 - ecgs
   dz0 = Z0 - esgs
   ASSIGN 700 TO iflld
!
!     CALCULATE  R-O  SQUARED AND CALL FLLD IF LARGE ENOUGH
!
   r2 = dy0**2 + dz0**2
   IF ( r2>=eps ) GOTO 1000
   fb = 0.0
   fc = 6.0
   ib = 1
   GOTO 800
 700  dko(1) = kd1(1)/r2 + kd2(1)/r4
   dko(2) = kd1(2)/r2 + kd2(2)/r4
!
 800  coef = 1.0/pi48
   Bre = bs/(te*cv) - coef*(fb*(dki(1)+dko(1))+fc*dkc(1))
   Bim = -coef*(fb*(dki(2)+dko(2))+fc*dkc(2))
   RETURN
!
 900  CALL snpdf(sl,cl,tl,Sgs,Cgs,Sgr,Cgr,x0,Y0,Z0,E,dij,Beta,cv)
   GOTO isnp
!
 1000 CALL flld(dx01,dx02,dy0,dz0,Sgr,Cgr,Sgs,Cgs,Kr,Cbar,Fmach,E,l,kd1(1),kd1(2),kd2(1),kd2(2))
   r4 = r2*r2
   GOTO iflld
!
END SUBROUTINE tvor
