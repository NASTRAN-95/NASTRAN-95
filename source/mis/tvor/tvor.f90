!*==tvor.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tvor(Sl1,Cl1,Tl1,Sl2,Cl2,Tl2,Sgs,Cgs,Sgr,Cgr,X01,X02,Y0,Z0,E,Beta,Cbar,Fmach,Kr,Bre,Bim)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Sl1
   REAL :: Cl1
   REAL :: Tl1
   REAL :: Sl2
   REAL :: Cl2
   REAL :: Tl2
   REAL :: Sgs
   REAL :: Cgs
   REAL :: Sgr
   REAL :: Cgr
   REAL :: X01
   REAL :: X02
   REAL :: Y0
   REAL :: Z0
   REAL :: E
   REAL :: Beta
   REAL :: Cbar
   REAL :: Fmach
   REAL :: Kr
   REAL :: Bre
   REAL :: Bim
!
! Local variable declarations rewritten by SPAG
!
   REAL :: bs , cl , coef , cv , dij , dx01 , dx02 , dy0 , dz0 , ecgs , ee , eps , esgs , etl1 , etl2 , fb , fc , r2 , r4 , sl ,    &
         & te , tl , x0
   REAL , DIMENSION(2) :: dkc , dki , dko , kd1 , kd2
   INTEGER :: ib , iflld , isnp , l , spag_nextblock_1
   REAL , SAVE :: pi48
   EXTERNAL flld , snpdf
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
!
!     CALL SNPDF
!
         ASSIGN 20 TO isnp
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 20      bs = dij
         sl = Sl2
         cl = Cl2
         tl = Tl2
         x0 = X02
!
!     CALL SNPDF
!
         ASSIGN 40 TO isnp
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 40      bs = bs - dij
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
         ASSIGN 60 TO iflld
!
!     CALCULATE  R-I  SQUARED AND CALL FLLD IF LARGE ENOUGH
!
         r2 = dy0**2 + dz0**2
         IF ( r2>=eps ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ib = 1
         fc = 6.0
         fb = 0.0
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      dki(1) = kd1(1)/r2 + kd2(1)/r4
         dki(2) = kd1(2)/r2 + kd2(2)/r4
         spag_nextblock_1 = 2
      CASE (2)
!
!     KD1C AND KD2C
!
         dx01 = X01
         dx02 = X02
         dy0 = Y0
         dz0 = Z0
         ASSIGN 80 TO iflld
!
!     CALCULATE  R-C  SQUARED AND CALL FLLD IF LARGE ENOUGH
!
         r2 = dy0**2 + dz0**2
         IF ( r2>=eps ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         fc = 0.0
         fb = 3.0
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 80      dkc(1) = kd1(1)/r2 + kd2(1)/r4
         dkc(2) = kd1(2)/r2 + kd2(2)/r4
         spag_nextblock_1 = 3
      CASE (3)
!
!     KD1O AND KD2O
!     SKIP IF  R-I IS TOO SMALL
!
         IF ( ib/=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         dx01 = X01 - etl1
         dx02 = X02 - etl2
         dy0 = Y0 - ecgs
         dz0 = Z0 - esgs
         ASSIGN 100 TO iflld
!
!     CALCULATE  R-O  SQUARED AND CALL FLLD IF LARGE ENOUGH
!
         r2 = dy0**2 + dz0**2
         IF ( r2>=eps ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         fb = 0.0
         fc = 6.0
         ib = 1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 100     dko(1) = kd1(1)/r2 + kd2(1)/r4
         dko(2) = kd1(2)/r2 + kd2(2)/r4
         spag_nextblock_1 = 4
      CASE (4)
!
         coef = 1.0/pi48
         Bre = bs/(te*cv) - coef*(fb*(dki(1)+dko(1))+fc*dkc(1))
         Bim = -coef*(fb*(dki(2)+dko(2))+fc*dkc(2))
         RETURN
      CASE (5)
!
         CALL snpdf(sl,cl,tl,Sgs,Cgs,Sgr,Cgr,x0,Y0,Z0,E,dij,Beta,cv)
         GOTO isnp
      CASE (6)
!
         CALL flld(dx01,dx02,dy0,dz0,Sgr,Cgr,Sgs,Cgs,Kr,Cbar,Fmach,E,l,kd1(1),kd1(2),kd2(1),kd2(2))
         r4 = r2*r2
         GOTO iflld
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE tvor
