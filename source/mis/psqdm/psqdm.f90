!*==psqdm.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE psqdm
   USE c_matin
   USE c_matout
   USE c_pla32c
   USE c_pla32e
   USE c_pla32s
   USE c_pla3es
   USE c_pla3uv
   USE c_plagp
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: esub0 , nu , plaans
   INTEGER :: i , matid1 , nirof
   INTEGER , DIMENSION(26) :: necpt , necpts
   EXTERNAL invers , mat , mesage , psqdm1 , pstrq2
!
! End of declarations rewritten by SPAG
!
!  THIS SUBROUTINE IS THE DRIVER FOR THE QUAD-MEMBRANE CALCULATIONS IN
!  PLA3
!
!
!     ECPT LIST
!                                                      IN
!                                                      THIS
!       ECPT       DESCRIPTION                         ROUTINE   TYPE
!     ******************************************************************
!       ECPT( 1) = ELEMENT ID                          NECPT(1)  INTEGER
!       ECPT( 2) = GRID POINT A                        NGRID(1)  INTEGER
!       ECPT( 3) = GRID POINT B                        NGRID(2)  INTEGER
!       ECPT( 4) = GRID POINT C                        NGRID(3)  INTEGER
!       ECPT( 5) = GRID POINT D                        NGRID(4)  INTEGER
!       ECPT( 6) = THETA = ANGLE OF MATERIAL           ANGLE     REAL
!       ECPT( 7) = MATERIAL ID                         MATID     INTEGER
!       ECPT( 8) = T                                   T         REAL
!       ECPT( 9) = NON-STRUCTURAL MASS                 FMU       REAL
!       ECPT(10) = COORD. SYSTEM ID 1                  NECPT(10) INTEGER
!       ECPT(11) = X1                                  X1        REAL
!       ECPT(12) = Y1                                  Y1        REAL
!       ECPT(13) = Z1                                  Z1        REAL
!       ECPT(14) = COORD. SYSTEM ID 2                  NECPT(14) INTEGER
!       ECPT(15) = X2                                  X2        REAL
!       ECPT(16) = Y2                                  Y2        REAL
!       ECPT(17) = Z2                                  Z2        REAL
!       ECPT(18) = COORD. SYSTEM ID 3                  NECPT(18) INTEGER
!       ECPT(19) = X3                                  X3        REAL
!       ECPT(20) = Y3                                  Y3        REAL
!       ECPT(21) = Z3                                  Z3        REAL
!       ECPT(22) = COORD. SYSTEM ID 4                  NECPT(22) INTEGER
!       ECPT(23) = X4                                  X4        REAL
!       ECPT(24) = Y4                                  Y4        REAL
!       ECPT(25) = Z4                                  Z4        REAL
!       ECPT(26) = ELEMENT TEMPERATURE                 ELTEMP    REAL
!       ECPT(27) = STRAIN (MINUS ONE)                  EPS0      REAL
!       ECPT(28) = STRAIN (PRESENT)                    EPSS      REAL
!       ECPT(29) = MODULUS OF ELASTICITY               ESTAR     REAL
!       ECPT(30) = STRESS SUB X                        SIGXS     REAL
!       ECPT(31) = STRESS SUB Y                        SIGYS     REAL
!       ECPT(32) = STRESS SUB XY                       SIGXYS    REAL
!       ECPT(33) = DISPLACEMENT VECTOR   A1            UI(1)     REAL
!       ECPT(34) = DISPLACEMENT VECTOR   A2            UI(2)     REAL
!       ECPT(35) = DISPLACEMENT VECTOR   A3            UI(3)     REAL
!       ECPT(36) = DISPLACEMENT VECTOR   B1            UI(4)     REAL
!       ECPT(37) = DISPLACEMENT VECTOR   B2            UI(5)     REAL
!       ECPT(38) = DISPLACEMENT VECTOR   B3            UI(6)     REAL
!       ECPT(39) = DISPLACEMENT VECTOR   C1            UI(7)     REAL
!       ECPT(40) = DISPLACEMENT VECTOR   C2            UI(8)     REAL
!       ECPT(41) = DISPLACEMENT VECTOR   C3            UI(9)     REAL
!       ECPT(42) = DISPLACEMENT VECTOR   D1            UI(10)    REAL
!       ECPT(43) = DISPLACEMENT VECTOR   D2            UI(11)    REAL
!       ECPT(44) = DISPLACEMENT VECTOR   D3            UI(12)    REAL
!
!     ******************************************************************
!
!
!
! SCRATCH BLOCK  325 CELLS
!
!
   !>>>>EQUIVALENCE (Necpt(7),matid1) , (Ecpt(1),Necpt(1)) , (G11,Plaans) , (G13,Nu) , (G11,Esub0) , (Necpts(1),Ecptsa(1)) , (G12,Nirof)
!
! SETUP GP MATRIX FOR PLAMAT
!
   elid = ecpt(1)
   midgp = matid1
   DO i = 1 , 9
      gp(i) = 0.0
   ENDDO
   tau0 = sqrt(sigxs**2-sigxs*sigys+sigys**2+3.0*sigxys**2)
   IF ( estar/=0.0 ) THEN
      IF ( ipass==1 ) THEN
         matid = matid1
         costh = 1.0
         sinth = 0.0E0
         inflag = 2
!
         CALL mat(ecpt(1))
!
         gp(1) = g11
         gp(2) = g12
         gp(3) = g13
         gp(4) = g12
         gp(5) = g22
         gp(6) = g23
         gp(7) = g13
         gp(8) = g23
         gp(9) = g33
      ELSEIF ( tau0/=0.0 ) THEN
         matid = matid1
         inflag = 1
!
         CALL mat(ecpt(1))
!
         f = 9.0*(esub0-estar)/(4.0*tau0**2*estar)
         sx = (2.0*sigxs-sigys)/3.0
         sy = (2.0*sigys-sigxs)/3.0
         gp(1) = (1.0+sx**2*f)/esub0
         gp(2) = (-nu+sx*sy*f)/esub0
         gp(3) = (2.0*sigxys*sx*f)/esub0
         gp(4) = gp(2)
         gp(5) = (1.0+sy**2*f)/esub0
         gp(6) = (2.0*sigxys*sy*f)/esub0
         gp(7) = gp(3)
         gp(8) = gp(6)
         gp(9) = (2.0*(1.0+nu)+4.0*f*sigxys**2)/esub0
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
         idum2 = -1
         CALL invers(3,gp,3,0,0,dum1,idum2,idum3)
!
! CHECK SINGULARITY
!
         IF ( idum2==2 ) CALL mesage(-30,38,ecpt(1))
      ENDIF
   ENDIF
!
! CALCULATE PHASE I STRESSES
!
   DO i = 1 , 32
      ecptsa(i) = ecpt(i)
   ENDDO
   necpts(2) = 1
   necpts(3) = 4
   necpts(4) = 7
   necpts(5) = 10
!
   CALL psqdm1
!
!
! CALCULATE PHASE II STRESSES
!
   ivec = 1
   DO i = 1 , 24
      z(i) = ui(i)
   ENDDO
   DO i = 1 , 200
      ecptsa(i) = ph1out(i)
   ENDDO
   s(1) = sigxs
   s(2) = sigys
   s(3) = sigxys
!
   CALL pstrq2(2)
!
!
!  UPDATE ECPT FOR STRESSES
!
   sigxs = s(1)
   sigys = s(2)
   sigxys = s(3)
   tau1 = sqrt(sigxs**2-sigxs*sigys+sigys**2+3.0*sigxys**2)
   matid = matid1
   inflag = 8
   plaarg = tau1
!
   CALL mat(ecpt(1))
!
!
! TEST FOR TAU 1 OUTSIDE THE RANGE OF FUNCTION
!
   IF ( nirof==1 ) THEN
!
      estar = 0.0
      RETURN
   ENDIF
!
! RETURNS EPS SUB 1 GIVEN TAU1
!
   eps1 = plaans
   deps = eps1 - epss
   depss = epss - eps0
   eps2 = eps1 + gamma*deps
   inflag = 6
   plaarg = eps2
!
   CALL mat(ecpt(1))
!
! RETURNS  TAU2 GIVEN EPS2
!
   tau2 = plaans
   estar = 0.0
   IF ( (eps2-eps1)/=0.0 ) estar = (tau2-tau1)/(eps2-eps1)
   eps0 = epss
   epss = eps1
END SUBROUTINE psqdm