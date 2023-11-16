
SUBROUTINE pstrm
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Costh , Deps , Depss , Dum(297) , Dum1 , Dummy(64) , Ecpt(21) , Ecptsa(100) , Elid , Eltemp , Eps0 , Eps1 , Eps2 , Epss ,   &
      & Estar , Esub0 , Extra(4) , F , G11 , G12 , G13 , G22 , G23 , G33 , Gamma , Gammas , Gp(9) , Nu , Ph1out(200) , Plaans ,     &
      & Plaarg , S(3) , Sigxs , Sigxys , Sigys , Sinth , Sx , Sy , Tau0 , Tau1 , Tau2 , Ui(9) , Z(24)
   INTEGER Idum2 , Idum3(3,3) , Inflag , Ipass , Ivec , Matid , Midgp , Necpt(21) , Necpts(21) , Nirof
   COMMON /matin / Matid , Inflag , Eltemp , Plaarg , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33
   COMMON /pla32c/ Gamma , Gammas , Ipass
   COMMON /pla32e/ Ecpt , Eps0 , Epss , Estar , Sigxs , Sigys , Sigxys , Ui , Dummy
   COMMON /pla32s/ S , Dum , Tau0 , Tau1 , Tau2 , F , Sx , Sy , Deps , Depss , Eps1 , Eps2 , Dum1 , Idum2 , Idum3 , Extra
   COMMON /pla3es/ Ecptsa , Ph1out
   COMMON /pla3uv/ Ivec , Z
   COMMON /plagp / Gp , Midgp , Elid
!
! Local variable declarations
!
   INTEGER i , matid1
!
! End of declarations
!
!
!     THIS SUBROUTINE IS THE DRIVER FOR THE  TRI-MEMBRANE CALCULATIONS
!     IN PLA3
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
!       ECPT( 5) = THETA = ANGLE OF MATERIAL           ANGLE     REAL
!       ECPT( 6) = MATERIAL ID                         MATID     INTEGER
!       ECPT( 7) = T                                   T         REAL
!       ECPT( 8) = NON-STRUCTURAL MASS                 FMU       REAL
!       ECPT( 9) = COORD. SYSTEM ID 1                  NECPT(9)  INTEGER
!       ECPT(10) = X1                                  X1        REAL
!       ECPT(11) = Y1                                  Y1        REAL
!       ECPT(12) = Z1                                  Z1        REAL
!       ECPT(13) = COORD. SYSTEM ID 2                  NECPT(13) INTEGER
!       ECPT(14) = X2                                  X2        REAL
!       ECPT(15) = Y2                                  Y2        REAL
!       ECPT(16) = Z2                                  Z2        REAL
!       ECPT(17) = COORD. SYSTEM ID 3                  NECPT(17) INTEGER
!       ECPT(18) = X3                                  X3        REAL
!       ECPT(19) = Y3                                  Y3        REAL
!       ECPT(20) = Z3                                  Z3        REAL
!       ECPT(21) = ELEMENT TEMPERATURE                 ELTEMP    REAL
!       ECPT(22) = STRAIN (MINUS ONE)                  EPS0      REAL
!       ECPT(23) = STRAIN (PRESENT)                    EPSS      REAL
!       ECPT(24) = MODULUS OF ELASTICITY               ESTAR     REAL
!       ECPT(25) = STRESS SUB X                        SIGXS     REAL
!       ECPT(26) = STRESS SUB Y                        SIGYS     REAL
!       ECPT(27) = STRESS SUB XY                       SIGXYS    REAL
!       ECPT(28) = DISPLACEMENT VECTOR   A1            UI(1)     REAL
!       ECPT(29) = DISPLACEMENT VECTOR   A2            UI(2)     REAL
!       ECPT(30) = DISPLACEMENT VECTOR   A3            UI(3)     REAL
!       ECPT(31) = DISPLACEMENT VECTOR   B1            UI(4)     REAL
!       ECPT(32) = DISPLACEMENT VECTOR   B2            UI(5)     REAL
!       ECPT(33) = DISPLACEMENT VECTOR   B3            UI(6)     REAL
!       ECPT(34) = DISPLACEMENT VECTOR   C1            UI(7)     REAL
!       ECPT(35) = DISPLACEMENT VECTOR   C2            UI(8)     REAL
!       ECPT(36) = DISPLACEMENT VECTOR   C3            UI(9)     REAL
!
!     ******************************************************************
!
!
!     SCRATCH BLOCK  325 CELLS
!
   EQUIVALENCE (Necpt(6),matid1) , (Ecpt(1),Necpt(1)) , (G11,Plaans) , (G13,Nu) , (G11,Esub0) , (Necpts(1),Ecptsa(1)) , (G12,Nirof)
!
!     SETUP GP MATRIX FOR PLAMAT
!
   Elid = Ecpt(1)
   Midgp = matid1
   DO i = 1 , 9
      Gp(i) = 0.0
   ENDDO
   Tau0 = sqrt(Sigxs**2-Sigxs*Sigys+Sigys**2+3.0*Sigxys**2)
   IF ( Estar/=0. ) THEN
      IF ( Ipass==1 ) THEN
         Matid = matid1
         Costh = 1.0
         Sinth = 0.0
         Inflag = 2
!
         CALL mat(Ecpt(1))
!
         Gp(1) = G11
         Gp(2) = G12
         Gp(3) = G13
         Gp(4) = G12
         Gp(5) = G22
         Gp(6) = G23
         Gp(7) = G13
         Gp(8) = G23
         Gp(9) = G33
      ELSEIF ( Tau0/=0.0 ) THEN
         Matid = matid1
         Inflag = 1
!
         CALL mat(Ecpt(1))
!
         F = 9.0*(Esub0-Estar)/(4.0*Tau0**2*Estar)
         Sx = (2.0*Sigxs-Sigys)/3.0
         Sy = (2.0*Sigys-Sigxs)/3.0
         Gp(1) = (1.0+Sx**2*F)/Esub0
         Gp(2) = (-Nu+Sx*Sy*F)/Esub0
         Gp(3) = (2.0*Sigxys*Sx*F)/Esub0
         Gp(4) = Gp(2)
         Gp(5) = (1.0+Sy**2*F)/Esub0
         Gp(6) = (2.0*Sigxys*Sy*F)/Esub0
         Gp(7) = Gp(3)
         Gp(8) = Gp(6)
         Gp(9) = (2.0*(1.0+Nu)+4.0*F*Sigxys**2)/Esub0
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUNTLY.
!
         Idum2 = -1
         CALL invers(3,Gp,3,0,0,Dum1,Idum2,Idum3)
!
!     CHECK SINGULARITY
!
         IF ( Idum2==2 ) CALL mesage(-30,38,Ecpt(1))
      ENDIF
   ENDIF
!
!     CALCULATE PHASE I STRESSES
!
   DO i = 1 , 32
      Ecptsa(i) = Ecpt(i)
   ENDDO
   Necpts(2) = 1
   Necpts(3) = 4
   Necpts(4) = 7
!
   CALL pstrm1(0)
!
!     CALCULATE PHASE II STRESSES
!
   Ivec = 1
   DO i = 1 , 24
      Z(i) = Ui(i)
   ENDDO
   DO i = 1 , 200
      Ecptsa(i) = Ph1out(i)
   ENDDO
   S(1) = Sigxs
   S(2) = Sigys
   S(3) = Sigxys
!
   CALL pstrq2(1)
!
!     UPDATE ECPT FOR STRESSES
!
   Sigxs = S(1)
   Sigys = S(2)
   Sigxys = S(3)
   Tau1 = sqrt(Sigxs**2-Sigxs*Sigys+Sigys**2+3.0*Sigxys**2)
   Matid = matid1
   Inflag = 8
   Plaarg = Tau1
!
   CALL mat(Ecpt(1))
!
!     TEST FOR TAU 1 OUTSIDE THE RANGE OF FUNCTION
!
   IF ( Nirof==1 ) THEN
!
      Estar = 0.0
      GOTO 99999
   ENDIF
!
!     RETURNS EPS SUB 1 GIVEN TAU1
!
   Eps1 = Plaans
   Deps = Eps1 - Epss
   Depss = Epss - Eps0
   Eps2 = Eps1 + Gamma*Deps
   Inflag = 6
   Plaarg = Eps2
!
   CALL mat(Ecpt(1))
!
!     RETURNS  TAU2 GIVEN EPS2
!
   Tau2 = Plaans
   Estar = 0.0
   IF ( Eps2-Eps1/=0.0 ) Estar = (Tau2-Tau1)/(Eps2-Eps1)
   Eps0 = Epss
   Epss = Eps1
   RETURN
99999 RETURN
END SUBROUTINE pstrm
