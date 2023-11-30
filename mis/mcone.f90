
SUBROUTINE mcone
   IMPLICIT NONE
   REAL Degra , Dum3(2) , Dum4(10) , Dum5(25) , Ecpt(100) , Eltemp , L , Mu , Pi , Ra , Radeg , Rb , Rho , S4pisq , T , Temp ,      &
      & Term , Twopi , Za , Zb
   INTEGER Ifmgg , Inflag , M1 , Matid , Necpt(100) , Npvt
   DOUBLE PRECISION Mass(36)
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /matin / Matid , Inflag , Eltemp
   COMMON /matout/ Rho
   COMMON /sma2cl/ Dum3 , Npvt
   COMMON /sma2dp/ Mass , Temp , L , Term , M1
   COMMON /sma2et/ Ecpt
   COMMON /sma2io/ Dum4 , Ifmgg , Dum5
   INTEGER i
!
!     MASS MATRIX GENERATION FOR AXIS-SYMETRIC CONICAL SHELL ELEMENT
!
!     ECPT( 1) = ELEMENT ID             INTEGER        ECT
!     ECPT( 2) = SIL PT A               INTEGER        ECT
!     ECPT( 3) = SIL PT B B             INTEGER        ECT
!     ECPT( 4) = MATID 1                INTEGER        EPT
!     ECPT( 5) = T   (MEMBRANE THICK)   REAL           EPT
!     ECPT( 6) = MATID 2                INTEGER        EPT
!     ECPT( 7) = I   (MOM.OF INERTIA)   REAL           EPT
!     ECPT( 8) = MATID 3                INTEGER        EPT
!     ECPT( 9) = TS  (SHEAR THICKNESS)  REAL           EPT
!     ECPT(10) = NON-STRUCTURAL-MASS    REAL           EPT
!     ECPT(11) = Z1                     REAL           EPT
!     ECPT(12) = Z2                     REAL           EPT
!     ECPT(13) = PHI  1                 REAL           EPT
!     ECPT(14) = PHI  2                 REAL           EPT
!     ECPT(15) = PHI  3                 REAL           EPT
!     ECPT(16) = PHI  4                 REAL           EPT
!     ECPT(17) = PHI  5                 REAL           EPT
!     ECPT(18) = PHI  6                 REAL           EPT
!     ECPT(19) = PHI  7                 REAL           EPT
!     ECPT(20) = PHI  8                 REAL           EPT
!     ECPT(21) = PHI  9                 REAL           EPT
!     ECPT(22) = PHI 10                 REAL           EPT
!     ECPT(23) = PHI 11                 REAL           EPT
!     ECPT(24) = PHI 12                 REAL           EPT
!     ECPT(25) = PHI 13                 REAL           EPT
!     ECPT(26) = PHI 14                 REAL           EPT
!     ECPT(27) = COORD. SYS. ID PT.1    INTEGER        BGPDT
!     ECPT(28) = RADIUS PT. 1           REAL           BGPDT
!     ECPT(29) = DISTANCE TO PT.1       REAL           BGPDT
!     ECPT(30) = NULL                   REAL           BGPDT
!     ECPT(31) = COORD. SYS. ID PT.2    INTEGER        BGPDT
!     ECPT(32) = RADIUS PT 2            REAL           BGPDT
!     ECPT(33) = DISTANCE TO PT. 2      REAL           BGPDT
!     ECPT(34) = NULL                   REAL           BGPDT
!     ECPT(35) = ELEMENT TEMPERATURE    REAL           GEOM3
!
   !>>>>EQUIVALENCE (Ra,Ecpt(28)) , (Rb,Ecpt(32)) , (Za,Ecpt(29)) , (Zb,Ecpt(33)) , (T,Ecpt(5)) , (Mu,Ecpt(10)) , (Necpt(1),Ecpt(1))
!
   L = sqrt((Rb-Ra)**2+(Zb-Za)**2)
!
!     NEXT LINE WAS REMOVED BY M.H./NAVY. ERROR FOR CONICAL SHELL MASS
!
!
   Temp = Rb/6.0 + Ra/3.0
!
   IF ( T/=0 ) THEN
      Inflag = 4
      Matid = Necpt(4)
      Eltemp = Ecpt(35)
      CALL mat(Necpt(1))
   ENDIF
   DO i = 1 , 36
      Mass(i) = 0.0D0
   ENDDO
   Term = Pi*L*Temp*(Rho*T+Mu)
   IF ( Necpt(1)-(Necpt(1)/1000)*1000==1 ) Term = Term*2.0
   Mass(1) = Term
   Mass(8) = Term
   Mass(15) = Term
   M1 = -1
   CALL sma2b(Mass(1),Npvt,M1,Ifmgg,0.0D0)
END SUBROUTINE mcone