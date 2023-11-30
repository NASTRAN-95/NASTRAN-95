
SUBROUTINE psrod
   IMPLICIT NONE
   REAL Diff(3) , Dum2(2) , Ecpt(100) , Esub0 , Gamma , Gammas , Gsub0 , Plaans , Plaarg , Sigma , Sigmac , Sigmas , Sigmat ,       &
      & Smsig , Smtau , Ta(9) , Tau , Tb(9) , Temdum , Ua(9) , Ub(9) , Xl , Xn(3)
   INTEGER Iecpt(100) , Jselid , Matdum(5) , Matflg , Matidc , Mssig , Mstau
   COMMON /matin / Matidc , Matflg , Temdum , Plaarg , Dum2
   COMMON /matout/ Esub0 , Gsub0 , Matdum , Sigmat , Sigmac , Sigmas
   COMMON /pla32c/ Gamma , Gammas
   COMMON /pla32e/ Ecpt
   COMMON /pla32s/ Xl , Xn , Ua , Ub , Ta , Tb , Diff
   COMMON /sout  / Jselid , Sigma , Smsig , Tau , Smtau
   REAL deps1 , deps2 , e , eps1 , eps2 , epsin1 , epsin2 , esub0l , g , gsub0l , p , sigma1 , sigma2 , t , term
   INTEGER i , ibasea , ibaseb , iselid
!*****
! THIS ROUTINE COMPUTES STRESSES AND FORCES FOR THE ROD ELEMENT FOR THE
! PLA3 FUNCTIONAL MODULE.
!*****
!
!                        E C P T  F O R  T H E  R O D
!
!                                                                CARD
!                                                 TYPE   TABLE   TYPE
! ECPT( 1)ELEMENT ID.                               I     ECT    CROD
! ECPT( 2)SCALAR INDEX NUMBER FOR GRID POINT A      I     ECT    CROD
! ECPT( 3)SCALAR INDEX NUMBER FOR GRID POINT B      I     ECT    CROD
! ECPT( 4)MATERIAL ID.                              I     EPT    PROD
! ECPT( 5)AREA  (A)                                 R     EPT    PROD
! ECPT( 6)POLAR MOMENT OF INERTIA (J)               R     EPT    PROD
! ECPT( 7) TORSIONAL STRESS COEFF (C)                R    EPT    PROD
! ECPT( 8) NON-STRUCTRAL MASS (MU)                   R    EPT    PROD
! ECPT( 9) COOR. SYS. ID. NO. FOR GRID POINT A       I   BGPDT   GRID
! ECPT(10) X-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
! ECPT(11) Y-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
! ECPT(12) Z-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
! ECPT(13) COOR. SYS. ID. NO. FOR GRID POINT B       I   BGPDT
! ECPT(14) X-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
! ECPT(15) Y-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
! ECPT(16) Z-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
! ECPT(17) ELEMENT TEMPERATURE
! ECPT(18) PREVIOUS STRAIN VALUE, ONCE REMOVED (EPS STAR SUB 0)
! ECPT(19) PREVIOUS STRAIN VALUE  (EPS STAR)
! ECPT(20) PREVIOUSLY COMPUTED VALUE OF MODULUS OF ELASTICITY (ESTAR)
! ECPT(21) PREVIOUSLY COMPUTED TORSIONAL MOMENT (TSTAR)
! ECPT(22) INCREMENTAL DISPLACEMENT VECTOR FOR GRID POINT A
! ECPT(23)                       ...
! ECPT(24)                       ...
! ECPT(25)                       ...
! ECPT(26)                       ...
! ECPT(27)                       ...
! ECPT(28) INCREMENTAL DISPLACEMENT VECTOR FOR GRID POINT B
! ECPT(29)                       ...
! ECPT(30)                       ...
! ECPT(31)                       ...
! ECPT(32)                       ...
! ECPT(33)                       ...
!
!
!
!
! EST (ECPT) COMMON BLOCK
!
!
! SCRATCH BLOCK FOR VARIABLES LOCAL TO PLA3 ELEMENT ROUTINES.
!
!
! PLA32 COMMUNICATION BLOCK
!
!
! OUTPUT BLOCK FOR ELEMENT STRESSES
!
!
! INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
!
!
!
!
!
!
   EQUIVALENCE (Iecpt(1),Ecpt(1)) , (Esub0,Plaans) , (Smsig,Mssig) , (Smtau,Mstau)
!
! CALL MAT ROUTINE TO GET MATERIAL PROPERTIES AND STORE IN LOCAL NAMES.
!
   Matidc = Iecpt(4)
   Matflg = 1
   CALL mat(Iecpt(1))
   esub0l = Esub0
   gsub0l = Gsub0
!
! SET UP VECTOR ALONG THE ROD, COMPUTE LENGTH AND NORMALIZE
!
   Xn(1) = Ecpt(10) - Ecpt(14)
   Xn(2) = Ecpt(11) - Ecpt(15)
   Xn(3) = Ecpt(12) - Ecpt(16)
   Xl = Xn(1)**2 + Xn(2)**2 + Xn(3)**2
   Xl = sqrt(Xl)
   Xn(1) = Xn(1)/Xl
   Xn(2) = Xn(2)/Xl
   Xn(3) = Xn(3)/Xl
!
! STORE DISPLACEMENT VECTORS IN LOCAL VARIABLES
!
   DO i = 1 , 6
      Ua(i) = Ecpt(i+21)
      Ub(i) = Ecpt(i+27)
   ENDDO
!
! TRANSFORM DISPLACEMENT VECTOR TRANSLATIONAL COMPONENTS IF NECESSARY
!
   ibasea = 0
   IF ( Iecpt(9)/=0 ) THEN
      ibasea = 6
      CALL transs(Iecpt(9),Ta)
      CALL gmmats(Ta,3,3,0,Ua(1),3,1,0,Ua(7))
   ENDIF
   ibaseb = 0
   IF ( Iecpt(13)/=0 ) THEN
      ibaseb = 6
      CALL transs(Iecpt(13),Tb)
      CALL gmmats(Tb,3,3,0,Ub(1),3,1,0,Ub(7))
   ENDIF
!
! FORM DIFFERENCE VECTOR, DOT PRODUCT AND INCREMENT OF STRAIN
!
   Diff(1) = Ua(ibasea+1) - Ub(ibaseb+1)
   Diff(2) = Ua(ibasea+2) - Ub(ibaseb+2)
   Diff(3) = Ua(ibasea+3) - Ub(ibaseb+3)
   CALL gmmats(Xn,3,1,1,Diff,3,1,0,term)
   deps1 = term/Xl
   epsin2 = Ecpt(19)
   epsin1 = Ecpt(18)
   deps2 = epsin2 - epsin1
!
! COMPUTE EPS1 AND EPS2 AND FETCH VIA MAT STRESSES SIGMA1 AND SIGMA2
!
   eps1 = epsin2 + deps1
   eps2 = epsin2 + (deps1+Gammas**2*deps2)*(Gamma+1.0)/(Gammas+1.0) + Gammas*(deps1-Gammas*deps2)*(Gamma+1.0)**2/(Gammas+1.0)
   Matflg = 6
   Plaarg = eps1
   CALL mat(Iecpt(1))
   sigma1 = Plaans
   Plaarg = eps2
   CALL mat(Iecpt(1))
   sigma2 = Plaans
   IF ( eps1==eps2 ) THEN
      e = Ecpt(20)
   ELSE
      e = (sigma2-sigma1)/(eps2-eps1)
   ENDIF
   g = Ecpt(20)*gsub0l/esub0l
!
! COMPUTE STRESSES
!
   iselid = Iecpt(1)
   Sigma = sigma1
   p = Ecpt(5)*sigma1
!
! TRANSFORM DISPLACEMENT VECTOR ROTATIONAL DISPLACEMENTS IF NECESSARY.
!
   ibasea = 3
   IF ( Iecpt(9)/=0 ) THEN
      CALL gmmats(Ta,3,3,0,Ua(4),3,1,0,Ua(7))
      ibasea = 6
   ENDIF
   ibaseb = 3
   IF ( Iecpt(13)/=0 ) THEN
      ibaseb = 6
      CALL gmmats(Tb,3,3,0,Ub(4),3,1,0,Ub(7))
   ENDIF
   Diff(1) = Ua(ibasea+1) - Ub(ibaseb+1)
   Diff(2) = Ua(ibasea+2) - Ub(ibaseb+2)
   Diff(3) = Ua(ibasea+3) - Ub(ibaseb+3)
   CALL gmmats(Xn,3,1,1,Diff,3,1,0,term)
   t = Ecpt(6)*g*term/Xl + Ecpt(21)
   IF ( Ecpt(6)==0.0 ) THEN
      Tau = 0.0
   ELSE
      Tau = Ecpt(7)*t/Ecpt(6)
   ENDIF
!
! COMPUTE MARGIN OF SAFETY IN EXTENSION
!
   IF ( Sigma<=0.0 ) THEN
      IF ( Sigma/=0.0 ) THEN
         Sigmac = -abs(Sigmac)
         Smsig = Sigmac/Sigma - 1.0
      ELSE
         Mssig = 1
      ENDIF
   ELSEIF ( Sigmat<=0.0 ) THEN
      Mssig = 1
   ELSE
      Smsig = Sigmat/Sigma - 1.0
   ENDIF
!
!     COMPUTE MARGIN OF SAFETY IN TORSION
!
   IF ( Sigmas<=0.0 ) THEN
      Mstau = 1
   ELSEIF ( Tau==0.0 ) THEN
      Mstau = 1
   ELSE
      Smtau = Sigmas/abs(Tau) - 1.0
   ENDIF
   Jselid = Iecpt(1)
!
! UPDATE EST (ECPT) ENTRY
!
   Ecpt(18) = Ecpt(19)
   Ecpt(19) = eps1
   Ecpt(20) = e
   Ecpt(21) = t
END SUBROUTINE psrod
