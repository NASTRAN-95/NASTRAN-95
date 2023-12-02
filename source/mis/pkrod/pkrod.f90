!*==pkrod.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE pkrod
   IMPLICIT NONE
   USE C_MATIN
   USE C_MATOUT
   USE C_PLA42C
   USE C_PLA42D
   USE C_PLA42E
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ibasea , ibaseb , idispa , idispb , ind , iretrn , itemp , k1 , k2 , ka , kb , nonpvt
   INTEGER , DIMENSION(200) :: iecpt
   REAL :: plaans
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE COMPUTES THE TWO 6 X 6 MATRICES  K(NPVT,NPVT) AND
!     K(NPVT,J) FOR A ROD HAVING END POINTS NUMBERED NPVT AND J.
!
!     ECPT FOR THE ROD
!     ================                                              CARD
!                                                      TYPE  TABLE  TYPE
!     ECPT( 1)ELEMENT ID.                                I    ECT   CROD
!     ECPT( 2)SCALAR INDEX NUMBER FOR GRID POINT A       I    ECT   CROD
!     ECPT( 3)SCALAR INDEX NUMBER FOR GRID POINT B       I    ECT   CROD
!     ECPT( 4)MATERIAL ID.                               I    EPT   PROD
!     ECPT( 5)AREA  (A)                                  R    EPT   PROD
!     ECPT( 6)POLAR MOMENT OF INERTIA (J)                R    EPT   PROD
!     ECPT( 7) TORSIONAL STRESS COEFF (C)                R    EPT   PROD
!     ECPT( 8) NON-STRUCTRAL MASS (MU)                   R    EPT   PROD
!     ECPT( 9) COOR. SYS. ID. NO. FOR GRID POINT A       I   BGPDT  GRID
!     ECPT(10) X-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
!     ECPT(11) Y-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
!     ECPT(12) Z-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
!     ECPT(13) COOR. SYS. ID. NO. FOR GRID POINT B       I   BGPDT
!     ECPT(14) X-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
!     ECPT(15) Y-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
!     ECPT(16) Z-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
!     ECPT(17) ELEMENT TEMPERATURE
!     ECPT(18) PREVIOUS STRAIN VALUE, ONCE REMOVED (EPSIN1)
!     ECPT(19) PREVIOUS STRAIN VALUE (EPSIN2)
!     ECPT(20) PREVIOUSLY COMPUTED VALUE OF MODULUS OF ELASTICITY, ESTAR
!     ECPT(21) DISPLACEMENT COORDINATES FOR GRID POINT A
!     ECPT(22)                   . . .
!     ECPT(23)                   . . .
!     ECPT(24) DISPLACEMENT COORDINATES FOR GRID POINT B
!     ECPT(25)                   . . .
!     ECPT(26)                   . . .
!
!
!     PLA42 PARAMETERS COMMUNICATION BLOCK
!
!     ECPT COMMON BLOCK
!
!     PLA42 LOCAL VARIABLE (SCRATCH) BLOCK
!
!     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1)) , (Plaans,Esub0)
!
!     BEGIN EXECUTION
!
   ind = 0
   IF ( iecpt(2)==Npvt ) THEN
      ka = 9
      kb = 13
      idispa = 20
      idispb = 23
   ELSE
      IF ( iecpt(3)/=Npvt ) CALL mesage(-30,34,iecpt(1))
      ind = 1
      itemp = iecpt(2)
      iecpt(2) = iecpt(3)
      iecpt(3) = itemp
      ka = 13
      kb = 9
      idispa = 23
      idispb = 20
   ENDIF
!
!     AT THIS POINT KA POINTS TO THE COOR. SYS. ID. OF THE PIVOT GRID
!     POINT. SIMILARLY FOR KB AND THE NON-PIVOT GRID POINT.
!     NOW COMPUTE THE LENGTH OF THE ROD.
!
!     WE STORE THE COORDINATES IN THE D ARRAY SO THAT ALL ARITHMETIC
!     WILL BE DOUBLE PRECISION
!
   D(1) = Ecpt(ka+1)
   D(2) = Ecpt(ka+2)
   D(3) = Ecpt(ka+3)
   D(4) = Ecpt(kb+1)
   D(5) = Ecpt(kb+2)
   D(6) = Ecpt(kb+3)
   X = D(1) - D(4)
   Y = D(2) - D(5)
   Z = D(3) - D(6)
   Xl = dsqrt(X**2+Y**2+Z**2)
   IF ( Xl/=0.0D0 ) THEN
!
!     CALCULATE A NORMALIZED DIRECTION VECTOR IN BASIC COORDINATES.
!
      Xn(1) = X/Xl
      Xn(2) = Y/Xl
      Xn(3) = Z/Xl
!
!     STORE DISPLACEMENT VECTORS IN DOUBLE PRECISION LOCATIONS
!
      Ua(1) = Ecpt(idispa+1)
      Ua(2) = Ecpt(idispa+2)
      Ua(3) = Ecpt(idispa+3)
      Ub(1) = Ecpt(idispb+1)
      Ub(2) = Ecpt(idispb+2)
      Ub(3) = Ecpt(idispb+3)
!
!
!     COMPUTE THE DIFFERENCE VECTOR DIFF =  T  * U   -  T  * U
!                                            A    A      B    B
!
      ibasea = 0
      IF ( iecpt(ka)/=0 ) THEN
         CALL transd(Ecpt(ka),Ta)
         ibasea = 3
         CALL gmmatd(Ta,3,3,0,Ua(1),3,1,0,Ua(4))
      ENDIF
      ibaseb = 0
      IF ( iecpt(kb)/=0 ) THEN
         CALL transd(Ecpt(kb),Tb)
         ibaseb = 3
         CALL gmmatd(Tb,3,3,0,Ub(1),3,1,0,Ub(4))
      ENDIF
      Diff(1) = Ua(ibasea+1) - Ub(ibaseb+1)
      Diff(2) = Ua(ibasea+2) - Ub(ibaseb+2)
      Diff(3) = Ua(ibasea+3) - Ub(ibaseb+3)
!
!     COMPUTE DOT PRODUCT XN . DIFF
!
      CALL gmmatd(Xn,3,1,1,Diff,3,1,0,Dpterm)
!
!     COMPUTE INCREMENT OF STRAIN
!
      Deps1 = Dpterm/Xl
      Epsin1 = Ecpt(18)
      Epsin2 = Ecpt(19)
      Deps2 = Epsin2 - Epsin1
!
!     COMPUTE CURRENT STRAIN AND ESTIMATED NEXT STRAIN
!
      Eps1 = Epsin2 + Deps1
      Gamma = Gnew
      Gammas = Gold
      Eps2 = Eps1 + Gamma*Deps1
!
!     CALL MAT ROUTINE TWICE TO GET SIGMA1 AND SIGMA2 AS A FUNCTION OF
!     EPS1 AND EPS2
!
      Matidc = iecpt(4)
      Matflg = 6
      Plaarg = Eps1
      CALL mat(iecpt(1))
      Sigma1 = plaans
      Plaarg = Eps2
      CALL mat(iecpt(1))
      Sigma2 = plaans
!
!     ON THE FIRST PASS, I.E. WHEN ECPT(19) = 0.0, SIGMA1 = E  * EPS1
!                                                            0
!
      IF ( Ecpt(19)==0.0 ) THEN
         Matflg = 1
         CALL mat(iecpt(1))
         D(2) = Esub0
         Sigma1 = D(2)*Eps1
      ENDIF
!
!     FOR STIFFNESS MATRIX GENERATION, COMPUTE THE NEW MATERIAL
!     PROPERTIES
!
      IF ( Eps1==Eps2 ) THEN
         E = Ecpt(20)
      ELSE
         E = (Sigma2-Sigma1)/(Eps2-Eps1)
      ENDIF
!
!     CALL MAT ROUTINE TO GET ELASTIC MODULI.  STORE IN D.P. LOCATIONS.
!
      Matflg = 1
      CALL mat(iecpt(1))
      D(2) = Esub0
      D(4) = Gsub0
!
!     SET UP STIFFNESS MATRIX CONSTANTS IN DSCL AND DSCR
!
      G = E*D(4)/D(2)
      D(1) = Ecpt(5)
      D(3) = Ecpt(6)
      Dscl = D(1)*E/Xl
      Dscr = D(3)*G/Xl
!
!     SET UP THE -N- MATRIX AND STORE AT D(1)
!
      D(1) = Xn(1)*Xn(1)
      D(2) = Xn(1)*Xn(2)
      D(3) = Xn(1)*Xn(3)
      D(4) = D(2)
      D(5) = Xn(2)*Xn(2)
      D(6) = Xn(2)*Xn(3)
      D(7) = D(3)
      D(8) = D(6)
      D(9) = Xn(3)*Xn(3)
!
!     ZERO OUT THE 6X6 WHICH WILL BE USED FOR STORAGE OF
!     KGG(NPVT,NONPVT), NONPVT = NPVT,J
!
      DO i = 1 , 36
         Ke(i) = 0.0D0
      ENDDO
      nonpvt = 2
      k2 = 1
!
!     IF PIVOT GRID POINT IS IN BASIC COORDINATES, GO TO 70
!
      IF ( iecpt(ka)==0 ) THEN
         ASSIGN 200 TO iretrn
      ELSE
         CALL gmmatd(Ta(1),3,3,1,D(1),3,3,0,D(10))
         CALL gmmatd(D(10),3,3,0,Ta(1),3,3,0,D(1))
!
!     AT THIS POINT D(1) CONTAINS THE MATRIX PRODUCT TAT*N*TA
!     AND D(10) CONTAINS THE MATRIX PRODUCT TAT*N.
!
         ASSIGN 300 TO iretrn
      ENDIF
   ELSE
      CALL mesage(30,26,iecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
      Nogo = 1
      RETURN
   ENDIF
!
!     FILL THE KE MATRIX
!
 100  Ke(1) = Dscl*D(k2)
   Ke(2) = Dscl*D(k2+1)
   Ke(3) = Dscl*D(k2+2)
   Ke(7) = Dscl*D(k2+3)
   Ke(8) = Dscl*D(k2+4)
   Ke(9) = Dscl*D(k2+5)
   Ke(13) = Dscl*D(k2+6)
   Ke(14) = Dscl*D(k2+7)
   Ke(15) = Dscl*D(k2+8)
   Ke(22) = Dscr*D(k2)
   Ke(23) = Dscr*D(k2+1)
   Ke(24) = Dscr*D(k2+2)
   Ke(28) = Dscr*D(k2+3)
   Ke(29) = Dscr*D(k2+4)
   Ke(30) = Dscr*D(k2+5)
   Ke(34) = Dscr*D(k2+6)
   Ke(35) = Dscr*D(k2+7)
   Ke(36) = Dscr*D(k2+8)
   CALL pla4b(Ke,iecpt(nonpvt))
!
!     RETURN FROM FILL CODE W/ IRETRN =  90 IMPLIES G.P. A WAS IN BASIC
!       .     .    .     .      .     = 100 IMPLIES G.P. A WAS NOT BASIC
!       .     .    .     .      .     = 140 IMPLIES THE K(NPVT,NONPVT)
!                                       HAS BEEN COMPUTED AND INSERTED
!                                       AND HENCE WE ARE FINISHED.
!
   GOTO iretrn
 200  k1 = 1
   k2 = 10
   GOTO 400
 300  k1 = 10
   k2 = 1
 400  nonpvt = 3
!
!     IF NON-PIVOT GRID POINT IS IN BASIC COORDINATES, GO TO 120
!
   IF ( iecpt(kb)==0 ) THEN
      k2 = k1
   ELSE
!
!     RECALL THAT D(K1) CONTAINS TAT*N.
!
!
!     AT THIS POINT D(K2) CONTAINS TAT*N*TB.
!
      CALL gmmatd(D(k1),3,3,0,Tb(1),3,3,0,D(k2))
   ENDIF
   ASSIGN 500 TO iretrn
!
!     SET CONSTANTS NEGATIVE TO PROPERLY COMPUTE K(NPVT,NONPVT)
!
   Dscr = -Dscr
   Dscl = -Dscl
   GOTO 100
!
!     A TRANSFER TO STATEMENT NO. 140 IMPLIES KGGNL CALCULATIONS HAVE
!     BEEN COMPLETED.  UPDATE ECPT ARRAY.
!
 500  IF ( ind/=0 ) THEN
      itemp = iecpt(2)
      iecpt(2) = iecpt(3)
      iecpt(3) = itemp
   ENDIF
   Ecpt(18) = Ecpt(19)
   Ecpt(19) = Eps1
   Ecpt(20) = E
END SUBROUTINE pkrod
