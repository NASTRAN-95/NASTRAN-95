
SUBROUTINE dtrmem(Iopt)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alph12 , Alpha1 , Alpha2 , Consts(5) , Costh , Degra , Ecpt(21) , Eldef , Eltemp , Ftemp , G11 , G12 , G13 , G22 , G23 ,    &
      & G2x211 , G2x212 , G2x222 , G33 , Gsube , Rho , Sdisp(9) , Sigcom , Sigshe , Sigten , Sinth , Stress , Theta , Tsub0
   DOUBLE PRECISION Areat , C(54) , Delta , Disp(9) , Dumdp(12) , E(9) , G(9) , Gamma1 , Gamma2 , Gamma3 , Kd(36) , Kij(36) ,       &
                  & Lamda , Mu , Sigx , Sigxy , Sigy , Sum(3) , T(9) , Temp , Temp1(18) , Temp2(18) , Xsubb , Xsubc , Ysubc
   INTEGER Icstm , Icstm1 , Idum , Inflag , Ldtemp , Matid , Ncstm , Necpt(6) , Npivot , Npvt
   COMMON /condas/ Consts
   COMMON /ds1aaa/ Npvt , Icstm , Ncstm
   COMMON /ds1adp/ E , C , Kd , Temp1 , Temp2 , G , T , Disp , Mu , Lamda , Delta , Temp , Gamma1 , Gamma2 , Gamma3 , Areat ,       &
                 & Xsubb , Xsubc , Ysubc , Dumdp , Theta , Icstm1 , Npivot , Idum , Sigx , Sigy , Sigxy
   COMMON /ds1aet/ Ecpt , Eldef , Ldtemp , Sdisp
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alpha1 , Alpha2 , Alph12 , Tsub0 , Gsube , Sigten , Sigcom , Sigshe ,  &
                 & G2x211 , G2x212 , G2x222
!
! Dummy argument declarations
!
   INTEGER Iopt
!
! Local variable declarations
!
   INTEGER i , j
!
! End of declarations
!
!
!     DIFFERENTIAL STIFFNESS CALCULATIONS FOR THE TRIANGULAR MEMBRANE
!     ELEMENT.  THREE 6X6 MATRICES FOR THE PIVOT POINT ARE INSERTED.
!     IF THIS ROUTINE IS CALLED FROM DTRIA OR DQUAD ONLY THE IN PLANE
!     EFFECTS ARE GENERATED AND THE STRESS VALUES ARE RETURNED.
!
!     THE VALUE OF IOPT TELLS US WHICH ROUTINE IS CALLING DTRMEM.
!      THE OPTIONS ARE
!                IOPT        ROUTINE
!               ******       *******
!                 0            DSIA
!                 1            DQDMEM
!                 2            DTRIA
!                 3            DQUAD
!
!
!     THIS ROUTINE COMPUTES AN E-MATRIX UNIQUE TO THIS ROUTINE.
!
!                       IX  IY  IZ
!                  E =  JX  JY  JZ
!                       KX  KY  KZ
!
!
!
!
!     INTERFACE DATA BLOCKS
!
!
   EQUIVALENCE (Consts(4),Degra)
   EQUIVALENCE (Ldtemp,Ftemp) , (Necpt(1),Ecpt(1)) , (Sum(1),Sigx)
   EQUIVALENCE (Kij(1),Kd(1))
!
!
!     ******************************************************************
!     ECPT( 1) = ELEMENT ID
!     ECPT( 2) = GRID POINT A OR 1
!     ECPT( 3) = GRID POINT B OR 2
!     ECPT( 4) = GRID POINT C OR 3
!     ECPT( 5) = THETA = ANGLE OF MATERIAL CUT IF ANISOTROPIC
!     ECPT( 6) = MATERIAL ID
!     ECPT( 7) = THICKNESS
!     ECPT( 8) = NON-STRUCTURAL MASS
!     ECPT( 9) = COORD. SYSTEM ID 1
!     ECPT(10) = X1
!     ECPT(11) = Y1
!     ECPT(12) = Z1
!     ECPT(13) = COORD. SYSTEM ID 2
!     ECPT(14) = X2
!     ECPT(15) = Y2
!     ECPT(16) = Z2
!     ECPT(17) = COORD. SYSTEM ID 3
!     ECPT(18) = X3
!     ECPT(19) = Y3
!     ECPT(20) = Z3
!     ECPT(21) = ELEMENT TEMPERATURE
!     ECPT(22) = ELEMENT DEFORMATION DELTA
!     ECPT(23) = AVG. LOADING TEMPERATURE =(-1) IF NO LOADING TEMP.
!     ECPT(24) = X-TRANS POINT 1
!     ECPT(25) = Y-TRANS POINT 1
!     ECPT(26) = Z-TRANS POINT 1
!     ECPT(27) = X-TRANS POINT 2
!     ECPT(28) = Y-TRANS POINT 2
!     ECPT(29) = Z-TRANS POINT 2
!     ECPT(30) = X-TRANS POINT 3
!     ECPT(31) = Y-TRANS POINT 3
!     ECPT(32) = Z-TRANS POINT 3
!     ******************************************************************
!//////
!     CALL BUG(4HTMET,0,ECPT,32)
!//////
!
   Sigx = 0.0D0
   Sigy = 0.0D0
   Sigxy = 0.0D0
   IF ( Ecpt(7)==0.0 .OR. Necpt(6)==0 ) RETURN
!     FILL ELEMENT TO GLOBAL E-TRANSFORMATION MATRIX
!
!     IVEC = E(1). . .E(3)
!     JVEC = E(4). . .E(6)
!     KVEC = E(7). . .E(9)
!
   DO i = 1 , 3
      E(i) = dble(Ecpt(i+13)) - dble(Ecpt(i+9))
   ENDDO
!
!     LENGTH THEN = XSUBB
!
   Xsubb = dsqrt(E(1)**2+E(2)**2+E(3)**2)
!
!     R  - R   (INTERMEDIATE STEP) AND NOMALIZE IVECTOR = E(1). . .E(3)
!      C    A
!
   DO i = 1 , 3
      E(i+3) = dble(Ecpt(i+17)) - dble(Ecpt(i+9))
      E(i) = E(i)/Xsubb
   ENDDO
!
!     XSUBC = I DOT (R  - R )
!                     C    A
!
   Xsubc = E(1)*E(4) + E(2)*E(5) + E(3)*E(6)
!
!     KVEC = IVEC CROSS (R  - R )
!                         C    A
!
   E(7) = E(2)*E(6) - E(3)*E(5)
   E(8) = E(3)*E(4) - E(1)*E(6)
   E(9) = E(1)*E(5) - E(2)*E(4)
!
!     LENGTH = YSUBC
!
   Ysubc = dsqrt(E(7)**2+E(8)**2+E(9)**2)
!
!     NORMALIZE KVECTOR
   E(7) = E(7)/Ysubc
   E(8) = E(8)/Ysubc
   E(9) = E(9)/Ysubc
!
!     JVECTOR = I CROSS K
!
   E(4) = E(3)*E(8) - E(2)*E(9)
   E(5) = E(1)*E(9) - E(3)*E(7)
   E(6) = E(2)*E(7) - E(1)*E(8)
!
!     NORMALIZE JVECTOR TO MAKE SURE
   Temp = dsqrt(E(4)**2+E(5)**2+E(6)**2)
   E(4) = E(4)/Temp
   E(5) = E(5)/Temp
   E(6) = E(6)/Temp
!
!     MU, LAMDA, AND DELTA
!
   Mu = 1.0D0/Xsubb
   Lamda = 1.0D0/Ysubc
   Delta = (Xsubc/Xsubb) - 1.0D0
   Areat = Xsubb*Ysubc*0.50D0*dble(Ecpt(7))
!
!     C MATRIX    C  =(3X2) STORED C( 1). . .C( 6)
!                  A
!                 C  =(3X2) STORED C( 7). . .C(12)
!                  B
!                 C  =(3X2) STORED C(13). . .C(18)
!                  C
!
   C(1) = -Mu
   C(2) = 0.0D0
   C(3) = 0.0D0
   C(4) = Lamda*Delta
   C(5) = C(4)
   C(6) = -Mu
   C(7) = Mu
   C(8) = 0.0D0
   C(9) = 0.0D0
   C(10) = -Lamda*Mu*Xsubc
   C(11) = C(10)
   C(12) = Mu
   C(13) = 0.0D0
   C(14) = 0.0D0
   C(15) = 0.0D0
   C(16) = Lamda
   C(17) = Lamda
   C(18) = 0.0D0
!
   IF ( Iopt<1 ) THEN
!     THE REASON FOR THIS IS THAT IF THE DQDMEM ROUTINE IS CALLING,
!     EACH INDIVIDUAL SUBTRIANGLE WILL ALREADY HAVE A SINTH AND COSTH.
!
      Theta = Ecpt(5)*Degra
      Sinth = sin(Theta)
      Costh = cos(Theta)
   ENDIF
   IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0E0
!
   Eltemp = Ecpt(21)
   Matid = Necpt(6)
   Inflag = 2
   CALL mat(Ecpt(1))
!
!     FILL G-MATRIX WITH OUTPUT FROM MAT ROUTINE.
!
   G(1) = G11
   G(2) = G12
   G(3) = G13
   G(4) = G12
   G(5) = G22
   G(6) = G23
   G(7) = G13
   G(8) = G23
   G(9) = G33
!
!     G, E, C MATRICES ARE COMPLETE
!
!     FOLLOWING COMPUTES SIG , SIG , SIG      (3X1) VECTOR
!                           X     Y     XY
!
!         I=3
!      = (SUM (G)(C )(E)(T )(DISP )) - (S )(LDTEMP - T )
!         I=1      I      I      I       T            0
!
!        WHERE  S  =(G)(ALPHAS)   (3X1)
!                T
!
   Sum(1) = 0.0E0
   Sum(2) = 0.0E0
   Sum(3) = 0.0E0
!
!     MAKE DISPLACEMENT VECTOR DOUBLE PRECISION
!
   DO i = 1 , 9
      Disp(i) = Sdisp(i)
   ENDDO
!
   DO i = 1 , 3
!     DO WE NEED TRANSFORMATIONS
!
      IF ( Necpt(4*i+5)/=0 ) THEN
         CALL transd(Necpt(4*i+5),T(1))
         CALL gmmatd(T(1),3,3,0,Disp(3*i-2),3,1,0,Temp1(1))
      ELSE
!
         DO j = 1 , 3
            Idum = 3*(i-1) + j
            Temp1(j) = Disp(Idum)
         ENDDO
      ENDIF
!
      CALL gmmatd(E(1),2,3,0,Temp1(1),3,1,0,Temp2(1))
      CALL gmmatd(C(6*i-5),3,2,0,Temp2(1),2,1,0,Temp1(1))
      CALL gmmatd(G(1),3,3,0,Temp1(1),3,1,0,Temp2(1))
!
      Sum(1) = Sum(1) + Temp2(1)
      Sum(2) = Sum(2) + Temp2(2)
      Sum(3) = Sum(3) + Temp2(3)
!
   ENDDO
!
   IF ( Ldtemp/=(-1) ) THEN
!     COMPUTE S MATRIX
!               T
!
      Temp2(1) = Alpha1
      Temp2(2) = Alpha2
      Temp2(3) = Alph12
!     ABOVE IS FOR SINGLE TO DOUBLE PRECISION.
!
      CALL gmmatd(G(1),3,3,0,Temp2(1),3,1,0,Temp1(1))
      Temp = Ftemp - Tsub0
      DO i = 1 , 3
         Sum(i) = Sum(i) - Temp1(i)*Temp
      ENDDO
   ENDIF
!
!//////
!     CALL BUG(4HSUMS,90,SUM,6)
!//////
!  90 AT 90 SIG = SUM(1),  SIG = SUM(2),  SIG   = SUM(3)
!              X              Y              XY
!
!     ABOVE SIMULATES SMA,SDR2-PHASE I+II
!     FROM ABOVE THE E MATRIX (3X3), AND THE SUM (3X1) MATRIX ALONG WITH
!     XSUBB, XSUBC, AND YSUBC ARE NOW USED...
   DO i = 1 , 36
      Kd(i) = 0.0D0
   ENDDO
!
   IF ( Iopt==3 ) Areat = Areat/2.0D0
!
   Mu = Sigx*Areat
   Lamda = Sigy*Areat
   Delta = Sigxy*Areat
!
   IF ( Iopt<2 ) THEN
      Kd(1) = Lamda
      Kd(2) = -Delta
      Kd(7) = Kd(2)
      Kd(8) = Mu
   ENDIF
   Kd(15) = Mu + Lamda
   Kd(16) = -Delta
   Kd(17) = Delta
   Kd(18) = Mu - Lamda
   Kd(21) = Kd(16)
   Kd(27) = Kd(17)
   Kd(33) = Kd(18)
!
!     GENERATE C MATRICES
!
   DO i = 1 , 54
      C(i) = 0.0D0
   ENDDO
!
!     FILL NON ZERO TERMS
!
   Gamma1 = 1.0D0/Xsubb
   Gamma2 = 1.0D0/Ysubc
   Gamma3 = Xsubc/(Xsubb*Ysubc)
   C(3) = Gamma3 - Gamma2
   C(6) = Gamma1
   C(7) = -C(3)/2.0D0
   C(8) = -Gamma1/2.0D0
   C(10) = -Gamma1
   C(14) = C(3)
   C(16) = -C(7)
   C(17) = C(8)
!
   C(21) = -Gamma3
   C(24) = -Gamma1
   C(25) = Gamma3/2.0D0
   C(26) = -C(8)
   C(28) = Gamma1
   C(32) = -Gamma3
   C(34) = -C(25)
   C(35) = C(26)
!
   C(39) = Gamma2
   C(43) = -Gamma2/2.0D0
   C(50) = Gamma2
   C(52) = -C(43)
!
!     REPLACE C MATRICES BY  (C)(E )(T) FOR EACH POINT
   DO i = 1 , 3
      IF ( Necpt(4*i+5)/=0 ) THEN
!
!     GLOBAL TO BASIC MATRIX T IS GENERATED AGAIN HERE
!
         CALL transd(Necpt(4*i+5),T(1))
         CALL gmmatd(E(1),3,3,0,T(1),3,3,0,Temp1(1))
      ELSE
         DO j = 1 , 9
            Temp1(j) = E(j)
         ENDDO
      ENDIF
!
      CALL gmmatd(C(18*i-17),6,3,0,Temp1(1),3,3,0,Temp2(1))
      DO j = 1 , 18
         Idum = 18*(i-1) + j
         C(Idum) = Temp2(j)
      ENDDO
!
   ENDDO
!
   DO i = 1 , 3
      IF ( Necpt(i+1)==Npvt ) THEN
         Npivot = i
         GOTO 100
      ENDIF
   ENDDO
   RETURN
 100  CALL gmmatd(C(18*Npivot-17),6,3,1,Kd(1),6,6,0,Temp1(1))
!
!     TEMP1 NOW CONTAINS                   T
!                           ( (C )(E)(T ) ) ( KD)
!                               J      J
!     WHERE J IS THE PIVOT POINT
!
!     GENERATE THE THREE BY THREE PARTITIONS IN GLOBAL COORDINATES HERE
!
   DO i = 1 , 3
      CALL gmmatd(Temp1,3,6,0,C(18*i-17),6,3,0,Temp2(1))
!//////
!     CALL BUG(4HTRMK,260,TEMP2,18)
!//////
      DO j = 1 , 36
         Kij(j) = 0.0D0
      ENDDO
      Kij(1) = Temp2(1)
      Kij(2) = Temp2(2)
      Kij(3) = Temp2(3)
      Kij(7) = Temp2(4)
      Kij(8) = Temp2(5)
      Kij(9) = Temp2(6)
      Kij(13) = Temp2(7)
      Kij(14) = Temp2(8)
      Kij(15) = Temp2(9)
!
      CALL ds1b(Kij(1),Necpt(i+1))
!
   ENDDO
END SUBROUTINE dtrmem
