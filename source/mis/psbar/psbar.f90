!*==psbar.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE psbar
   USE c_matin
   USE c_matout
   USE c_pla32c
   USE c_pla32e
   USE c_pla32s
   USE c_sout
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: abasic , aofset , basic , bbasic , bofset , offset
   REAL :: ael , beta , deps1 , deps2 , e , e1 , ei1 , ei2 , eps1 , eps2 , esub0l , fl , fll , fx , g , gak1 , gak2 , gjl , gsub0l ,&
         & k1a , k1b , k2a , k2b , l , l2b3 , l2b6 , lb , lcube , lr1 , lr2 , lsq , m1a , m1b , m2a , m2b , plaans , q , r1 , r2 ,  &
         & sigma1 , sigma2 , sk1 , sk2 , sk3 , sk4 , smcom , smten , t , v1 , v2 , w
   REAL , DIMENSION(9) :: d , tb
   REAL , DIMENSION(100) :: ecpt
   REAL , DIMENSION(6) :: fa , fb , smalv0
   INTEGER :: i , iab , icsida , icsidb , idela , idelb , ig , ii , ij , ikel , il , ill , index , ipass , isv , iwbeg , j , jcsid ,&
            & jcsida , jcsidb , ji , jll , jofset , jofsta , jofstb , jpina , jpinb , k , ka , kb , lim , ll , low
   INTEGER , DIMENSION(100) :: iecpt
   INTEGER , DIMENSION(10) :: ipin
   REAL , DIMENSION(72) :: sa
   REAL , DIMENSION(36) :: sb
   REAL , DIMENSION(18) :: ta
   REAL , DIMENSION(24) :: u
   REAL , DIMENSION(3) :: veci , vecj , veck
   EXTERNAL gmmats , mat , transs
!
! End of declarations rewritten by SPAG
!
!*****
! THIS ROUTINE COMPUTES THE TWO 6 X 6 MATRICES K(NPVT,NPVT) AND
! K(NPVT,J) FOR A BAR ELEMENT HAVING END POINTS NUMBERED NPVT AND J.
!*****
!
!                     E C P T  F O R  T H E  B A R
!
! ECPT( 1)  -  IELID          ELEMENT ID. NUMBER
! ECPT( 2)  -  ISILNO(2)      * SCALAR INDEX NOS. OF THE GRID POINTS
! ECPT( 3)  -    ...          *
! ECPT( 4)  -  SMALLV(3)      $ REFERENCE VECTOR
! ECPT( 5)  -    ...          $
! ECPT( 6)  -    ...          $
! ECPT( 7)  -  ICSSV          COOR. SYS. ID FOR SMALLV VECTOR
! ECPT( 8)  -  IPINFL(2)      * PIN FLAGS
! ECPT( 9)  -    ...          *
! ECPT(10)  -  ZA(3)          $ OFFSET VECTOR FOR POINT A
! ECPT(11)  -    ...          $
! ECPT(12)  -    ...          $
! ECPT(13)  -  ZB(3)          * OFFSET VECTOR FOR POINT B
! ECPT(14)  -    ...          *
! ECPT(15)  -    ...          *
! ECPT(16)  -  IMATID         MATERIAL ID.
! ECPT(17)  -  A              CROSS-SECTIONAL AREA
! ECPT(18)  -  I1             $ AREA MOMENTS OF INERTIA
! ECPT(19)  -  I2             $
! ECPT(20)  -  FJ             POLAR MOMENT OF INERTIA
! ECPT(21)  -  NSM            NON-STRUCTURAL MASS
! ECPT(22)  -  FE             FORCE ELEMENT DESCRIPTIONS (FORCE METHOD)
! ECPT(23)  -  C1             * STRESS RECOVERY COEFFICIENTS
! ECPT(24)  -  C2             *
! ECPT(25)  -  D1             *
! ECPT(26)  -  D2             *
! ECPT(27)  -  F1             *
! ECPT(28)  -  F2             *
! ECPT(29)  -  G1             *
! ECPT(30)  -  G2             *
! ECPT(31)  -  K1             $ AREA FACTORS FOR SHEAR
! ECPT(32)  -  K2             $
! ECPT(33)  -  I12            AREA MOMENT OF INERTIA
! ECPT(34)  -  MCSIDA         COOR. SYS. ID. FOR GRID POINT A
! ECPT(35)  -  GPA(3)         * BASIC COORDINATES FOR GRID POINT A
! ECPT(36)  -    ...          *
! ECPT(37)  -    ...          *
! ECPT(38)  -  MCSIDB         COOR. SYS. ID. FOR GRID POINT B
! ECPT(39)  -  GPB(3)         $ BASIC COORDINATES FOR GRID POINT B
! ECPT(40)  -    ...          $
! ECPT(41)  -    ...          $
! ECPT(42)  -  ELTEMP         AVG. ELEMENT TEMPERATURE
! ECPT(43)  -  EPSIN1         PREVIOUS STRAIN VALUE ONCE REMOVED
! ECPT(44)  -  EPSIN2         PREVIOUS STRAIN VALUE
! ECPT(45)  -  ESTAR          PREVIOUSLY COMPUTED MODULUS OF ELASTICITY
! ECPT(46)  -  V1STAR         * ELEMENT FORCES, INITIALLY ZERO
! ECPT(47)  -   V2STAR        *
! ECPT(48)  -   TSTAR         *
! ECPT(49)  -   M1ASTR        *
! ECPT(50)  -   M2ASTR        *
! ECPT(51)  -   UAIN(6)       $ INCREMENTAL DISPLACEMENT VECTOR AT PT. A
! ECPT(52)  -   ...           $
! ECPT(53)  -   ...           $
! ECPT(54)  -   ...           $
! ECPT(55)  -   ...           $
! ECPT(56)  -   ...           $
! ECPT(57)  -   UBIN(6)       * INCREMENTAL DISPLACEMENT VECTOR AT PT. B
! ECPT(58)  -   ...           *
! ECPT(59)  -   ...           *
! ECPT(60)  -   ...           *
! ECPT(61)  -   ...           *
! ECPT(62)  -   ...           *
!
!
!
   !>>>>EQUIVALENCE (Ielid,Ecpt(1),Iecpt(1)) , (ta(10),tb(1)) , (Ecpt(71),D(1)) , (Esub0,Plaans) , (sa(37),sb(1)) , (Msten,Smten) ,      &
!>>>>    & (Mscom,Smcom)
!
!-----------------------------------------------------------------------
!
! SET UP POINTERS TO COOR. SYS. IDS., OFFSET VECTORS, AND PIN FLAGS.
! ICSIDA AND ICSIDB ARE COOR. SYS. IDS.
!
   jcsida = 34
   jcsidb = 38
   jofsta = 10
   jofstb = 13
   jpina = 8
   jpinb = 9
   icsida = iecpt(34)
   icsidb = iecpt(38)
!
! NORMALIZE THE REFERENCE VECTOR WHICH LIES IN THE FIRST PRINCIPAL AXIS
! PLANE  (FMMS - 36 P. 4)
!
   fl = 0.0
   DO i = 1 , 3
      fl = fl + smallv(i)**2
   ENDDO
   fl = sqrt(fl)
   DO i = 1 , 3
      smallv(i) = smallv(i)/fl
   ENDDO
!
! DETERMINE IF POINT A AND B ARE IN BASIC COORDINATES OR NOT.
!
   abasic = .TRUE.
   bbasic = .TRUE.
   IF ( icsida/=0 ) abasic = .FALSE.
   IF ( icsidb/=0 ) bbasic = .FALSE.
!
! COMPUTE THE TRANSFORMATION MATRICES TA AND TB IF NECESSARY
!
   IF ( .NOT.abasic ) CALL transs(ecpt(jcsida),ta)
   IF ( .NOT.bbasic ) CALL transs(ecpt(jcsida),tb)
!
! DETERMINE IF WE HAVE NON-ZERO OFFSET VECTORS.
!
   aofset = .TRUE.
   j = jofsta - 1
   DO i = 1 , 3
      j = j + 1
      IF ( ecpt(j)/=0.0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
   aofset = .FALSE.
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      Bofset = .TRUE.
      J = Jofstb - 1
      DO I = 1 , 3
         J = J + 1
         IF ( Ecpt(J)/=0.0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
      ENDDO
      Bofset = .FALSE.
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
! FORM THE CENTER AXIS OF THE BEAM WITHOUT OFFSETS.
!
      Veci(1) = Ecpt(Jcsida+1) - Ecpt(Jcsidb+1)
      Veci(2) = Ecpt(Jcsida+2) - Ecpt(Jcsidb+2)
      Veci(3) = Ecpt(Jcsida+3) - Ecpt(Jcsidb+3)
!
! TRANSFORM THE OFFSET VECTORS IF NECESSARY
!
      IF ( .NOT.(.NOT.Aofset .AND. .NOT.Bofset) ) THEN
!
! TRANSFORM THE OFFSET VECTOR FOR POINT A IF NECESSARY.
!
         Idela = 1
         J = Jofsta - 1
         DO I = 1 , 3
            J = J + 1
            dela(I) = Ecpt(J)
         ENDDO
         IF ( .NOT.(Abasic) ) THEN
            Idela = 4
            CALL gmmats(Ta,3,3,0,dela(1),3,1,0,dela(4))
         ENDIF
!
! TRANSFORM THE OFFSET VECTOR FOR POINT B IF NECESSARY
!
         Idelb = 1
         J = Jofstb - 1
         DO I = 1 , 3
            J = J + 1
            delb(I) = Ecpt(J)
         ENDDO
         IF ( .NOT.(Bbasic) ) THEN
            Idelb = 4
            CALL gmmats(Tb,3,3,0,delb(1),3,1,0,delb(4))
         ENDIF
!
! SINCE THERE WAS AT LEAST ONE NON-ZERO OFFSET VECTOR RECOMPUTE VECI
!
         Veci(1) = Veci(1) + dela(Idela) - delb(Idelb)
         Veci(2) = Veci(2) + dela(Idela+1) - delb(Idelb+1)
         Veci(3) = Veci(3) + dela(Idela+2) - delb(Idelb+2)
      ENDIF
!
! COMPUTE THE LENGTH OF THE BIG V (VECI) VECTOR AND NORMALIZE
!
      Veci(1) = -Veci(1)
      Veci(2) = -Veci(2)
      Veci(3) = -Veci(3)
      Fl = sqrt(Veci(1)**2+Veci(2)**2+Veci(3)**2)
      DO I = 1 , 3
         Veci(I) = Veci(I)/Fl
      ENDDO
!
! COMPUTE THE SMALL V SUB 0 VECTOR, SMALV0.  ****CHECK THIS LOGIC****
!
      DO I = 1 , 3
         Smalv0(I) = smallv(I)
      ENDDO
      Isv = 1
      IF ( icssv/=0 ) THEN
         Isv = 4
         CALL gmmats(Ta,3,3,0,Smalv0(1),3,1,0,Smalv0(4))
      ENDIF
!
! COMPUTE THE K VECTOR, VECK = VECI  X  SMALV0, AND NORMALIZE
!
      Veck(1) = Veci(2)*Smalv0(Isv+2) - Veci(3)*Smalv0(Isv+1)
      Veck(2) = Veci(3)*Smalv0(Isv) - Veci(1)*Smalv0(Isv+2)
      Veck(3) = Veci(1)*Smalv0(Isv+1) - Veci(2)*Smalv0(Isv)
      Fll = sqrt(Veck(1)**2+Veck(2)**2+Veck(3)**2)
      Veck(1) = Veck(1)/Fll
      Veck(2) = Veck(2)/Fll
      Veck(3) = Veck(3)/Fll
!
! COMPUTE THE J VECTOR, VECJ = VECK  X  VECI, AND NORMALIZE
!
      Vecj(1) = Veck(2)*Veci(3) - Veck(3)*Veci(2)
      Vecj(2) = Veck(3)*Veci(1) - Veck(1)*Veci(3)
      Vecj(3) = Veck(1)*Veci(2) - Veck(2)*Veci(1)
      Fll = sqrt(Vecj(1)**2+Vecj(2)**2+Vecj(3)**2)
      Vecj(1) = Vecj(1)/Fll
      Vecj(2) = Vecj(2)/Fll
      Vecj(3) = Vecj(3)/Fll
!
! SET UP INTERMEDIATE VARIABLES FOR ELEMENT STIFFNESS MATRIX CALCULATION
!
      L = Fl
      Lsq = L**2
      Lcube = Lsq*L
!
! STORE INCREMENTAL DISPLACEMENT VECTORS IN DOUBLE PRECISION LOCATIONS
!
      DO I = 1 , 6
         U(I) = uain(I)
         U(I+12) = ubin(I)
      ENDDO
!*****
! COMPUTE ON FIRST PASS C  * E  * U   AND C  * E  * U  ON SECOND PASS
!                        B    B    B       A    A    A
!*****
      Ipass = 1
      Basic = Bbasic
      Offset = Bofset
      Jofset = Jofstb
      Jcsid = 10
      Index = 13
      SPAG_Loop_1_1: DO
!
! IF THERE ARE OFFSETS FOR THIS POINT, CONSTRUCT THE 3 X 3 MATRIX D.
!
         IF ( Offset ) THEN
            D(1) = 0.0
            D(2) = Ecpt(Jofset+2)
            D(3) = -Ecpt(Jofset+1)
            D(4) = -D(2)
            D(5) = 0.0
            D(6) = Ecpt(Jofset)
            D(7) = -D(3)
            D(8) = -D(6)
            D(9) = 0.0
!
! COMPUTE THE 3 VECTOR  D * U , WHERE U  IS THE VECTOR OF THE 3
!                            R         R
! ROTATIONAL DISPLACEMENTS
!
            CALL gmmats(D,3,3,0,U(Index+3),3,1,0,U(Index+6))
!
! ADD OFFSET CONTRIBUTION TO THE TRANSLATION COMPONENTS OF THE DISPLACE-
! MENT VECTOR
!
            J = Index
            DO I = 1 , 3
               U(J) = U(J) + U(J+6)
               J = J + 1
            ENDDO
         ENDIF
!
! TRANSFORM TRANSLATIONAL COMPONENTS TO BASIC COORDINATES IF NECESSARY
!
         IF ( .NOT.(Basic) ) THEN
            CALL gmmats(Ta(Jcsid),3,3,0,U(Index),3,1,0,U(Index+3))
!
! STORE TRANSFORMED VECTOR BACK INTO ITS ORIGINAL D.P. LOCATION
!
            U(Index) = U(Index+3)
            U(Index+1) = U(Index+4)
            U(Index+2) = U(Index+5)
         ENDIF
         IF ( Ipass==2 ) THEN
!
! FORM THE DIFFERENCE OF THE TRANSLATIONAL COMPONENTS OF THE TRANSFORMED
! DISPLACEMENT VECTORS
!
            DO I = 1 , 3
               U(I+12) = U(I+12) - U(I)
            ENDDO
!
! FORM DOT PRODUCT
!
            CALL gmmats(Veci,3,1,1,U(13),3,1,0,D(1))
!
! CALCULATE THE INCREMENTAL ELEMENT STRAIN
!
            Deps1 = D(1)/L
!
! PERFORM EXTENSIONAL STRAIN CALCULATIONS
!
            Deps2 = epsin2 - epsin1
            Eps1 = epsin2 + Deps1
            Eps2 = epsin2 + (Deps1+gammas**2*Deps2)*(gamma+1.0E0)/(gammas+1.0E0) + gammas*(Deps1-gammas*Deps2)*(gamma+1.0E0)        &
                 & **2/(gammas+1.0E0)
!
! CALL MAT ROUTINE TO GET SIGMA1 AND SIGMA2 AS FUNCTIONS OF EPS1,EPS2
!
            matidc = imatid
            matflg = 1
            CALL mat(Iecpt(1))
            Esub0l = esub0
            Gsub0l = gsub0
            matflg = 6
            plaarg = Eps1
            CALL mat(Iecpt(1))
            Sigma1 = Plaans
            plaarg = Eps2
            CALL mat(Iecpt(1))
            Sigma2 = Plaans
!
! NOTE THAT E1 IS USED IN THIS ROUTINE ONLY TO UPDATE THE EST (ECPT)
! ENTRY
!
            IF ( Eps1==Eps2 ) THEN
               E1 = estar
            ELSE
               E1 = (Sigma2-Sigma1)/(Eps2-Eps1)
            ENDIF
!
! BEGIN ELEMENT STRESS MATRIX CALCULATIONS.
!
            E = estar
            G = estar*Gsub0l/Esub0l
            Ei1 = E*i1
            Ei2 = E*i2
            IF ( k1==0.0 .OR. i12/=0.0 ) THEN
               R1 = 12.0*Ei1/Lcube
            ELSE
               Gak1 = G*a*k1
               R1 = (12.0*Ei1*Gak1)/(Gak1*Lcube+12.0*L*Ei1)
            ENDIF
            IF ( k2==0.0 .OR. i12/=0.0 ) THEN
               R2 = 12.0*Ei2/Lcube
            ELSE
               Gak2 = G*a*k2
               R2 = (12.0*Ei2*Gak2)/(Gak2*Lcube+12.0*L*Ei2)
            ENDIF
!
! COMPUTE THE -SMALL- K-S, SK1, SK2, SK3 AND SK4
!
            Sk1 = 0.25*R1*Lsq + Ei1/L
            Sk2 = 0.25*R2*Lsq + Ei2/L
            Sk3 = 0.25*R1*Lsq - Ei1/L
            Sk4 = 0.25*R2*Lsq - Ei2/L
!
! COMPUTE THE TERMS THAT WILL BE NEEDED FOR THE 12 X 12 MATRIX KE
!
            Ael = a*E/L
            Lr1 = L*R1/2.0
            Lr2 = L*R2/2.0
            Gjl = G*fj/L
!
! CONSTRUCT THE 12 X 12 MATRIX KE
!
            DO I = 1 , 144
               ke(I) = 0.0
            ENDDO
            ke(1) = Ael
            ke(7) = -Ael
            ke(14) = R1
            ke(18) = Lr1
            ke(20) = -R1
            ke(24) = Lr1
            ke(27) = R2
            ke(29) = -Lr2
            ke(33) = -R2
            ke(35) = -Lr2
            ke(40) = Gjl
            ke(46) = -Gjl
            ke(51) = -Lr2
            ke(53) = Sk2
            ke(57) = Lr2
            ke(59) = Sk4
            ke(62) = Lr1
            ke(66) = Sk1
            ke(68) = -Lr1
            ke(72) = Sk3
            ke(73) = -Ael
            ke(79) = Ael
            ke(86) = -R1
            ke(90) = -Lr1
            ke(92) = R1
            ke(96) = -Lr1
            ke(99) = -R2
            ke(101) = Lr2
            ke(105) = R2
            ke(107) = Lr2
            ke(112) = -Gjl
            ke(118) = Gjl
            ke(123) = -Lr2
            ke(125) = Sk4
            ke(129) = Lr2
            ke(131) = Sk2
            ke(134) = Lr1
            ke(138) = Sk3
            ke(140) = -Lr1
            ke(144) = Sk1
            IF ( i12/=0.0 ) THEN
               Beta = 12.0*E*i12/Lcube
               Lb = L*Beta/2.0
               L2b3 = Lsq*Beta/3.0
               L2b6 = Lsq*Beta/6.0
               ke(15) = Beta
               ke(17) = -Lb
               ke(21) = -Beta
               ke(23) = -Lb
               ke(26) = Beta
               ke(30) = Lb
               ke(32) = -Beta
               ke(36) = Lb
               ke(50) = -Lb
               ke(54) = -L2b3
               ke(56) = Lb
               ke(60) = -L2b6
               ke(63) = Lb
               ke(65) = -L2b3
               ke(69) = -Lb
               ke(71) = -L2b6
               ke(87) = -Beta
               ke(89) = Lb
               ke(93) = Beta
               ke(95) = Lb
               ke(98) = -Beta
               ke(102) = -Lb
               ke(104) = Beta
               ke(108) = -Lb
               ke(122) = -Lb
               ke(126) = -L2b6
               ke(128) = Lb
               ke(132) = -L2b3
               ke(135) = Lb
               ke(137) = -L2b6
               ke(141) = -Lb
               ke(143) = -L2b3
            ENDIF
!
! DETERMINE IF THERE ARE NON-ZERO PIN FLAGS.
!
            Ka = Iecpt(Jpina)
            Kb = Iecpt(Jpinb)
            IF ( Ka/=0 .OR. Kb/=0 ) THEN
!
! SET UP THE IPIN ARRAY
!
               DO I = 1 , 5
                  Ipin(I) = mod(Ka,10)
                  Ipin(I+5) = mod(Kb,10) + 6
                  IF ( Ipin(I+5)==6 ) Ipin(I+5) = 0
                  Ka = Ka/10
                  Kb = Kb/10
               ENDDO
!
! ALTER KE MATRIX DUE TO PIN FLAGS.
!
               DO I = 1 , 10
                  IF ( Ipin(I)/=0 ) THEN
                     Ii = 13*Ipin(I) - 12
                     IF ( ke(Ii)/=0.0 ) THEN
                        DO J = 1 , 12
                           Ji = 12*(J-1) + Ipin(I)
                           Ij = 12*(Ipin(I)-1) + J
                           DO Ll = 1 , 12
                              Jll = 12*(J-1) + Ll
                              Ill = 12*(Ipin(I)-1) + Ll
                              kep(Jll) = ke(Jll) - (ke(Ill)/ke(Ii))*ke(Ji)
                           ENDDO
                           kep(Ij) = 0.0
                           kep(Ji) = 0.0
                        ENDDO
                        DO K = 1 , 144
                           ke(K) = kep(K)
                        ENDDO
                     ELSE
                        Il = Ipin(I)
                        Ii = Ii - Il
                        DO J = 1 , 12
                           Ii = Ii + 1
                           ke(Ii) = 0.0
                           ke(Il) = 0.0
                           Il = Il + 12
                        ENDDO
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
!
!        E
! STORE K   IN KEP(1),...,KEP(36) AND
!        AA
!
!        E
! STORE K   IN KEP(37),...,KEP(72)
!        AB
!
            J = 0
            DO I = 1 , 72 , 12
               Low = I
               Lim = Low + 5
               DO K = Low , Lim
                  J = J + 1
                  kep(J) = ke(K)
                  kep(J+36) = ke(K+6)
               ENDDO
            ENDDO
!
!                                                        T
! STORE VECI, VECJ, VECK IN KE(1),...,KE(9) FORMING THE A  MATRIX.
!
            ke(1) = Veci(1)
            ke(2) = Veci(2)
            ke(3) = Veci(3)
            ke(4) = Vecj(1)
            ke(5) = Vecj(2)
            ke(6) = Vecj(3)
            ke(7) = Veck(1)
            ke(8) = Veck(2)
            ke(9) = Veck(3)
!
! SET POINTERS SO THAT WE WILL BE WORKING WITH POINT A.
!
            Basic = Abasic
            Jcsid = Jcsida
            Offset = Aofset
            Jofset = Jofsta
            Iwbeg = 0
            Ikel = 1
            Iab = 1
            Index = isilno(1)
!
! ZERO OUT THE ARRAY WHERE THE 3 X 3 MATRIX AND THE W  AND W  6 X 6
! MATRICES WILL RESIDE.                              A      B
!
            DO I = 28 , 108
               ke(I) = 0.0
            ENDDO
            EXIT SPAG_Loop_1_1
         ELSE
            Ipass = 2
            Basic = Abasic
            Offset = Aofset
            Jofset = Jofsta
            Jcsid = 1
            Index = 1
         ENDIF
      ENDDO SPAG_Loop_1_1
      SPAG_Loop_1_2: DO
!
! SET UP THE -G- MATRIX.  IG POINTS TO THE BEGINNING OF THE G MATRIX.
! G = AT X TI
!
         Ig = 1
         IF ( .NOT.(Basic) ) THEN
            CALL transs(Ecpt(Jcsid),ke(10))
            CALL gmmats(ke(1),3,3,0,ke(10),3,3,0,ke(19))
            Ig = 19
         ENDIF
!
! IF THERE IS A NON-ZERO OFFSET FOR THE POINT, SET UP THE D 3 X3 MATRIX.
!
         IF ( Offset ) THEN
            ke(10) = 0.0
            ke(11) = Ecpt(Jofset+2)
            ke(12) = -Ecpt(Jofset+1)
            ke(13) = -ke(11)
            ke(14) = 0.0
            ke(15) = Ecpt(Jofset)
            ke(16) = -ke(12)
            ke(17) = -ke(15)
            ke(18) = 0.0
!
! FORM THE 3 X 3 PRODUCT H = G X D, I.E., KE(28) = KE(IG) X KE(10)
!
            CALL gmmats(ke(Ig),3,3,0,ke(10),3,3,0,ke(28))
         ENDIF
!
!
! FORM THE W  MATRIX OR THE W  MATRIX IN KE(37) OR KE(73) DEPENDING
!           A                B
! UPON WHICH POINT - A OR B - IS UNDER CONSIDERATION.  G WILL BE STORED
! IN THE UPPER LEFT AND LOWER RIGHT CORNERS.  H, IF NON-ZERO, WILL BE
! STORED IN THE UPPER RIGHT CORNER.
!
!
         ke(Iwbeg+37) = ke(Ig)
         ke(Iwbeg+38) = ke(Ig+1)
         ke(Iwbeg+39) = ke(Ig+2)
         ke(Iwbeg+43) = ke(Ig+3)
         ke(Iwbeg+44) = ke(Ig+4)
         ke(Iwbeg+45) = ke(Ig+5)
         ke(Iwbeg+49) = ke(Ig+6)
         ke(Iwbeg+50) = ke(Ig+7)
         ke(Iwbeg+51) = ke(Ig+8)
         ke(Iwbeg+58) = ke(Ig)
         ke(Iwbeg+59) = ke(Ig+1)
         ke(Iwbeg+60) = ke(Ig+2)
         ke(Iwbeg+64) = ke(Ig+3)
         ke(Iwbeg+65) = ke(Ig+4)
         ke(Iwbeg+66) = ke(Ig+5)
         ke(Iwbeg+70) = ke(Ig+6)
         ke(Iwbeg+71) = ke(Ig+7)
         ke(Iwbeg+72) = ke(Ig+8)
         IF ( Offset ) THEN
            ke(Iwbeg+40) = ke(28)
            ke(Iwbeg+41) = ke(29)
            ke(Iwbeg+42) = ke(30)
            ke(Iwbeg+46) = ke(31)
            ke(Iwbeg+47) = ke(32)
            ke(Iwbeg+48) = ke(33)
            ke(Iwbeg+52) = ke(34)
            ke(Iwbeg+53) = ke(35)
            ke(Iwbeg+54) = ke(36)
         ENDIF
!
!                          E                      E
! FORM THE PRODUCT  S  =  K    X  W   OR  S   = K    X  W  , DEPENDING
!                    A     AA      A       B     AB      B
! UPON WHICH POINT WE ARE WORKING WITH.
!
         CALL gmmats(kep(Ikel),6,6,0,ke(Iwbeg+37),6,6,0,Sa(Iab))
!
! IF THE POINT UNDER CONSIDERATION IS POINT B WE ARE FINISHED.  IF NOT,
! SET UP POINTS AND INDICATORS FOR WORKING WITH POINT B.
!
         IF ( Iwbeg==36 ) THEN
!
! COMPUTE FORCES AND MOMENTS FROM  S   AND   S   AND DISPLACEMENT
!                                   A         B
! VECTORS
!
            CALL gmmats(Sa,6,6,0,uain,6,1,0,Fa)
            CALL gmmats(Sb,6,6,0,ubin,6,1,0,Fb)
            Fx = a*Sigma1
            V1 = -Fa(2) - Fb(2) + v1star
            V2 = -Fa(3) - Fb(3) + v2star
            T = -Fa(4) - Fb(4) + tstar
            M2a = Fa(5) + Fb(5) + m2astr
            M1a = -Fa(6) - Fb(6) + m1astr
            M1b = M1a - V1*L
            M2b = M2a - V2*L
!*****
! COMPUTE ELEMENT STRESSES AT 4 POINTS
!*****
!
! COMPUTE K1A AND K2A
!
            IF ( i12/=0.0 ) THEN
               K1a = (M2a*i12-M1a*i2)/(i1*i2-i12**2)
               K2a = (M1a*i12-M2a*i1)/(i1*i2-i12**2)
            ELSE
               IF ( i1/=0.0 ) THEN
                  K1a = -M1a/i1
               ELSE
                  K1a = 0.0
               ENDIF
               IF ( i2/=0.0 ) THEN
                  K2a = -M2a/i2
               ELSE
                  K2a = 0.0
               ENDIF
            ENDIF
!
! COMPUTE SIG1A, SIG2A, SIG3A AND SIG4A
!
            sig1a = K1a*c1 + K2a*c2
            sig2a = K1a*d1 + K2a*d2
            sig3a = K1a*f1 + K2a*f2
            sig4a = K1a*g1 + K2a*g2
!
! COMPUTE K1B AND K2B
!
            IF ( i12/=0.0 ) THEN
               K1b = (M2b*i12-M1b*i2)/(i1*i2-i12**2)
               K2b = (M1b*i12-M2b*i1)/(i1*i2-i12**2)
            ELSE
               IF ( i1/=0.0 ) THEN
                  K1b = -M1b/i1
               ELSE
                  K1b = 0.0
               ENDIF
               IF ( i2/=0.0 ) THEN
                  K2b = -M2b/i2
               ELSE
                  K2b = 0.0
               ENDIF
            ENDIF
!
! COMPUTE SIG1B, SIG2B, SIG3B AND SIG4B
!
            sig1b = K1b*c1 + K2b*c2
            sig2b = K1b*d1 + K2b*d2
            sig3b = K1b*f1 + K2b*f2
            sig4b = K1b*g1 + K2b*g2
!
! COMPUTE AXIAL STRESS
!
            sigax = 0.0
            IF ( a/=0.0 ) sigax = Fx/a
!
! COMPUTE MAXIMA AND MINIMA
!
            sigamx = sigax + amax1(sig1a,sig2a,sig3a,sig4a)
            sigbmx = sigax + amax1(sig1b,sig2b,sig3b,sig4b)
            sigamn = sigax + amin1(sig1a,sig2a,sig3a,sig4a)
            sigbmn = sigax + amin1(sig1b,sig2b,sig3b,sig4b)
!
! COMPUTE MARGIN OF SAFETY IN TENSION
!
            IF ( sigmat<=0.0 ) THEN
               msten = 1
            ELSEIF ( amax1(sigamx,sigbmx)<=0.0 ) THEN
               msten = 1
            ELSE
               Q = sigmat/amax1(sigamx,sigbmx)
               Smten = Q - 1.0
            ENDIF
!
!      COMPUTE MARGIN OF SAFETY IN COMPRESSION
!
            sigmac = -abs(sigmac)
            IF ( amin1(sigamn,sigbmn)>=0.0 ) THEN
               mscom = 1
            ELSE
               W = sigmac/amin1(sigamn,sigbmn)
               Smcom = W - 1.0
            ENDIF
            iselid = ielid
!
! UPDATE EST (ECPT) ENTRIES
!
            epsin1 = epsin2
            epsin2 = Eps1
            estar = E1
            v1star = V1
            v2star = V2
            tstar = T
            m1astr = M1a
            m2astr = M2a
            EXIT SPAG_Loop_1_2
         ELSE
            Basic = Bbasic
            Jcsid = Jcsidb
            Offset = Bofset
            Jofset = Jofstb
            Iwbeg = 36
            Ikel = 37
            Iab = 37
            Index = isilno(2)
            DO I = 28 , 36
               ke(I) = 0.0
            ENDDO
         ENDIF
      ENDDO SPAG_Loop_1_2
   END SUBROUTINE spag_block_2
END SUBROUTINE psbar
