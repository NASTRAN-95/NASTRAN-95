
SUBROUTINE psbar
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A , Alpha , C1 , C2 , D(9) , D1 , D2 , Dela(6) , Delb(6) , Dum14(14) , Dum2(2) , Ecpt(100) , Eltemp , Epsin1 , Epsin2 ,     &
      & Estar , Esub0 , F1 , F2 , Fe , Fj , G1 , G2 , Gamma , Gammas , Gpa(3) , Gpb(3) , Gsub0 , Gsube , I1 , I12 , I2 , K1 , K2 ,  &
      & Ke(144) , Kep(144) , M1astr , M2astr , Nsm , Plaans , Plaarg , Rho , Sig1a , Sig1b , Sig2a , Sig2b , Sig3a , Sig3b , Sig4a ,&
      & Sig4b , Sigamn , Sigamx , Sigax , Sigbmn , Sigbmx , Sigmac , Sigmas , Sigmat , Smallv(3) , Smcom , Smten , Temdum , Tstar , &
      & Tsub0 , Uain(6) , Ubin(6) , V1star , V2star , Za(3) , Zb(3)
   INTEGER Icssv , Iecpt(100) , Ielid , Imatid , Ipinfl(2) , Iselid , Isilno(2) , Matflg , Matidc , Mcsida , Mcsidb , Mscom ,       &
         & Msten , Nu
   COMMON /matin / Matidc , Matflg , Temdum , Plaarg , Dum2
   COMMON /matout/ Esub0 , Gsub0 , Nu , Rho , Alpha , Tsub0 , Gsube , Sigmat , Sigmac , Sigmas
   COMMON /pla32c/ Gamma , Gammas
   COMMON /pla32e/ Ielid , Isilno , Smallv , Icssv , Ipinfl , Za , Zb , Imatid , A , I1 , I2 , Fj , Nsm , Fe , C1 , C2 , D1 , D2 ,  &
                 & F1 , F2 , G1 , G2 , K1 , K2 , I12 , Mcsida , Gpa , Mcsidb , Gpb , Eltemp , Epsin1 , Epsin2 , Estar , V1star ,    &
                 & V2star , Tstar , M1astr , M2astr , Uain , Ubin
   COMMON /pla32s/ Ke , Kep , Dela , Delb
   COMMON /sout  / Iselid , Sig1a , Sig2a , Sig3a , Sig4a , Sigax , Sigamx , Sigamn , Msten , Sig1b , Sig2b , Sig3b , Sig4b ,       &
                 & Sigbmx , Sigbmn , Mscom , Dum14
!
! Local variable declarations
!
   LOGICAL abasic , aofset , basic , bbasic , bofset , offset
   REAL ael , beta , deps1 , deps2 , e , e1 , ei1 , ei2 , eps1 , eps2 , esub0l , fa(6) , fb(6) , fl , fll , fx , g , gak1 , gak2 ,  &
      & gjl , gsub0l , k1a , k1b , k2a , k2b , l , l2b3 , l2b6 , lb , lcube , lr1 , lr2 , lsq , m1a , m1b , m2a , m2b , q , r1 ,    &
      & r2 , sa(72) , sb(36) , sigma1 , sigma2 , sk1 , sk2 , sk3 , sk4 , smalv0(6) , t , ta(18) , tb(9) , u(24) , v1 , v2 , veci(3) &
      & , vecj(3) , veck(3) , w
   INTEGER i , iab , icsida , icsidb , idela , idelb , ig , ii , ij , ikel , il , ill , index , ipass , ipin(10) , isv , iwbeg , j ,&
         & jcsid , jcsida , jcsidb , ji , jll , jofset , jofsta , jofstb , jpina , jpinb , k , ka , kb , lim , ll , low
!
! End of declarations
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
   EQUIVALENCE (Ielid,Ecpt(1),Iecpt(1)) , (ta(10),tb(1)) , (Ecpt(71),D(1)) , (Esub0,Plaans) , (sa(37),sb(1)) , (Msten,Smten) ,      &
    & (Mscom,Smcom)
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
   icsida = Iecpt(34)
   icsidb = Iecpt(38)
!
! NORMALIZE THE REFERENCE VECTOR WHICH LIES IN THE FIRST PRINCIPAL AXIS
! PLANE  (FMMS - 36 P. 4)
!
   fl = 0.0
   DO i = 1 , 3
      fl = fl + Smallv(i)**2
   ENDDO
   fl = sqrt(fl)
   DO i = 1 , 3
      Smallv(i) = Smallv(i)/fl
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
   IF ( .NOT.abasic ) CALL transs(Ecpt(jcsida),ta)
   IF ( .NOT.bbasic ) CALL transs(Ecpt(jcsida),tb)
!
! DETERMINE IF WE HAVE NON-ZERO OFFSET VECTORS.
!
   aofset = .TRUE.
   j = jofsta - 1
   DO i = 1 , 3
      j = j + 1
      IF ( Ecpt(j)/=0.0 ) GOTO 100
   ENDDO
   aofset = .FALSE.
 100  bofset = .TRUE.
   j = jofstb - 1
   DO i = 1 , 3
      j = j + 1
      IF ( Ecpt(j)/=0.0 ) GOTO 200
   ENDDO
   bofset = .FALSE.
!
! FORM THE CENTER AXIS OF THE BEAM WITHOUT OFFSETS.
!
 200  veci(1) = Ecpt(jcsida+1) - Ecpt(jcsidb+1)
   veci(2) = Ecpt(jcsida+2) - Ecpt(jcsidb+2)
   veci(3) = Ecpt(jcsida+3) - Ecpt(jcsidb+3)
!
! TRANSFORM THE OFFSET VECTORS IF NECESSARY
!
   IF ( .NOT.(.NOT.aofset .AND. .NOT.bofset) ) THEN
!
! TRANSFORM THE OFFSET VECTOR FOR POINT A IF NECESSARY.
!
      idela = 1
      j = jofsta - 1
      DO i = 1 , 3
         j = j + 1
         Dela(i) = Ecpt(j)
      ENDDO
      IF ( .NOT.(abasic) ) THEN
         idela = 4
         CALL gmmats(ta,3,3,0,Dela(1),3,1,0,Dela(4))
      ENDIF
!
! TRANSFORM THE OFFSET VECTOR FOR POINT B IF NECESSARY
!
      idelb = 1
      j = jofstb - 1
      DO i = 1 , 3
         j = j + 1
         Delb(i) = Ecpt(j)
      ENDDO
      IF ( .NOT.(bbasic) ) THEN
         idelb = 4
         CALL gmmats(tb,3,3,0,Delb(1),3,1,0,Delb(4))
      ENDIF
!
! SINCE THERE WAS AT LEAST ONE NON-ZERO OFFSET VECTOR RECOMPUTE VECI
!
      veci(1) = veci(1) + Dela(idela) - Delb(idelb)
      veci(2) = veci(2) + Dela(idela+1) - Delb(idelb+1)
      veci(3) = veci(3) + Dela(idela+2) - Delb(idelb+2)
   ENDIF
!
! COMPUTE THE LENGTH OF THE BIG V (VECI) VECTOR AND NORMALIZE
!
   veci(1) = -veci(1)
   veci(2) = -veci(2)
   veci(3) = -veci(3)
   fl = sqrt(veci(1)**2+veci(2)**2+veci(3)**2)
   DO i = 1 , 3
      veci(i) = veci(i)/fl
   ENDDO
!
! COMPUTE THE SMALL V SUB 0 VECTOR, SMALV0.  ****CHECK THIS LOGIC****
!
   DO i = 1 , 3
      smalv0(i) = Smallv(i)
   ENDDO
   isv = 1
   IF ( Icssv/=0 ) THEN
      isv = 4
      CALL gmmats(ta,3,3,0,smalv0(1),3,1,0,smalv0(4))
   ENDIF
!
! COMPUTE THE K VECTOR, VECK = VECI  X  SMALV0, AND NORMALIZE
!
   veck(1) = veci(2)*smalv0(isv+2) - veci(3)*smalv0(isv+1)
   veck(2) = veci(3)*smalv0(isv) - veci(1)*smalv0(isv+2)
   veck(3) = veci(1)*smalv0(isv+1) - veci(2)*smalv0(isv)
   fll = sqrt(veck(1)**2+veck(2)**2+veck(3)**2)
   veck(1) = veck(1)/fll
   veck(2) = veck(2)/fll
   veck(3) = veck(3)/fll
!
! COMPUTE THE J VECTOR, VECJ = VECK  X  VECI, AND NORMALIZE
!
   vecj(1) = veck(2)*veci(3) - veck(3)*veci(2)
   vecj(2) = veck(3)*veci(1) - veck(1)*veci(3)
   vecj(3) = veck(1)*veci(2) - veck(2)*veci(1)
   fll = sqrt(vecj(1)**2+vecj(2)**2+vecj(3)**2)
   vecj(1) = vecj(1)/fll
   vecj(2) = vecj(2)/fll
   vecj(3) = vecj(3)/fll
!
! SET UP INTERMEDIATE VARIABLES FOR ELEMENT STIFFNESS MATRIX CALCULATION
!
   l = fl
   lsq = l**2
   lcube = lsq*l
!
! STORE INCREMENTAL DISPLACEMENT VECTORS IN DOUBLE PRECISION LOCATIONS
!
   DO i = 1 , 6
      u(i) = Uain(i)
      u(i+12) = Ubin(i)
   ENDDO
!*****
! COMPUTE ON FIRST PASS C  * E  * U   AND C  * E  * U  ON SECOND PASS
!                        B    B    B       A    A    A
!*****
   ipass = 1
   basic = bbasic
   offset = bofset
   jofset = jofstb
   jcsid = 10
   index = 13
   DO
!
! IF THERE ARE OFFSETS FOR THIS POINT, CONSTRUCT THE 3 X 3 MATRIX D.
!
      IF ( offset ) THEN
         D(1) = 0.0
         D(2) = Ecpt(jofset+2)
         D(3) = -Ecpt(jofset+1)
         D(4) = -D(2)
         D(5) = 0.0
         D(6) = Ecpt(jofset)
         D(7) = -D(3)
         D(8) = -D(6)
         D(9) = 0.0
!
! COMPUTE THE 3 VECTOR  D * U , WHERE U  IS THE VECTOR OF THE 3
!                            R         R
! ROTATIONAL DISPLACEMENTS
!
         CALL gmmats(D,3,3,0,u(index+3),3,1,0,u(index+6))
!
! ADD OFFSET CONTRIBUTION TO THE TRANSLATION COMPONENTS OF THE DISPLACE-
! MENT VECTOR
!
         j = index
         DO i = 1 , 3
            u(j) = u(j) + u(j+6)
            j = j + 1
         ENDDO
      ENDIF
!
! TRANSFORM TRANSLATIONAL COMPONENTS TO BASIC COORDINATES IF NECESSARY
!
      IF ( .NOT.(basic) ) THEN
         CALL gmmats(ta(jcsid),3,3,0,u(index),3,1,0,u(index+3))
!
! STORE TRANSFORMED VECTOR BACK INTO ITS ORIGINAL D.P. LOCATION
!
         u(index) = u(index+3)
         u(index+1) = u(index+4)
         u(index+2) = u(index+5)
      ENDIF
      IF ( ipass==2 ) THEN
!
! FORM THE DIFFERENCE OF THE TRANSLATIONAL COMPONENTS OF THE TRANSFORMED
! DISPLACEMENT VECTORS
!
         DO i = 1 , 3
            u(i+12) = u(i+12) - u(i)
         ENDDO
!
! FORM DOT PRODUCT
!
         CALL gmmats(veci,3,1,1,u(13),3,1,0,D(1))
!
! CALCULATE THE INCREMENTAL ELEMENT STRAIN
!
         deps1 = D(1)/l
!
! PERFORM EXTENSIONAL STRAIN CALCULATIONS
!
         deps2 = Epsin2 - Epsin1
         eps1 = Epsin2 + deps1
         eps2 = Epsin2 + (deps1+Gammas**2*deps2)*(Gamma+1.0E0)/(Gammas+1.0E0) + Gammas*(deps1-Gammas*deps2)*(Gamma+1.0E0)           &
              & **2/(Gammas+1.0E0)
!
! CALL MAT ROUTINE TO GET SIGMA1 AND SIGMA2 AS FUNCTIONS OF EPS1,EPS2
!
         Matidc = Imatid
         Matflg = 1
         CALL mat(Iecpt(1))
         esub0l = Esub0
         gsub0l = Gsub0
         Matflg = 6
         Plaarg = eps1
         CALL mat(Iecpt(1))
         sigma1 = Plaans
         Plaarg = eps2
         CALL mat(Iecpt(1))
         sigma2 = Plaans
!
! NOTE THAT E1 IS USED IN THIS ROUTINE ONLY TO UPDATE THE EST (ECPT)
! ENTRY
!
         IF ( eps1==eps2 ) THEN
            e1 = Estar
         ELSE
            e1 = (sigma2-sigma1)/(eps2-eps1)
         ENDIF
!
! BEGIN ELEMENT STRESS MATRIX CALCULATIONS.
!
         e = Estar
         g = Estar*gsub0l/esub0l
         ei1 = e*I1
         ei2 = e*I2
         IF ( K1==0.0 .OR. I12/=0.0 ) THEN
            r1 = 12.0*ei1/lcube
         ELSE
            gak1 = g*A*K1
            r1 = (12.0*ei1*gak1)/(gak1*lcube+12.0*l*ei1)
         ENDIF
         IF ( K2==0.0 .OR. I12/=0.0 ) THEN
            r2 = 12.0*ei2/lcube
         ELSE
            gak2 = g*A*K2
            r2 = (12.0*ei2*gak2)/(gak2*lcube+12.0*l*ei2)
         ENDIF
!
! COMPUTE THE -SMALL- K-S, SK1, SK2, SK3 AND SK4
!
         sk1 = 0.25*r1*lsq + ei1/l
         sk2 = 0.25*r2*lsq + ei2/l
         sk3 = 0.25*r1*lsq - ei1/l
         sk4 = 0.25*r2*lsq - ei2/l
!
! COMPUTE THE TERMS THAT WILL BE NEEDED FOR THE 12 X 12 MATRIX KE
!
         ael = A*e/l
         lr1 = l*r1/2.0
         lr2 = l*r2/2.0
         gjl = g*Fj/l
!
! CONSTRUCT THE 12 X 12 MATRIX KE
!
         DO i = 1 , 144
            Ke(i) = 0.0
         ENDDO
         Ke(1) = ael
         Ke(7) = -ael
         Ke(14) = r1
         Ke(18) = lr1
         Ke(20) = -r1
         Ke(24) = lr1
         Ke(27) = r2
         Ke(29) = -lr2
         Ke(33) = -r2
         Ke(35) = -lr2
         Ke(40) = gjl
         Ke(46) = -gjl
         Ke(51) = -lr2
         Ke(53) = sk2
         Ke(57) = lr2
         Ke(59) = sk4
         Ke(62) = lr1
         Ke(66) = sk1
         Ke(68) = -lr1
         Ke(72) = sk3
         Ke(73) = -ael
         Ke(79) = ael
         Ke(86) = -r1
         Ke(90) = -lr1
         Ke(92) = r1
         Ke(96) = -lr1
         Ke(99) = -r2
         Ke(101) = lr2
         Ke(105) = r2
         Ke(107) = lr2
         Ke(112) = -gjl
         Ke(118) = gjl
         Ke(123) = -lr2
         Ke(125) = sk4
         Ke(129) = lr2
         Ke(131) = sk2
         Ke(134) = lr1
         Ke(138) = sk3
         Ke(140) = -lr1
         Ke(144) = sk1
         IF ( I12/=0.0 ) THEN
            beta = 12.0*e*I12/lcube
            lb = l*beta/2.0
            l2b3 = lsq*beta/3.0
            l2b6 = lsq*beta/6.0
            Ke(15) = beta
            Ke(17) = -lb
            Ke(21) = -beta
            Ke(23) = -lb
            Ke(26) = beta
            Ke(30) = lb
            Ke(32) = -beta
            Ke(36) = lb
            Ke(50) = -lb
            Ke(54) = -l2b3
            Ke(56) = lb
            Ke(60) = -l2b6
            Ke(63) = lb
            Ke(65) = -l2b3
            Ke(69) = -lb
            Ke(71) = -l2b6
            Ke(87) = -beta
            Ke(89) = lb
            Ke(93) = beta
            Ke(95) = lb
            Ke(98) = -beta
            Ke(102) = -lb
            Ke(104) = beta
            Ke(108) = -lb
            Ke(122) = -lb
            Ke(126) = -l2b6
            Ke(128) = lb
            Ke(132) = -l2b3
            Ke(135) = lb
            Ke(137) = -l2b6
            Ke(141) = -lb
            Ke(143) = -l2b3
         ENDIF
!
! DETERMINE IF THERE ARE NON-ZERO PIN FLAGS.
!
         ka = Iecpt(jpina)
         kb = Iecpt(jpinb)
         IF ( ka/=0 .OR. kb/=0 ) THEN
!
! SET UP THE IPIN ARRAY
!
            DO i = 1 , 5
               ipin(i) = mod(ka,10)
               ipin(i+5) = mod(kb,10) + 6
               IF ( ipin(i+5)==6 ) ipin(i+5) = 0
               ka = ka/10
               kb = kb/10
            ENDDO
!
! ALTER KE MATRIX DUE TO PIN FLAGS.
!
            DO i = 1 , 10
               IF ( ipin(i)/=0 ) THEN
                  ii = 13*ipin(i) - 12
                  IF ( Ke(ii)/=0.0 ) THEN
                     DO j = 1 , 12
                        ji = 12*(j-1) + ipin(i)
                        ij = 12*(ipin(i)-1) + j
                        DO ll = 1 , 12
                           jll = 12*(j-1) + ll
                           ill = 12*(ipin(i)-1) + ll
                           Kep(jll) = Ke(jll) - (Ke(ill)/Ke(ii))*Ke(ji)
                        ENDDO
                        Kep(ij) = 0.0
                        Kep(ji) = 0.0
                     ENDDO
                     DO k = 1 , 144
                        Ke(k) = Kep(k)
                     ENDDO
                  ELSE
                     il = ipin(i)
                     ii = ii - il
                     DO j = 1 , 12
                        ii = ii + 1
                        Ke(ii) = 0.0
                        Ke(il) = 0.0
                        il = il + 12
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
         j = 0
         DO i = 1 , 72 , 12
            low = i
            lim = low + 5
            DO k = low , lim
               j = j + 1
               Kep(j) = Ke(k)
               Kep(j+36) = Ke(k+6)
            ENDDO
         ENDDO
!
!                                                        T
! STORE VECI, VECJ, VECK IN KE(1),...,KE(9) FORMING THE A  MATRIX.
!
         Ke(1) = veci(1)
         Ke(2) = veci(2)
         Ke(3) = veci(3)
         Ke(4) = vecj(1)
         Ke(5) = vecj(2)
         Ke(6) = vecj(3)
         Ke(7) = veck(1)
         Ke(8) = veck(2)
         Ke(9) = veck(3)
!
! SET POINTERS SO THAT WE WILL BE WORKING WITH POINT A.
!
         basic = abasic
         jcsid = jcsida
         offset = aofset
         jofset = jofsta
         iwbeg = 0
         ikel = 1
         iab = 1
         index = Isilno(1)
!
! ZERO OUT THE ARRAY WHERE THE 3 X 3 MATRIX AND THE W  AND W  6 X 6
! MATRICES WILL RESIDE.                              A      B
!
         DO i = 28 , 108
            Ke(i) = 0.0
         ENDDO
         EXIT
      ELSE
         ipass = 2
         basic = abasic
         offset = aofset
         jofset = jofsta
         jcsid = 1
         index = 1
      ENDIF
   ENDDO
   DO
!
! SET UP THE -G- MATRIX.  IG POINTS TO THE BEGINNING OF THE G MATRIX.
! G = AT X TI
!
      ig = 1
      IF ( .NOT.(basic) ) THEN
         CALL transs(Ecpt(jcsid),Ke(10))
         CALL gmmats(Ke(1),3,3,0,Ke(10),3,3,0,Ke(19))
         ig = 19
      ENDIF
!
! IF THERE IS A NON-ZERO OFFSET FOR THE POINT, SET UP THE D 3 X3 MATRIX.
!
      IF ( offset ) THEN
         Ke(10) = 0.0
         Ke(11) = Ecpt(jofset+2)
         Ke(12) = -Ecpt(jofset+1)
         Ke(13) = -Ke(11)
         Ke(14) = 0.0
         Ke(15) = Ecpt(jofset)
         Ke(16) = -Ke(12)
         Ke(17) = -Ke(15)
         Ke(18) = 0.0
!
! FORM THE 3 X 3 PRODUCT H = G X D, I.E., KE(28) = KE(IG) X KE(10)
!
         CALL gmmats(Ke(ig),3,3,0,Ke(10),3,3,0,Ke(28))
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
      Ke(iwbeg+37) = Ke(ig)
      Ke(iwbeg+38) = Ke(ig+1)
      Ke(iwbeg+39) = Ke(ig+2)
      Ke(iwbeg+43) = Ke(ig+3)
      Ke(iwbeg+44) = Ke(ig+4)
      Ke(iwbeg+45) = Ke(ig+5)
      Ke(iwbeg+49) = Ke(ig+6)
      Ke(iwbeg+50) = Ke(ig+7)
      Ke(iwbeg+51) = Ke(ig+8)
      Ke(iwbeg+58) = Ke(ig)
      Ke(iwbeg+59) = Ke(ig+1)
      Ke(iwbeg+60) = Ke(ig+2)
      Ke(iwbeg+64) = Ke(ig+3)
      Ke(iwbeg+65) = Ke(ig+4)
      Ke(iwbeg+66) = Ke(ig+5)
      Ke(iwbeg+70) = Ke(ig+6)
      Ke(iwbeg+71) = Ke(ig+7)
      Ke(iwbeg+72) = Ke(ig+8)
      IF ( offset ) THEN
         Ke(iwbeg+40) = Ke(28)
         Ke(iwbeg+41) = Ke(29)
         Ke(iwbeg+42) = Ke(30)
         Ke(iwbeg+46) = Ke(31)
         Ke(iwbeg+47) = Ke(32)
         Ke(iwbeg+48) = Ke(33)
         Ke(iwbeg+52) = Ke(34)
         Ke(iwbeg+53) = Ke(35)
         Ke(iwbeg+54) = Ke(36)
      ENDIF
!
!                          E                      E
! FORM THE PRODUCT  S  =  K    X  W   OR  S   = K    X  W  , DEPENDING
!                    A     AA      A       B     AB      B
! UPON WHICH POINT WE ARE WORKING WITH.
!
      CALL gmmats(Kep(ikel),6,6,0,Ke(iwbeg+37),6,6,0,sa(iab))
!
! IF THE POINT UNDER CONSIDERATION IS POINT B WE ARE FINISHED.  IF NOT,
! SET UP POINTS AND INDICATORS FOR WORKING WITH POINT B.
!
      IF ( iwbeg==36 ) THEN
!
! COMPUTE FORCES AND MOMENTS FROM  S   AND   S   AND DISPLACEMENT
!                                   A         B
! VECTORS
!
         CALL gmmats(sa,6,6,0,Uain,6,1,0,fa)
         CALL gmmats(sb,6,6,0,Ubin,6,1,0,fb)
         fx = A*sigma1
         v1 = -fa(2) - fb(2) + V1star
         v2 = -fa(3) - fb(3) + V2star
         t = -fa(4) - fb(4) + Tstar
         m2a = fa(5) + fb(5) + M2astr
         m1a = -fa(6) - fb(6) + M1astr
         m1b = m1a - v1*l
         m2b = m2a - v2*l
!*****
! COMPUTE ELEMENT STRESSES AT 4 POINTS
!*****
!
! COMPUTE K1A AND K2A
!
         IF ( I12/=0.0 ) THEN
            k1a = (m2a*I12-m1a*I2)/(I1*I2-I12**2)
            k2a = (m1a*I12-m2a*I1)/(I1*I2-I12**2)
         ELSE
            IF ( I1/=0.0 ) THEN
               k1a = -m1a/I1
            ELSE
               k1a = 0.0
            ENDIF
            IF ( I2/=0.0 ) THEN
               k2a = -m2a/I2
            ELSE
               k2a = 0.0
            ENDIF
         ENDIF
!
! COMPUTE SIG1A, SIG2A, SIG3A AND SIG4A
!
         Sig1a = k1a*C1 + k2a*C2
         Sig2a = k1a*D1 + k2a*D2
         Sig3a = k1a*F1 + k2a*F2
         Sig4a = k1a*G1 + k2a*G2
!
! COMPUTE K1B AND K2B
!
         IF ( I12/=0.0 ) THEN
            k1b = (m2b*I12-m1b*I2)/(I1*I2-I12**2)
            k2b = (m1b*I12-m2b*I1)/(I1*I2-I12**2)
         ELSE
            IF ( I1/=0.0 ) THEN
               k1b = -m1b/I1
            ELSE
               k1b = 0.0
            ENDIF
            IF ( I2/=0.0 ) THEN
               k2b = -m2b/I2
            ELSE
               k2b = 0.0
            ENDIF
         ENDIF
!
! COMPUTE SIG1B, SIG2B, SIG3B AND SIG4B
!
         Sig1b = k1b*C1 + k2b*C2
         Sig2b = k1b*D1 + k2b*D2
         Sig3b = k1b*F1 + k2b*F2
         Sig4b = k1b*G1 + k2b*G2
!
! COMPUTE AXIAL STRESS
!
         Sigax = 0.0
         IF ( A/=0.0 ) Sigax = fx/A
!
! COMPUTE MAXIMA AND MINIMA
!
         Sigamx = Sigax + amax1(Sig1a,Sig2a,Sig3a,Sig4a)
         Sigbmx = Sigax + amax1(Sig1b,Sig2b,Sig3b,Sig4b)
         Sigamn = Sigax + amin1(Sig1a,Sig2a,Sig3a,Sig4a)
         Sigbmn = Sigax + amin1(Sig1b,Sig2b,Sig3b,Sig4b)
!
! COMPUTE MARGIN OF SAFETY IN TENSION
!
         IF ( Sigmat<=0.0 ) THEN
            Msten = 1
         ELSEIF ( amax1(Sigamx,Sigbmx)<=0.0 ) THEN
            Msten = 1
         ELSE
            q = Sigmat/amax1(Sigamx,Sigbmx)
            Smten = q - 1.0
         ENDIF
!
!      COMPUTE MARGIN OF SAFETY IN COMPRESSION
!
         Sigmac = -abs(Sigmac)
         IF ( amin1(Sigamn,Sigbmn)>=0.0 ) THEN
            Mscom = 1
         ELSE
            w = Sigmac/amin1(Sigamn,Sigbmn)
            Smcom = w - 1.0
         ENDIF
         Iselid = Ielid
!
! UPDATE EST (ECPT) ENTRIES
!
         Epsin1 = Epsin2
         Epsin2 = eps1
         Estar = e1
         V1star = v1
         V2star = v2
         Tstar = t
         M1astr = m1a
         M2astr = m2a
         EXIT
      ELSE
         basic = bbasic
         jcsid = jcsidb
         offset = bofset
         jofset = jofstb
         iwbeg = 36
         ikel = 37
         iab = 37
         index = Isilno(2)
         DO i = 28 , 36
            Ke(i) = 0.0
         ENDDO
      ENDIF
   ENDDO
END SUBROUTINE psbar
