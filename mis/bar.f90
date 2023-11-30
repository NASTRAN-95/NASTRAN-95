
SUBROUTINE bar(Z,Idefm,Nogptt,Noedt)
   IMPLICIT NONE
   REAL A , Alpha , C1 , C2 , Costh , D1 , D2 , Dela(6) , Delb(6) , E , Ecpt(100) , Eltemp , F1 , F2 , Fe , Fj , G , G1 , G2 ,      &
      & Gpa(3) , Gpb(3) , Gsube , I1 , I12 , I2 , K1 , K2 , Ke(144) , Kep(144) , Nsm , Rho , Sigc , Sigs , Sigt , Sinth , Smallv(3) &
      & , Stress , Tempel , Tsub0 , Za(3) , Zb(3)
   INTEGER Icssv , Iecpt(38) , Ielid , Imatid , Ipinfl(2) , Isilno(2) , Matflg , Matidc , Mcsida , Mcsidb , Nu
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , G , Nu , Rho , Alpha , Tsub0 , Gsube , Sigt , Sigc , Sigs
   COMMON /ssgwrk/ Ke , Kep , Dela , Delb
   COMMON /trimex/ Ielid , Isilno , Smallv , Icssv , Ipinfl , Za , Zb , Imatid , A , I1 , I2 , Fj , Nsm , Fe , C1 , C2 , D1 , D2 ,  &
                 & F1 , F2 , G1 , G2 , K1 , K2 , I12 , Mcsida , Gpa , Mcsidb , Gpb , Tempel
   INTEGER Idefm , Noedt , Nogptt
   REAL Z(1)
   LOGICAL abasic , aofset , basic , bbasic , bofset , offset
   REAL ael , alphal , beta , delta , ei1 , ei2 , fl , fll , gak1 , gak2 , gjl , l , l2b3 , l2b6 , lb , lcube , lr1 , lr2 , lsq ,   &
      & r1 , r2 , sk1 , sk2 , sk3 , sk4 , smalv0(6) , ta(18) , tb(9) , tbar , ua(6) , veci(3) , vecj(3) , veck(3)
   INTEGER i , icsida , icsidb , idela , idelb , ig , ii , ij , ikel , il , ill , index , ipin(10) , isasb , isv , iwbeg , j ,      &
         & jcsid , jcsida , jcsidb , ji , jll , jofset , jofsta , jofstb , jpina , jpinb , k , ka , kb , lim , ll , low
!
!     THIS IS THE ELEMENT TEMPERATURE AND DEFORMATION LOADING ROUTINE
!     FOR THE BAR ELEMENT.
!
!     THIS ROUTINE IS VERY MUCH SIMILIAR TO THAT OF SUBROUTINES KBAR AND
!     SBAR1 THUS ANY ALTERS HERE MAY BE REQUIRED IN THESE OTHER TWO
!     ROUTINES ALSO.
!
!     ECPT FOR THE BAR
!
!     ECPT( 1)  -  IELID         ELEMENT ID. NUMBER
!     ECPT( 2)  -  ISILNO(2)     * SCALAR INDEX NOS. OF THE GRID POINTS
!     ECPT( 3)  -    ...         *
!     ECPT( 4)  -  SMALLV(3)     $ REFERENCE VECTOR
!     ECPT( 5)  -    ...         $
!     ECPT( 6)  -    ...         $
!     ECPT( 7)  -  ICSSV         COOR. SYS. ID FOR SMALLV VECTOR
!     ECPT( 8)  -  IPINFL(2)     * PIN FLAGS
!     ECPT( 9)  -    ...         *
!     ECPT(10)  -  ZA(3)         $ OFFSET VECTOR FOR POINT A
!     ECPT(11)  -    ...         $
!     ECPT(12)  -    ...         $
!     ECPT(13)  -  ZB(3)         * OFFSET VECTOR FOR POINT B
!     ECPT(14)  -    ...         *
!     ECPT(15)  -    ...         *
!     ECPT(16)  -  IMATID        MATERIAL ID.
!     ECPT(17)  -  A             CROSS-SECTIONAL AREA
!     ECPT(18)  -  I1            $ AREA MOMENTS OF INERTIA
!     ECPT(19)  -  I2            $
!     ECPT(20)  -  FJ            TORSIONAL CONSTANT
!     ECPT(21)  -  NSM           NON-STRUCTURAL MASS
!     ECPT(22)  -  FE            FORCE ELEM DESCRIPTIONS (FORCE METHOD)
!     ECPT(23)  -  C1            * STRESS RECOVERY COEFFICIENTS
!     ECPT(24)  -  C2            *
!     ECPT(25)  -  D1            *
!     ECPT(26)  -  D2            *
!     ECPT(27)  -  F1            *
!     ECPT(28)  -  F2            *
!     ECPT(29)  -  G1            *
!     ECPT(30)  -  G2            *
!     ECPT(31)  -  K1            $ AREA FACTORS FOR SHEAR
!     ECPT(32)  -  K2            $
!     ECPT(33)  -  I12           AREA MOMENT OF INERTIA
!     ECPT(34)  -  MCSIDA        COOR. SYS. ID. FOR GRID POINT A
!     ECPT(35)  -  GPA(3)        * BASIC COORDINATES FOR GRID POINT A
!     ECPT(36)  -    ...         *
!     ECPT(37)  -    ...         *
!     ECPT(38)  -  MCSIDB        COOR. SYS. ID. FOR GRID POINT B
!     ECPT(39)  -  GPB(3)        $ BASIC COORDINATES FOR GRID POINT B
!     ECPT(40)  -    ...         $
!     ECPT(41)  -    ...         $
!     ECPT(42)  -  ELTEMP        AVG. ELEMENT TEMPERATURE
!
!
!     SDR2 PHASE I INPUT AND OUTPUT COMMON BLOCK
!
!
!
!     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
   !>>>>EQUIVALENCE (Ielid,Ecpt(1),Iecpt(1)) , (ta(10),tb(1))
!
!
!     SET UP POINTERS TO COOR. SYS. IDS., OFFSET VECTORS, AND PIN FLAGS.
!     ICSIDA AND ICSIDB ARE COOR. SYS. IDS.
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
!     NORMALIZE THE REFERENCE VECTOR WHICH LIES IN THE FIRST PRINCIPAL
!     AXIS PLANE  (FMMS - 36 P. 4)
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
!     DETERMINE IF POINT A AND B ARE IN BASIC COORDINATES OR NOT.
!
   abasic = .TRUE.
   bbasic = .TRUE.
   IF ( icsida/=0 ) abasic = .FALSE.
   IF ( icsidb/=0 ) bbasic = .FALSE.
!
!     COMPUTE THE TRANSFORMATION MATRICES TA AND TB IF NECESSARY
!
   IF ( .NOT.abasic ) CALL gbtran(Ecpt(jcsida),Ecpt(jcsida+1),ta)
   IF ( .NOT.bbasic ) CALL gbtran(Ecpt(jcsidb),Ecpt(jcsidb+1),tb)
!
!     DETERMINE IF WE HAVE NON-ZERO OFFSET VECTORS.
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
!     FORM THE CENTER AXIS OF THE BEAM WITHOUT OFFSETS.
!
 200  veci(1) = Ecpt(jcsida+1) - Ecpt(jcsidb+1)
   veci(2) = Ecpt(jcsida+2) - Ecpt(jcsidb+2)
   veci(3) = Ecpt(jcsida+3) - Ecpt(jcsidb+3)
!
!     TRANSFORM THE OFFSET VECTORS IF NECESSARY
!
   IF ( .NOT.(.NOT.aofset .AND. .NOT.bofset) ) THEN
!
!     TRANSFORM THE OFFSET VECTOR FOR POINT A IF NECESSARY.
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
!     TRANSFORM THE OFFSET VECTOR FOR POINT B IF NECESSARY
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
!     SINCE THERE WAS AT LEAST ONE NON-ZERO OFFSET VECTOR RECOMPUTE VECI
!
      veci(1) = veci(1) + Dela(idela) - Delb(idelb)
      veci(2) = veci(2) + Dela(idela+1) - Delb(idelb+1)
      veci(3) = veci(3) + Dela(idela+2) - Delb(idelb+2)
   ENDIF
!
!     COMPUTE THE LENGTH OF THE BIG V (VECI) VECTOR AND NORMALIZE
!
   veci(1) = -veci(1)
   veci(2) = -veci(2)
   veci(3) = -veci(3)
   fl = sqrt(veci(1)**2+veci(2)**2+veci(3)**2)
   DO i = 1 , 3
      veci(i) = veci(i)/fl
   ENDDO
!
!     COMPUTE THE SMALL V SUB 0 VECTOR, SMALV0.  ***CHECK THIS LOGIC***
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
!     COMPUTE THE K VECTOR, VECK = VECI X SMALV0, AND NORMALIZE
!
   veck(1) = veci(2)*smalv0(isv+2) - veci(3)*smalv0(isv+1)
   veck(2) = veci(3)*smalv0(isv) - veci(1)*smalv0(isv+2)
   veck(3) = veci(1)*smalv0(isv+1) - veci(2)*smalv0(isv)
   fll = sqrt(veck(1)**2+veck(2)**2+veck(3)**2)
   veck(1) = veck(1)/fll
   veck(2) = veck(2)/fll
   veck(3) = veck(3)/fll
!
!     COMPUTE THE J VECTOR, VECJ = VECK X VECI, AND NORMALIZE
!
   vecj(1) = veck(2)*veci(3) - veck(3)*veci(2)
   vecj(2) = veck(3)*veci(1) - veck(1)*veci(3)
   vecj(3) = veck(1)*veci(2) - veck(2)*veci(1)
   fll = sqrt(vecj(1)**2+vecj(2)**2+vecj(3)**2)
   vecj(1) = vecj(1)/fll
   vecj(2) = vecj(2)/fll
   vecj(3) = vecj(3)/fll
!
!     CALL MAT TO GET MATERIAL PROPERTIES.
!
   Matidc = Imatid
   Matflg = 1
   Eltemp = Tempel
   CALL mat(Iecpt(1))
!
!     SET UP INTERMEDIATE VARIABLES FOR ELEMENT STIFFNESS MATRIX
!     CALCULATION
!
   l = fl
   lsq = l**2
   lcube = lsq*l
   ei1 = E*I1
   ei2 = E*I2
   IF ( K1==0.0 .OR. I12/=0.0 ) THEN
      r1 = 12.0*ei1/lcube
   ELSE
      gak1 = G*A*K1
      r1 = (12.0*ei1*gak1)/(gak1*lcube+12.0*l*ei1)
   ENDIF
   IF ( K2==0.0 .OR. I12/=0.0 ) THEN
      r2 = 12.0*ei2/lcube
   ELSE
      gak2 = G*A*K2
      r2 = (12.0*ei2*gak2)/(gak2*lcube+12.0*l*ei2)
   ENDIF
!
!     COMPUTE THE -SMALL- K-S, SK1, SK2, SK3, AND SK4
!
   sk1 = 0.25*r1*lsq + ei1/l
   sk2 = 0.25*r2*lsq + ei2/l
   sk3 = 0.25*r1*lsq - ei1/l
   sk4 = 0.25*r2*lsq - ei2/l
!
!     COMPUTE THE TERMS THAT WILL BE NEEDED FOR THE 12 X 12 MATRIX KE
!
   ael = A*E/l
   lr1 = l*r1/2.0
   lr2 = l*r2/2.0
   gjl = G*Fj/l
!
!     CONSTRUCT THE 12 X 12 MATRIX KE
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
      beta = 12.0*E*I12/lcube
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
!     DETERMINE IF THERE ARE NON-ZERO PIN FLAGS.
!
   ka = Iecpt(jpina)
   kb = Iecpt(jpinb)
   IF ( ka/=0 .OR. kb/=0 ) THEN
!
!     SET UP THE IPIN ARRAY
!
      DO i = 1 , 5
         ipin(i) = mod(ka,10)
         ipin(i+5) = mod(kb,10) + 6
         IF ( ipin(i+5)==6 ) ipin(i+5) = 0
         ka = ka/10
         kb = kb/10
      ENDDO
!
!     ALTER KE MATRIX DUE TO PIN FLAGS.
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
!            E
!     STORE K   IN KEP(1),...,KEP(36) AND
!            AA
!
!            E
!     STORE K   IN KEP(37),...,KEP(72)
!            AB
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
!                                                            T
!     STORE VECI, VECJ, VECK IN KE(1),...,KE(9) FORMING THE A  MATRIX.
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
!     SET POINTERS SO THAT WE WILL BE WORKING WITH POINT A.
!
   basic = abasic
   jcsid = jcsida
   offset = aofset
   jofset = jofsta
   iwbeg = 0
   ikel = 1
   isasb = 73
   index = Isilno(1)
!
!     ZERO OUT THE ARRAY WHERE THE 3 X 3 MATRIX AND THE W  AND W  6 X 6
!     MATRICES WILL RESIDE.                              A      B
!
   DO i = 28 , 108
      Ke(i) = 0.0
   ENDDO
   DO
!
!     SET UP THE -G- MATRIX.  IG POINTS TO THE BEGINNING OF THE G MATRIX
!     G = AT X TI
!
      ig = 1
      IF ( .NOT.(basic) ) THEN
         CALL gbtran(Ecpt(jcsid),Ecpt(jcsid+1),Ke(10))
         CALL gmmats(Ke(1),3,3,0,Ke(10),3,3,0,Ke(19))
         ig = 19
      ENDIF
!
!     IF THERE IS A NON-ZERO OFFSET FOR THE POINT, SET UP THE D 3 X 3
!     MATRIX.
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
!     FORM THE 3 X 3 PRODUCT H = G X D, I.E., KE(28) = KE(IG) X KE(10)
!
         CALL gmmats(Ke(ig),3,3,0,Ke(10),3,3,0,Ke(28))
      ENDIF
!
!     FORM THE W  MATRIX OR THE W  MATRIX IN KE(37) OR KE(73) DEPENDING
!               A                B
!     UPON WHICH POINT - A OR B - IS UNDER CONSIDERATION.  G WILL BE
!     STORED IN THE UPPER LEFT AND LOWER RIGHT CORNERS.  H, IF NON-ZERO,
!     WILL BE STORED IN THE UPPER RIGHT CORNER.
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
!                              E                    E
!     FORM THE PRODUCT  S  =  K   X  W   OR  S  = K   X  W  , DEPENDING
!                        A     AA     A       B    AB     B
!
!     UPON WHICH POINT WE ARE WORKING WITH.
!
      CALL gmmats(Kep(ikel),6,6,0,Ke(iwbeg+37),6,6,0,Kep(isasb))
!
!     IF THE POINT UNDER CONSIDERATION IS POINT B WE ARE FINISHED.  IF
!     NOT, SET UP POINTS AND INDICATORS FOR WORKING WITH POINT B.
!
      IF ( iwbeg==36 ) THEN
!
!     NOW PERFORM THE ELEMENT TEMPERATURE AND DEFORMATION LOADING.
!
         tbar = Tsub0
         IF ( Nogptt/=0 ) THEN
            CALL ssgetd(Ecpt(1),Ke(1),0)
            tbar = (Ke(1)+Ke(2))/2.0
         ENDIF
         delta = 0.0
         IF ( Noedt==0 ) THEN
            delta = 0.0
         ELSE
            Ke(3) = 0.0
            Ke(4) = 0.0
            Ke(5) = 0.0
            Ke(6) = 0.0
            CALL fedt(Ecpt(1),delta,Idefm)
         ENDIF
!
!     ELEMENT TEMPERATURE DATA BEGINS AT KE(1)
!     ELEMENT DEFORMATION DATA = DELTA
!
!     S  BEGINS AT KEP(73)             (6 X 6)
!      A
!
!     S  BEGINS AT KEP(109)            (6 X 6)
!      B
!
!     NOW FILL THE U  MATRIX           (6 X 1)
!                   A
!
         alphal = Alpha*l
!
         ua(1) = -alphal*(tbar-Tsub0) - delta
         ua(2) = -alphal*l*(Ke(3)+2.0*Ke(4))/6.0
         ua(3) = -alphal*l*(Ke(5)+2.0*Ke(6))/6.0
         ua(4) = 0.0
         ua(5) = -alphal*(Ke(5)+Ke(6))/2.0
         ua(6) = alphal*(Ke(3)+Ke(4))/2.0
!
!     COMPUTE P  AND P  AND STORE THEM INTO Z (OPEN CORE)
!              A      B
!
         DO i = 1 , 2
            CALL gmmats(Kep(36*i+37),6,6,1,ua(1),6,1,0,Ke(1))
            k = Iecpt(i+1) - 1
            DO j = 1 , 6
               k = k + 1
               Z(k) = Z(k) + Ke(j)
            ENDDO
         ENDDO
         EXIT
      ELSE
         basic = bbasic
         jcsid = jcsidb
         offset = bofset
         jofset = jofstb
         iwbeg = 36
         ikel = 37
         isasb = 109
         index = Isilno(2)
         DO i = 28 , 36
            Ke(i) = 0.0
         ENDDO
      ENDIF
   ENDDO
!
END SUBROUTINE bar