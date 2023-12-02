!*==kbar.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE kbar
   USE c_hmtout
   USE c_matin
   USE c_matout
   USE c_sma1bk
   USE c_sma1cl
   USE c_sma1dp
   USE c_sma1et
   USE c_sma1ht
   USE c_sma1io
   USE c_system
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: abasic , aofset , basic , bbasic , bofset , offset
   REAL(REAL64) :: ael , beta , dampc , ei1 , ei2 , fl , fll , gak1 , gak2 , gjl , l , l2b3 , l2b6 , lb , lcube , lr1 , lr2 , lsq , &
                 & r1 , r2 , sk1 , sk2 , sk3 , sk4
   REAL(REAL64) , DIMENSION(8) :: dp
   REAL , DIMENSION(100) :: ecpt
   INTEGER :: i , icsida , icsidb , idela , idelb , ig , ii , ij , ikel , il , ilim , ill , ilow , index , ipass , ipvt , isv ,     &
            & iwbeg , j , jcsid , jcsida , jcsidb , ji , jll , jofset , jofsta , jofstb , jpina , jpinb , k , ka , kb , lim , ll ,  &
            & low
   INTEGER , DIMENSION(100) :: iecpt
   INTEGER , DIMENSION(10) :: ipin
   REAL(REAL64) , DIMENSION(6) :: smalv0
   REAL(REAL64) , DIMENSION(18) :: ta
   REAL(REAL64) , DIMENSION(9) :: tb
   REAL(REAL64) , DIMENSION(3) :: veci , vecj , veck
   EXTERNAL gmmatd , hmat , mat , mesage , sma1b , transd
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     THIS ROUTINE COMPUTES THE TWO 6X6 MATRICES K(NPVT,NPVT) AND
!     K(NPVT,J) FOR A BAR ELEMENT HAVING END POINTS NUMBERED NPVT AND J.
!
!                  ECPT FOR THE BAR
!
!     ECPT( 1)  -  IELID       ELEMENT ID. NUMBER
!     ECPT( 2)  -  ISILNO(2)   * SCALAR INDEX NOS. OF THE GRID POINTS
!     ECPT( 3)  -   ...        *
!     ECPT( 4)  -  SMALLV(3)   $ REFERENCE VECTOR
!     ECPT( 5)  -   ...        $
!     ECPT( 6)  -   ...        $
!     ECPT( 7)  -  ICSSV       COOR. SYS. ID FOR SMALLV VECTOR
!     ECPT( 8)  -  IPINFL(2)   * PIN FLAGS
!     ECPT( 9)  -   ...        *
!     ECPT(10)  -  ZA(3)       $ OFFSET VECTOR FOR POINT A
!     ECPT(11)  -   ...        $
!     ECPT(12)  -   ...        $
!     ECPT(13)  -  ZB(3)       * OFFSET VECTOR FOR POINT B
!     ECPT(14)  -   ...        *
!     ECPT(15)  -   ...        *
!     ECPT(16)  -  IMATID      MATERIAL ID.
!     ECPT(17)  -  A           CROSS-SECTIONAL AREA
!     ECPT(18)  -  I1          $ AREA MOMENTS OF INERTIA
!     ECPT(19)  -  I2          $
!     ECPT(20)  -  FJ          TORSIONAL CONSTANT
!     ECPT(21)  -  NSM         NON-STRUCTURAL MASS
!     ECPT(22)  -  FE          FORCE ELEMENT DESCRIPTIONS (FORCE METHOD)
!     ECPT(23)  -  C1          * STRESS RECOVERY COEFFICIENTS
!     ECPT(24)  -  C2          *
!     ECPT(25)  -  D1          *
!     ECPT(26)  -  D2          *
!     ECPT(27)  -  F1          *
!     ECPT(28)  -  F2          *
!     ECPT(29)  -  G1          *
!     ECPT(30)  -  G2          *
!     ECPT(31)  -  K1          $ AREA FACTORS FOR SHEAR
!     ECPT(32)  -  K2          $
!     ECPT(33)  -  I12         AREA MOMENT OF INERTIA
!     ECPT(34)  -  MCSIDA      COOR. SYS. ID. FOR GRID POINT A
!     ECPT(35)  -  GPA(3)      * BASIC COORDINATES FOR GRID POINT A
!     ECPT(36)  -   ...        *
!     ECPT(37)  -   ...        *
!     ECPT(38)  -  MCSIDB      COOR. SYS. ID. FOR GRID POINT B
!     ECPT(39)  -  GPB(3)      $ BASIC COORDINATES FOR GRID POINT B
!     ECPT(40)  -   ...        $
!     ECPT(41)  -   ...        $
!     ECPT(42)  -  ELTEMP      AVG. ELEMENT TEMPERATURE
!
!     SEE SUBROUTINE KELBOW ON THE DISCUSSION OF K1 AND K2, THE AERA
!     FACTORS FOR SHEAR CORRECTION
!
!
!     SMA1 VARIABLE CORE BOOKKEEPING PARAMETERS
!
!
!     SMA1 PROGRAM CONTROL PARAMETERS
!
!
!     SMA1 LOCAL VARIABLES
!
!
!     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
!
!
   !>>>>EQUIVALENCE (Ielid,Ecpt(1),Iecpt(1)) , (ta(10),tb(1)) , (Ecpt(71),Dp(1))
!
!     DETERMINE WHICH POINT IS THE PIVOT POINT.
!
         ipvt = 1
         IF ( isilno(1)/=npvt ) THEN
            ipvt = 2
            IF ( isilno(2)/=npvt ) CALL mesage(-30,34,iecpt(1))
         ENDIF
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
         icsida = iecpt(34)
         icsidb = iecpt(38)
!
!     NORMALIZE THE REFERENCE VECTOR WHICH LIES IN THE FIRST PRINCIPAL
!     AXIS PLANE  (FMMS - 36 P. 4)
!     WE STORE SMALLV IN SMALV0 SO THAT ARITHMETIC WILL BE DOUBLE
!     PRECISION
!
         DO i = 1 , 3
            smalv0(i) = smallv(i)
         ENDDO
         fl = dsqrt(smalv0(1)**2+smalv0(2)**2+smalv0(3)**2)
         IF ( fl<=0.0D0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = 1 , 3
            smalv0(i) = smalv0(i)/fl
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
         IF ( .NOT.abasic ) CALL transd(ecpt(jcsida),ta)
         IF ( .NOT.bbasic ) CALL transd(ecpt(jcsidb),tb)
!
!     DETERMINE IF WE HAVE NON-ZERO OFFSET VECTORS.
!
         aofset = .TRUE.
         j = jofsta - 1
         DO i = 1 , 3
            j = j + 1
            IF ( ecpt(j)/=0.0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         aofset = .FALSE.
         spag_nextblock_1 = 2
      CASE (2)
         bofset = .TRUE.
         j = jofstb - 1
         DO i = 1 , 3
            j = j + 1
            IF ( ecpt(j)/=0.0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         bofset = .FALSE.
         spag_nextblock_1 = 3
      CASE (3)
!
!     FORM THE CENTER AXIS OF THE BEAM WITHOUT OFFSETS.
!     FIRST WE STORE THE COORDINATES IN THE ARRAY DP SO THAT ALL
!     ARITHMETIC WILL BE DOUBLE PRECISION.
!
         dp(1) = ecpt(jcsida+1)
         dp(2) = ecpt(jcsida+2)
         dp(3) = ecpt(jcsida+3)
         dp(4) = ecpt(jcsidb+1)
         dp(5) = ecpt(jcsidb+2)
         dp(6) = ecpt(jcsidb+3)
         veci(1) = dp(1) - dp(4)
         veci(2) = dp(2) - dp(5)
         veci(3) = dp(3) - dp(6)
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
               dela(i) = ecpt(j)
            ENDDO
            IF ( .NOT.(abasic) ) THEN
               idela = 4
               CALL gmmatd(ta,3,3,0,dela(1),3,1,0,dela(4))
            ENDIF
!
!     TRANSFORM THE OFFSET VECTOR FOR POINT B IF NECESSARY
!
            idelb = 1
            j = jofstb - 1
            DO i = 1 , 3
               j = j + 1
               delb(i) = ecpt(j)
            ENDDO
            IF ( .NOT.(bbasic) ) THEN
               idelb = 4
               CALL gmmatd(tb,3,3,0,delb(1),3,1,0,delb(4))
            ENDIF
!
!     SINCE THERE WAS AT LEAST ONE NON-ZERO OFFSET VECTOR RECOMPUTE VECI
!
            veci(1) = veci(1) + dela(idela) - delb(idelb)
            veci(2) = veci(2) + dela(idela+1) - delb(idelb+1)
            veci(3) = veci(3) + dela(idela+2) - delb(idelb+2)
         ENDIF
!
!     COMPUTE THE LENGTH OF THE BIG V (VECI) VECTOR AND NORMALIZE
!
         veci(1) = -veci(1)
         veci(2) = -veci(2)
         veci(3) = -veci(3)
         fl = dsqrt(veci(1)**2+veci(2)**2+veci(3)**2)
         IF ( fl==0.0D0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     NOW THAT LENGTH HAS BEEN COMPUTED, BRANCH IF THIS IS A -HEAT-
!     FORMULATION.
!
         IF ( heat ) THEN
!
!     HEAT FORMULATION CONTINUES HERE.  GET MATERIAL PROPERTY -K- FROM
!     HMAT
!
            matflg = 1
            matidc = iecpt(16)
            eltemp = ecpt(42)
            CALL hmat(ielid)
!
            fl = dble(fk)*dble(ecpt(17))/fl
            IF ( npvt==iecpt(3) ) fl = -fl
            DO i = 1 , 2
               CALL sma1b(fl,iecpt(i+1),npvt,ifkgg,0.0D0)
               fl = -fl
            ENDDO
            RETURN
         ELSE
            DO i = 1 , 3
               veci(i) = veci(i)/fl
            ENDDO
!
!     COMPUTE THE SMALL V SUB 0 VECTOR, SMALV0.  ***CHECK THIS LOGIC***
!
            isv = 1
            IF ( icssv/=0 ) THEN
               isv = 4
               CALL gmmatd(ta,3,3,0,smalv0(1),3,1,0,smalv0(4))
            ENDIF
!
!     COMPUTE THE K VECTOR, VECK = VECI  X  SMALV0, AND NORMALIZE
!
            veck(1) = veci(2)*smalv0(isv+2) - veci(3)*smalv0(isv+1)
            veck(2) = veci(3)*smalv0(isv) - veci(1)*smalv0(isv+2)
            veck(3) = veci(1)*smalv0(isv+1) - veci(2)*smalv0(isv)
            fll = dsqrt(veck(1)**2+veck(2)**2+veck(3)**2)
            IF ( fll==0.0D0 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            veck(1) = veck(1)/fll
            veck(2) = veck(2)/fll
            veck(3) = veck(3)/fll
!
!     COMPUTE THE J VECTOR, VECJ = VECK  X  VECI, AND NORMALIZE
!
            vecj(1) = veck(2)*veci(3) - veck(3)*veci(2)
            vecj(2) = veck(3)*veci(1) - veck(1)*veci(3)
            vecj(3) = veck(1)*veci(2) - veck(2)*veci(1)
            fll = dsqrt(vecj(1)**2+vecj(2)**2+vecj(3)**2)
            IF ( fll==0.0D0 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            vecj(1) = vecj(1)/fll
            vecj(2) = vecj(2)/fll
            vecj(3) = vecj(3)/fll
!
!     SEARCH THE MATERIAL PROPERTIES TABLE FOR E, G AND THE DAMPING
!     CONSTANT.
!
            matidc = imatid
            matflg = 1
            eltemp = tempel
            CALL mat(iecpt(1))
            dampc = gsube
!
!     SET UP INTERMEDIATE VARIABLES FOR ELEMENT STIFFNESS MATRIX
!     CALCULATION
!
            l = fl
            lsq = l**2
            lcube = lsq*l
!
!     STORE ECPT AND MPT VARIABLES IN DOUBLE PRECISION LOCATIONS.
!
            dp(1) = e
            dp(2) = g
            dp(3) = i1
            dp(4) = i2
            dp(5) = a
            ei1 = dp(1)*dp(3)
            ei2 = dp(1)*dp(4)
            IF ( k1==0.0 .OR. i12/=0.0 ) THEN
               r1 = 12.0D0*ei1/lcube
            ELSE
               dp(6) = k1
               gak1 = dp(2)*dp(5)*dp(6)
               r1 = (12.0D0*ei1*gak1)/(gak1*lcube+12.0D0*l*ei1)
            ENDIF
            IF ( k2==0.0 .OR. i12/=0.0 ) THEN
               r2 = 12.0D0*ei2/lcube
            ELSE
               dp(7) = k2
               gak2 = dp(2)*dp(5)*dp(7)
               r2 = (12.0D0*ei2*gak2)/(gak2*lcube+12.0D0*l*ei2)
            ENDIF
!
!     COMPUTE THE -SMALL- K-S, SK1, SK2, SK3 AND SK4
!
            sk1 = 0.25D0*r1*lsq + ei1/l
            sk2 = 0.25D0*r2*lsq + ei2/l
            sk3 = 0.25D0*r1*lsq - ei1/l
            sk4 = 0.25D0*r2*lsq - ei2/l
!
!     COMPUTE THE TERMS THAT WILL BE NEEDED FOR THE 12 X 12 MATRIX KE
!
            ael = dp(5)*dp(1)/l
            lr1 = l*r1/2.0D0
            lr2 = l*r2/2.0D0
            dp(8) = fj
            gjl = dp(2)*dp(8)/l
!
!     CONSTRUCT THE 12 X 12 MATRIX KE
!
            DO i = 1 , 144
               ke(i) = 0.0D0
            ENDDO
            ke(1) = ael
            ke(7) = -ael
            ke(14) = r1
            ke(18) = lr1
            ke(20) = -r1
            ke(24) = lr1
            ke(27) = r2
            ke(29) = -lr2
            ke(33) = -r2
            ke(35) = -lr2
            ke(40) = gjl
            ke(46) = -gjl
            ke(51) = -lr2
            ke(53) = sk2
            ke(57) = lr2
            ke(59) = sk4
            ke(62) = lr1
            ke(66) = sk1
            ke(68) = -lr1
            ke(72) = sk3
            ke(73) = -ael
            ke(79) = ael
            ke(86) = -r1
            ke(90) = -lr1
            ke(92) = r1
            ke(96) = -lr1
            ke(99) = -r2
            ke(101) = lr2
            ke(105) = r2
            ke(107) = lr2
            ke(112) = -gjl
            ke(118) = gjl
            ke(123) = -lr2
            ke(125) = sk4
            ke(129) = lr2
            ke(131) = sk2
            ke(134) = lr1
            ke(138) = sk3
            ke(140) = -lr1
            ke(144) = sk1
            IF ( i12/=0.0 ) THEN
               dp(8) = i12
               beta = 12.0D0*dp(1)*dp(8)/lcube
               lb = l*beta/2.0D0
               l2b3 = lsq*beta/3.0D0
               l2b6 = lsq*beta/6.0D0
               ke(15) = beta
               ke(17) = -lb
               ke(21) = -beta
               ke(23) = -lb
               ke(26) = beta
               ke(30) = lb
               ke(32) = -beta
               ke(36) = lb
               ke(50) = -lb
               ke(54) = -l2b3
               ke(56) = lb
               ke(60) = -l2b6
               ke(63) = lb
               ke(65) = -l2b3
               ke(69) = -lb
               ke(71) = -l2b6
               ke(87) = -beta
               ke(89) = lb
               ke(93) = beta
               ke(95) = lb
               ke(98) = -beta
               ke(102) = -lb
               ke(104) = beta
               ke(108) = -lb
               ke(122) = -lb
               ke(126) = -l2b6
               ke(128) = lb
               ke(132) = -l2b3
               ke(135) = lb
               ke(137) = -l2b6
               ke(141) = -lb
               ke(143) = -l2b3
            ENDIF
!
!     DETERMINE IF THERE ARE NON-ZERO PIN FLAGS.
!
            ka = iecpt(jpina)
            kb = iecpt(jpinb)
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
                     IF ( ke(ii)/=0.0D0 ) THEN
                        DO j = 1 , 12
                           ji = 12*(j-1) + ipin(i)
                           ij = 12*(ipin(i)-1) + j
                           DO ll = 1 , 12
                              jll = 12*(j-1) + ll
                              ill = 12*(ipin(i)-1) + ll
                              kep(jll) = ke(jll) - (ke(ill)/ke(ii))*ke(ji)
                           ENDDO
                           kep(ij) = 0.0D0
                           kep(ji) = 0.0D0
                        ENDDO
                        DO k = 1 , 144
                           ke(k) = kep(k)
                        ENDDO
                     ELSE
                        il = ipin(i)
                        ii = ii - il
                        DO j = 1 , 12
                           ii = ii + 1
                           ke(ii) = 0.0D0
                           ke(il) = 0.0D0
                           il = il + 12
                        ENDDO
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
!
!            E
!     STORE K       AT KEP(1),...,KEP(36)   AND
!            NPVT,A
!
!            E
!           K        AT KEP(37),...,KEP(72)
!            NPVT,B
!
            j = 0
            IF ( ipvt==2 ) THEN
               ilow = 73
               ilim = 144
            ELSE
               ilow = 1
               ilim = 72
            ENDIF
            DO i = ilow , ilim , 12
               low = i
               lim = low + 5
               DO k = low , lim
                  j = j + 1
                  kep(j) = ke(k)
                  kep(j+36) = ke(k+6)
               ENDDO
            ENDDO
!
!                                                            T
!     STORE VECI, VECJ, VECK IN KE(1),...,KE(9) FORMING THE A  MATRIX.
!
            ke(1) = veci(1)
            ke(2) = veci(2)
            ke(3) = veci(3)
            ke(4) = vecj(1)
            ke(5) = vecj(2)
            ke(6) = vecj(3)
            ke(7) = veck(1)
            ke(8) = veck(2)
            ke(9) = veck(3)
!
!     ZERO OUT THE ARRAY WHERE THE 3X3 MATRIX H AND THE W  AND W  6X6
!     MATRICES WILL RESIDE.                              A      B
!
            DO i = 28 , 108
               ke(i) = 0.0D0
            ENDDO
            ipass = 1
            iwbeg = 0
!
!     SET UP POINTERS
!
            IF ( ipvt/=1 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         basic = abasic
         jcsid = jcsida
         offset = aofset
         jofset = jofsta
         ikel = 1
         index = isilno(1)
         spag_nextblock_1 = 6
      CASE (5)
         basic = bbasic
         jcsid = jcsidb
         offset = bofset
         jofset = jofstb
         ikel = 37
         index = isilno(2)
         spag_nextblock_1 = 6
      CASE (6)
!
!     SET UP THE -G- MATRIX. IG POINTS TO THE BEGINNING OF THE G MATRIX.
!     G = AT X TI
!
         ig = 1
         IF ( .NOT.(basic) ) THEN
            CALL transd(ecpt(jcsid),ke(10))
            CALL gmmatd(ke(1),3,3,0,ke(10),3,3,0,ke(19))
            ig = 19
         ENDIF
!
!     IF THERE IS A NON-ZERO OFFSET FOR THE POINT, SET UP THE D 3X3
!     MATRIX.
!
         IF ( offset ) THEN
            ke(10) = 0.0D0
            ke(11) = ecpt(jofset+2)
            ke(12) = -ecpt(jofset+1)
            ke(13) = -ke(11)
            ke(14) = 0.0D0
            ke(15) = ecpt(jofset)
            ke(16) = -ke(12)
            ke(17) = -ke(15)
            ke(18) = 0.0D0
!
!     FORM THE 3X3 PRODUCT H = G X D, I.E., KE(28) = KE(IG) X KE(10)
!
            CALL gmmatd(ke(ig),3,3,0,ke(10),3,3,0,ke(28))
         ENDIF
!
!
!     FORM THE W  MATRIX OR THE W  MATRIX IN KE(37) OR KE(73) DEPENDING
!               A                B
!     UPON WHICH POINT - A OR B - IS UNDER CONSIDERATION.  G WILL BE
!     STORED IN THE UPPER LEFT AND LOWER RIGHT CORNERS.  H, IF NON-ZERO,
!     WILL BE STORED IN THE UPPER RIGHT CORNER.
!
         ke(iwbeg+37) = ke(ig)
         ke(iwbeg+38) = ke(ig+1)
         ke(iwbeg+39) = ke(ig+2)
         ke(iwbeg+43) = ke(ig+3)
         ke(iwbeg+44) = ke(ig+4)
         ke(iwbeg+45) = ke(ig+5)
         ke(iwbeg+49) = ke(ig+6)
         ke(iwbeg+50) = ke(ig+7)
         ke(iwbeg+51) = ke(ig+8)
         ke(iwbeg+58) = ke(ig)
         ke(iwbeg+59) = ke(ig+1)
         ke(iwbeg+60) = ke(ig+2)
         ke(iwbeg+64) = ke(ig+3)
         ke(iwbeg+65) = ke(ig+4)
         ke(iwbeg+66) = ke(ig+5)
         ke(iwbeg+70) = ke(ig+6)
         ke(iwbeg+71) = ke(ig+7)
         ke(iwbeg+72) = ke(ig+8)
         IF ( offset ) THEN
            ke(iwbeg+40) = ke(28)
            ke(iwbeg+41) = ke(29)
            ke(iwbeg+42) = ke(30)
            ke(iwbeg+46) = ke(31)
            ke(iwbeg+47) = ke(32)
            ke(iwbeg+48) = ke(33)
            ke(iwbeg+52) = ke(34)
            ke(iwbeg+53) = ke(35)
            ke(iwbeg+54) = ke(36)
         ENDIF
!
!                       T      E
!     FORM THE PRODUCT W   X  K   AND STORE IN KEP(73)
!                       NPVT
!
         CALL gmmatd(ke(37),6,6,1,kep(ikel),6,6,0,kep(73))
!
!     COMPUTE THE FINAL ANSWER AND STORE IN KEP(109)
!
         CALL gmmatd(kep(73),6,6,0,ke(iwbeg+37),6,6,0,kep(109))
!
!     INSERT THIS 6X6
!
         CALL sma1b(kep(109),index,-1,ifkgg,0.0D0)
         IF ( iopt4/=0 .AND. gsube/=0.0 ) THEN
            k4ggsw = 1
            CALL sma1b(kep(109),index,-1,if4gg,dampc)
         ENDIF
!
!     IF IPASS = 2, WE ARE DONE.  OTHERWISE COMPUTE THE OFF-DIAGONAL 6X6
!
         IF ( ipass==2 ) THEN
            RETURN
         ELSE
            iwbeg = 36
            ipass = 2
            DO i = 28 , 36
               ke(i) = 0.0D0
            ENDDO
            IF ( ipvt/=1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 5
         ENDIF
      CASE (7)
!
         CALL mesage(30,26,iecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
         nogo = 1
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE kbar
