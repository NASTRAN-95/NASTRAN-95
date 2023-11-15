
SUBROUTINE kbar
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A , Alpha , C1 , C2 , Clsnrw , Clsrw , D1 , D2 , Dodet , E , Ecpt(100) , Eltemp , Eor , F1 , F2 , Fe , Fj , Fk , Frowic ,   &
      & G , G1 , G2 , Gpa(3) , Gpb(3) , Gsube , I1 , I12 , I2 , K1 , K2 , Nsm , Outrw , Rho , Sigc , Sigs , Sigt , Smallv(3) ,      &
      & Stress , Tempel , Tnrows , Tsubo , Za(3) , Zb(3)
   DOUBLE PRECISION Dela(6) , Delb(6) , Dp(8) , Ke(144) , Kep(144)
   LOGICAL Heat
   INTEGER I6x64 , I6x6k , Icssv , Icstm , Idetck , Idum1 , Iecpt(100) , Ielid , If4gg , Ifcstm , Ifdit , Ifecpt , Ifgei , Ifgpct , &
         & Ifgpst , Ifkgg , Ifmpt , Ig4gg , Igecpt , Iggei , Iggpct , Iggpst , Igkgg , Igpct , Imatid , Inrw , Iopt4 , Ipinfl(2) ,  &
         & Ipoint , Isilno(2) , Isys , Jmax , K4ggsw , Left , Link(10) , Lrowic , Matflg , Matidc , Mcb4gg(7) , Mcbkgg(7) , Mcsida ,&
         & Mcsidb , N6x64 , N6x6k , Ncstm , Neor , Ngpct , Nlinks , Nogo , Npoint , Npvt , Nrowsc , Nu
   COMMON /hmtout/ Fk
   COMMON /matin / Matidc , Matflg , Eltemp , Stress
   COMMON /matout/ E , G , Nu , Rho , Alpha , Tsubo , Gsube , Sigt , Sigc , Sigs
   COMMON /sma1bk/ Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6k , N6x6k , I6x64 , N6x64
   COMMON /sma1cl/ Iopt4 , K4ggsw , Npvt , Left , Frowic , Lrowic , Nrowsc , Tnrows , Jmax , Nlinks , Link , Idetck , Dodet , Nogo
   COMMON /sma1dp/ Ke , Kep , Dela , Delb
   COMMON /sma1et/ Ielid , Isilno , Smallv , Icssv , Ipinfl , Za , Zb , Imatid , A , I1 , I2 , Fj , Nsm , Fe , C1 , C2 , D1 , D2 ,  &
                 & F1 , F2 , G1 , G2 , K1 , K2 , I12 , Mcsida , Gpa , Mcsidb , Gpb , Tempel
   COMMON /sma1ht/ Heat
   COMMON /sma1io/ Ifcstm , Ifmpt , Ifdit , Idum1 , Ifecpt , Igecpt , Ifgpct , Iggpct , Ifgei , Iggei , Ifkgg , Igkgg , If4gg ,     &
                 & Ig4gg , Ifgpst , Iggpst , Inrw , Outrw , Clsnrw , Clsrw , Neor , Eor , Mcbkgg , Mcb4gg
   COMMON /system/ Isys
!
! Local variable declarations
!
   LOGICAL abasic , aofset , basic , bbasic , bofset , offset
   DOUBLE PRECISION ael , beta , dampc , ei1 , ei2 , fl , fll , gak1 , gak2 , gjl , l , l2b3 , l2b6 , lb , lcube , lr1 , lr2 , lsq ,&
                  & r1 , r2 , sk1 , sk2 , sk3 , sk4 , smalv0(6) , ta(18) , tb(9) , veci(3) , vecj(3) , veck(3)
   INTEGER i , icsida , icsidb , idela , idelb , ig , ii , ij , ikel , il , ilim , ill , ilow , index , ipass , ipin(10) , ipvt ,   &
         & isv , iwbeg , j , jcsid , jcsida , jcsidb , ji , jll , jofset , jofsta , jofstb , jpina , jpinb , k , ka , kb , lim ,    &
         & ll , low
!
! End of declarations
!
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
   EQUIVALENCE (Ielid,Ecpt(1),Iecpt(1)) , (ta(10),tb(1)) , (Ecpt(71),Dp(1))
!
!     DETERMINE WHICH POINT IS THE PIVOT POINT.
!
   ipvt = 1
   IF ( Isilno(1)/=Npvt ) THEN
      ipvt = 2
      IF ( Isilno(2)/=Npvt ) CALL mesage(-30,34,Iecpt(1))
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
   icsida = Iecpt(34)
   icsidb = Iecpt(38)
!
!     NORMALIZE THE REFERENCE VECTOR WHICH LIES IN THE FIRST PRINCIPAL
!     AXIS PLANE  (FMMS - 36 P. 4)
!     WE STORE SMALLV IN SMALV0 SO THAT ARITHMETIC WILL BE DOUBLE
!     PRECISION
!
   DO i = 1 , 3
      smalv0(i) = Smallv(i)
   ENDDO
   fl = dsqrt(smalv0(1)**2+smalv0(2)**2+smalv0(3)**2)
   IF ( fl<=0.0D0 ) GOTO 600
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
   IF ( .NOT.abasic ) CALL transd(Ecpt(jcsida),ta)
   IF ( .NOT.bbasic ) CALL transd(Ecpt(jcsidb),tb)
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
!     FIRST WE STORE THE COORDINATES IN THE ARRAY DP SO THAT ALL
!     ARITHMETIC WILL BE DOUBLE PRECISION.
!
 200  Dp(1) = Ecpt(jcsida+1)
   Dp(2) = Ecpt(jcsida+2)
   Dp(3) = Ecpt(jcsida+3)
   Dp(4) = Ecpt(jcsidb+1)
   Dp(5) = Ecpt(jcsidb+2)
   Dp(6) = Ecpt(jcsidb+3)
   veci(1) = Dp(1) - Dp(4)
   veci(2) = Dp(2) - Dp(5)
   veci(3) = Dp(3) - Dp(6)
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
         CALL gmmatd(ta,3,3,0,Dela(1),3,1,0,Dela(4))
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
         CALL gmmatd(tb,3,3,0,Delb(1),3,1,0,Delb(4))
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
   fl = dsqrt(veci(1)**2+veci(2)**2+veci(3)**2)
   IF ( fl==0.0D0 ) GOTO 600
!
!     NOW THAT LENGTH HAS BEEN COMPUTED, BRANCH IF THIS IS A -HEAT-
!     FORMULATION.
!
   IF ( Heat ) THEN
!
!     HEAT FORMULATION CONTINUES HERE.  GET MATERIAL PROPERTY -K- FROM
!     HMAT
!
      Matflg = 1
      Matidc = Iecpt(16)
      Eltemp = Ecpt(42)
      CALL hmat(Ielid)
!
      fl = dble(Fk)*dble(Ecpt(17))/fl
      IF ( Npvt==Iecpt(3) ) fl = -fl
      DO i = 1 , 2
         CALL sma1b(fl,Iecpt(i+1),Npvt,Ifkgg,0.0D0)
         fl = -fl
      ENDDO
      GOTO 99999
   ELSE
      DO i = 1 , 3
         veci(i) = veci(i)/fl
      ENDDO
!
!     COMPUTE THE SMALL V SUB 0 VECTOR, SMALV0.  ***CHECK THIS LOGIC***
!
      isv = 1
      IF ( Icssv/=0 ) THEN
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
      IF ( fll==0.0D0 ) GOTO 600
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
      IF ( fll==0.0D0 ) GOTO 600
      vecj(1) = vecj(1)/fll
      vecj(2) = vecj(2)/fll
      vecj(3) = vecj(3)/fll
!
!     SEARCH THE MATERIAL PROPERTIES TABLE FOR E, G AND THE DAMPING
!     CONSTANT.
!
      Matidc = Imatid
      Matflg = 1
      Eltemp = Tempel
      CALL mat(Iecpt(1))
      dampc = Gsube
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
      Dp(1) = E
      Dp(2) = G
      Dp(3) = I1
      Dp(4) = I2
      Dp(5) = A
      ei1 = Dp(1)*Dp(3)
      ei2 = Dp(1)*Dp(4)
      IF ( K1==0.0 .OR. I12/=0.0 ) THEN
         r1 = 12.0D0*ei1/lcube
      ELSE
         Dp(6) = K1
         gak1 = Dp(2)*Dp(5)*Dp(6)
         r1 = (12.0D0*ei1*gak1)/(gak1*lcube+12.0D0*l*ei1)
      ENDIF
      IF ( K2==0.0 .OR. I12/=0.0 ) THEN
         r2 = 12.0D0*ei2/lcube
      ELSE
         Dp(7) = K2
         gak2 = Dp(2)*Dp(5)*Dp(7)
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
      ael = Dp(5)*Dp(1)/l
      lr1 = l*r1/2.0D0
      lr2 = l*r2/2.0D0
      Dp(8) = Fj
      gjl = Dp(2)*Dp(8)/l
!
!     CONSTRUCT THE 12 X 12 MATRIX KE
!
      DO i = 1 , 144
         Ke(i) = 0.0D0
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
         Dp(8) = I12
         beta = 12.0D0*Dp(1)*Dp(8)/lcube
         lb = l*beta/2.0D0
         l2b3 = lsq*beta/3.0D0
         l2b6 = lsq*beta/6.0D0
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
               IF ( Ke(ii)/=0.0D0 ) THEN
                  DO j = 1 , 12
                     ji = 12*(j-1) + ipin(i)
                     ij = 12*(ipin(i)-1) + j
                     DO ll = 1 , 12
                        jll = 12*(j-1) + ll
                        ill = 12*(ipin(i)-1) + ll
                        Kep(jll) = Ke(jll) - (Ke(ill)/Ke(ii))*Ke(ji)
                     ENDDO
                     Kep(ij) = 0.0D0
                     Kep(ji) = 0.0D0
                  ENDDO
                  DO k = 1 , 144
                     Ke(k) = Kep(k)
                  ENDDO
               ELSE
                  il = ipin(i)
                  ii = ii - il
                  DO j = 1 , 12
                     ii = ii + 1
                     Ke(ii) = 0.0D0
                     Ke(il) = 0.0D0
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
            Kep(j) = Ke(k)
            Kep(j+36) = Ke(k+6)
         ENDDO
      ENDDO
!
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
!     ZERO OUT THE ARRAY WHERE THE 3X3 MATRIX H AND THE W  AND W  6X6
!     MATRICES WILL RESIDE.                              A      B
!
      DO i = 28 , 108
         Ke(i) = 0.0D0
      ENDDO
      ipass = 1
      iwbeg = 0
!
!     SET UP POINTERS
!
      IF ( ipvt/=1 ) GOTO 400
   ENDIF
 300  basic = abasic
   jcsid = jcsida
   offset = aofset
   jofset = jofsta
   ikel = 1
   index = Isilno(1)
   GOTO 500
 400  basic = bbasic
   jcsid = jcsidb
   offset = bofset
   jofset = jofstb
   ikel = 37
   index = Isilno(2)
!
!     SET UP THE -G- MATRIX. IG POINTS TO THE BEGINNING OF THE G MATRIX.
!     G = AT X TI
!
 500  ig = 1
   IF ( .NOT.(basic) ) THEN
      CALL transd(Ecpt(jcsid),Ke(10))
      CALL gmmatd(Ke(1),3,3,0,Ke(10),3,3,0,Ke(19))
      ig = 19
   ENDIF
!
!     IF THERE IS A NON-ZERO OFFSET FOR THE POINT, SET UP THE D 3X3
!     MATRIX.
!
   IF ( offset ) THEN
      Ke(10) = 0.0D0
      Ke(11) = Ecpt(jofset+2)
      Ke(12) = -Ecpt(jofset+1)
      Ke(13) = -Ke(11)
      Ke(14) = 0.0D0
      Ke(15) = Ecpt(jofset)
      Ke(16) = -Ke(12)
      Ke(17) = -Ke(15)
      Ke(18) = 0.0D0
!
!     FORM THE 3X3 PRODUCT H = G X D, I.E., KE(28) = KE(IG) X KE(10)
!
      CALL gmmatd(Ke(ig),3,3,0,Ke(10),3,3,0,Ke(28))
   ENDIF
!
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
!
!                       T      E
!     FORM THE PRODUCT W   X  K   AND STORE IN KEP(73)
!                       NPVT
!
   CALL gmmatd(Ke(37),6,6,1,Kep(ikel),6,6,0,Kep(73))
!
!     COMPUTE THE FINAL ANSWER AND STORE IN KEP(109)
!
   CALL gmmatd(Kep(73),6,6,0,Ke(iwbeg+37),6,6,0,Kep(109))
!
!     INSERT THIS 6X6
!
   CALL sma1b(Kep(109),index,-1,Ifkgg,0.0D0)
   IF ( Iopt4/=0 .AND. Gsube/=0.0 ) THEN
      K4ggsw = 1
      CALL sma1b(Kep(109),index,-1,If4gg,dampc)
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
         Ke(i) = 0.0D0
      ENDDO
      IF ( ipvt==1 ) GOTO 400
      GOTO 300
   ENDIF
!
 600  CALL mesage(30,26,Iecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
   Nogo = 1
   RETURN
99999 END SUBROUTINE kbar
