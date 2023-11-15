
SUBROUTINE bars
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A , Alpha , C1 , C2 , Costh , D1 , D2 , Dela(6) , Delb(6) , Dum(12) , E , Ecpt(42) , Elid , Eltemp , F1 , F2 , Fe , Fj ,    &
      & Fk , G , G1 , G2 , Gpa(3) , Gpb(3) , Heat , I1 , I12 , I2 , K1 , K2 , Ke(144) , Kep(144) , Kk(144) , M(12,12) , Me(144) ,   &
      & Mep(144) , Nsm , Rho , Sigc , Sigs , Sigt , Sinth , Smallv(3) , Smalvn(6) , Stress , Ta(18) , Tb(9) , Tempel , Tsubo ,      &
      & Vec(10) , Veci(3) , Vecj(3) , Veck(3) , Za(3) , Zb(3)
   INTEGER Estid , Gsube , Icmbar , Icssv , Idamp , Idumm , Iecpt(38) , Ielid , Iheat , Imass , Imatid , Ioutpt , Ipinfl(2) ,       &
         & Iprec , Isilno(2) , Istif , Ixtra , Jcore , Ksy87 , Ksystm(100) , Lcstm , Ldict , Lhmat , Lmat , Matflg , Matidc ,       &
         & Mcsida , Mcsidb , Ncore , Ngrids , Nu
   LOGICAL Nogo
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /emgdic/ Idumm , Ldict , Ngrids , Elid , Estid
   COMMON /emgest/ Ielid , Isilno , Smallv , Icssv , Ipinfl , Za , Zb , Imatid , A , I1 , I2 , Fj , Nsm , Fe , C1 , C2 , D1 , D2 ,  &
                 & F1 , F2 , G1 , G2 , K1 , K2 , I12 , Mcsida , Gpa , Mcsidb , Gpb , Tempel
   COMMON /emgprm/ Ixtra , Jcore , Ncore , Dum , Istif , Imass , Idamp , Iprec , Nogo , Heat , Icmbar , Lcstm , Lmat , Lhmat
   COMMON /emgtrx/ Ke , Kep , M , Me , Mep , Kk , Smalvn , Ta , Tb , Vec , Dela , Delb
   COMMON /hmtout/ Fk
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , G , Nu , Rho , Alpha , Tsubo , Gsube , Sigt , Sigc , Sigs
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm
!
! Local variable declarations
!
   REAL a2b , ael , beta , bl , bl13 , bl22 , blcube , blsq , blsq3 , blsq4 , const , ei1 , ei2 , epsi , epsi2 , fl , fll , fm ,    &
      & gak1 , gak2 , gjl , l2b3 , l2b6 , lb , limit , lr1 , lr2 , r1 , r2 , sk1 , sk2 , sk3 , sk4
   LOGICAL aofset , basic , bofset , offset
   INTEGER dict(7) , i , iaft , icode , icsida , icsidb , idela , idelb , ifore , ig , ii , ij , ik , ikk(4) , ikx , il , ill ,     &
         & ip5 , ipin(10) , is , is12or(4) , is21or(4) , isv , iwbeg , ix , ix1 , ix2 , j , j1 , j2 , jcsid , jcsida , jcsidb , ji ,&
         & jj , jll , jofset , jofsta , jofstb , jpina , jpinb , jta , jtb , k , ka , kb , korm , l1 , lim , lj , ll , low , mm ,   &
         & ndof , nsq
!
! End of declarations
!
!
!     SINGLE PRECISION VERSION
!
!     THIS SUBROUTINE PROCESSES BAR  ELEMENT DATA TO PRODUCE STIFFNESS
!     AND MASS MATRICES. IF THE HEAT TRANSFER OPTION IS ON, CONDUCTIVITY
!     AND CAPACITY MATRICES ARE PRODUCED.
!
!     THIS ROUTINE WILL PRODUCE MASS MATRICES BY EITHER THE CONSISTENT
!     OR CONVENTIONAL MASS METHODS.
!     THE ECPT/EST ENTRIES FOR THE BAR (ELEMENT TYPE 34) ARE
!
!     ECPT( 1)  -  IELID          ELEMENT ID. NUMBER
!     ECPT( 2)  -  ISILNO(2)      * SCALAR INDEX NOS. OF THE GRID POINTS
!     ECPT( 3)  -    ...          *
!     ECPT( 4)  -  SMALLV(3)      $ REFERENCE VECTOR
!     ECPT( 5)  -    ...          $
!     ECPT( 6)  -    ...          $
!     ECPT( 7)  -  ICSSV          COOR. SYS. ID FOR SMALLV VECTOR
!     ECPT( 8)  -  IPINFL(2)      * PIN FLAGS
!     ECPT( 9)  -    ...          *
!     ECPT(10)  -  ZA(3)          $ OFFSET VECTOR FOR POINT A
!     ECPT(11)  -    ...          $
!     ECPT(12)  -    ...          $
!     ECPT(13)  -  ZB(3)          * OFFSET VECTOR FOR POINT B
!     ECPT(14)  -    ...          *
!     ECPT(15)  -    ...          *
!     ECPT(16)  -  IMATID         MATERIAL ID.
!     ECPT(17)  -  A              CROSS-SECTIONAL AREA
!     ECPT(18)  -  I1             $ AREA MOMENTS OF INERTIA
!     ECPT(19)  -  I2             $
!     ECPT(20)  -  FJ             TORSIONAL CONSTANT
!     ECPT(21)  -  NSM            NON-STRUCTURAL MASS
!     ECPT(22)  -  FE             FORCE ELEM DESCRIPTIONS (FORCE METHOD)
!     ECPT(23)  -  C1             * STRESS RECOVERY COEFFICIENTS
!     ECPT(24)  -  C2             *
!     ECPT(25)  -  D1             *
!     ECPT(26)  -  D2             *
!     ECPT(27)  -  F1             *
!     ECPT(28)  -  F2             *
!     ECPT(29)  -  G1             *
!     ECPT(30)  -  G2             *
!     ECPT(31)  -  K1             $ AREA FACTORS FOR SHEAR
!     ECPT(32)  -  K2             $
!     ECPT(33)  -  I12            AREA MOMENT OF INERTIA
!     ECPT(34)  -  MCSIDA         COOR. SYS. ID. FOR GRID POINT A
!     ECPT(35)  -  GPA(3)         * BASIC COORDINATES FOR GRID POINT A
!     ECPT(36)  -    ...          *
!     ECPT(37)  -    ...          *
!     ECPT(38)  -  MCSIDB         COOR. SYS. ID. FOR GRID POINT B
!     ECPT(39)  -  GPB(3)         $ BASIC COORDINATES FOR GRID POINT B
!     ECPT(40)  -    ...          $
!     ECPT(41)  -    ...          $
!     ECPT(42)  -  ELTEMP         AVG. ELEMENT TEMPERATURE
!
   EQUIVALENCE (Ksystm(2),Ioutpt) , (Ksystm(56),Iheat) , (Ecpt(1),Iecpt(1),Ielid) , (Ksystm(87),Ksy87) , (Vec(1),Veci(1)) ,         &
    & (Vec(4),Vecj(1)) , (Vec(7),Veck(1))
   DATA ikk/1 , 7 , 73 , 79/ , epsi , epsi2/1.0E-18 , 1.0E-7/
   DATA is12or/1 , 37 , 109 , 73/ , is21or/73 , 109 , 37 , 1/
!
!
   dict(1) = Estid
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
   limit = iabs(Ksy87)*.01
!
!     NORMALIZE THE REFERENCE VECTOR WHICH LIES IN THE FIRST PRINCIPAL
!     AXIS PLANE  (FMMS - 36 P. 4)
!
   fl = sqrt(Smallv(1)**2+Smallv(2)**2+Smallv(3)**2)
   IF ( abs(fl)<epsi ) GOTO 1100
   DO i = 1 , 3
      Smalvn(i) = Smallv(i)/fl
   ENDDO
!
!     DETERMINE IF POINT A AND B ARE IN BASIC COORDINATES OR NOT.
!     COMPUTE THE TRANSFORMATION MATRICES TA AND TB IF NECESSARY
!
   IF ( icsida/=0 ) CALL transs(Ecpt(jcsida),Ta)
   IF ( icsidb/=0 ) CALL transs(Ecpt(jcsidb),Tb)
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
 200  DO i = 1 , 3
      jta = i + jcsida
      jtb = i + jcsidb
      Veci(i) = Ecpt(jta) - Ecpt(jtb)
   ENDDO
!
!     SAVE IN A2B THE LENGTH OF BAR WITHOUT OFFSET, FROM GRID PT. A TO B
!
   a2b = sqrt(Veci(1)**2+Veci(2)**2+Veci(3)**2)
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
      IF ( icsida/=0 ) THEN
         idela = 4
         CALL gmmats(Ta,3,3,0,Dela(1),3,1,0,Dela(4))
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
      IF ( icsidb/=0 ) THEN
         idelb = 4
         CALL gmmats(Tb,3,3,0,Delb(1),3,1,0,Delb(4))
      ENDIF
!
!     SINCE THERE WAS AT LEAST ONE NON-ZERO OFFSET VECTOR RECOMPUTE VECI
!
      DO i = 1 , 3
         jta = i - 1 + idela
         jtb = i - 1 + idelb
         Veci(i) = Veci(i) + Dela(jta) - Delb(jtb)
      ENDDO
   ENDIF
!
!     COMPUTE THE LENGTH OF THE BIG V (VECI) VECTOR AND NORMALIZE
!
   fl = 0.
   DO i = 1 , 3
      Veci(i) = -Veci(i)
      fl = fl + Veci(i)**2
   ENDDO
   fl = sqrt(fl)
   IF ( abs(fl)<epsi ) GOTO 1100
   DO i = 1 , 3
      Veci(i) = Veci(i)/fl
   ENDDO
!
!     NOW THAT LENGTH HAS BEEN COMPUTED, CHECK POSSIBLE OFFSET ERROR
!     ISSUE WARNING MESSAGE IF OFFSET EXCEEDS A2B BY 'LIMIT' PERCENT.
!     (DEFAULT IS 15 PERCENT, KSYSTM(87) WORD)
!
   IF ( abs(fl-a2b)/a2b>limit ) THEN
      WRITE (Ioutpt,99001) Uwm , Ielid
99001 FORMAT (A25,' - UNUSUALLY LARGE OFFSET IS DETECTED FOR CBAR ','ELEMENT ID =',I8,' ***')
      IF ( Ksy87>0 ) THEN
         WRITE (Ioutpt,99002) Ksy87
99002    FORMAT (/5X,'(OFFSET BAR LENGTH EXCEEDS NON-OFFSET LENGTH BY',I4,' PERCENT, SET BY SYSTEM 87TH WORD)')
         Ksy87 = -Ksy87
      ENDIF
   ENDIF
!
!     BRANCH IF THIS IS A -HEAT- FORMULATION.
!
   IF ( Iheat==1 ) THEN
!
!     HEAT FORMULATION CONTINUES HERE.  GET MATERIAL PROPERTY -K- FROM
!     HMAT
!
      Matflg = 1
      Matidc = Iecpt(16)
      Eltemp = Ecpt(42)
      dict(2) = 1
      dict(3) = 2
      dict(4) = 1
      dict(5) = 0
      IF ( Istif/=0 ) THEN
         CALL hmat(Ielid)
!
         Kk(1) = Fk*Ecpt(17)/fl
         IF ( Kk(1)/=0. ) THEN
            Kk(2) = -Kk(1)
            Kk(3) = Kk(2)
            Kk(4) = Kk(1)
            CALL emgout(Kk(1),Kk(1),4,1,dict,1,Iprec)
         ENDIF
!
         Matflg = 4
!
!     ERROR IN NEXT CARD FOR HEAT FORMULATION. REMOVED BY
!     G.CHAN/UNISYS, 1984
!     ALSO, CHANGE  GO TO 520  TO 540, 11-TH CARD ABOVE, AND
!     CALL EMGOUT BELOW AND WRITE TO THE 3-RD FILE INSTEAD OF THE 2-ND.
!
!
         CALL hmat(Ielid)
         Kk(1) = (Fk*Ecpt(17))*fl/2.
         IF ( Kk(1)==0. ) RETURN
         Kk(2) = Kk(1)
         dict(2) = 2
         CALL emgout(Kk(1),Kk(1),2,1,dict,3,Iprec)
      ENDIF
      RETURN
   ELSE
!
!     COMPUTE THE  SMALV0  VECTOR
!
      isv = 1
      IF ( Icssv/=0 ) THEN
         isv = 4
         CALL gmmats(Ta,3,3,0,Smalvn(1),3,1,0,Smalvn(4))
      ENDIF
!
!     COMPUTE THE K VECTOR, VECK = VECI  X  SMALV0, AND NORMALIZE
!
      Veck(1) = Veci(2)*Smalvn(isv+2) - Veci(3)*Smalvn(isv+1)
      Veck(2) = Veci(3)*Smalvn(isv) - Veci(1)*Smalvn(isv+2)
      Veck(3) = Veci(1)*Smalvn(isv+1) - Veci(2)*Smalvn(isv)
      fll = sqrt(Veck(1)**2+Veck(2)**2+Veck(3)**2)
      IF ( abs(fll)<epsi2 ) GOTO 1100
      DO i = 1 , 3
         Veck(i) = Veck(i)/fll
      ENDDO
!
!     COMPUTE THE J VECTOR, VECJ = VECK  X  VECI, AND NORMALIZE
!
      Vecj(1) = Veck(2)*Veci(3) - Veck(3)*Veci(2)
      Vecj(2) = Veck(3)*Veci(1) - Veck(1)*Veci(3)
      Vecj(3) = Veck(1)*Veci(2) - Veck(2)*Veci(1)
      fll = sqrt(Vecj(1)**2+Vecj(2)**2+Vecj(3)**2)
      IF ( abs(fll)<epsi2 ) GOTO 1100
      Vecj(1) = Vecj(1)/fll
      Vecj(2) = Vecj(2)/fll
      Vecj(3) = Vecj(3)/fll
!
!     SEARCH THE MATERIAL PROPERTIES TABLE FOR E,G AND THE DAMPING
!     CONSTANT.
!
      Matidc = Imatid
      Matflg = 1
      Eltemp = Tempel
      CALL mat(Iecpt(1))
!
      IF ( Istif==0 ) GOTO 700
!
!     IF ELASTICITY AND SHEAR MODULES BOTH ZERO, SKIP STIFFNESS
!     CALCULATION
!
      IF ( E==0. .AND. G==0. ) GOTO 700
!
!     SET UP INTERMEDIATE VARIABLES FOR ELEMENT STIFFNESS MATRIX
!     CALCULATION
!
      ASSIGN 400 TO korm
   ENDIF
 300  bl = fl
   blsq = fl**2
   blcube = blsq*bl
!
!     COMPUTE SOME TERMS TO BE USED IN STIFFNESS MATRIX KE
!
   ei1 = E*I1
   ei2 = E*I2
   IF ( K1==0.0 .OR. I12/=0.0 ) THEN
      r1 = 12.*ei1/blcube
   ELSE
      gak1 = G*A*K1
      r1 = (12.*ei1*gak1)/(gak1*blcube+12.*bl*ei1)
   ENDIF
   IF ( K2==0.0 .OR. I12/=0.0 ) THEN
      r2 = 12.*ei2/blcube
   ELSE
      gak2 = G*A*K2
      r2 = (12.*ei2*gak2)/(gak2*blcube+12.*bl*ei2)
   ENDIF
!
   sk1 = .25*r1*blsq + ei1/bl
   sk2 = .25*r2*blsq + ei2/bl
   sk3 = .25*r1*blsq - ei1/bl
   sk4 = .25*r2*blsq - ei2/bl
!
   ael = A*E/bl
   lr1 = bl*r1/2.
   lr2 = bl*r2/2.
   gjl = G*Fj/bl
!
!     CONSTRUCT  THE GENERAL 12X12 MATRIX FOR THE BAR ELEMENT
!
!                      **       **
!                      * K    K  *
!                      *  AA   AB*
!                K =   *  T      *
!                      * K    K  *
!                      *  AB   BB*
!                      **       **
!
!
!     FIRST SET THE COMPONENT CODE AND THE DOF
!
   icode = 63
   ndof = 12
   nsq = ndof**2
!
!     CONSTRUCT THE 12 X 12 MATRIX KE
!
   DO i = 1 , 144
      Ke(i) = 0.
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
   IF ( I12/=0. ) THEN
      beta = 12.*E*I12/blcube
      lb = bl*beta/2.0
      l2b3 = blsq*beta/3.0
      l2b6 = blsq*beta/6.0
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
   GOTO korm
!
!     DETERMINE IF THERE ARE NON-ZERO PIN FLAGS.
!
 400  ka = Iecpt(jpina)
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
            IF ( Ke(ii)/=0. ) THEN
               DO j = 1 , 12
                  ji = 12*(j-1) + ipin(i)
                  ij = 12*(ipin(i)-1) + j
                  DO ll = 1 , 12
                     jll = 12*(j-1) + ll
                     ill = 12*(ipin(i)-1) + ll
                     Kep(jll) = Ke(jll) - (Ke(ill)/Ke(ii))*Ke(ji)
                  ENDDO
                  Kep(ij) = 0.
                  Kep(ji) = 0.
               ENDDO
               DO k = 1 , 144
                  Ke(k) = Kep(k)
               ENDDO
            ELSE
               il = ipin(i)
               ii = ii - il
               DO j = 1 , 12
                  ii = ii + 1
                  Ke(ii) = 0.
                  Ke(il) = 0.
                  il = il + 12
               ENDDO
            ENDIF
         ENDIF
      ENDDO
   ENDIF
!
!     DIVIDE KE INTO FOUR SUBMATRICES AND STORE IN OPEN CORE
!
!      E                   E                   E
!     K   = KK(1 TO 36)   K   = KK(37 TO 72)  K   = KK(73 TO 108)
!      AA                  AB                  BA
!
!      E
!     K   =  KK(109 TO 144)
!      BB
!
!
   j = 0
   DO i = 1 , 72 , 12
      low = i
      lim = i + 5
      DO k = low , lim
         j = j + 1
         Kk(j) = Ke(k)
         Kk(j+36) = Ke(k+6)
         Kk(j+72) = Ke(k+72)
         Kk(j+108) = Ke(k+78)
      ENDDO
   ENDDO
!
   ASSIGN 600 TO korm
!
!     ZERO OUT THE ARRAY WHERE THE 3 X 3 MATRIX H AND THE W AND W  6 X 6
!     MATRICES WILL RESIDE.                                A      B
!      T
!     A   MATRIX NOW STORED IN KE
!
 500  DO i = 1 , 9
      Ke(i) = Vec(i)
   ENDDO
!
   DO i = 28 , 144
      Ke(i) = 0.
   ENDDO
!
!     SET UP POINTERS
!
   basic = icsida==0
   jcsid = jcsida
   offset = aofset
   jofset = jofsta
   DO i = 1 , 2
      iwbeg = i*36
!
!     SET UP THE -G- MATRIX.  IG POINTS TO THE BEGINNING OF THE G MATRIX
!     G = AT X TI
!
      ig = 1
      IF ( .NOT.(basic) ) THEN
         CALL transs(Ecpt(jcsid),Ke(10))
         CALL gmmats(Ke(1),3,3,0,Ke(10),3,3,0,Ke(19))
         ig = 19
      ENDIF
!
!     IF THERE IS A NON-ZERO OFFSET FOR THE POINT, SET UP THE D 3 X 3
!     MATRIX.
!
      IF ( offset ) THEN
         Ke(10) = 0.
         Ke(11) = Ecpt(jofset+2)
         Ke(12) = -Ecpt(jofset+1)
         Ke(13) = -Ke(11)
         Ke(14) = 0.
         Ke(15) = Ecpt(jofset)
         Ke(16) = -Ke(12)
         Ke(17) = -Ke(15)
         Ke(18) = 0.
!
!     FORM THE 3 X 3 PRODUCT H = G X D, I.E., KE(28) = KE(IG) X KE(10)
!
         CALL gmmats(Ke(ig),3,3,0,Ke(10),3,3,0,Ke(28))
      ENDIF
!
!
!     FORM THE W SUBMATRICES IN KE(37) AND KE(73)
!
!
      Ke(iwbeg+1) = Ke(ig)
      Ke(iwbeg+2) = Ke(ig+1)
      Ke(iwbeg+3) = Ke(ig+2)
      Ke(iwbeg+7) = Ke(ig+3)
      Ke(iwbeg+8) = Ke(ig+4)
      Ke(iwbeg+9) = Ke(ig+5)
      Ke(iwbeg+13) = Ke(ig+6)
      Ke(iwbeg+14) = Ke(ig+7)
      Ke(iwbeg+15) = Ke(ig+8)
      Ke(iwbeg+22) = Ke(ig)
      Ke(iwbeg+23) = Ke(ig+1)
      Ke(iwbeg+24) = Ke(ig+2)
      Ke(iwbeg+28) = Ke(ig+3)
      Ke(iwbeg+29) = Ke(ig+4)
      Ke(iwbeg+30) = Ke(ig+5)
      Ke(iwbeg+34) = Ke(ig+6)
      Ke(iwbeg+35) = Ke(ig+7)
      Ke(iwbeg+36) = Ke(ig+8)
      IF ( offset ) THEN
         Ke(iwbeg+4) = Ke(28)
         Ke(iwbeg+5) = Ke(29)
         Ke(iwbeg+6) = Ke(30)
         Ke(iwbeg+10) = Ke(31)
         Ke(iwbeg+11) = Ke(32)
         Ke(iwbeg+12) = Ke(33)
         Ke(iwbeg+16) = Ke(34)
         Ke(iwbeg+17) = Ke(35)
         Ke(iwbeg+18) = Ke(36)
      ENDIF
      basic = icsidb==0
      jcsid = jcsidb
      offset = bofset
      jofset = jofstb
   ENDDO
!
!     CONVERT THE K PARTITIONS TO GLOBAL COORDINATES AND STORE IN KEP
!
   iaft = 37
   DO i = 1 , 4
      ikx = (i-1)*36 + 1
      ik = ikx
      IF ( i>=3 ) ikx = (7-i-1)*36 + 1
      ifore = ((i-1)/2)*36 + 37
      CALL gmmats(Ke(ifore),6,6,1,Kk(ikx),6,6,0,Ke(109))
      CALL gmmats(Ke(109),6,6,0,Ke(iaft),6,6,0,Kep(ik))
      iaft = 73
      IF ( i==3 ) iaft = 37
   ENDDO
!
!     REFORM THE K MATRIX (12X12) FROM THE FOUR SUBMATRICES (6X6) AND
!     ORDER  THE SUBMATRICES BY INCREASING SIL VALUE
!
   DO ii = 1 , 4
      ix1 = ikk(ii)
      ix2 = ix1 + 60
      is = is12or(ii)
      IF ( Isilno(1)>Isilno(2) ) is = is21or(ii)
      DO i = ix1 , ix2 , 12
         ip5 = i + 5
         DO j = i , ip5
            Ke(j) = Kep(is)
            is = is + 1
         ENDDO
      ENDDO
   ENDDO
!
   GOTO korm
!
!     OUTPUT THE STIFFNESS MATRIX
!
 600  dict(2) = 1
   dict(3) = ndof
   dict(4) = icode
   dict(5) = Gsube
   CALL emgout(Ke(1),Ke(1),nsq,1,dict,1,Iprec)
!
!     THE MASS MATRIX IS GENERATED HERE.  IF THE PARAMETER ICMBAR IS
!     .LT. 0, CALL THE CONVENTIONAL MASS MATRIX GENERATION ROUTINE FOR
!     THE BAR.  OTHERWISE CALL THE ROUTINE TO GENERATE CONSISTENT MASS
!     MATRICES FOR THE BAR.
!
 700  const = (fl*(Rho*A+Nsm))/420.
   IF ( Imass==0 .OR. const==0. ) RETURN
   IF ( Icmbar<0 ) THEN
!
!     CALCULATE THE CONVENTIONAL MASS MATRIX HERE
!
!     GET RHO FROM MPT BY CALLING MAT
!
      Matidc = Imatid
      Matflg = 4
      Eltemp = Tempel
      CALL mat(Ecpt(1))
      DO i = 1 , 72
         Mep(i) = 0.
      ENDDO
      fm = .5*fl*(Rho*A+Nsm)
!
!     DETERMINE IF THE GRID POINT IS ASSOCIATED WITH A NON-ZERO OFFSET.
!
      jofset = 9
      DO ii = 1 , 2
         ix = (ii-1)*36
         j = jofset
         DO i = 1 , 3
            j = j + 1
            IF ( Ecpt(j)/=0. ) GOTO 720
         ENDDO
!
!     HERE WE HAVE A ZERO OFFSET VECTOR
!
         Mep(ix+1) = fm
         Mep(ix+8) = fm
         Mep(ix+15) = fm
         GOTO 740
!
!     FORM UPPER RIGHT CORNER OF THE MATRIX
!
 720     Mep(ix+1) = 1.
         Mep(ix+8) = 1.
         Mep(ix+15) = 1.
         Mep(ix+5) = Ecpt(jofset+3)
         Mep(ix+6) = -Ecpt(jofset+2)
         Mep(ix+12) = Ecpt(jofset+1)
         Mep(ix+10) = -Mep(ix+5)
         Mep(ix+16) = -Mep(ix+6)
         Mep(ix+17) = -Mep(ix+12)
         Mep(ix+20) = -Mep(ix+5)
         Mep(ix+21) = -Mep(ix+6)
         Mep(ix+25) = -Mep(ix+10)
         Mep(ix+27) = -Mep(ix+12)
         Mep(ix+31) = -Mep(ix+16)
         Mep(ix+32) = -Mep(ix+17)
         Mep(ix+22) = Ecpt(jofset+3)**2 + Ecpt(jofset+2)**2
         Mep(ix+29) = Ecpt(jofset+3)**2 + Ecpt(jofset+1)**2
         Mep(ix+36) = Ecpt(jofset+2)**2 + Ecpt(jofset+1)**2
         Mep(ix+23) = -Ecpt(jofset+1)*Ecpt(jofset+2)
         Mep(ix+24) = -Ecpt(jofset+1)*Ecpt(jofset+3)
         Mep(ix+30) = -Ecpt(jofset+2)*Ecpt(jofset+3)
         Mep(ix+28) = Mep(ix+23)
         Mep(ix+34) = Mep(ix+24)
         Mep(ix+35) = Mep(ix+30)
!
!     MULTIPLY M BY THE CONSTANT FL
!
         DO i = 1 , 36
            is = ix + i
            Mep(is) = Mep(is)*fm
         ENDDO
 740     jofset = 12
      ENDDO
!
!     INSERT M  AND M  SUBMATRICES INTO M ACCORDING TO INCREASING SIL
!             A      B
!
      DO i = 1 , 144
         Me(i) = 0.
      ENDDO
!
      IF ( Isilno(1)<=Isilno(2) ) THEN
         ix1 = 1
         ix2 = 37
      ELSE
         ix1 = 37
         ix2 = 1
      ENDIF
      DO jj = 1 , 36
         mm = mod(jj,6)
         IF ( mm==0 ) mm = 6
         i = ((jj-1)/6)*12 + mm
         j = i + 78
         Me(i) = Mep(ix1)
         Me(j) = Mep(ix2)
         ix1 = ix1 + 1
         ix2 = ix2 + 1
      ENDDO
!
!     OUTPUT THE CONVENTIONAL MASS MATRIX
!
      dict(2) = 1
      dict(3) = ndof
      dict(4) = icode
      dict(5) = 0
!
      CALL emgout(Me,Me,144,1,dict,2,Iprec)
!
      RETURN
   ELSE
!
!     CALCULATE THE CONSISTENT/CONVENTIONAL MASS MATRIX
!
!     CALL THE MAT ROUTINE TO FETCH SINGLE PRECISION MATERIAL PROPERTIES
!
      Matidc = Imatid
      Matflg = 1
      Eltemp = Tempel
      CALL mat(Iecpt(1))
!
!
!     COMPUTE TERMS OF THE ELEMENT MASS MATRIX
!
      bl22 = 22.*fl
      bl13 = 13.*fl
      blsq4 = 4.0*fl**2
      blsq3 = 3.0*fl**2
!
!     CONSTRUCT THE ELEMENT MASS MATRIX.
!
      DO i = 1 , 12
         DO j = 1 , 12
            M(i,j) = 0.
         ENDDO
      ENDDO
      M(1,1) = 175.
      M(1,7) = 35.
      M(2,2) = 156.
      M(2,6) = bl22
      M(2,8) = 54.
      M(2,12) = -bl13
      M(3,3) = 156.
      M(3,5) = -bl22
      M(3,9) = 54.
      M(3,11) = bl13
      M(5,5) = blsq4
      M(5,9) = -bl13
      M(5,11) = -blsq3
      M(6,6) = blsq4
      M(6,8) = bl13
      M(6,12) = -blsq3
      M(7,7) = 175.
      M(8,8) = 156.
      M(8,12) = -bl22
      M(9,9) = 156.
      M(9,11) = bl22
      M(11,11) = blsq4
      M(12,12) = blsq4
!
!     STORE THE UPPER TRIANGULAR PART OF THE MATRIX IN THE LOWER PART.
!
      DO i = 1 , 10
         low = i + 1
         DO j = low , 12
            M(j,i) = M(i,j)
         ENDDO
      ENDDO
!
!     MULTIPLY BY CONSTANT AND STORE ROW-WISE IN THE ARRAY ME
!
      k = 0
      DO i = 1 , 12
         DO j = 1 , 12
            k = k + 1
            Me(k) = const*M(i,j)
         ENDDO
      ENDDO
!
!     IF THERE ARE NO PIN FLAGS THERE IS NO NEED TO CALCULATE THE
!     ELEMENT STIFFNESS MATRIX
!
      ka = Iecpt(jpina)
      kb = Iecpt(jpinb)
      IF ( ka==0 .AND. kb==0 ) GOTO 900
!
!     COMPUTE THE STIFFNESS MATRIX KE
!
      ASSIGN 800 TO korm
      GOTO 300
   ENDIF
!
!     RETURN HERE AFTER COMPUTING THE STIFFNESS MATRIX
!
!
!     SET UP TNHE IPIN ARRAY
!
 800  DO i = 1 , 5
      ipin(i) = mod(ka,10)
      ipin(i+5) = mod(kb,10) + 6
      IF ( ipin(i+5)==6 ) ipin(i+5) = 0
      ka = ka/10
      kb = kb/10
   ENDDO
!
!     ALTER THE ELEMENT MASS MATRIX DUE TO PIN FLAGS.  NOTE THAT THE
!     FOLLOWING CODE IS CONGRUENT AS IT WERE TO THE CODE IN SUBROUTINE
!     DBEAM IN THE DSMG1 MODULE.
!
   DO j = 1 , 10
      IF ( ipin(j)/=0 ) THEN
         jj = 12*(ipin(j)-1) + ipin(j)
         IF ( Ke(jj)/=0. ) THEN
            DO i = 1 , 12
               ji = 12*(ipin(j)-1) + i
               ij = 12*(i-1) + ipin(j)
               DO l1 = 1 , 12
                  il = 12*(i-1) + l1
                  lj = 12*(l1-1) + ipin(j)
                  Mep(il) = Me(il) - Ke(lj)*Me(ji)/Ke(jj) - Ke(ji)*Me(lj)/Ke(jj) + Ke(lj)*Ke(ji)*Me(jj)/Ke(jj)**2
               ENDDO
            ENDDO
            DO k = 1 , 144
               Me(k) = Mep(k)
            ENDDO
         ENDIF
!
!     ZERO OUT THE IPIN(J) TH ROW AND COLUMN OF ME
!
         j1 = jj - ipin(j)
         j2 = ipin(j)
         DO k = 1 , 12
            j1 = j1 + 1
            Me(j1) = 0.
            Me(j2) = 0.
            j2 = j2 + 12
         ENDDO
      ENDIF
   ENDDO
!
!            E                  E                    E
!     STORE M  AT KK(1 TO 36), M  AT KK (37 TO 72), M  AT KK(73 TO 108)
!            AA                 AB                   BA
!
!            E
!     AND   M  AT KK(109 TO 144)
!            BB
!
 900  j = 0
   DO i = 1 , 72 , 12
      low = i
      lim = low + 5
      DO k = low , lim
         j = j + 1
         Kk(j) = Me(k)
         Kk(j+36) = Me(k+6)
         Kk(j+72) = Me(k+72)
         Kk(j+108) = Me(k+78)
      ENDDO
   ENDDO
!
!     CALCULATE THE TRANSFORMATION VECTORS
!
   ASSIGN 1000 TO korm
   GOTO 500
!
!     OUTPUT THE CONSISTENT MASS MATRIX
!
 1000 dict(2) = 1
   dict(3) = ndof
   dict(4) = icode
   dict(5) = 0
   CALL emgout(Ke(1),Ke(1),144,1,dict,2,Iprec)
   RETURN
!
!     ERROR RETURNS
!
 1100 WRITE (Ioutpt,99003) Ufm , Ielid
99003 FORMAT (A23,' 3176, BAR ELEMENT ID',I9,' HAS ILLEGAL GEOMETRY OR CONNECTIONS.')
   Nogo = .TRUE.
END SUBROUTINE bars
