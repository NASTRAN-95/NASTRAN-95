!*==dbar.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dbar
   USE c_ds1aaa
   USE c_ds1adp
   USE c_ds1aet
   USE c_matin
   USE c_matout
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: abasic , aofset , basic , bbasic , bofset , offset
   REAL(REAL64) :: alpha , da , dfj , e , ei1 , ei2 , fl , fll , fx , gak1 , gak2 , l , lcube , lsq , may , maz , mby , mbz , rrv1 ,&
                 & rrv2 , sk1 , sk2 , sk3 , sk4 , term1 , term10 , term11 , term2 , term3 , term4 , term5 , term6 , term7 , term8 , &
                 & term9 , tsub0 , vy , vz
   REAL(REAL64) , DIMENSION(8) :: dp
   REAL(REAL64) , DIMENSION(6) :: dpveca , dpvecb , smalv0 , ua , ub
   REAL , DIMENSION(100) :: ecpt
   INTEGER :: i , iab , icsida , icsidb , idela , idelb , ig , ii , ij , ikde , ikel , il , ilim , ill , ilow , ipvt , isv , ita ,  &
            & iwbeg , iwleft , iwrght , j , j1 , j2 , jcsid , jcsida , jcsidb , ji , jj , jll , jofset , jofsta , jofstb , jpina ,  &
            & jpinb , k , ka , kb , kk , l1 , lim , lj , ll , low
   INTEGER , DIMENSION(100) :: iecpt
   INTEGER , DIMENSION(10) :: ipin
   REAL(REAL64) , DIMENSION(12,12) :: kc
   REAL(REAL64) , DIMENSION(144) :: kd , kdp , kes
   REAL(REAL64) , DIMENSION(72) :: sa
   REAL(REAL64) , DIMENSION(36) :: sb
   REAL(REAL64) , DIMENSION(18) :: ta
   REAL(REAL64) , DIMENSION(9) :: tb
   REAL(REAL64) , DIMENSION(3) :: veci , vecj , veck
   EXTERNAL ds1b , gmmatd , mat , mesage , transd
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE COMPUTES THE 2 6X6 DIFFERENTIAL STIFFNESS MATRICES
!     K(NPVT,NPVT) AND K(NPVT,J) FOR A BEAM HAVING END POINTS OF SIL
!     NOS. NPVT AND J.
!
!     ECPT FOR THE BEAM
!
!     ECPT( 1)  -  IELID          ELEMENT ID. NO.
!     ECPT( 2)  -  ISILNO(2)      SCALAR INDEX NOS.
!     ECPT( 3)  -   ...
!     ECPT( 4)  -  SMALLV(3)      REFERENCE VECTOR
!     ECPT( 5)  -   ...
!     ECPT( 6)  -   ...
!     ECPT( 7)  -  IGSUB0         OPTION FOR DEFINING REFERENCE NUMBER.
!     ECPT( 8)  -  IPINFL(2)      PIN FLAGS
!     ECPT( 9)  -   ...
!     ECPT(10)  -  ZA(3)          OFFSET VECTOR AT POINT A
!     ECPT(11)  -   ...
!     ECPT(12)  -   ...
!     ECPT(13)  -  ZB(3)          OFFSET VECTOR AT POINT B
!     ECPT(14)  -   ...
!     ECPT(15)  -   ...
!     ECPT(16)  -  GEF(4)         ECCENTRICITIES FOR FORCE
!     ECPT(17)  -   ...
!     ECPT(18)  -   ...
!     ECPT(19)  -   ...
!     ECPT(20)  -  IMATID         MATERIAL ID.
!     ECPT(21)  -  A              CROSS-SECTIONAL AREA
!     ECPT(22)  -  C1             STRESS COEFFICIENTS
!     ECPT(23)  -  C2                    ...
!     ECPT(24)  -  I1             AREA MOMENTS OF INERTIA
!     ECPT(25)  -  I2                    ...
!     ECPT(26)  -  I3                    ...
!     ECPT(27)  -  FJ             TORSIONAL CONSTANT
!     ECPT(28)  -  FMU            NON-STRUCTURAL MASS
!     ECPT(29)  -  K1             AREA FACTORS FOR SHEAR
!     ECPT(30)  -  K2                    ...
!     ECPT(31)  -  C3 (D1)        STRESS COEFFICIENTS
!     ECPT(32)  -  C4 (D2)               ...
!     ECPT(33)  -  B1             WIDTHS FOR FORCE
!     ECPT(34)  -  B2                    ...
!     ECPT(35)  -  HS1            DEPTHS FOR FORCE
!     ECPT(36)  -  HS2                   ...
!     ECPT(37)  -  HT1                   ...
!     ECPT(38)  -  HT2                   ...
!     ECPT(39)  -  MCSIDA         COOR. SYS. ID. FOR GRID PT. A
!     ECPT(40)  -  GPA(3)         BASIC COORDINATES FOR PT. A
!     ECPT(41)  -   ...                  ...
!     ECPT(42)  -   ...                  ...
!     ECPT(43)  -  MCSIDB         COOR. SYS. ID. FOR GRID PT. B
!     ECPT(44)  -  GPB(3)         BASIC COORDINATES FOR PT. B
!     ECPT(45)  -   ...                  ...
!     ECPT(46)  -   ...                  ...
!     ECPT(47)  -  ELTEMP         ELEMENT TEMPERATURE
!     ECPT(48)  -  ELDEF          ELEMENT DEFORMATION
!     ECPT(49)  -  TEMPER         ELEMENT LOADING TEMPERATURE
!     ECPT(50)  -  UAS(1)                ...
!     ECPT(51)  -  UAS(2)                ...
!     ECPT(52)  -  UAS(3)         SINGLE PRECISION DISPLACEMENTS
!     ECPT(53)  -  UAS(4)               FOR GRID POINT A
!     ECPT(54)  -  UAS(5)                ...
!     ECPT(55)  -  UAS(6)                ...
!     ECPT(56)  -  UBS(1)                ...
!     ECPT(57)  -  UBS(2)                ...
!     ECPT(58)  -  UBS(3)         SINGLE PRECISION DISPLACEMENTS
!     ECPT(59)  -  UBS(4)               FOR GRID POINT B
!     ECPT(60)  -  UBS(5)                ...
!     ECPT(61)  -  UBS(6)                ...
!
   !>>>>EQUIVALENCE (Ielid,Ecpt(1),Iecpt(1)) , (Iz(1),Z(1)) , (ta(10),tb(1)) , (Ecpt(71),Dp(1)) , (kc(1,1),kd(1)) , (sa(37),sb(1))
!
!     DETERMINE WHICH SIL IS THE PIVOT POINT.
!
!     IPVT = 0
   ipvt = 1
   IF ( isilno(1)/=npvt ) THEN
      ipvt = 2
      IF ( isilno(2)/=npvt ) CALL mesage(-30,34,iecpt(1))
   ENDIF
!
!     JCSIDA IS AN INDEX WHICH POINTS TO THE COOR. SYS. ID. OF POINT A.
!     JOFSTA IS AN INDEX WHICH POINTS TO THE OFFSET VECTOR FOR POINT A.
!     SIMILARY FOR JCSIDB AND JOFSTB AND POINT B.
!
   jcsida = 39
   jcsidb = 43
   jofsta = 10
   jofstb = 13
   jpina = 8
   jpinb = 9
   icsida = iecpt(jcsida)
   icsidb = iecpt(jcsidb)
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
   IF ( fl>0.0D0 ) THEN
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
         IF ( ecpt(j)/=0.0 ) GOTO 50
      ENDDO
      aofset = .FALSE.
 50   bofset = .TRUE.
      j = jofstb - 1
      DO i = 1 , 3
         j = j + 1
         IF ( ecpt(j)/=0.0 ) GOTO 100
      ENDDO
      bofset = .FALSE.
!
!     FORM THE CENTER AXIS OF THE BEAM WITHOUT OFFSETS.
!     FIRST WE STORE THE COORDINATES IN THE ARRAY DP SO THAT ALL
!     ARITHMETIC WILL BE DOUBLE PRECISION.
!
 100  dp(1) = ecpt(jcsida+1)
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
      fl = dsqrt(veci(1)**2+veci(2)**2+veci(3)**2)
      IF ( fl/=0.0D0 ) THEN
         DO i = 1 , 3
            veci(i) = veci(i)/fl
         ENDDO
!
!     COMPUTE THE SMALL V SUB 0 VECTOR, SMALV0.  ***CHECK THIS LOGIC***
!
         ita = 1
         isv = 1
         IF ( mcsida/=0 .AND. igsub0/=0 ) THEN
            IF ( jcsida/=39 ) ita = 10
            isv = 4
            CALL gmmatd(ta(ita),3,3,0,smalv0(1),3,1,0,smalv0(4))
         ENDIF
!
!     COMPUTE THE K VECTOR, VECK = VECI X SMALV0, AND NORMALIZE
!
         veck(1) = veci(2)*smalv0(isv+2) - veci(3)*smalv0(isv+1)
         veck(2) = veci(3)*smalv0(isv) - veci(1)*smalv0(isv+2)
         veck(3) = veci(1)*smalv0(isv+1) - veci(2)*smalv0(isv)
         fll = dsqrt(veck(1)**2+veck(2)**2+veck(3)**2)
         IF ( fll/=0.0D0 ) THEN
            veck(1) = veck(1)/fll
            veck(2) = veck(2)/fll
            veck(3) = veck(3)/fll
!
!     COMPUTE THE J VECTOR, VECJ = VECK X VECI, AND NORMALIZE
!
            vecj(1) = veck(2)*veci(3) - veck(3)*veci(2)
            vecj(2) = veck(3)*veci(1) - veck(1)*veci(3)
            vecj(3) = veck(1)*veci(2) - veck(2)*veci(1)
            fll = dsqrt(vecj(1)**2+vecj(2)**2+vecj(3)**2)
            IF ( fll/=0.0D0 ) THEN
               vecj(1) = vecj(1)/fll
               vecj(2) = vecj(2)/fll
               vecj(3) = vecj(3)/fll
!
!     SEARCH THE MATERIAL PROPERTIES TABLE FOR E,G AND THE DAMPING
!     CONSTANT.
!
               matidc = imatid
               matflg = 1
               eltemp = tempel
               CALL mat(iecpt(1))
!
!     COMPUTE THE RECIPROCALS OF RV1 AND RV2 (CALLING THEM RRV1 AND
!     RRV2)
!
               l = fl
               lsq = l**2
               lcube = lsq*l
!
!     STORE ECPT AND MPT VARIABLES IN DOUBLE PRECISION LOCATIONS.
!
               dp(1) = es
               dp(2) = gs
               dp(3) = i1
               dp(4) = i2
               dp(5) = a
               ei1 = dp(1)*dp(3)
               ei2 = dp(1)*dp(4)
               IF ( k1==0.0 ) THEN
                  rrv1 = 12.0D0*ei1/lcube
               ELSE
                  dp(6) = k1
                  gak1 = dp(2)*dp(5)*dp(6)
                  rrv1 = (12.0D0*ei1*gak1)/(gak1*lcube+12.0D0*l*ei1)
               ENDIF
               IF ( k2==0.0 ) THEN
                  rrv2 = 12.0D0*ei2/lcube
               ELSE
                  dp(7) = k2
                  gak2 = dp(2)*dp(5)*dp(7)
                  rrv2 = (12.0D0*ei2*gak2)/(gak2*lcube+12.0D0*l*ei2)
               ENDIF
!
!     COMPUTE THE -SMALL- K-S, SK1, SK2, SK3 AND SK4
!
               sk1 = 0.25D0*rrv1*lsq + ei1/l
               sk2 = 0.25D0*rrv2*lsq + ei2/l
               sk3 = 0.25D0*rrv1*lsq - ei1/l
               sk4 = 0.25D0*rrv2*lsq - ei2/l
!
!     COMPUTE THE TERMS THAT WILL BE NEEDED FOR THE 12 X 12 MATRIX KE
!
               term1 = dp(5)*dp(1)/l
               term2 = 0.5D0*l*rrv1
               term3 = 0.5D0*l*rrv2
               dp(8) = fj
               term4 = dp(2)*dp(8)/l
!
!     CONSTRUCT THE 12 X 12 MATRIX KE
!
               DO i = 1 , 144
                  ke(i) = 0.0D0
               ENDDO
               ke(1) = term1
               ke(7) = -term1
               ke(14) = rrv1
               ke(18) = -term2
               ke(20) = -rrv1
               ke(24) = -term2
               ke(27) = rrv2
               ke(29) = term3
               ke(33) = -rrv2
               ke(35) = term3
               ke(40) = term4
               ke(46) = -term4
               ke(51) = term3
               ke(53) = sk2
               ke(57) = -term3
               ke(59) = sk4
               ke(62) = -term2
               ke(66) = sk1
               ke(68) = term2
               ke(72) = sk3
               ke(73) = -term1
               ke(79) = term1
               ke(86) = -rrv1
               ke(90) = term2
               ke(92) = rrv1
               ke(96) = term2
               ke(99) = -rrv2
               ke(101) = -term3
               ke(105) = rrv2
               ke(107) = -term3
               ke(112) = -term4
               ke(118) = term4
               ke(123) = term3
               ke(125) = sk4
               ke(129) = -term3
               ke(131) = sk2
               ke(134) = -term2
               ke(138) = sk3
               ke(140) = term2
               ke(144) = sk1
!
!     DETERMINE IF THERE ARE NON-ZERO PIN FLAGS.
!
               ka = iecpt(jpina)
               kb = iecpt(jpinb)
               IF ( ka/=0 .OR. kb/=0 ) THEN
!
!     SAVE THE KE (UNPINNED) MATRIX IN KES.
!
                  DO i = 1 , 144
                     kes(i) = ke(i)
                  ENDDO
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
!     SET POINTERS SO THAT WE WILL BE WORKING WITH POINT A.
!
               basic = abasic
               jcsid = jcsida
               offset = aofset
               jofset = jofsta
               iwbeg = 0
               ikel = 1
               iab = 1
!
!     ZERO OUT THE ARRAY WHERE THE 3 X 3 MATRIX H AND THE W  AND W
!     6 X 6 MATRICES WILL RESIDE.                          A      B
!
               DO i = 28 , 108
                  ke(i) = 0.0D0
               ENDDO
               DO
!
!     SET UP THE -G- MATRIX.  IG POINTS TO THE BEGINNING OF THE G MATRIX
!     G = AT X TI
!
                  ig = 1
                  IF ( .NOT.(basic) ) THEN
                     CALL transd(ecpt(jcsid),ke(10))
                     CALL gmmatd(ke(1),3,3,0,ke(10),3,3,0,ke(19))
                     ig = 19
                  ENDIF
!
!     IF THERE IS A NON-ZERO OFFSET FOR THE POINT, SET UP THE D 3 X 3
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
!     FORM THE 3 X 3 PRODUCT H = G X D, I.E., KE(28) = KE(IG) X KE(10)
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
!                                 E                     E
!     COMPUTE THE PRODUCT S   =  K   X  W   OR  S   =  K    X  W
!                          A      AA     A       B      AB      B
!     WHERE
!                  T                           T
!           W  =  T   X  C  X  E   AND  W  =  T   X  C   X  E
!            A     EB     A     A        B     EB     B      B
!
!     W AT KE(37) AND W AT KE(73) WILL BE USED AGAIN BEFORE FINAL STEPS.
!      A               B
!
                  CALL gmmatd(kep(ikel),6,6,0,ke(iwbeg+37),6,6,0,sa(iab))
!
!     IF THE POINT UNDER CONSIDERATION IS POINT B WE ARE FINISHED. IF
!     NOT, SET UP POINTS AND INDICATORS FOR WORKING WITH POINT B.
!
                  IF ( iwbeg==36 ) THEN
!
!     BEGIN DIFFERENTIAL STIFFNESS PORTION OF THIS ROUTINE.
!
!     STORE DISPLACEMENT VECTORS IN DOUBLE PRECISION LOCATIONS
!
                     DO i = 1 , 6
                        ua(i) = ecpt(i+49)
                        ub(i) = ecpt(i+55)
                     ENDDO
!
!     COMPUTE  S  X  U   AND  S  X  U
!               A     A        B     B
!
                     CALL gmmatd(sa(1),6,6,0,ua,6,1,0,dpveca)
                     CALL gmmatd(sb(1),6,6,0,ub,6,1,0,dpvecb)
!
!     COMPUTE THE NEEDED COMPONENTS OF THE FORCE VECTOR.
!
                     fx = dpveca(1) + dpvecb(1)
                     vy = dpveca(2) + dpvecb(2)
                     vz = dpveca(3) + dpvecb(3)
                     may = dpveca(5) + dpvecb(5)
                     maz = dpveca(6) + dpvecb(6)
                     mbz = -maz - vy*l
                     mby = -may + vz*l
                     e = es
                     fx = fx - e*eldef/l
                     IF ( iecpt(49)/=-1 ) THEN
                        alpha = alphas
                        tsub0 = tsub0s
                        dp(1) = temper
                        fx = fx - a*alpha*e*(dp(1)-tsub0)
                     ENDIF
!
!     ZERO OUT THE KD (KC) MATRIX
!
                     DO i = 1 , 144
                        kd(i) = 0.0D0
                     ENDDO
!
!     FORM THE ELEMENT DIFFERENTIAL STIFFNESS MATRIX (UPPER HALF)
!
                     term1 = 6.0D0*fx/(5.0D0*l)
                     term2 = -may/l
                     term3 = fx/10.0D0
                     term4 = -mby/l
                     term5 = -maz/l
                     term6 = -mbz/l
                     dfj = i1 + i2
                     da = a
                     term7 = dfj*fx/(l*da)
                     term8 = l*vy/6.0D0
                     term9 = l*vz/6.0D0
                     term10 = 2.0D0*l*fx/15.0D0
                     term11 = l*fx/30.0D0
                     kc(2,2) = term1
                     kc(2,4) = term2
                     kc(2,6) = -term3
                     kc(2,8) = -term1
                     kc(2,10) = term4
                     kc(2,12) = -term3
                     kc(3,3) = term1
                     kc(3,4) = term5
                     kc(3,5) = term3
                     kc(3,9) = -term1
                     kc(3,10) = term6
                     kc(3,11) = term3
                     kc(4,4) = term7
                     kc(4,5) = -term8
                     kc(4,6) = -term9
                     kc(4,8) = -term2
                     kc(4,9) = -term5
                     kc(4,10) = -term7
                     kc(4,11) = term8
                     kc(4,12) = term9
                     kc(5,5) = term10
                     kc(5,9) = -term3
                     kc(5,10) = term8
                     kc(5,11) = -term11
                     kc(6,6) = term10
                     kc(6,8) = term3
                     kc(6,10) = term9
                     kc(6,12) = -term11
                     kc(8,8) = term1
                     kc(8,10) = -term4
                     kc(8,12) = term3
                     kc(9,9) = term1
                     kc(9,10) = -term6
                     kc(9,11) = -term3
                     kc(10,10) = term7
                     kc(10,11) = -term8
                     kc(10,12) = -term9
                     kc(11,11) = term10
                     kc(12,12) = term10
!
!     STORE THE UPPER HALF IN THE LOWER HALF.
!
                     DO i = 2 , 10
                        low = i + 1
                        DO j = low , 12
                           kc(j,i) = kc(i,j)
                        ENDDO
                     ENDDO
!
!     IF THERE PIN FLAGS, ALTER THE KD MATRIX
!
                     IF ( ka/=0 .OR. kb/=0 ) THEN
!
!     ALTER KD DUE TO PIN FLAGS.
!
                        DO j = 1 , 10
                           IF ( ipin(j)/=0 ) THEN
                              jj = 12*(ipin(j)-1) + ipin(j)
                              IF ( kes(jj)/=0.0D0 ) THEN
                                 DO i = 1 , 12
                                    ji = 12*(ipin(j)-1) + i
                                    ij = 12*(i-1) + ipin(j)
                                    DO l1 = 1 , 12
                                       il = 12*(i-1) + l1
                                       lj = 12*(l1-1) + ipin(j)
                                       kdp(il) = kd(il) - kes(lj)*kd(ji)/kes(jj) - kes(ji)*kd(lj)/kes(jj) + kes(lj)*kes(ji)*kd(jj)  &
                                        & /kes(jj)**2
                                    ENDDO
                                 ENDDO
                                 DO kk = 1 , 144
                                    kd(kk) = kdp(kk)
                                 ENDDO
                              ENDIF
!
!     ZERO OUT THE IPIN(J) TH ROW AND COLUMN OF KD.
!
                              j1 = jj - ipin(j)
                              j2 = ipin(j)
                              DO kk = 1 , 12
                                 j1 = j1 + 1
                                 kd(j1) = 0.0D0
                                 kd(j2) = 0.0D0
                                 j2 = j2 + 12
                              ENDDO
                           ENDIF
                        ENDDO
                     ENDIF
!
!            D
!     STORE K        AT KEP(1),...,KEP(36)  AND
!            NPVT,A
!
!            D
!           K        AT KEP(37),...,KEP(72)
!            NPVT,B
!
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
                           kep(j) = kd(k)
                           kep(j+36) = kd(k+6)
                        ENDDO
                     ENDDO
!
!     COMPUTE THE FINAL 2 6X6 DIFFERENTIAL STIFFNESS MATRICES FOR THIS
!     BEAM.
!
                     iwleft = 37
                     IF ( ipvt==2 ) iwleft = 73
                     i = 1
                     ikde = 1
                     iwrght = 37
                     DO
                        CALL gmmatd(ke(iwleft),6,6,1,kep(ikde),6,6,0,kep(73))
                        CALL gmmatd(kep(73),6,6,0,ke(iwrght),6,6,0,kep(109))
                        CALL ds1b(kep(109),isilno(i))
                        IF ( i==2 ) RETURN
                        i = 2
                        ikde = 37
                        iwrght = 73
                     ENDDO
                  ELSE
                     basic = bbasic
                     jcsid = jcsidb
                     offset = bofset
                     jofset = jofstb
                     iwbeg = 36
                     ikel = 37
                     iab = 37
                     DO i = 28 , 36
                        ke(i) = 0.0D0
                     ENDDO
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
!     FATAL ERROR
!
   CALL mesage(30,26,iecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
   nogo = 1
END SUBROUTINE dbar
