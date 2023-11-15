
SUBROUTINE selbo1
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A , Alpha , Betar , C , C1 , C2 , Costh , D1 , D2 , Dela(6) , Delb(6) , Dum3(61) , E , Ecpt(100) , Eltemp , F1 , F2 , Fe ,  &
      & Fj , G , G1 , G2 , Gpa(3) , Gpb(3) , Gsube , I1 , I2 , K1 , K2 , Ke(144) , Kee(12,12) , Kep(144) , Kx , Ky , Kz , Nsm ,     &
      & Out(21) , R , Rho , S(12,12) , Sa(36) , Sb(36) , Sigc , Sigs , Sigt , Sinth , Smallv(3) , Stress , Tempel , Therm(30) ,     &
      & Tsub0
   INTEGER Icssv , Iecpt(100) , Ielid , Imatid , Isilno(2) , Jelid , Jsilno(2) , Matflg , Matidc , Mcsida , Mcsidb , Nu
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , G , Nu , Rho , Alpha , Tsub0 , Gsube , Sigt , Sigc , Sigs
   COMMON /sdr2x5/ Ielid , Isilno , Smallv , Icssv , Imatid , A , I1 , I2 , Fj , Nsm , Fe , C1 , C2 , D1 , D2 , F1 , F2 , G1 , G2 , &
                 & K1 , K2 , C , Kx , Ky , Kz , R , Betar , Mcsida , Gpa , Mcsidb , Gpb , Tempel , Dum3 , Jelid , Jsilno , Sa , Sb ,&
                 & Out , Therm
   COMMON /sdr2x6/ Ke , Kep , Dela , Delb
!
! Local variable declarations
!
   LOGICAL abasic , basic , bbasic
   REAL alphar , c2t , ct , dampc , dcr , determ , df(6,6) , dp(20) , dum , f(6,6) , fi1 , fi2 , fjk , fld , fll , h(6,6) , hut(6) ,&
      & l , r2 , ra , rb0 , rb1 , rt , rv1 , rv2 , s2t , smalv0(6) , st , t , ta(18) , tb(9) , veci(3) , vecj(3) , veck(3) , x
   REAL cod , dtr , sid
   INTEGER i , i12 , iab , ic , icsida , icsidb , ig , ikel , in , index , ir , ising , isop , iwbeg , j , jcsid , jcsida , jcsidb ,&
         & k , lim , low
!
! End of declarations
!
!
!     THIS ROUTINE IS PHASE 1 OF STRESS DATA RECOVERY FOR THE ELBOW
!     ELEMENT MUCH OF THE CODE WAS LIFTED FROM THE KELBOW SUBROUTINE
!
!     ECPT FOR THE ELBOW
!
!     ECPT( 1)  -  IELID          ELEMENT ID. NUMBER
!     ECPT( 2)  -  ISILNO(2)      * SCALAR INDEX NOS. OF THE GRID POINTS
!     ECPT( 3)  -    ...          *
!     ECPT( 4)  -  SMALLV(3)      $ REFERENCE VECTOR
!     ECPT( 5)  -    ...          $
!     ECPT( 6)  -    ...          $
!     ECPT( 7)  -  ICSSV          COOR. SYS. ID FOR SMALLV VECTOR
!     ECPT( 8)  -  IMATID         MATERIAL ID.
!     ECPT( 9)  -  A              CROSS-SECTIONAL AREA
!     ECPT(10)  -  I1             $ AREA MOMENTS OF INERTIA
!     ECPT(11)  -  I2             $
!     ECPT(12)  -  FJ             TORSIONAL CONSTANT
!     ECPT(13)  -  NSM            NON-STRUCTURAL MASS
!     ECPT(14)  -  FE             FORCE ELEM. DESCRIPTIONS, FORCE METHOD
!     ECPT(15)  -  R1             *STRESS RECOVERY COEFFICIENTS
!     ECPT(16)  -  T1             *  RI=RADIAL LOCATION
!     ECPT(17)  -  R2             *  TI=ANGULAR LOCATION
!     ECPT(18)  -  T2             *     OF STRESS RECOVERY POINTS
!     ECPT(19)  -  R3             *
!     ECPT(20)  -  T3             *
!     ECPT(21)  -  R4             *
!     ECPT(22)  -  T4             *
!     ECPT(23)  -  K1             $ AREA FACTOR FOR SHEAR
!     ECPT(24)  -  K2             $
!     ECPT(25)  -  C              STRESS INTENSIFICATION FACTOR
!     ECPT(26)  -  KX             * FLEXIBILITY CORRECTION FACTORS
!     ECPT(27)  -  KY             *
!     ECPT(28)  -  KZ             *
!     ECPT(29)  -  R              RADIUS OF CURVATURE
!     ECPT(30)  -  BETAR          ANGLE FROM GA TO GB
!     ECPT(31)  -  MCSIDA         COORD. SYS. ID. FOR GRID POINT A
!     ECPT(32)  -  GPA(3)         *BASIC COORD. FOR GRID POINT A
!     ECPT(33)  -   ...           *
!     ECPT(34)  -   ...           *
!     ECPT(35)  -  MCSIDB         COORD. SYS. ID. FOR GRID POINT B
!     ECPT(36)  -  GPB(3)         *BASIC COORD. FOR GRID POINT B
!     ECPT(37)  -   ...           *
!     ECPT(38)  -   ...           *
!     ECPT(39)  -  ELTEMP         AVG. ELEMENT TEMPERATURE
!
!
   EQUIVALENCE (Ielid,Ecpt(1),Iecpt(1)) , (ta(10),tb(1)) , (Kee(1,1),Ke(1),S(1,1))
   DATA dcr/.017453292/
!
   sid(x) = sin(x*dcr)
   cod(x) = cos(x*dcr)
   dtr(x) = x*dcr
!
   x = 1.0
   isop = -1
!
!     SET UP POINTERS TO COORD. SYSTEM IDS
!
   jcsida = 31
   jcsidb = 35
   icsida = Iecpt(31)
   icsidb = Iecpt(35)
!
!     DEFINE LOCATION OF END A, END B IN TERMS OF DP(1) THRU DP(6)
!
   dp(1) = Ecpt(jcsida+1)
   dp(2) = Ecpt(jcsida+2)
   dp(3) = Ecpt(jcsida+3)
   dp(4) = Ecpt(jcsidb+1)
   dp(5) = Ecpt(jcsidb+2)
   dp(6) = Ecpt(jcsidb+3)
!
!     DEFINE COMPONENTS OF VECTOR FROM END A TO CENTER OF CURVATURE,C
!
   dp(7) = Ecpt(4)
   dp(8) = Ecpt(5)
   dp(9) = Ecpt(6)
   fld = sqrt(dp(7)**2+dp(8)**2+dp(9)**2)
   dp(7) = dp(7)/fld
   dp(8) = dp(8)/fld
   dp(9) = dp(9)/fld
!
!     DETERMINE IF POINT A AND B ARE IN BASIC COORDINATES
!
   abasic = .TRUE.
   bbasic = .TRUE.
   IF ( icsida/=0 ) abasic = .FALSE.
   IF ( icsidb/=0 ) bbasic = .FALSE.
!
!     COMPUTE THE TRANSFORMATION MATRICES TA AND TB IF NECESSARY
!
   IF ( abasic ) THEN
      vecj(1) = dp(7)
      vecj(2) = dp(8)
      vecj(3) = dp(9)
   ELSE
      CALL transs(Ecpt(jcsida),ta)
      CALL gmmats(ta,3,3,0,dp(7),3,1,0,vecj)
      CALL gmmats(ta,3,3,0,dp(1),3,1,0,dp(14))
      dp(1) = dp(14)
      dp(2) = dp(15)
      dp(3) = dp(16)
   ENDIF
   IF ( .NOT.(bbasic) ) THEN
      CALL transs(Ecpt(jcsidb),tb)
      CALL gmmats(tb,3,3,0,dp(4),3,1,0,dp(14))
      dp(4) = dp(14)
      dp(5) = dp(15)
      dp(6) = dp(16)
   ENDIF
!
!     CONSTRUCT VECTOR FROM A TO B
!
   smalv0(1) = dp(4) - dp(1)
   smalv0(2) = dp(5) - dp(2)
   smalv0(3) = dp(6) - dp(3)
   fll = sqrt(smalv0(1)**2+smalv0(2)**2+smalv0(3)**2)
   smalv0(1) = smalv0(1)/fll
   smalv0(2) = smalv0(2)/fll
   smalv0(3) = smalv0(3)/fll
!
!     COMPUTE THE K VECTOR VECK = SMALV0 X VECJ
!
   veck(1) = smalv0(2)*vecj(3) - smalv0(3)*vecj(2)
   veck(2) = smalv0(3)*vecj(1) - smalv0(1)*vecj(3)
   veck(3) = smalv0(1)*vecj(2) - smalv0(2)*vecj(1)
   fll = sqrt(veck(1)**2+veck(2)**2+veck(3)**2)
   veck(1) = veck(1)/fll
   veck(2) = veck(2)/fll
   veck(3) = veck(3)/fll
!
!     COMPUTE THE I VECTOR  VECI = VECJ X VECK
!
   veci(1) = vecj(2)*veck(3) - vecj(3)*veck(2)
   veci(2) = vecj(3)*veck(1) - vecj(1)*veck(3)
   veci(3) = vecj(1)*veck(2) - vecj(2)*veck(1)
   fll = sqrt(veci(1)**2+veci(2)**2+veci(3)**2)
   veci(1) = veci(1)/fll
   veci(2) = veci(2)/fll
   veci(3) = veci(3)/fll
!
!     SEARCH THE MATERIAL PROPERTIES TABLE FOR E,G AND THE DAMPING
!     CONSTANT.
!
   Matidc = Imatid
   Matflg = 1
   IF ( isop==3 ) Matflg = 12
   Eltemp = Tempel
   CALL mat(Iecpt(1))
   dampc = Gsube
!
!     SET UP INTERMEDIATE VARIABLES FOR ELEMENT STIFFNESS MATRIX
!     CALCULATION
!
   IF ( Kx<1.0E-8 ) Kx = 1.0
   IF ( Ky<1.0E-8 ) Ky = 1.0
   IF ( Kz<1.0E-8 ) Kz = 1.0
   fi1 = I1/Kz
   fi2 = I2/Ky
   fjk = Fj/Kx
!
!
!     THE FOLLOWING CODE WAS TAKEN FROM SAP4 BENDKS ROUTINE FOR A CURVED
!     PIPE ELEMENT
!
!
!     COMPUTE SECTION PROPERTY CONSTANTS
!
   t = dtr(Betar)
   ra = R/(A*E)
   rv1 = K1*R/(2.*G*A)
   rv2 = K2/K1*rv1
   rt = R/(G*fjk*2.)
   rb0 = R/(E*fi2*2.)
   rb1 = R/(E*fi1)
   r2 = R**2
!
!     COMPUTE COMMON TRIGONOMETRIC CONSTANTS
!
   st = sid(Betar)
   ct = cod(Betar)
   s2t = sid(2.0*Betar)
   c2t = cod(2.0*Betar)
!
!     FORM THE NODE FLEXIBILITY MATRIX AT NODE J REFERENCED TO THE
!     LOCAL (X,Y,Z) COORDINATE SYSTEM AT NODE I.
!
!     X - DIRECTION  IN-PLANE TANGENT TO THE BEND AT NODE I AND
!                    DIRECTED TOWARD NODE J
!     Y - DIRECTION  IN-PLANE AND DIRECTED RADIALLY INWARD TO THE
!                    CENTER OF CURVATURE
!     Z - DIRECTION  OUT OF PLANE AND ORTHOGONAL TO X AND Y
!
   DO i = 1 , 6
      DO k = i , 6
         f(i,k) = 0.0
      ENDDO
   ENDDO
!
!     A X I A L
!
   f(1,1) = f(1,1) + 0.25*ra*(2.0*t+s2t)
   f(2,2) = f(2,2) + 0.25*ra*(2.0*t-s2t)
!
!     N O T E   (COEFFICIENT CHANGE)
!
   f(1,2) = f(1,2) + 0.50*ra*st**2
!
!     S H E A R
!
   f(1,1) = f(1,1) + 0.5*rv1*(2.0*t-s2t)
   f(2,2) = f(2,2) + 0.5*rv1*(2.0*t+s2t)
   f(3,3) = f(3,3) + 2.0*rv2*t
!
!     N O T E   (SIGN CHANGE)
!
   f(1,2) = f(1,2) - rv1*st**2
!
!     T O R S I O N
!
   f(3,3) = f(3,3) + 0.5*rt*r2*(6.0*t+s2t-8.0*st)
   f(4,4) = f(4,4) + 0.5*rt*(2.0*t+s2t)
   f(5,5) = f(5,5) + 0.5*rt*(2.0*t-s2t)
   f(3,4) = f(3,4) + rt*R*(st-t*ct)
   f(3,5) = f(3,5) + rt*R*(2.0-2.0*ct-t*st)
   f(4,5) = f(4,5) + 0.5*rt*(1.0-c2t)
!
!     B E N D I N G
!
   f(1,1) = f(1,1) + 0.25*rb1*r2*(2.0*t*(2.0+c2t)-3.0*s2t)
   f(2,2) = f(2,2) + 0.25*rb1*r2*(2.0*t*(2.0-c2t)+3.0*s2t-8.0*st)
   f(3,3) = f(3,3) + 0.50*rb0*r2*(2.0*t-s2t)
   f(4,4) = f(4,4) + 0.50*rb0*(2.0*t-s2t)
   f(5,5) = f(5,5) + 0.50*rb0*(2.0*t+s2t)
   f(6,6) = f(6,6) + rb1*t
   f(1,2) = f(1,2) + 0.25*rb1*r2*(1.0+3.0*c2t+2.0*t*s2t-4.0*ct)
   f(1,6) = f(1,6) - rb1*R*(st-t*ct)
   f(2,6) = f(2,6) + rb1*R*(t*st+ct-1.0)
   f(3,4) = f(3,4) + rb0*R*(st-t*ct)
   f(3,5) = f(3,5) - rb0*R*t*st
   f(4,5) = f(4,5) - 0.50*rb0*(1.0-c2t)
!
!
!     FORM SYMMETRICAL UPPER PART OF FLEX MATRIX
!
   DO i = 1 , 6
      DO k = i , 6
         df(k,i) = f(i,k)
         df(i,k) = df(k,i)
      ENDDO
   ENDDO
!
!
!     INVERT FLEX TO FORM STIFFNESS
!
   CALL invers(6,df,6,dum,0,determ,ising,h)
   IF ( ising==2 ) WRITE (6,99001) f
99001 FORMAT (35H ELBOW STIFFNESS MATRIX IS SINGULAR,/,(5X,6E13.5))
   IF ( ising==2 ) CALL mesage(-30,38,Ecpt(1))
!
!
!     SET UP THE FORCE TRANSFORMATION RELATING REACTIONS AT NODE I
!     ACTING ON THE MEMBER END DUE TO UNIT LOADS APPLIED TO THE MEMBER
!     END AT NODE J.
!
   DO i = 1 , 6
      DO k = 1 , 6
         h(i,k) = 0.0
      ENDDO
   ENDDO
!
   DO k = 1 , 6
      h(k,k) = -1.0
   ENDDO
!
   h(4,3) = -(R*(1.0-ct))
   h(5,3) = (R*st)
   h(6,1) = -h(4,3)
   h(6,2) = -h(5,3)
!
!     FORM THE UPPER TRIANGULAR PORTION OF THE LOCAL ELEMENT STIFFNESS
!     MATRIX FOR THE BEND
!
   DO k = 1 , 6
      DO i = k , 6
         S(k+6,i+6) = df(k,i)
      ENDDO
   ENDDO
!
   DO ir = 1 , 6
      DO ic = 1 , 6
         S(ir,ic+6) = 0.0
         DO in = 1 , 6
            S(ir,ic+6) = S(ir,ic+6) + h(ir,in)*df(in,ic)
         ENDDO
      ENDDO
   ENDDO
!
   DO ir = 1 , 6
      DO ic = ir , 6
         S(ir,ic) = 0.0
         DO in = 1 , 6
            S(ir,ic) = S(ir,ic) + S(ir,in+6)*h(ic,in)
         ENDDO
      ENDDO
   ENDDO
!
!     REFLECT FOR SYMMETRY
!
   DO i = 1 , 12
      DO k = i , 12
         S(k,i) = S(i,k)
      ENDDO
   ENDDO
!
!            E
!     STORE K   IN KEP(1) THRU KEP(36) AND
!            AA
!
!            E
!     STORE K   IN KEP(37) THRU KEP(72)
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
!
!     COMPUTE THERMAL MATRIX
!
   l = dcr*Ecpt(29)*Ecpt(30)
   DO i = 1 , 6
      hut(i) = 0.0
   ENDDO
   alphar = Alpha*R
   hut(1) = -alphar*sid(Betar)
   hut(2) = -alphar*(1.-cod(Betar))
   hut(6) = 0.0
   CALL gmmats(Kep(1),6,6,0,hut,6,1,0,Therm(1))
!
!                                                             T
!     STORE VECI, VECJ, VECK IN KE(1) THRU KE(9) FORMING THE A  MATRIX.
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
   iwbeg = 0
   ikel = 1
   iab = 1
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
!     SET UP THE -G- MATRIX. IG POINTS TO THE BEGINNING OF THE G MATRIX.
!     G = AT X TI
!
      ig = 1
      IF ( .NOT.(basic) ) THEN
         CALL transs(Ecpt(jcsid),Ke(10))
         CALL gmmats(Ke(1),3,3,0,Ke(10),3,3,0,Ke(19))
         ig = 19
      ENDIF
!
!     FORM THE W  MATRIX OR THE W  MATRIX IN KE(37) OR KE(73) DEPENDING
!               A                B
!     UPON WHICH POINT - A OR B - IS UNDER CONSIDERATION.  G WILL BE
!     STORED IN THE UPPER LEFT AND LOWER RIGHT CORNERS.  H, IF NON-ZERO,
!     WILL BE STORED IN THE UPPER RIGHT CORNER.
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
!
!                              E                    E
!     FORM THE PRODUCT  S  =  K   X  W   OR  S   = K    X  W, DEPENDING
!                        A     AA     A       B     AB      B
!     UPON WHICH POINT WE ARE WORKING WITH.
!
      CALL gmmats(Kep(ikel),6,6,0,Ke(iwbeg+37),6,6,0,Sa(iab))
!
!     IF THE POINT UNDER CONSIDERATION IS POINT B WE ARE FINISHED. IF
!     NOT, SET UP POINTS AND INDICATORS FOR WORKING WITH POINT B.
!
      IF ( iwbeg==36 ) THEN
!
!     FILL REMAINDER OF OUTPUT BLOCK.
!
         Jelid = Ielid
         Jsilno(1) = Isilno(1)
         Jsilno(2) = Isilno(2)
         i12 = 0.
         Out(1) = A*E*Alpha
         Out(2) = A*E/l
         Out(3) = A
         Out(4) = Fj
         Out(5) = I1
         Out(6) = I2
         Out(7) = C
         Out(8) = C1
         Out(9) = C2
         Out(10) = D1
         Out(11) = D2
         Out(12) = F1
         Out(13) = F2
         Out(14) = G1
         Out(15) = G2
         Out(16) = Tsub0
         Out(17) = Sigt
         Out(18) = Sigc
         Out(19) = l
         Out(20) = R
         Out(21) = Betar
         EXIT
      ELSE
         basic = bbasic
         jcsid = jcsidb
         iwbeg = 36
         ikel = 37
         iab = 37
         index = Isilno(2)
         DO i = 28 , 36
            Ke(i) = 0.0
         ENDDO
      ENDIF
   ENDDO
END SUBROUTINE selbo1
