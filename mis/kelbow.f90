
SUBROUTINE kelbow
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A , Alpha , Betar , C , C1 , C2 , Clsnrw , Clsrw , Costh , D1 , D2 , Dodet , E , Ecpt(100) , Eltemp , Eor , F1 , F2 , Fe ,  &
      & Fj , Fk , Frowic , G , G1 , G2 , Gpa(3) , Gpb(3) , Gsube , I1 , I2 , K1 , K2 , Kx , Ky , Kz , Nsm , Outrw , R , Rho , Sigc ,&
      & Sigs , Sigt , Sinth , Smallv(3) , Stress , Sysbuf , Tempel , Tnrows , Tsubo , Z(1)
   DOUBLE PRECISION Dela(6) , Delb(6) , Dp(16) , Ke(144) , Kee(12,12) , Kep(144) , S(12,12)
   LOGICAL Heat
   INTEGER I6x64 , I6x6k , Icssv , Icstm , Idetck , Idum1 , Iecpt(100) , Ielid , If4gg , Ifcstm , Ifdit , Ifecpt , Ifgei , Ifgpct , &
         & Ifgpst , Ifkgg , Ifmpt , Ig4gg , Igecpt , Iggei , Iggpct , Iggpst , Igkgg , Igpct , Imatid , Inrw , Iopt4 , Ipoint ,     &
         & Isilno(2) , Iz(1) , Jmax , K4ggsw , Left , Link(10) , Lrowic , Matflg , Matidc , Mcb4gg(7) , Mcbkgg(7) , Mcsida ,        &
         & Mcsidb , N6x64 , N6x6k , Ncstm , Neor , Ngpct , Nlinks , Nogo , Nout , Npoint , Npvt , Nrowsc , Nu
   COMMON /hmtout/ Fk
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , G , Nu , Rho , Alpha , Tsubo , Gsube , Sigt , Sigc , Sigs
   COMMON /sma1bk/ Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6k , N6x6k , I6x64 , N6x64
   COMMON /sma1cl/ Iopt4 , K4ggsw , Npvt , Left , Frowic , Lrowic , Nrowsc , Tnrows , Jmax , Nlinks , Link , Idetck , Dodet , Nogo
   COMMON /sma1dp/ Ke , Kep , Dela , Delb
   COMMON /sma1et/ Ielid , Isilno , Smallv , Icssv , Imatid , A , I1 , I2 , Fj , Nsm , Fe , C1 , C2 , D1 , D2 , F1 , F2 , G1 , G2 , &
                 & K1 , K2 , C , Kx , Ky , Kz , R , Betar , Mcsida , Gpa , Mcsidb , Gpb , Tempel
   COMMON /sma1ht/ Heat
   COMMON /sma1io/ Ifcstm , Ifmpt , Ifdit , Idum1 , Ifecpt , Igecpt , Ifgpct , Iggpct , Ifgei , Iggei , Ifkgg , Igkgg , If4gg ,     &
                 & Ig4gg , Ifgpst , Iggpst , Inrw , Outrw , Clsnrw , Clsrw , Neor , Eor , Mcbkgg , Mcb4gg
   COMMON /system/ Sysbuf , Nout
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   LOGICAL abasic , basic , bbasic
   REAL c2t , ct , dcr , dum , f(6,6) , fi1 , fi2 , fjk , fld , r2 , ra , rb0 , rb1 , rt , rv1 , rv2 , s2t , st , t , x
   REAL cod , dtr , sid
   DOUBLE PRECISION dampc , determ , df(6,6) , fl , fll , h(6,6) , smalv0(6) , ta(18) , tb(9) , veci(3) , vecj(3) , veck(3)
   INTEGER i , ic , icsida , icsidb , ig , ikel , ilim , ilow , in , index , ipass , ipvt , ir , ising , iwbeg , iwork(6,3) , j ,   &
         & jcsid , jcsida , jcsidb , k , lim , low
!
! End of declarations
!
!
!     THIS ROUTINE COMPUTES THE TWO 6 X 6 MATRICES K(NPVT,NPVT) AND
!     K(NPVT,J) FOR A CURVED BAR ELEMENT HAVING END POINTS NUMBERED
!     NPVT AND J
!
!                   ECPT FOR THE ELBOW
!
!     ECPT( 1)  -  IELID         ELEMENT ID. NUMBER
!     ECPT( 2)  -  ISILNO(2)     * SCALAR INDEX NOS. OF THE GRID POINTS
!     ECPT( 3)  -    ...         *
!     ECPT( 4)  -  SMALLV(3)     $ REFERENCE VECTOR
!     ECPT( 5)  -    ...         $
!     ECPT( 6)  -    ...         $
!     ECPT( 7)  -  ICSSV         COOR. SYS. ID FOR SMALLV VECTOR
!     ECPT( 8)  -  IMATID        MATERIAL ID.
!     ECPT( 9)  -  A             CROSS-SECTIONAL AREA
!     ECPT(10)  -  I1            $ AREA MOMENTS OF INERTIA
!     ECPT(11)  -  I2            $
!     ECPT(12)  -  FJ            TORSIONAL CONSTANT
!     ECPT(13)  -  NSM           NON-STRUCTURAL MASS
!     ECPT(14)  -  FE            FORCE ELEM. DESCRIPTIONS, FORCE METHOD
!     ECPT(15)  -  R1            *STRESS RECOVERY COEFFICIENTS
!     ECPT(16)  -  T1            *  RI = RADIAL  LOCATION
!     ECPT(17)  -  R2            *  TI = ANGULAR LOCATION
!     ECPT(18)  -  T2            *       OF STRESS RECOVERY POINTS
!     ECPT(19)  -  R3            *
!     ECPT(20)  -  T3            *
!     ECPT(21)  -  R4            *
!     ECPT(22)  -  T4            *
!     ECPT(23)  -  K1            $  AREA FACTOR FOR SHEAR
!     ECPT(24)  -  K2            $
!     ECPT(25)  -  C             STRESS INTENSIFICATION FACTOR
!     ECPT(26)  -  KX            *  FLEXIBILITY CORRECTION FACTORS
!     ECPT(27)  -  KY            *
!     ECPT(28)  -  KZ            *
!     ECPT(29)  -  R             RADIUS OF CURVATURE
!     ECPT(30)  -  BETAR         ANGLE FROM GA TO GB
!     ECPT(31)  -  MCSIDA        COORD. SYS. ID. FOR GRID POINT A
!     ECPT(32)  -  GPA(3)        *  BASIC COORD. FOR GRID POINT A
!     ECPT(33)  -   ...          *
!     ECPT(34)  -   ...          *
!     ECPT(35)  -  MCSIDB        COORD. SYS. ID. FOR GRID POINT B
!     ECPT(36)  -  GPB(3)        *  BASIC COORD. FOR GRID POINT B
!     ECPT(37)  -   ...          *
!     ECPT(38)  -   ...          *
!     ECPT(39)  -  ELTEMP        AVG. ELEMENT TEMPERATURE
!
!     COMMENTS FROM G.CHAN/UNISYS   7/91
!     ABOUT K1 AND K2, THE AREA FACTORS FOR SHEAR
!
!     THE K1,K2 FOR BAR ARE 0. TO 1.0, AND ARE USED IN K1*G*A AND K2*G*A
!         THE K1,K2 ARE THEREFORE CORRECTION FACTORS FOR STIFFNESS
!     THE K1,K2 ARE USED IN ELBOW IN K1/G*A AND K2/G*A. AND THEREFORE
!         THE K1,K2 ARE COORECTION FACTORS FOR FLEXIBILITY. THE K1,K2
!         IN ELBOW ARE EQUIVALENT TO 1./K1 AND 1./K2 IN BAR ELEMENT.
!         THE PROPER VALUE FOR K1 AND K2 SHOULD BE INFINITY TO 1.0
!
!     IN 1992 COSMIC/NASTRAN, THE USE OF K1 AND K2 IN ELBOW AND BAR
!     ELMENTS ARE SYMCHRONIZED, WITH PROPER VALUES FROM 0. TO 1.0
!     THE K1 AND K2 ARE CHANGED TO 1./K1 AND 1./K2 IN ELBOW ELEMENT
!     SHEAR COMPUTATION. THAT IS, CORRECTION FACTORS FOR STIFFNESS IS
!     USED.
!
!     REFERENCE -  R.J. ROARK: FORMULAS FOR STRESS AND STRAIN,
!     SECTION 35, 'BEAMS FOR RELATIVELY GREAT DEPTH',
!     FOR BEAMS OF SAMLL SPAN/DEPTH RATIO
!
!     K = 1/F = 5/6 FOR RECTANGULAR SECTION
!             = 0.9 FOR SOLID CIRCULAR
!             = 0.5 FOR THIN-WALLED HOOLOW CIRCULAR SECTION
!             = 1.0 CAN BE USED FOR I-BEAM
!
!
!
   EQUIVALENCE (Ielid,Ecpt(1),Iecpt(1)) , (Iz(1),Z(1)) , (ta(10),tb(1)) , (Ecpt(71),Dp(1)) , (Kee(1,1),Ke(1),S(1,1))
   DATA dcr/.017453292/
!
   sid(x) = sin(x*dcr)
   cod(x) = cos(x*dcr)
   dtr(x) = x*dcr
!
!     DETERMINE WHICH POINT IS THE PIVOT POINT.
!
   x = 1.
   ipvt = 1
   IF ( Isilno(1)/=Npvt ) THEN
      ipvt = 2
      IF ( Isilno(2)/=Npvt ) CALL mesage(-30,34,Iecpt(1))
   ENDIF
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
   Dp(1) = Ecpt(jcsida+1)
   Dp(2) = Ecpt(jcsida+2)
   Dp(3) = Ecpt(jcsida+3)
   Dp(4) = Ecpt(jcsidb+1)
   Dp(5) = Ecpt(jcsidb+2)
   Dp(6) = Ecpt(jcsidb+3)
!
!     DEFINE COMPONENTS OF VECTOR FROM END A TO CENTER OF CURVATURE,C
!
   Dp(7) = Ecpt(4)
   Dp(8) = Ecpt(5)
   Dp(9) = Ecpt(6)
   fld = dsqrt(Dp(7)**2+Dp(8)**2+Dp(9)**2)
   IF ( fld<=0.000 ) GOTO 400
   Dp(7) = Dp(7)/fld
   Dp(8) = Dp(8)/fld
   Dp(9) = Dp(9)/fld
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
      vecj(1) = Dp(7)
      vecj(2) = Dp(8)
      vecj(3) = Dp(9)
   ELSE
      CALL transd(Ecpt(jcsida),ta)
      CALL gmmatd(ta,3,3,0,Dp(7),3,1,0,vecj)
      CALL gmmatd(ta,3,3,0,Dp(1),3,1,0,Dp(14))
      Dp(1) = Dp(14)
      Dp(2) = Dp(15)
      Dp(3) = Dp(16)
   ENDIF
   IF ( .NOT.(bbasic) ) THEN
      CALL transd(Ecpt(jcsidb),tb)
      CALL gmmatd(tb,3,3,0,Dp(4),3,1,0,Dp(14))
      Dp(4) = Dp(14)
      Dp(5) = Dp(15)
      Dp(6) = Dp(16)
   ENDIF
!
!     CALCULATE TRUE LENGTH OF ELBOW
!
   fl = dble(R*dtr(Betar))
   IF ( fl==0.0D0 ) GOTO 400
!
!     NOW THAT LENGTH HAS BEEN COMPUTED, BRANCH IF THIS IS A -HEAT-
!     FORMULATION.
!
   IF ( Heat ) THEN
!
!
!     HEAT FORMULATION CONTINUES HERE.  GET MATERIAL PROPERTY -K- FROM
!     HMAT
!
      Matflg = 1
      Matidc = Iecpt(8)
      Eltemp = Ecpt(39)
      CALL hmat(Ielid)
!
      fl = dble(Fk)*dble(Ecpt(9))/(Dp(9)*Dp(10)*dble(dcr))
      IF ( Npvt==Iecpt(3) ) fl = -fl
      DO i = 1 , 2
         CALL sma1b(fl,Iecpt(i+1),Npvt,Ifkgg,0.0D0)
         fl = -fl
      ENDDO
      GOTO 99999
   ELSE
!
!     CONSTRUCT VECTOR FROM A TO B
!
      smalv0(1) = Dp(4) - Dp(1)
      smalv0(2) = Dp(5) - Dp(2)
      smalv0(3) = Dp(6) - Dp(3)
      fll = dsqrt(smalv0(1)**2+smalv0(2)**2+smalv0(3)**2)
      IF ( fll==0.0D0 ) GOTO 400
      smalv0(1) = smalv0(1)/fll
      smalv0(2) = smalv0(2)/fll
      smalv0(3) = smalv0(3)/fll
!
!     COMPUTE THE K VECTOR VECK = SMALV0 X VECJ
!
      veck(1) = smalv0(2)*vecj(3) - smalv0(3)*vecj(2)
      veck(2) = smalv0(3)*vecj(1) - smalv0(1)*vecj(3)
      veck(3) = smalv0(1)*vecj(2) - smalv0(2)*vecj(1)
      fll = dsqrt(veck(1)**2+veck(2)**2+veck(3)**2)
      IF ( fll==0.0D0 ) GOTO 400
      veck(1) = veck(1)/fll
      veck(2) = veck(2)/fll
      veck(3) = veck(3)/fll
!
!     COMPUTE THE I VECTOR  VECI = VECJ X VECK
!
      veci(1) = vecj(2)*veck(3) - vecj(3)*veck(2)
      veci(2) = vecj(3)*veck(1) - vecj(1)*veck(3)
      veci(3) = vecj(1)*veck(2) - vecj(2)*veck(1)
      fll = dsqrt(veci(1)**2+veci(2)**2+veci(3)**2)
      IF ( fll==0.0D0 ) GOTO 400
      veci(1) = veci(1)/fll
      veci(2) = veci(2)/fll
      veci(3) = veci(3)/fll
!
!     SEARCH THE MATERIAL PROPERTIES TABLE FOR E,G AND THE DAMPING
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
      IF ( Kx<1.0E-8 ) Kx = 1.0
      IF ( Ky<1.0E-8 ) Ky = 1.0
      IF ( Kz<1.0E-8 ) Kz = 1.0
      fi1 = I1/Kz
      fi2 = I2/Ky
      fjk = Fj/Kx
!
!     AREA FACTORS FOR SHEAR ARE FROM NEAR ZERO TO ONE
!
      IF ( K1<1.0E-8 ) K1 = 1.0
      IF ( K2<1.0E-8 ) K2 = 1.0
      IF ( K1>1.0 ) K1 = 1.0/K1
      IF ( K2>1.0 ) K2 = 1.0/K2
!
!     THE FOLLOWING CODE WAS TAKEN FROM SAP4 BENDKS ROUTINE
!     FOR A CURVED PIPE ELEMENT
!
!     COMPUTE SECTION PROPERTY CONSTANTS
!
      t = dtr(Betar)
      ra = R/(A*E)
      rv1 = R/(2.*K1*G*A)
      rv2 = K1/K2*rv1
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
!     X - DIRECTION...  IN-PLANE TANGENT TO THE BEND AT NODE I AND
!                       DIRECTED TOWARD NODE J
!     Y - DIRECTION...  IN-PLANE AND DIRECTED RADIALLY INWARD TO THE
!                       CENTER OF CURVATURE
!     Z - DIRECTION...  OUT OF PLANE AND ORTHOGONAL TO X AND Y
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
            df(k,i) = dble(f(i,k))
            df(i,k) = df(k,i)
         ENDDO
      ENDDO
!
!     WRITE (6,4005) DF
!
!     INVERT FLEX TO FORM STIFFNESS
!
      CALL inverd(6,df,6,dum,0,determ,ising,iwork)
      IF ( ising==2 ) WRITE (6,99001) f
99001 FORMAT (1X,34HELBOW STIFFNESS MATRIX IS SINGULAR,/,(5X,6E13.5))
      IF ( ising==2 ) CALL mesage(-30,38,Ecpt(1))
!
!
!     SET UP THE FORCE TRANSFORMATION RELATING REACTIONS AT NODE I
!     ACTING ON THE MEMBER END DUE TO UNIT LOADS APPLIED TO THE MEMBER
!     END AT NODE J.
!
      DO i = 1 , 6
         DO k = 1 , 6
            h(i,k) = 0.0D0
         ENDDO
      ENDDO
!
      DO k = 1 , 6
         h(k,k) = -1.0D0
      ENDDO
!
      h(4,3) = -dble(R*(1.0-ct))
      h(5,3) = dble(R*st)
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
            S(ir,ic+6) = 0.0D0
            DO in = 1 , 6
               S(ir,ic+6) = S(ir,ic+6) + h(ir,in)*df(in,ic)
            ENDDO
         ENDDO
      ENDDO
!
      DO ir = 1 , 6
         DO ic = ir , 6
            S(ir,ic) = 0.0D0
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
!     ZERO OUT THE ARRAY WHERE THE 3 X 3 MATRIX H AND THE W  AND W
!     6 X 6  MATRICES WILL RESIDE.                         A      B
!
      DO i = 28 , 108
         Ke(i) = 0.0D0
      ENDDO
      ipass = 1
      iwbeg = 0
!
!     SET UP POINTERS
!
      IF ( ipvt/=1 ) GOTO 200
   ENDIF
 100  basic = abasic
   jcsid = jcsida
   ikel = 1
   index = Isilno(1)
   GOTO 300
 200  basic = bbasic
   jcsid = jcsidb
   ikel = 37
   index = Isilno(2)
!
!     SET UP THE -G- MATRIX.  IG POINTS TO THE BEGINNING OF THE G
!     MATRIX. G = AT X TI
!
 300  ig = 1
   IF ( .NOT.(basic) ) THEN
      CALL transd(Ecpt(jcsid),Ke(10))
      CALL gmmatd(Ke(1),3,3,0,Ke(10),3,3,0,Ke(19))
      ig = 19
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
!     INSERT THIS 6 X 6
!
   CALL sma1b(Kep(109),index,-1,Ifkgg,0.0D0)
   IF ( Iopt4/=0 .AND. Gsube/=0.0 ) THEN
      K4ggsw = 1
      CALL sma1b(Kep(109),index,-1,If4gg,dampc)
   ENDIF
!
!     IF IPASS = 2, WE ARE DONE.  OTHERWISE COMPUTE THE OFF-DIAGONAL
!     6 X 6.
!
   IF ( ipass==2 ) THEN
      RETURN
   ELSE
      iwbeg = 36
      ipass = 2
      DO i = 28 , 36
         Ke(i) = 0.0D0
      ENDDO
      IF ( ipvt==1 ) GOTO 200
      GOTO 100
   ENDIF
!
 400  CALL mesage(30,26,Iecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
   Nogo = 1
   RETURN
99999 RETURN
END SUBROUTINE kelbow
