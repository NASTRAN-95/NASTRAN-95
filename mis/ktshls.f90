
SUBROUTINE ktshls
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alf(3) , Balotr(36) , C1 , C10 , C2 , C3 , C4 , C5 , C6 , C7 , C8 , C9 , Cab(3) , Cc(10) , Costh , Csub(5,5) , Csubt(6,5) , &
      & Dumy(12) , Ee(30) , Eltemp , Em(6) , Est(100) , F(14,14) , Fac , Gsube , Ivect(3) , Jvect(3) , Ksub(6,6) , Ksubt(6,6) ,     &
      & Ksup(36) , Ksupt(36) , Kvect(3) , Pla34 , Q(6,6) , Rhoy , Rj11 , Rj12 , Rj22 , Sigcy , Sigsy , Sigty , Sinth , Trand(9) ,   &
      & Tref , Xc(6) , Yc(6) , Zc(6)
   INTEGER Dict(15) , Elid , Eltype , Estid , Ics(6) , Iest(42) , Ind(6,3) , Index(20,3) , Ioutpt , Iprec , Ixtra , Izr , Kdummy(22)&
         & , Kmbgg(3) , Knogo , Ksystm(65) , Ldict , Matflg , Matid , Nl(6) , Nlocs , Nob , Nok , Nom , Nzr , Save(6) , Sil(6) ,    &
         & Small(6)
   LOGICAL Nogo
   CHARACTER*23 Ufm
   COMMON /blank / Nok , Nom , Nob
   COMMON /emgdic/ Eltype , Ldict , Nlocs , Elid , Estid
   COMMON /emgest/ Est
   COMMON /emgprm/ Ixtra , Izr , Nzr , Dumy , Kmbgg , Iprec , Nogo
   COMMON /matin / Matid , Matflg , Eltemp , Pla34 , Sinth , Costh
   COMMON /matout/ Em , Rhoy , Alf , Tref , Gsube , Sigty , Sigcy , Sigsy , Rj11 , Rj12 , Rj22
   COMMON /sma1cl/ Kdummy , Knogo
   COMMON /sma1dp/ F , Q , Ee , Csubt , Csub
   COMMON /sma2dp/ Trand , Balotr , Ksub , Ksubt , Fac , Xc , Yc , Zc , Ivect , Jvect , Kvect , Cc , Cab , Dict , Sil , Save ,      &
                 & Small , Index , Ics , Nl
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm
!
! Local variable declarations
!
   REAL a , a1 , a1sq , a2 , a2sq , a3 , a3sq , amass , area , b , blank , c , cm1(30,30) , cms(900) , cmt(1296) , ctm(36,36) ,     &
      & d11 , d12 , d13 , d132 , d22 , d23 , d232 , d33 , d334 , degra , determ , dista , distb , distc , g11 , g12 , g13 , g22 ,   &
      & g23 , g33 , h4 , h5 , h6 , j11 , j12 , j22 , kshl(1024) , mshl(1024) , nsm , qks(960) , qqq(20,20) , qqqinv(360) , rho ,    &
      & rix , riy , rjx , rjy , rkx , rky , rlx , rly , rmnx , rmny , rmx , rmx1 , rmy , rmy1 , rnx , rnx1 , rny , rny1 , s11 ,     &
      & s13 , s22 , s23 , s33 , sb1 , sb10 , sb11 , sb12 , sb13 , sb14 , sb15 , sb16
   INTEGER i , i1 , i2 , i3 , icode , idele , ii , ij , ijk , ipass , ising , ism , ismall , ix , ix0 , ix01 , ix011 , ix1 , ixiy0 ,&
         & ixky0 , ixky1 , ixky2 , ixmyr , ixmyr1 , ixp1 , ixr01 , ixr1 , iy , iykx0 , iykx1 , iykx2 , iymxr , j , j1 , ji , jj ,   &
         & jx , jx0 , jx01 , jx011 , jx1 , jxjy0 , jxly0 , jxly1 , jxly2 , jxnys , jxnys1 , jxp1 , jxs01 , jxs1 , jy , jylx0 ,      &
         & jylx1 , jylx2 , jynxs1 , k , k1 , k2 , kx , kx0 , kx01 , kx1 , kxky0 , kxmyr , kxmyr1 , kxp1 , kxr01 , kxr1 , ky , l ,   &
         & l1 , lx , lx0 , lx01 , lx1 , lxly0 , lxnys , lxnys1 , lxp1 , lxs01
   LOGICAL imass , nots , uniben , unimem
   INTEGER lxs1 , ly , m , matid1 , matid2 , matid3 , mx , mx0 , mx01 , mx011 , mx0x , mx1 , mx1x , mx2 , mx2x , mx3 , mx3x ,       &
         & mxiyr , mxiyr1 , mxkyr , mxkyr1 , mxmyr1 , mxp1 , my , my1 , my1x , name(2) , ndof , nsq , nx , nx0 , nx01 , nx011 ,     &
         & nx0y , nx1 , nx1y , nx2 , nx2y , nx3 , nx3y , nxjys , nxjys1 , nxlys , nxlys1 , nxnys1 , nxp1 , ny , ny1 , ny1y , rk(3) ,&
         & sil1 , sil2 , sk(3) , xthk(10) , xu(32) , xv(32) , xw(32) , ythk(10) , yu(32) , yv(32) , yw(32)
   REAL sb17 , sb18 , sb19 , sb2 , sb20 , sb21 , sb22 , sb23 , sb24 , sb25 , sb26 , sb27 , sb28 , sb29 , sb3 , sb30 , sb31 , sb32 , &
      & sb33 , sb34 , sb35 , sb36 , sb37 , sb38 , sb39 , sb4 , sb40 , sb41 , sb5 , sb6 , sb7 , sb8 , sb9 , st , st1 , st11 , st121 ,&
      & st122 , st131 , st132 , st133 , st22 , st231 , st232 , st233 , st331 , st332 , tbend1 , tbend3 , tbend5 , theta1 , thetam , &
      & thk1 , thk2 , thk3 , tmem1 , tmem3 , tmem5 , tshr , tshr1 , tshr3 , tshr5 , vol
!
! End of declarations
!
!
!     ECPT ENTRIES
!
!     ECPT( 1) = ELEMENT ID                                     INTEGER
!     ECPT( 2) = SCALAR INDEX NUMBER FOR GRID POINT 1           INTEGER
!     ECPT( 3) = SCALAR INDEX NUMBER FOR GRID POINT 2           INTEGER
!     ECPT( 4) = SCALAR INDEX NUMBER FOR GRID POINT 3           INTEGER
!     ECPT( 5) = SCALAR INDEX NUMBER FOR GRID POINT 4           INTEGER
!     ECPT( 6) = SCALAR INDEX NUMBER FOR GRID POINT 5           INTEGER
!     ECPT( 7) = SCALAR INDEX NUMBER FOR GRID POINT 6           INTEGER
!     ECPT( 8) = THETA                                          REAL
!     ECPT( 9) = MATERIAL ID 1                                  INTEGER
!     ECPT(10) = THICKNESS T1 AT GRID POINT G1
!     ECPT(11) = THICKNESS T3 AT GRID POINT G3
!     ECPT(12) = THICKNESS T5 AT GRID POINT G5
!     ECPT(13) = MATERIAL ID 2                                  INTEGER
!     ECPT(14) = THICKNESS TBEND1 FOR BENDING AT GRID POINT G1
!     ECPT(15) = THICKNESS TBEND3 FOR BENDING AT GRID POINT G3
!     ECPT(16) = THICKNESS TBEND5 FOR BENDING AT GRID POINT G5
!     ECPT(17) = MATERIAL ID 3                                  INTEGER
!     ECPT(18) = THICKNESS TSHR1 FOR TRANSVERSE SHEAR AT GRID POINT G1
!     ECPT(19) = THICKNESS TSHR3 FOR TRANSVERSE SHEAR AT GRID POINT G3
!     ECPT(20) = THICKNESS TSHR5 FOR TRANSVERSE SHEAR AT GRID POINT G5
!     ECPT(21) = NON-STRUCTURAL MASS                            REAL
!     ECPT(22) = DISTANCE Z11 FOR STRESS CALCULATION  AT GRID POINT G1
!     ECPT(23) = DISTANCE Z21 FOR STRESS CALCULATION  AT GRID POINT G1
!     ECPT(24) = DISTANCE Z13 FOR STRESS CALCULATION  AT GRID POINT G3
!     ECPT(25) = DISTANCE Z23 FOR STRESS CALCULATION  AT GRID POINT G3
!     ECPT(26) = DISTANCE Z15 FOR STRESS CALCULATION  AT GRID POINT G5
!     ECPT(27) = DISTANCE Z25 FOR STRESS CALCULATION  AT GRID POINT G5
!
!     X1,Y1,Z1 FOR ALL SIX POINTS ARE  IN NASTRAN BASIC SYSTEM
!
!     ECPT(28) = COORDINATE SYSTEM ID FOR GRID A                INTEGER
!     ECPT(29) = COORDINATE X1                                  REAL
!     ECPT(30) = COORDINATE Y1                                  REAL
!     ECPT(31) = COORDINATE Z1                                  REAL
!     ECPT(32) = COORDINATE SYSTEM ID FOR GRID B                INTEGER
!     ECPT(33) = COORDINATE X1                                  REAL
!     ECPT(34) = COORDINATE Y1                                  REAL
!     ECPT(35) = COORDINATE Z1                                  REAL
!     ECPT(36) = COORDINATE SYSTEM ID FOR GRID C                INTEGER
!     ECPT(37) = COORDINATE X1                                  REAL
!     ECPT(38) = COORDINATE Y1                                  REAL
!     ECPT(39) = COORDINATE Z1                                  REAL
!     ECPT(40) = COORDINATE SYSTEM ID FOR GRID D                INTEGER
!     ECPT(41) = COORDINATE X1                                  REAL
!     ECPT(42) = COORDINATE Y1                                  REAL
!     ECPT(43) = COORDINATE Z1                                  REAL
!     ECPT(44) = COORDINATE SYSTEM ID FOR GRID E                INTEGER
!     ECPT(45) = COORDINATE X1                                  REAL
!     ECPT(46) = COORDINATE Y1                                  REAL
!     ECPT(47) = COORDINATE Z1                                  REAL
!     ECPT(48) = COORDINATE SYSTEM ID FOR GRID F                INTEGER
!     ECPT(49) = COORDINATE X1                                  REAL
!     ECPT(50) = COORDINATE Y1                                  REAL
!     ECPT(51) = COORDINATE Z1                                  REAL
!     EST (52) = ELEMENT TEMPERATURE
!
!                     RK AND SK ARE EXPONENTS IN THICKNESS VARIATION
!
!     SMA1 WORKING STORAGE
!
!     EQUIVALENCE IECPT WITH ECPT IN COMMON BLOCK /SMA1ET/ SINCE ECPT IS
!     A MIXED INTEGER AND REAL ARRAY
!
   EQUIVALENCE (C1,Cc(1)) , (C2,Cc(2)) , (C3,Cc(3)) , (C4,Cc(4)) , (C5,Cc(5)) , (C6,Cc(6)) , (C7,Cc(7)) , (C8,Cc(8)) , (C9,Cc(9)) , &
    & (C10,Cc(10)) , (Ksub(1,1),Ksup(1)) , (Ksubt(1,1),Ksupt(1)) , (cmt(1),ctm(1,1)) , (qks(1),cmt(1025))
   EQUIVALENCE (a,dista) , (b,distb) , (c,distc) , (Iest(1),Est(1))
   EQUIVALENCE (cmt(1),kshl(1),mshl(1),qqq(1,1))
   EQUIVALENCE (Ksystm(2),Ioutpt)
   EQUIVALENCE (thk1,tbend1) , (thk2,tbend3) , (thk3,tbend5)
   EQUIVALENCE (cm1(1,1),cms(1)) , (Ind(1,1),Index(1,1))
   DATA xu/0 , 1 , 0 , 2 , 1 , 0 , 26*0/ , yu/0 , 0 , 1 , 0 , 1 , 2 , 26*0/ , xv/6*0 , 0 , 1 , 0 , 2 , 1 , 0 , 20*0/ , yv/6*0 , 0 , &
      & 0 , 1 , 0 , 1 , 2 , 20*0/ , xw/12*0 , 0 , 1 , 0 , 2 , 1 , 0 , 3 , 2 , 1 , 0 , 4 , 3 , 2 , 1 , 0 , 5 , 3 , 2 , 1 ,           &
      & 0/yw/12*0 , 0 , 0 , 1 , 0 , 1 , 2 , 0 , 1 , 2 , 3 , 0 , 1 , 2 , 3 , 4 , 0 , 2 , 3 , 4 , 5/
   DATA blank , name/4H     , 4HTRSH , 4HL   /
   DATA rk/0 , 1 , 0/
   DATA sk/0 , 0 , 1/
   DATA degra/0.0174532925/
   DATA xthk/0 , 1 , 0 , 2 , 1 , 0 , 3 , 2 , 1 , 0/
   DATA ythk/0 , 0 , 1 , 0 , 1 , 2 , 0 , 1 , 2 , 3/
!
   Dict(1) = Estid
!
!     COMPONENT CODE,ICODE,IS  111111  AND HAS A VALUE OF 63
!
   icode = 63
   ndof = 36
   nsq = ndof**2
   Dict(2) = 1
   Dict(3) = ndof
   Dict(4) = icode
   Dict(5) = Gsube
   nots = .FALSE.
   imass = .FALSE.
   IF ( Nom>0 ) imass = .TRUE.
   ipass = 1
   idele = Iest(1)
   DO i = 1 , 6
      Nl(i) = Iest(i+1)
   ENDDO
   thetam = Est(8)
   matid1 = Iest(9)
   tmem1 = Est(10)
   tmem3 = Est(11)
   tmem5 = Est(12)
   matid2 = Iest(13)
   tbend1 = (Est(14)*12.0)**0.3333333333
   tbend3 = (Est(15)*12.0)**0.3333333333
   tbend5 = (Est(16)*12.0)**0.3333333333
   matid3 = Iest(17)
   tshr1 = Est(18)
   tshr3 = Est(19)
   tshr5 = Est(20)
   nsm = Est(21)
   j = 0
   DO i = 28 , 48 , 4
      j = j + 1
      Ics(j) = Iest(i)
      Xc(j) = Est(i+1)
      Yc(j) = Est(i+2)
      Zc(j) = Est(i+3)
   ENDDO
!
!     IF TMEM3 OR TMEM5 EQUAL TO ZERO OR BLANK, THEY WILL BE
!     SET EQUAL TO TMEM1 SO ALSO FOR TSHR3,TSHR5,TBEND3 AND TBEND5
!
   IF ( tmem3==0.0 .OR. tmem3==blank ) tmem3 = tmem1
   IF ( tmem5==0.0 .OR. tmem5==blank ) tmem5 = tmem1
   IF ( tshr3==0.0 .OR. tshr3==blank ) tshr3 = tshr1
   IF ( tshr5==0.0 .OR. tshr5==blank ) tshr5 = tshr1
   IF ( tshr1==0.0 ) nots = .TRUE.
   tshr = (tshr1+tshr3+tshr5)/3.0
   IF ( tbend3==0.0 .OR. tbend3==blank ) tbend3 = tbend1
   IF ( tbend5==0.0 .OR. tbend5==blank ) tbend5 = tbend1
   Eltemp = Est(52)
   theta1 = thetam*degra
   Sinth = sin(theta1)
   Costh = cos(theta1)
   IF ( abs(Sinth)<=1.0E-06 ) Sinth = 0.0
!
!     EVALUTE MATERIAL PROPERTIES
!
   Matflg = 2
   Matid = matid1
   IF ( matid1/=0 ) THEN
      CALL mat(idele)
!
      g11 = Em(1)
      g12 = Em(2)
      g13 = Em(3)
      g22 = Em(4)
      g23 = Em(5)
      g33 = Em(6)
   ENDIF
   Matflg = 2
   Matid = matid2
   IF ( matid2/=0 ) THEN
      CALL mat(idele)
      d11 = Em(1)
      d12 = Em(2)
      d13 = Em(3)
      d22 = Em(4)
      d23 = Em(5)
      d33 = Em(6)
      j11 = 0.0
      j12 = 0.0
      j22 = 0.0
      IF ( .NOT.(nots) ) THEN
         Matflg = 3
         Matid = matid3
         CALL mat(idele)
         j11 = 1.0/(Rj11*tshr)
         j12 = 0.0
         j22 = 1.0/(Rj22*tshr)
      ENDIF
   ENDIF
!
!     CALCULATIONS FOR THE TRIANGLE
!
   CALL trif(Xc,Yc,Zc,Ivect,Jvect,Kvect,a,b,c,Iest(1),name)
!
!     COMPUTE THE AREA INTEGRATION FUNCTION F
!
   CALL af(F,14,a,b,c,0,0,0,0,0,0,-1)
!
!     CALCULATIONS FOR QMATRIX (QQQ) AND ITS INVERSE
!
   DO i = 1 , 20
      DO j = 1 , 20
         qqq(i,j) = 0.0
      ENDDO
   ENDDO
   DO i = 1 , 6
      i1 = (i-1)*3 + 1
      i2 = (i-1)*3 + 2
      i3 = (i-1)*3 + 3
      qqq(i1,1) = 1.0
      qqq(i1,2) = Xc(i)
      qqq(i1,3) = Yc(i)
      qqq(i1,4) = Xc(i)*Xc(i)
      qqq(i1,5) = Xc(i)*Yc(i)
      qqq(i1,6) = Yc(i)*Yc(i)
      qqq(i1,7) = qqq(i1,4)*Xc(i)
      qqq(i1,8) = qqq(i1,4)*Yc(i)
      qqq(i1,9) = qqq(i1,5)*Yc(i)
      qqq(i1,10) = qqq(i1,6)*Yc(i)
      qqq(i1,11) = qqq(i1,7)*Xc(i)
      qqq(i1,12) = qqq(i1,7)*Yc(i)
      qqq(i1,13) = qqq(i1,8)*Yc(i)
      qqq(i1,14) = qqq(i1,9)*Yc(i)
      qqq(i1,15) = qqq(i1,10)*Yc(i)
      qqq(i1,16) = qqq(i1,11)*Xc(i)
      qqq(i1,17) = qqq(i1,12)*Yc(i)
      qqq(i1,18) = qqq(i1,13)*Yc(i)
      qqq(i1,19) = qqq(i1,14)*Yc(i)
      qqq(i1,20) = qqq(i1,15)*Yc(i)
      qqq(i2,3) = 1.0
      qqq(i2,5) = Xc(i)
      qqq(i2,6) = Yc(i)*2.0
      qqq(i2,8) = qqq(i1,4)
      qqq(i2,9) = qqq(i1,5)*2.0
      qqq(i2,10) = qqq(i1,6)*3.0
      qqq(i2,12) = qqq(i1,7)
      qqq(i2,13) = qqq(i1,8)*2.0
      qqq(i2,14) = qqq(i1,9)*3.0
      qqq(i2,15) = qqq(i1,10)*4.0
      qqq(i2,17) = qqq(i1,12)*2.0
      qqq(i2,18) = qqq(i1,13)*3.0
      qqq(i2,19) = qqq(i1,14)*4.0
      qqq(i2,20) = qqq(i1,15)*5.0
      qqq(i3,2) = -1.0
      qqq(i3,4) = -2.0*Xc(i)
      qqq(i3,5) = -Yc(i)
      qqq(i3,7) = -qqq(i1,4)*3.0
      qqq(i3,8) = -qqq(i1,5)*2.0
      qqq(i3,9) = -qqq(i1,6)
      qqq(i3,11) = -qqq(i1,7)*4.0
      qqq(i3,12) = -qqq(i1,8)*3.0
      qqq(i3,13) = -qqq(i1,9)*2.0
      qqq(i3,14) = -qqq(i1,10)
      qqq(i3,16) = -qqq(i1,11)*5.0
      qqq(i3,17) = -qqq(i1,13)*3.0
      qqq(i3,18) = -qqq(i1,14)*2.0
      qqq(i3,19) = -qqq(i1,15)
   ENDDO
   qqq(19,16) = 5.0*a**4*c
   qqq(19,17) = 3.0*a**2*c**3 - 2.0*a**4*c
   qqq(19,18) = -2.0*a*c**4 + 3.0*a**3*c**2
   qqq(19,19) = c**5 - 4.0*a**2*c**3
   qqq(19,20) = 5.0*a*c**4
   qqq(20,16) = 5.0*b**4*c
   qqq(20,17) = 3.0*b**2*c**3 - 2.0*b**4*c
   qqq(20,18) = 2.0*b*c**4 - 3.0*b**3*c**2
   qqq(20,19) = c**5 - 4.0*b**2*c**3
   qqq(20,20) = -5.0*b*c**4
   DO i = 1 , 6
      DO j = 1 , 6
         i1 = (i-1)*3 + 1
         Q(i,j) = qqq(i1,j)
      ENDDO
   ENDDO
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
   ising = -1
   CALL invers(6,Q,6,Balotr(1),0,determ,ising,Ind)
   IF ( ising==2 ) GOTO 400
!
!     FOURTH ARGUMENT IS A DUMMY LOCATION FOR INVERSE AND HENCE TS1(1)
!     IS U
!
   ising = -1
   CALL invers(20,qqq,20,Balotr(1),0,determ,ising,Index)
!
!     ISING EQUAL TO 2 IMPLIES THAT QQQ IS SINGULAR
!
   IF ( ising==2 ) GOTO 400
!
!     FIRST 18 COLUMNS OF QQQ INVERSE IS THE QQQINV FOR USE IN STIFFNESS
!     CALCULATIONS
!
!
   DO i = 1 , 20
      DO j = 1 , 18
         ijk = (i-1)*18 + j
         qqqinv(ijk) = qqq(i,j)
      ENDDO
   ENDDO
!
!     START EXECUTION FOR STIFFNESS MATRIX CALCULATION
!
!     CM IS STIFFNESS MATRIX IN ELEMENT COORDINATES
!
!
!     EVALUATE THE CONSTANTS C1,C2,AND C3 IN THE LINEAR EQUATION FOR
!     THICKNESS VARIATION - MEMBRANE
!
 100  CALL af(F,14,a,b,c,C1,C2,C3,tmem1,tmem3,tmem5,1)
   Cab(1) = C1
   Cab(2) = C2
   Cab(3) = C3
   area = F(1,1)
   vol = C1*F(1,1) + C2*F(2,1) + C3*F(1,2)
!
!
   d334 = d33*4.0
   d132 = d13*2.0
   d232 = d23*2.0
!
!     A1,A2,A3 ARE THE COEFFICIENTS OF LINEAR EQUATION FOR VARIATION
!     OF BENDING THICKNESSES
!
   CALL af(F,14,a,b,c,a1,a2,a3,thk1,thk2,thk3,1)
   unimem = .FALSE.
   uniben = .FALSE.
   IF ( abs(C2)<=1.0E-06 .AND. abs(C3)<=1.0E-06 ) unimem = .TRUE.
   IF ( abs(a2)<=1.0E-06 .AND. abs(a3)<=1.0E-06 ) uniben = .TRUE.
   a1sq = a1*a1
   a2sq = a2*a2
   a3sq = a3*a3
   C1 = a1sq*a1
   C2 = 3.0*a1sq*a2
   C3 = 3.0*a1sq*a3
   C4 = 3.0*a1*a2sq
   C5 = 6.0*a1*a2*a3
   C6 = 3.0*a3sq*a1
   C7 = a2sq*a2
   C8 = 3.0*a2sq*a3
   C9 = 3.0*a2*a3sq
   C10 = a3*a3sq
!
!     AA1, AA2, AA3  ARE COEFFICIENTS IN THICKNESS VARIATION FOR
!     TRANSVERSE SHEAR
!
!
!    (POSSIBLY AN ERROR HERE - AA1,AA2, AND AA3 ARE NOT USED IN PROGRAM)
!     CALL AF (F,14,A,B,C,AA1,AA2,AA3,TSHR1,TSHR3,TSHR5,1)
!
   h4 = Q(4,1)*Zc(1) + Q(4,2)*Zc(2) + Q(4,3)*Zc(3) + Q(4,4)*Zc(4) + Q(4,5)*Zc(5) + Q(4,6)*Zc(6)
   h5 = Q(5,1)*Zc(1) + Q(5,2)*Zc(2) + Q(5,3)*Zc(3) + Q(5,4)*Zc(4) + Q(5,5)*Zc(5) + Q(5,6)*Zc(6)
   h6 = Q(6,1)*Zc(1) + Q(6,2)*Zc(2) + Q(6,3)*Zc(3) + Q(6,4)*Zc(4) + Q(6,5)*Zc(5) + Q(6,6)*Zc(6)
   h4 = h4*2.0
   h6 = h6*2.0
!
!     H5 IS MULTIPLIED BY 2.0, SO THAT EXY=DU/DY + DV/DX - ZXY*W
!
   h5 = h5*2.0
!
   DO i = 1 , 32
      ix = xu(i)
      rix = ix
      jx = yu(i)
      rjx = jx
      kx = xv(i)
      rkx = kx
      lx = yv(i)
      rlx = lx
      mx = xw(i)
      rmx = mx
      nx = yw(i)
      rnx = nx
      rmnx = rmx*rnx
      rmx1 = rmx*(rmx-1.0)
      rnx1 = rnx*(rnx-1.0)
      ixp1 = ix + 1
      jxp1 = jx + 1
      kxp1 = kx + 1
      lxp1 = lx + 1
      mxp1 = mx + 1
      nxp1 = nx + 1
      DO j = i , 32
         ij = (i-1)*32 + j
         ji = (j-1)*32 + i
         iy = xu(j)
         riy = iy
         jy = yu(j)
         rjy = jy
         ky = xv(j)
         rky = ky
         ly = yv(j)
         rly = ly
         my = xw(j)
         rmy = my
         ny = yw(j)
         rny = ny
         rmny = rmy*rny
         rmy1 = rmy*(rmy-1.0)
         rny1 = rny*(rny-1.0)
         mx0 = mx + my
         mx1 = mx + my - 1
         mx2 = mx + my - 2
         mx3 = mx + my - 3
         nx0 = nx + ny
         nx1 = nx + ny - 1
         nx2 = nx + ny - 2
         nx3 = nx + ny - 3
         my1 = mx + my + 1
         ny1 = nx + ny + 1
         ix0 = ix + iy
         ix1 = ix0 - 1
         ix01 = ix0 + 1
         jx0 = jx + jy
         jx1 = jx0 - 1
         jx01 = jx0 + 1
         kx0 = kx + ky
         kx1 = kx0 - 1
         kx01 = kx0 + 1
         lx0 = lx + ly
         lx1 = lx0 - 1
         lx01 = lx0 + 1
         IF ( ipass==1 ) THEN
            st = 0.0
            IF ( i>12 .OR. j<=12 ) THEN
               IF ( i>12 ) THEN
                  st = 0.0
                  DO k = 1 , 10
                     mx3x = mx3 + xthk(k)
                     ny1y = ny1 + ythk(k)
                     my1x = my1 + xthk(k)
                     nx3y = nx3 + ythk(k)
                     mx1x = mx1 + xthk(k)
                     nx1y = nx1 + ythk(k)
                     mx2x = mx2 + xthk(k)
                     nx0y = nx0 + ythk(k)
                     mx0x = mx0 + xthk(k)
                     nx2y = nx2 + ythk(k)
                     s11 = 0.0
                     s22 = 0.0
                     s33 = 0.0
                     s13 = 0.0
                     s23 = 0.0
                     IF ( mx3x>0 ) s11 = d11*rmx1*rmy1*Cc(k)*F(mx3x,ny1y)
                     IF ( nx3y>0 ) s22 = d22*rnx1*rny1*Cc(k)*F(my1x,nx3y)
                     IF ( mx1x>0 .AND. nx1y>0 ) s33 = (d334*rmnx*rmny+d12*(rmx1*rny1+rmy1*rnx1))*Cc(k)*F(mx1x,nx1y)
                     IF ( mx2x>0 .AND. nx0y>0 ) s13 = d132*(rmx1*rmny+rmnx*rmy1)*Cc(k)*F(mx2x,nx0y)
                     IF ( mx0x>0 .AND. nx2y>0 ) s23 = d232*(rmnx*rny1+rnx1*rmny)*Cc(k)*F(mx0x,nx2y)
                     st = st + (s11+s22+s33+s13+s23)/12.0
                     IF ( uniben ) EXIT
                  ENDDO
               ELSE
                  DO k = 1 , 3
                     ixr1 = ix1 + rk(k)
                     jxs01 = jx01 + sk(k)
                     lxs1 = lx1 + sk(k)
                     kxr01 = kx01 + rk(k)
                     ixr01 = ix01 + rk(k)
                     jxs1 = jx1 + sk(k)
                     kxr1 = kx1 + rk(k)
                     lxs01 = lx01 + sk(k)
                     iykx1 = iy + kx + rk(k)
                     jylx1 = jy + lx + sk(k)
                     ixky1 = ix + ky + rk(k)
                     jxly1 = jx + ly + sk(k)
                     ixiy0 = ix + iy + rk(k)
                     jxjy0 = jx + jy + sk(k)
                     iykx2 = iykx1 - 1
                     jylx0 = jylx1 + 1
                     ixky2 = ixky1 - 1
                     jxly0 = jxly1 + 1
                     kxky0 = kx + ky + rk(k)
                     lxly0 = lx + ly + sk(k)
                     ixky0 = ix + ky + rk(k) + 1
                     jxly2 = jxly1 - 1
                     iykx0 = iy + kx + rk(k) + 1
                     jylx2 = jylx1 - 1
                     st11 = 0.0
                     st22 = 0.0
                     st331 = 0.0
                     st332 = 0.0
                     st121 = 0.0
                     st122 = 0.0
                     st131 = 0.0
                     st132 = 0.0
                     st133 = 0.0
                     st231 = 0.0
                     st232 = 0.0
                     st233 = 0.0
                     IF ( ixr1>0 ) st11 = g11*rix*riy*F(ixr1,jxs01)
                     IF ( lxs1>0 ) st22 = g22*rlx*rly*F(kxr01,lxs1)
                     IF ( jxs1>0 ) st331 = g33*rjx*rjy*F(ixr01,jxs1)
                     IF ( kxr1>0 ) st332 = g33*rkx*rky*F(kxr1,lxs01)
                     IF ( ixky1>0 .AND. jxly1>0 ) st121 = (g33*rjx*rky+g12*rix*rly)*F(ixky1,jxly1)
                     IF ( iykx1>0 .AND. jylx1>0 ) st122 = (g33*rjy*rkx+g12*riy*rlx)*F(iykx1,jylx1)
                     IF ( ixiy0>0 .AND. jxjy0>0 ) st131 = g13*(riy*rjx+rix*rjy)*F(ixiy0,jxjy0)
                     IF ( iykx2>0 ) st132 = g13*riy*rkx*F(iykx2,jylx0)
                     IF ( ixky2>0 ) st133 = g13*rix*rky*F(ixky2,jxly0)
                     IF ( kxky0>0 .AND. lxly0>0 ) st231 = g23*(rkx*rly+rky*rlx)*F(kxky0,lxly0)
                     IF ( jxly2>0 ) st232 = g23*rjx*rly*F(ixky0,jxly2)
                     IF ( jylx2>0 ) st233 = g23*rjy*rlx*F(iykx0,jylx2)
!
                     st1 = (st11+st22+st331+st332+st121+st122+st131+st132+st133+st231+st232+st233)*Cab(k)
                     st = st + st1
                     IF ( unimem ) EXIT
                  ENDDO
                  GOTO 120
               ENDIF
            ENDIF
            sb7 = 0.0
            sb9 = 0.0
            sb10 = 0.0
            sb18 = 0.0
            sb21 = 0.0
            sb26 = 0.0
            sb28 = 0.0
            sb31 = 0.0
            sb36 = 0.0
            sb38 = 0.0
            DO k = 1 , 3
               ixmyr = ix + my + rk(k)
               jxnys1 = jx + ny + sk(k) + 1
               sb1 = 0.0
               sb2 = 0.0
               sb3 = 0.0
               sb4 = 0.0
               sb5 = 0.0
               sb6 = 0.0
               sb8 = 0.0
               sb11 = 0.0
               sb12 = 0.0
               sb13 = 0.0
               sb14 = 0.0
               sb15 = 0.0
               sb16 = 0.0
               sb17 = 0.0
               sb19 = 0.0
               sb20 = 0.0
               sb22 = 0.0
               sb23 = 0.0
               sb24 = 0.0
               sb25 = 0.0
               sb27 = 0.0
               sb29 = 0.0
               sb30 = 0.0
               sb32 = 0.0
               sb33 = 0.0
               sb34 = 0.0
               sb35 = 0.0
               sb37 = 0.0
               sb39 = 0.0
               sb40 = 0.0
               IF ( ixmyr>0 ) sb1 = -g11*rix*h4*Cab(k)*F(ixmyr,jxnys1)
               iymxr = iy + mx + rk(k)
               jynxs1 = jy + nx + sk(k) + 1
               IF ( iymxr>0 ) sb2 = -g11*riy*h4*Cab(k)*F(iymxr,jynxs1)
               mxmyr1 = mx + my + rk(k) + 1
               nxnys1 = nx + ny + sk(k) + 1
               sb3 = g11*h4**2*Cab(k)*F(mxmyr1,nxnys1)
               kxmyr1 = kx + my + rk(k) + 1
               lxnys = lx + ny + sk(k)
               IF ( lxnys>0 ) sb4 = -g22*rlx*h6*Cab(k)*F(kxmyr1,lxnys)
               mxkyr1 = mx + ky + rk(k) + 1
               nxlys = nx + ly + sk(k)
               IF ( nxlys>0 ) sb5 = -g22*rly*h6*Cab(k)*F(mxkyr1,nxlys)
               mxmyr1 = mx + my + rk(k) + 1
               nxnys1 = nx + ny + sk(k) + 1
               sb6 = g22*h6**2*Cab(k)*F(mxmyr1,nxnys1)
               ixmyr1 = ix + my + rk(k) + 1
               jxnys = jx + ny + sk(k)
               IF ( jxnys>0 ) sb8 = -g33*rjx*h5*Cab(k)*F(ixmyr1,jxnys)
               kxmyr = kx + my + rk(k)
               lxnys1 = lx + ny + sk(k) + 1
               IF ( kxmyr>0 ) sb11 = -g33*rkx*h5*Cab(k)*F(kxmyr,lxnys1)
               mxiyr1 = mx + iy + rk(k) + 1
               nxjys = nx + jy + sk(k)
               IF ( nxjys>0 ) sb12 = -g33*rjy*h5*Cab(k)*F(mxiyr1,nxjys)
               mxkyr = mx + ky + rk(k)
               nxlys1 = nx + ly + sk(k) + 1
               IF ( mxkyr>0 ) sb13 = -g33*rky*h5*Cab(k)*F(mxkyr,nxlys1)
               mxmyr1 = mx + my + rk(k) + 1
               nxnys1 = nx + ny + sk(k) + 1
               sb14 = g33*h5**2*Cab(k)*F(mxmyr1,nxnys1)
               ixmyr = ix + my + rk(k)
               jxnys1 = jx + ny + sk(k) + 1
               IF ( ixmyr>0 ) sb15 = -g12*rix*h6*Cab(k)*F(ixmyr,jxnys1)
               mxkyr1 = mx + ky + rk(k) + 1
               nxlys = nx + ly + sk(k)
               IF ( nxlys>0 ) sb16 = -g12*rly*h4*Cab(k)*F(mxkyr1,nxlys)
               mxmyr1 = mx + my + rk(k) + 1
               nxnys1 = nx + ny + sk(k) + 1
               sb17 = 2*g12*h4*h6*Cab(k)*F(mxmyr1,nxnys1)
               kxmyr1 = kx + my + rk(k) + 1
               lxnys = lx + ny + sk(k)
               IF ( lxnys>0 ) sb19 = -g12*rlx*h4*Cab(k)*F(kxmyr1,lxnys)
               mxiyr = mx + iy + rk(k)
               nxjys1 = nx + jy + sk(k) + 1
               IF ( mxiyr>0 ) sb20 = -g12*riy*h6*Cab(k)*F(mxiyr,nxjys1)
               ixmyr = ix + my + rk(k)
               jxnys1 = jx + ny + sk(k) + 1
               IF ( ixmyr>0 ) sb22 = -g13*rix*h5*Cab(k)*F(ixmyr,jxnys1)
               mxiyr1 = mx + iy + rk(k) + 1
               nxjys = nx + jy + sk(k)
               IF ( nxjys>0 ) sb23 = -g13*rjy*h4*Cab(k)*F(mxiyr1,nxjys)
               mxkyr = mx + ky + rk(k)
               nxlys1 = nx + ly + sk(k) + 1
               IF ( mxkyr>0 ) sb24 = -g13*rky*h4*Cab(k)*F(mxkyr,nxlys1)
               mxmyr1 = mx + my + rk(k) + 1
               nxnys1 = nx + ny + sk(k) + 1
               sb25 = 2*g13*h4*h5*Cab(k)*F(mxmyr1,nxnys1)
               ixmyr1 = ix + my + rk(k) + 1
               jxnys = jx + ny + sk(k)
               IF ( jxnys>0 ) sb27 = -g13*rjx*h4*Cab(k)*F(ixmyr1,jxnys)
               kxmyr = kx + my + rk(k)
               lxnys1 = lx + ny + sk(k) + 1
               IF ( kxmyr>0 ) sb29 = -g13*rkx*h4*Cab(k)*F(kxmyr,lxnys1)
               mxiyr = mx + iy + rk(k)
               nxjys1 = nx + jy + sk(k) + 1
               IF ( mxiyr>0 ) sb30 = -g13*riy*h5*Cab(k)*F(mxiyr,nxjys1)
               kxmyr1 = kx + my + rk(k) + 1
               lxnys = lx + ny + sk(k)
               IF ( lxnys>0 ) sb32 = -g23*rlx*h5*Cab(k)*F(kxmyr1,lxnys)
               mxiyr1 = mx + iy + rk(k) + 1
               nxjys = nx + jy + sk(k)
               IF ( nxjys>0 ) sb33 = -g23*rjy*h6*Cab(k)*F(mxiyr1,nxjys)
               mxkyr = mx + ky + rk(k)
               nxlys1 = nx + ly + sk(k) + 1
               IF ( mxkyr>0 ) sb34 = -g23*rky*h6*Cab(k)*F(mxkyr,nxlys1)
               mxmyr1 = mx + my + rk(k) + 1
               nxnys1 = nx + ny + sk(k) + 1
               sb35 = 2*g23*h5*h6*Cab(k)*F(mxmyr1,nxnys1)
               ixmyr1 = ix + my + rk(k) + 1
               jxnys = jx + ny + sk(k)
               IF ( jxnys>0 ) sb37 = -g23*rjx*h6*Cab(k)*F(ixmyr1,jxnys)
               kxmyr = kx + my + rk(k)
               lxnys1 = lx + ny + sk(k) + 1
               IF ( kxmyr>0 ) sb39 = -g23*rkx*h6*Cab(k)*F(kxmyr,lxnys1)
               mxkyr1 = mx + ky + rk(k) + 1
               nxlys = nx + ly + sk(k)
               IF ( nxlys>0 ) sb40 = -g23*rly*h5*Cab(k)*F(mxkyr1,nxlys)
               sb41 = sb3 + sb6 + sb14 + sb17 + sb25 + sb35
               IF ( i<=12 ) sb41 = 0.0
               st = st + sb1 + sb2 + sb4 + sb5 + sb7 + sb8 + sb9 + sb10 + sb11 + sb12 + sb13 + sb15 + sb16 + sb18 + sb19 + sb20 +   &
                  & sb21 + sb22 + sb23 + sb24 + sb26 + sb27 + sb28 + sb29 + sb30 + sb31 + sb32 + sb33 + sb34 + sb36 + sb37 + sb38 + &
                  & sb39 + sb40 + sb41
               IF ( unimem ) EXIT
            ENDDO
         ELSE
            ix011 = ix01 + 1
            jx011 = jx01 + 1
            rho = Rhoy*1.0
            IF ( j<=12 ) THEN
               mshl(ij) = rho*(Cab(1)*F(ix01,jx01)+Cab(2)*F(ix011,jx01)+Cab(3)*F(ix01,jx011)) + nsm*F(ix01,jx01)
               mshl(ji) = mshl(ij)
            ENDIF
            mx01 = mx0 + 1
            nx01 = nx0 + 1
            mx011 = mx01 + 1
            nx011 = nx01 + 1
            mshl(ij) = rho*(a1*F(mx01,nx01)+a2*F(mx011,nx01)+a3*F(mx01,nx011)) + nsm*F(mx01,nx01)
            mshl(ji) = mshl(ij)
            CYCLE
         ENDIF
 120     kshl(ij) = st
         kshl(ji) = kshl(ij)
      ENDDO
   ENDDO
   IF ( ipass==2 ) THEN
   ENDIF
!
!    CURRENTLY,TRANSVERSE SHEAR CALCULATIONS ARE NOT CODED FOR SHELL
!    ELEMENT WHEN IT IS CODED, CALL THE ROUTINE HERE
!
!
!     (QQQINV) TRANSPOSE (KTR3)  (QQQINV)
!
   CALL gmmats(Q,6,6,0,kshl(1),6,32,0,qks(1))
   CALL gmmats(Q,6,6,0,kshl(193),6,32,0,qks(193))
   CALL gmmats(qqqinv,20,18,+1,kshl(385),20,32,0,qks(385))
   DO i = 1 , 30
      DO j = 1 , 6
         ij = (i-1)*32 + j
         ji = (i-1)*6 + j
         kshl(ji) = qks(ij)
         kshl(180+ji) = qks(6+ij)
      ENDDO
   ENDDO
   DO i = 1 , 30
      DO j = 1 , 20
         ij = (i-1)*32 + j + 12
         ji = (i-1)*20 + j + 360
         kshl(ji) = qks(ij)
      ENDDO
   ENDDO
   CALL gmmats(kshl(1),30,6,0,Q,6,6,1,qks(1))
   CALL gmmats(kshl(181),30,6,0,Q,6,6,1,qks(181))
   CALL gmmats(kshl(361),30,20,0,qqqinv,20,18,0,qks(361))
   DO i = 1 , 30
      DO j = 1 , 6
         ij = (i-1)*30 + j
         ji = (i-1)*6 + j
         cms(ij) = qks(ji)
         cms(ij+6) = qks(ji+180)
      ENDDO
   ENDDO
   DO i = 1 , 30
      DO j = 1 , 18
         ij = (i-1)*30 + j + 12
         ji = (i-1)*18 + j + 360
         cms(ij) = qks(ji)
      ENDDO
   ENDDO
   DO i = 1 , 30
      Ee(i) = 0.0
   ENDDO
   Ee(1) = Ivect(1)
   Ee(2) = Jvect(1)
   Ee(3) = Kvect(1)
   Ee(6) = Ivect(2)
   Ee(7) = Jvect(2)
   Ee(8) = Kvect(2)
   Ee(11) = Ivect(3)
   Ee(12) = Jvect(3)
   Ee(13) = Kvect(3)
   Ee(19) = Ivect(1)
   Ee(20) = Jvect(1)
   Ee(24) = Ivect(2)
   Ee(25) = Jvect(2)
   Ee(29) = Ivect(3)
   Ee(30) = Jvect(3)
   DO k = 1 , 6
      DO i = 1 , 2
         k1 = 6*(i-1) + k
         i1 = 5*(k-1) + i
         DO j = 1 , 30
            ctm(i1,j) = cm1(k1,j)
         ENDDO
      ENDDO
   ENDDO
   DO k = 1 , 6
      DO i = 1 , 3
         i2 = 5*(k-1) + i + 2
         k2 = 12 + (k-1)*3 + i
         DO j = 1 , 30
            ctm(i2,j) = cm1(k2,j)
         ENDDO
      ENDDO
   ENDDO
   DO k = 1 , 6
      DO i = 1 , 2
         k1 = 6*(i-1) + k
         i1 = 5*(k-1) + i
         DO j = 1 , 30
            cm1(j,i1) = ctm(j,k1)
         ENDDO
      ENDDO
   ENDDO
   DO k = 1 , 6
      DO i = 1 , 3
         i2 = 5*(k-1) + i + 2
         k2 = 12 + (k-1)*3 + i
         DO j = 1 , 30
            cm1(j,i2) = ctm(j,k2)
         ENDDO
      ENDDO
   ENDDO
   DO i = 1 , 1296
      cmt(i) = 0.0
   ENDDO
!
!     LUMPED MASS COMPUTATION
!
   IF ( ipass/=2 ) THEN
!
!     LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL (THAT IS
!     COORDINATE AT ANY GRID POINT IN WHICH DISPLACEMENT AND STRESSES
!     ARE R
!     - NOT NEEDED IF FIELD 7 IN GRID CARD IS ZERO)
!
!     TRANSFORM STIFFNESS MATRIX FROM ELEMENT COORDINATES TO BASIC
!     COORDINATES
!
!     TRANSFORM STIFFNESS MATRIX FROM BASIC COORDINAYES TO GLOBAL (DISP)
!     COORDINATES
!
!     INSERT THE 6X6 SUBMATRIX  INTO KGG MATRIX
!
      DO i = 1 , 6
         Save(i) = Nl(i)
      ENDDO
      DO i = 1 , 6
         Small(i) = i
         ismall = Nl(i)
         DO j = 1 , 6
            IF ( ismall>Nl(j) ) THEN
               Small(i) = j
               ismall = Nl(j)
            ENDIF
         ENDDO
         ism = Small(i)
         Nl(ism) = 1000000
      ENDDO
      DO i = 1 , 6
         Nl(i) = Save(i)
      ENDDO
      DO i = 1 , 6
         sil1 = Small(i)
         DO j = i , 6
            sil2 = Small(j)
            DO ii = 1 , 36
               Balotr(ii) = 0.0
               Ksup(ii) = 0.0
            ENDDO
            DO k = 1 , 5
               k1 = (sil1-1)*5 + k
               DO l = 1 , 5
                  l1 = (sil2-1)*5 + l
                  Csub(k,l) = cm1(k1,l1)
               ENDDO
            ENDDO
            CALL gmmats(Ee,6,5,0,Csub,5,5,0,Csubt)
            CALL gmmats(Csubt,6,5,0,Ee,6,5,+1,Ksupt)
            DO k = 1 , 6
               DO l = 1 , 6
                  k1 = (k-1)*6 + l
                  l1 = (l-1)*6 + k
                  Ksup(l1) = Ksupt(k1)
               ENDDO
            ENDDO
!
!     TRANSFORM THE KSUP(36) FROM BASIC TO DISPLACEMENT COORDINATES
!
            IF ( Nl(sil1)/=0 .AND. Ics(sil1)/=0 ) THEN
               jj = 4*sil1 + 24
               CALL transs(Iest(jj),Trand)
               DO jj = 1 , 3
                  l = 6*(jj-1) + 1
                  m = 3*(jj-1) + 1
                  Balotr(l) = Trand(m)
                  Balotr(l+1) = Trand(m+1)
                  Balotr(l+2) = Trand(m+2)
                  Balotr(l+21) = Trand(m)
                  Balotr(l+22) = Trand(m+1)
                  Balotr(l+23) = Trand(m+2)
               ENDDO
               CALL gmmats(Balotr(1),6,6,1,Ksup(1),6,6,0,Ksupt)
               DO k = 1 , 36
                  Ksup(k) = Ksupt(k)
               ENDDO
            ENDIF
            IF ( Nl(sil2)/=0 .AND. Ics(sil2)/=0 ) THEN
               IF ( j/=i ) THEN
                  jj = 4*sil2 + 24
                  CALL transs(Iest(jj),Trand)
                  DO jj = 1 , 3
                     l = 6*(jj-1) + 1
                     m = 3*(jj-1) + 1
                     Balotr(l) = Trand(m)
                     Balotr(l+1) = Trand(m+1)
                     Balotr(l+2) = Trand(m+2)
                     Balotr(l+21) = Trand(m)
                     Balotr(l+22) = Trand(m+1)
                     Balotr(l+23) = Trand(m+2)
                  ENDDO
               ENDIF
               CALL gmmats(Ksup(1),6,6,0,Balotr(1),6,6,0,Ksupt)
               DO k = 1 , 36
                  Ksup(k) = Ksupt(k)
               ENDDO
            ENDIF
            DO ii = 1 , 6
               DO jj = 1 , 6
                  i1 = (i-1)*6 + ii
                  j1 = (j-1)*6 + jj
                  ctm(i1,j1) = Ksub(jj,ii)
                  ctm(j1,i1) = Ksub(jj,ii)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      GOTO 300
   ENDIF
 200  amass = (Rhoy*vol+nsm*area)/6.
   DO i = 1 , 1296 , 37
      cmt(i) = amass
   ENDDO
   ipass = 2
 300  CALL emgout(cmt(1),cmt(1),1296,1,Dict,ipass,Iprec)
   IF ( .NOT.imass .OR. ipass>=2 ) RETURN
!
!     TO TO 295 TO COMPUTE LUMPED MASS MATRIX
!     GO TO 211 TO COMPUTE CONSIST. MASS MATRIX (THIS PATH DOES NOT
!     WROK)
!
   ipass = 3
   IF ( ipass==1 ) GOTO 99999
   IF ( ipass==2 ) GOTO 100
   IF ( ipass==3 ) GOTO 200
!
!     ERROR
!
 400  Nogo = .TRUE.
   Knogo = 1
   WRITE (Ioutpt,99001) Ufm , Iest(1)
99001 FORMAT (A23,' 2416, MATRIX RELATING GENERALIZED PARAMETERS AND ','GRID POINT DISPLACEMENTS IS SINGULAR.',//26X,               &
             &'CHECK COORDINATES OF ELEMENT  TRSHL WITH ID',I9,1H.)
99999 END SUBROUTINE ktshls
