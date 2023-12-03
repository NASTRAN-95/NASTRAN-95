!*==ktrpld.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ktrpld
   USE c_blank
   USE c_emgdic
   USE c_emgest
   USE c_emgprm
   USE c_matin
   USE c_matout
   USE c_sma1dp
   USE c_sma1io
   USE c_system
   USE c_xmssg
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , amass , area , b , c , d11 , d12 , d13 , d22 , d23 , d33 , nsm , rho , t1 , t2 , t3 , theta1 , thetam , thk1 , thk2 ,&
         & thk3 , tmem1 , tmem3 , tmem5 , tshr1 , tshr3 , tshr5 , vol
   REAL(REAL64) :: a1sq , a2sq , a3sq , c1 , c10 , c2 , c3 , c4 , c5 , c6 , c7 , c8 , c9 , d132 , d232 , d334 , determ , rmnx ,     &
                 & rmny , rmx , rmx1 , rmy , rmy1 , rnx , rnx1 , rny , rny1 , s11 , s13 , s22 , s23 , s33 , st
   REAL(REAL64) , DIMENSION(36) :: balotr , ksup , ksupt
   REAL , SAVE :: blank , degra
   REAL(REAL64) , DIMENSION(10) :: cc
   REAL(REAL64) , DIMENSION(1296) :: cmt
   REAL(REAL64) , DIMENSION(3,3) :: csub
   REAL(REAL64) , DIMENSION(6,3) :: csubt
   INTEGER , DIMENSION(11) :: dict
   REAL(REAL64) , DIMENSION(18) :: e
   REAL , DIMENSION(14,14) :: f
   INTEGER :: i , i1 , i1j1 , i2 , i3 , icode , idele , ii , ij , ioutpt , ipass , iprec , ising , ism , ismall , j , j1 , j1i1 ,   &
            & ji , jj , k , k1 , l , l1 , m , matid1 , matid2 , mx , mx0 , mx01 , mx011 , mx0x , mx1 , mx1x , mx2 , mx2x , mx3 ,    &
            & mx3x , my , my1 , my1x , ndof , nx , nx0 , nx01 , nx011 , nx0y , nx1 , nx1y , nx2 , nx2y , nx3 , nx3y , ny , ny1 ,    &
            & ny1y , sil1 , sil2
   INTEGER , DIMENSION(6) :: ics , nl , save , small
   INTEGER , DIMENSION(42) :: iest
   LOGICAL :: imass , nogo , nots , uniben
   INTEGER , DIMENSION(20,3) :: index
   REAL , DIMENSION(3) :: ivect , jvect , kvect
   REAL(REAL64) , DIMENSION(6,6) :: ksub , ksubt
   REAL(REAL64) , DIMENSION(400) :: ktr3 , mtr3
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL(REAL64) , DIMENSION(20,20) :: qqq
   REAL(REAL64) , DIMENSION(360) :: qqqinv
   REAL(REAL64) , DIMENSION(9) :: trand
   REAL(REAL64) , DIMENSION(60) :: ts1 , ts2 , ts7
   REAL(REAL64) , DIMENSION(40) :: ts6 , ts6s
   REAL , DIMENSION(6) :: xc , yc , zc
   INTEGER , DIMENSION(20) , SAVE :: xpower , ypower
   INTEGER , DIMENSION(10) , SAVE :: xthk , ythk
   EXTERNAL af , emgout , gmmatd , inverd , mat , sswtch , transd , trif , tspl1d , tspl3d
!
! End of declarations rewritten by SPAG
!
!
!     STIFFNESS SUBROUTINE FOR HIGHER ORDER PLATE ELEMENT CTRPLT1
!
!     ECPT ENTRIES
!
!     ECPT( 1) = ELEMENT ID                                   INTEGER
!     ECPT( 2) = SCALAR INDEX NUMBER FOR GRID POINT 1         INTEGER
!     ECPT( 3) = SCALAR INDEX NUMBER FOR GRID POINT 2         INTEGER
!     ECPT( 4) = SCALAR INDEX NUMBER FOR GRID POINT 3         INTEGER
!     ECPT( 5) = SCALAR INDEX NUMBER FOR GRID POINT 4         INTEGER
!     ECPT( 6) = SCALAR INDEX NUMBER FOR GRID POINT 5         INTEGER
!     ECPT( 7) = SCALAR INDEX NUMBER FOR GRID POINT 6         INTEGER
!     ECPT( 8) = THETA                                        REAL
!     ECPT( 9) = MATERIAL ID 1                                INTEGER
!     ECPT(10) = AREA MOMENT OF INERTIA R1 AT GRID POINT 1    REAL
!     ECPT(11) = AREA MOMENT OF INERTIA R3 AT GRID POINT 3    REAL
!     ECPT(12) = AREA MOMENT OF INERTIA R5 AT GRID POINT 5    REAL
!     ECPT(13) = MATERIAL ID 2                                INTEGER
!     ECPT(14) = THICKNESS TSHR1 FOR TRANSVERSE SHEAR AT      REAL
!                 GRID POINT 1
!     ECPT(15) = THICKNESS TSHR3 FOR TRANSVERSE SHEAR AT      REAL
!                 GRID POINT 3
!     ECPT(16) = THICKNESS TSHR5 FOR TRANSVERSE SHEAR AT      REAL
!                 GRID POINT 5
!     ECPT(17) = NON-STRUCTURAL MASS                          REAL
!     ECPT(18) = DISTANCE Z11 FOR STRESS CALCULATION AT GRID 1
!     ECPT(19) = DISTANCE Z21 FOR STRESS CALCULATION AT GRID 1
!     ECPT(20) = DISTANCE Z13 FOR STRESS CALCULATION AT GRID 3
!     ECPT(21) = DISTANCE Z23 FOR STRESS CALCULATION AT GRID 3
!     ECPT(22) = DISTANCE Z15 FOR STRESS CALCULATION AT GRID 5
!     ECPT(23) = DISTANCE Z25 FOR STRESS CALCULATION AT GRID 5
!
!     X1,Y1,Z1 FOR ALL SIX POINTS ARE IN NASTRAN BASIC SYSTEM
!
!     ECPT(24) = COORDINATE SYSTEM ID FOR GRID A              INTEGER
!     ECPT(25) = COORDINATE X1                                REAL
!     ECPT(26) = COORDINATE Y1                                REAL
!     ECPT(27) = COORDINATE Z1                                REAL
!     ECPT(28) = COORDINATE SYSTEM ID FOR GRID B              INTEGER
!     ECPT(29) = COORDINATE X1                                REAL
!     ECPT(30) = COORDINATE Y1                                REAL
!     ECPT(31) = COORDINATE Z1                                REAL
!     ECPT(32) = COORDINATE SYSTEM ID FOR GRID C              INTEGER
!     ECPT(33) = COORDINATE X1                                REAL
!     ECPT(34) = COORDINATE Y1                                REAL
!     ECPT(35) = COORDINATE Z1                                REAL
!     ECPT(36) = COORDINATE SYSTEM ID FOR GRID D              INTEGER
!     ECPT(37) = COORDINATE X1                                REAL
!     ECPT(38) = COORDINATE Y1                                REAL
!     ECPT(39) = COORDINATE Z1                                REAL
!     ECPT(40) = COORDINATE SYSTEM ID FOR GRID E              INTEGER
!     ECPT(41) = COORDINATE X1                                REAL
!     ECPT(42) = COORDINATE Y1                                REAL
!     ECPT(43) = COORDINATE Z1                                REAL
!     ECPT(44) = COORDINATE SYSTEM ID FOR GRID F              INTEGER
!     ECPT(45) = COORDINATE X1                                REAL
!     ECPT(46) = COORDINATE Y1                                REAL
!     ECPT(47) = COORDINATE Z1                                REAL
!     ECPT(48) = ELEMENT TEMPERATURE                          REAL
!
   !>>>>EQUIVALENCE (c1,cc(1)) , (c2,cc(2)) , (c3,cc(3)) , (c4,cc(4)) , (c5,cc(5)) , (c6,cc(6)) , (c7,cc(7)) , (c8,cc(8)) , (c9,cc(9)) , &
!>>>>    & (c10,cc(10))
   !>>>>EQUIVALENCE (Ksystm(2),Ioutpt) , (ksub(1,1),ksup(1)) , (ksubt(1,1),ksupt(1))
   !>>>>EQUIVALENCE (thk1,tmem1) , (thk2,tmem3) , (thk3,tmem5)
   !>>>>EQUIVALENCE (A,Dista) , (B,Distb) , (C,Distc) , (Iest(1),Est(1)) , (cmt(1),ktr3(1),mtr3(1),qqq(1,1)) , (Cm1(1,1),Ts6(1)) ,       &
!>>>>    & (Cm1(5,3),Ts1(1)) , (Cm1(11,6),Ts6s(1)) , (Cm1(15,8),Ts2(1)) , (Cm1(3,12),Ts7(1))
   DATA xpower/0 , 1 , 0 , 2 , 1 , 0 , 3 , 2 , 1 , 0 , 4 , 3 , 2 , 1 , 0 , 5 , 3 , 2 , 1 , 0/
   DATA ypower/0 , 0 , 1 , 0 , 1 , 2 , 0 , 1 , 2 , 3 , 0 , 1 , 2 , 3 , 4 , 0 , 2 , 3 , 4 , 5/
   DATA xthk/0 , 1 , 0 , 2 , 1 , 0 , 3 , 2 , 1 , 0/
   DATA ythk/0 , 0 , 1 , 0 , 1 , 2 , 0 , 1 , 2 , 3/
   DATA degra/0.0174532925/
   DATA blank , name/4H     , 4HTRPL , 4HT1  /
!
!     COMPONENT CODE,ICODE,IS  111111  AND HAS A VALUE OF 63
!
   icode = 63
   ndof = 36
   iprec = precis
   nlocs = 6
   dict(1) = estid
   dict(2) = 1
   dict(3) = ndof
   dict(4) = icode
   dict(5) = gsube
   nots = .FALSE.
   imass = .FALSE.
   IF ( nom>0 ) imass = .TRUE.
   ipass = 1
   idele = iest(1)
   DO i = 1 , 6
      nl(i) = iest(i+1)
   ENDDO
   thetam = est(8)
   matid1 = iest(9)
   tmem1 = (est(10)*12.0)**0.333333333333
   tmem3 = (est(11)*12.0)**0.333333333333
   tmem5 = (est(12)*12.0)**0.333333333333
   matid2 = iest(13)
   tshr1 = est(14)
   tshr3 = est(15)
   tshr5 = est(16)
   nsm = est(17)
   j = 0
   DO i = 24 , 44 , 4
      j = j + 1
      ics(j) = iest(i)
      xc(j) = est(i+1)
      yc(j) = est(i+2)
      zc(j) = est(i+3)
   ENDDO
!
!     IF TMEM3 OR TMEM5 EQUAL TO ZERO OR BLANK,THEY WILL BE SET EQUAL TO
!     SO ALSO FOR TEMP3 AND TEMP5
!
   IF ( tmem3==0.0 .OR. tmem3==blank ) tmem3 = tmem1
   IF ( tmem5==0.0 .OR. tmem5==blank ) tmem5 = tmem1
   IF ( tshr3==0.0 .OR. tshr3==blank ) tshr3 = tshr1
   IF ( tshr5==0.0 .OR. tshr5==blank ) tshr5 = tshr1
   IF ( tshr1==0.0 ) nots = .TRUE.
   eltemp = est(48)
   theta1 = thetam*degra
   sinth = sin(theta1)
   costh = cos(theta1)
   IF ( abs(sinth)<=1.0E-06 ) sinth = 0.0
!
!     EVALUATE MATERIAL PROPERTIES
!
   matflg = 2
   matid = matid1
   CALL mat(idele)
!
   matid = matid2
   matflg = 3
   IF ( .NOT.(nots) ) CALL mat(idele)
   d11 = em(1)
   d12 = em(2)
   d13 = em(3)
   d22 = em(4)
   d23 = em(5)
   d33 = em(6)
!
!     CALCULATIONS FOR THE TRIANGLE
!
   CALL trif(xc,yc,zc,ivect,jvect,kvect,a,b,c,iest(1),name)
!
!     FILL E-MATRIX
!
   DO i = 1 , 18
      e(i) = 0.0D0
   ENDDO
   e(1) = kvect(1)
   e(4) = kvect(2)
   e(7) = kvect(3)
   e(11) = ivect(1)
   e(14) = ivect(2)
   e(17) = ivect(3)
   e(12) = jvect(1)
   e(15) = jvect(2)
   e(18) = jvect(3)
!
   d334 = d33*4.0D0
   d132 = d13*2.0D0
   d232 = d23*2.0D0
   CALL af(f,14,a,b,c,a1,a2,a3,thk1,thk2,thk3,1)
   a1sq = a1*a1
   a2sq = a2*a2
   a3sq = a3*a3
   c1 = a1sq*a1
   c2 = 3.0*a1sq*a2
   c3 = 3.0*a1sq*a3
   c4 = 3.0*a1*a2sq
   c5 = 6.0*a1*a2*a3
   c6 = 3.0*a3sq*a1
   c7 = a2sq*a2
   c8 = 3.0*a2sq*a3
   c9 = 3.0*a2*a3sq
   c10 = a3*a3sq
   CALL af(f,14,a,b,c,aa1,aa2,aa3,tshr1,tshr3,tshr5,1)
   uniben = .FALSE.
   IF ( abs(a2)<=1.0E-06 .AND. abs(a3)<=1.0E-06 ) uniben = .TRUE.
!
!     COMPUTE THE AREA INTEGRATION FUNCTION F
!
   CALL af(f,14,a,b,c,0,0,0,0,0,0,-1)
!
!     CALCULATIONS FOR QMATRIX (QQQ) AND ITS INVERSE
!
   DO i = 1 , 20
      DO j = 1 , 20
         qqq(i,j) = 0.0D0
      ENDDO
   ENDDO
   DO i = 1 , 6
      i3 = (i-1)*3
      i1 = i3 + 1
      i2 = i3 + 2
      i3 = i3 + 3
      qqq(i1,1) = 1.0D0
      qqq(i1,2) = xc(i)
      qqq(i1,3) = yc(i)
      qqq(i1,4) = xc(i)*xc(i)
      qqq(i1,5) = xc(i)*yc(i)
      qqq(i1,6) = yc(i)*yc(i)
      qqq(i1,7) = qqq(i1,4)*xc(i)
      qqq(i1,8) = qqq(i1,4)*yc(i)
      qqq(i1,9) = qqq(i1,5)*yc(i)
      qqq(i1,10) = qqq(i1,6)*yc(i)
      qqq(i1,11) = qqq(i1,7)*xc(i)
      qqq(i1,12) = qqq(i1,7)*yc(i)
      qqq(i1,13) = qqq(i1,8)*yc(i)
      qqq(i1,14) = qqq(i1,9)*yc(i)
      qqq(i1,15) = qqq(i1,10)*yc(i)
      qqq(i1,16) = qqq(i1,11)*xc(i)
      qqq(i1,17) = qqq(i1,12)*yc(i)
      qqq(i1,18) = qqq(i1,13)*yc(i)
      qqq(i1,19) = qqq(i1,14)*yc(i)
      qqq(i1,20) = qqq(i1,15)*yc(i)
      qqq(i2,3) = 1.0D0
      qqq(i2,5) = xc(i)
      qqq(i2,6) = yc(i)*2.0
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
      qqq(i3,2) = -1.0D0
      qqq(i3,4) = -2.0*xc(i)
      qqq(i3,5) = -yc(i)
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
!
!     IF NO TRANSVERSE SHEAR GO TO 113
!
      IF ( .NOT.(nots) ) THEN
         x = xc(i)
         y = yc(i)
         CALL tspl3d(ts6)
         DO jj = 1 , 20
            qqq(i2,jj) = qqq(i2,jj) - ts6(20+jj)
            qqq(i3,jj) = qqq(i3,jj) + ts6(jj)
         ENDDO
      ENDIF
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
!
!     FOURTH ARGUMENT IS A DUMMY LOCATION FOR INVERSE AND HENCE TS1(1) I
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
   ising = -1
   CALL inverd(20,qqq,20,ts6(1),0,determ,ising,index)
!
!     ISING EQUAL TO 2 IMPLIES THAT QQQ IS SINGULAR
!
   IF ( ising/=2 ) THEN
!
!     FIRST 18 COLUMNS OF QQQ INVERSE IS THE QQQINV FOR USE IN STIFFNESS
!     MATRIX CALCULATIONS
!
      DO i = 1 , 20
         DO j = 1 , 18
            ij = (i-1)*18 + j
            qqqinv(ij) = qqq(i,j)
         ENDDO
      ENDDO
      SPAG_Loop_1_2: DO
!
!     START EXECUTION FOR STIFFNESS MATRIX CALCULATION
!
!     CM IS STIFFNESS MATRIX IN ELEMENT COORDINATES
!
         DO i = 1 , 400
            ktr3(i) = 0.0D0
         ENDDO
         DO i = 1 , 20
            mx = xpower(i)
            rmx = mx
            nx = ypower(i)
            rnx = nx
            rmnx = rmx*rnx
            rmx1 = rmx*(rmx-1.0D0)
            rnx1 = rnx*(rnx-1.0D0)
            DO j = i , 20
               ij = (i-1)*20 + j
               ji = (j-1)*20 + i
               my = xpower(j)
               rmy = my
               ny = ypower(j)
               rny = ny
               rmny = rmy*rny
               rmy1 = rmy*(rmy-1.0D0)
               rny1 = rny*(rny-1.0D0)
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
               IF ( ipass==1 ) THEN
                  st = 0.0D0
                  SPAG_Loop_4_1: DO k = 1 , 10
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
                     s11 = 0.0D0
                     s22 = 0.0D0
                     s33 = 0.0D0
                     s13 = 0.0D0
                     s23 = 0.0D0
                     IF ( mx3x>0 ) s11 = d11*rmx1*rmy1*cc(k)*f(mx3x,ny1y)
                     IF ( nx3y>0 ) s22 = d22*rnx1*rny1*cc(k)*f(my1x,nx3y)
                     IF ( mx1x>0 .AND. nx1y>0 ) s33 = (d334*rmnx*rmny+d12*(rmx1*rny1+rmy1*rnx1))*cc(k)*f(mx1x,nx1y)
                     IF ( mx2x>0 .AND. nx0y>0 ) s13 = d132*(rmx1*rmny+rmnx*rmy1)*cc(k)*f(mx2x,nx0y)
                     IF ( mx0x>0 .AND. nx2y>0 ) s23 = d232*(rmnx*rny1+rnx1*rmny)*cc(k)*f(mx0x,nx2y)
                     st = st + s11 + s22 + s33 + s13 + s23
                     IF ( uniben ) EXIT SPAG_Loop_4_1
                  ENDDO SPAG_Loop_4_1
                  ktr3(ij) = st/12.0D0
                  ktr3(ji) = ktr3(ij)
               ELSE
                  mx01 = mx0 + 1
                  nx01 = nx0 + 1
                  mx011 = mx01 + 1
                  nx011 = nx01 + 1
                  rho = rhoy*1.0D0
                  mtr3(ij) = rho*(a1*f(mx01,nx01)+a2*f(mx011,nx01)+a3*f(mx01,nx011)) + nsm*f(mx01,nx01)
                  mtr3(ji) = mtr3(ij)
               ENDIF
            ENDDO
         ENDDO
         IF ( ipass/=2 ) THEN
!
!     IF NO TRANSVERSE SHEAR GO TO 230
!
!     IF TSHR EQUAL TO ZERO OR MATID3 EQUAL TO ZERO, SKIP THESE
!     CALCULATION
!
!
!
            IF ( .NOT.(nots) ) CALL tspl1d(ts1,ts2,ts6,ts6s,ts7,ktr3,cmt(761))
         ENDIF
!
!     (QQQINV) TRANSPOSE (KTR3)  (QQQINV)
!
         CALL gmmatd(qqqinv,20,18,+1,ktr3,20,20,0,cmt(761))
         CALL gmmatd(cmt(761),18,20,0,qqqinv,20,18,0,cm1)
         SPAG_Loop_2_3: DO
!
            DO i = 1 , 1296
               cmt(i) = 0.0
            ENDDO
            IF ( ipass<=2 ) THEN
!
!     LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL (THAT IS
!     COORDINATE AT ANY GRID POINT IN WHICH DISPLACEMENT AND STRESSES
!     ARE R??
!     - NOT NEEDED IF FIELD 7 IN GRID CARD IS ZERO)
!
!     TRANSFORM STIFFNESS MATRIX FROM ELEMENT COORDINATES TO BASIC
!     COORDINATES
!
!     TRANSFORM STIFFNESS MATRIX FROM BASIC COORDINATES TO GLOBAL (DISP)
!     COORDINATES
!
!     INSERT THE 6X6 SUBMATRIX  INTO KGG MATRIX
!
               DO i = 1 , 6
                  save(i) = nl(i)
               ENDDO
               DO i = 1 , 6
                  small(i) = i
                  ismall = nl(i)
                  DO j = 1 , 6
                     IF ( ismall>nl(j) ) THEN
                        small(i) = j
                        ismall = nl(j)
                     ENDIF
                  ENDDO
                  ism = small(i)
                  nl(ism) = 1000000
               ENDDO
               DO i = 1 , 6
                  nl(i) = save(i)
               ENDDO
               DO i = 1 , 6
                  DO j = i , 6
                     DO ii = 1 , 36
                        balotr(ii) = 0.0D0
                        ksup(ii) = 0.0D0
                     ENDDO
                     DO k = 1 , 3
                        sil1 = small(i)
                        k1 = (sil1-1)*3 + k
                        DO l = 1 , 3
                           sil2 = small(j)
                           l1 = (sil2-1)*3 + l
                           csub(k,l) = cm1(k1,l1)
                        ENDDO
                     ENDDO
                     CALL gmmatd(e,6,3,0,csub,3,3,0,csubt)
                     CALL gmmatd(csubt,6,3,0,e,6,3,+1,ksubt)
                     DO k = 1 , 6
                        DO l = 1 , 6
                           k1 = (k-1)*6 + l
                           l1 = (l-1)*6 + k
                           ksup(l1) = ksupt(k1)
                        ENDDO
                     ENDDO
!
!     TRANSFORM THE KSUP(36) FROM BASIC TO DISPLACEMENT COORDINATES
!
                     IF ( nl(sil1)/=0 .AND. ics(sil1)/=0 ) THEN
                        jj = 4*i + 20
                        CALL transd(iest(jj),trand)
                        DO jj = 1 , 3
                           l = 6*(jj-1) + 1
                           m = 3*(jj-1) + 1
                           balotr(l) = trand(m)
                           balotr(l+1) = trand(m+1)
                           balotr(l+2) = trand(m+2)
                           balotr(l+21) = trand(m)
                           balotr(l+22) = trand(m+1)
                           balotr(l+23) = trand(m+2)
                        ENDDO
                        CALL gmmatd(balotr(1),6,6,1,ksup(1),6,6,0,ksupt)
                        DO k = 1 , 36
                           ksup(k) = ksupt(k)
                        ENDDO
                     ENDIF
                     IF ( nl(sil2)/=0 .AND. ics(sil2)/=0 ) THEN
                        IF ( j/=i ) THEN
                           CALL transd(iest(4*j+20),trand)
                           DO jj = 1 , 3
                              l = 6*(jj-1) + 1
                              m = 3*(jj-1) + 1
                              balotr(l) = trand(m)
                              balotr(l+1) = trand(m+1)
                              balotr(l+2) = trand(m+2)
                              balotr(l+21) = trand(m)
                              balotr(l+22) = trand(m+1)
                              balotr(l+23) = trand(m+2)
                           ENDDO
                        ENDIF
                        CALL gmmatd(ksup(1),6,6,0,balotr(1),6,6,0,ksupt)
                        DO k = 1 , 36
                           ksup(k) = ksupt(k)
                        ENDDO
                     ENDIF
                     DO ii = 1 , 6
                        DO jj = 1 , 6
                           i1 = (i-1)*6 + ii
                           j1 = (j-1)*6 + jj
                           i1j1 = (i1-1)*36 + j1
                           j1i1 = (j1-1)*36 + i1
                           cmt(i1j1) = ksub(jj,ii)
                           cmt(j1i1) = ksub(jj,ii)
                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
            ELSE
!
!     LUMPED MASS MATRIX
!
               CALL af(f,14,a,b,c,t1,t2,t3,est(10),est(11),est(12),1)
               area = f(1,1)
               vol = t1*f(1,1) + t2*f(2,1) + t3*f(1,2)
               amass = (rhoy*vol+nsm*area)/6.
               DO i = 1 , 1296 , 37
                  cmt(i) = amass
               ENDDO
               ipass = 2
            ENDIF
!
!     CALL INSERTION ROUTINE
!
            CALL emgout(cmt(1),cmt(1),1296,1,dict,ipass,iprec)
            IF ( .NOT.imass .OR. ipass>=2 ) RETURN
!
!     GO TO 290 TO COMPUTE LUMPED MASS MATRIX
!     GO TO 211 TO COMPUTE CONSIST. MASS MATRIX (THIS PATH MAY NOT WORK)
!
            ipass = 3
            CALL sswtch(46,j)
            IF ( j==1 ) ipass = 2
            IF ( ipass==1 ) RETURN
            IF ( ipass==2 ) CYCLE SPAG_Loop_1_2
            IF ( ipass/=3 ) EXIT SPAG_Loop_2_3
         ENDDO SPAG_Loop_2_3
         EXIT SPAG_Loop_1_2
      ENDDO SPAG_Loop_1_2
   ENDIF
!
!     ERRORS
!
   nogo = .TRUE.
   WRITE (ioutpt,99001) ufm , iest(1)
99001 FORMAT (A23,' 2411, MATRIX RELATING GENERALIZED PARAMETERS AND ','GRID POINT DISPLACEMENTS IS SINGULAR.',/26X,                &
             &'CHECK COORDINATES OF ELEMENT  TRPLT1 WITH ID',I9,1H.)
END SUBROUTINE ktrpld