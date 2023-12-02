!*==tlodsl.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tlodsl(Treal,Tint)
   IMPLICIT NONE
   USE C_EMGDIC
   USE C_MATIN
   USE C_MATOUT
   USE C_SSGWRK
   USE C_SYSTEM
   USE C_TRIMEX
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: Treal
   INTEGER , DIMENSION(6) :: Tint
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , a1 , a1sq , a2 , a2sq , a3 , a3sq , aa1 , aa2 , aa3 , area , aviner , avthk , b , c , c1 , c10 , c2 , c3 , c4 , c5 , &
         & c6 , c7 , c8 , c9 , d11 , d12 , d13 , d132 , d22 , d23 , d232 , d33 , d334 , determ , dista , distb , distc , el2 , el3 ,&
         & g11 , g11pr , g12 , g13 , g22 , g22pr , g23 , g33 , g33pr , gm1 , gm2 , gm3 , h4 , h5 , h6 , ptemp , rix , rjx , rkx ,   &
         & rlx , rmnx , rmx , rmx1 , rnx , rnx1 , t1prim , t3prim , t5prim , tbend1 , tbend3 , tbend5 , theta1 , thetam , thk1 ,    &
         & thk2 , thk3 , tmem1 , tmem3 , tmem5 , tshr1
   REAL , DIMENSION(36) :: balotr
   REAL , SAVE :: blank , degra
   REAL , DIMENSION(3) :: cab , dd , el , g1 , ivect , jvect , kvect , pl
   REAL , DIMENSION(10) :: cc
   REAL , DIMENSION(30) :: ee
   REAL , DIMENSION(9) :: g , ge1 , trand
   INTEGER :: i , i1 , i2 , i3 , idele , ii , ij , ij1 , ik , ising , ix , ixp1 , ixr , ixrt , ixrt1 , j , jj , jx , jxp1 , jxs ,   &
            & jxsu , jxsu1 , k , k1 , k2 , kx , kxp1 , kxr , kxrt , kxrt1 , l , lx , lxp1 , lxs , lxsu , lxsu1 , m , matid1 ,       &
            & matid2 , matid3 , mkr1 , mx , mx01 , mx01x , mx01xp , mx1 , mx1x , mx1xp , mxp1 , mxx , mxxp , nls1 , nsm , nx ,      &
            & nx01 , nx01y , nx01yq , nx1 , nx1y , nx1yq , nxp1 , nxy , nxyq , sil1
   INTEGER , DIMENSION(6) :: ics , nl , small
   INTEGER , DIMENSION(42) :: iest
   INTEGER , DIMENSION(20,3) :: index
   REAL , DIMENSION(2) , SAVE :: name
   LOGICAL :: nogo , nots , uniben , unimem , unitem
   REAL , DIMENSION(5) :: p8
   REAL , DIMENSION(6) :: p9 , xc , yc , zc
   INTEGER , DIMENSION(3) , SAVE :: pt , qt , rk , sk , tl , ul
   REAL , DIMENSION(32) :: ptem
   REAL , DIMENSION(6,6) :: q
   REAL , DIMENSION(960) :: qq
   REAL , DIMENSION(20,20) :: qqq
   REAL , DIMENSION(360) :: qqqinv
   REAL , DIMENSION(40) :: ts6
   REAL :: tshr3 , tshr5 , vol
   INTEGER , DIMENSION(10) , SAVE :: xthk , ythk
   INTEGER , DIMENSION(32) , SAVE :: xu , xv , xw , yu , yv , yw
   EXTERNAL af , gmmats , invers , mat , transs , trif
!
! End of declarations rewritten by SPAG
!
!
!     ECPT ENTRIES
!
!     ECPT ( 1) = ELEMENT ID                                    INTEGER
!     ECPT ( 2) = SCALAR INDEX NUMBER FOR GRID POINT 1          INTEGER
!     ECPT ( 3) = SCALAR INDEX NUMBER FOR GRID POINT 2          INTEGER
!     ECPT ( 4) = SCALAR INDEX NUMBER FOR GRID POINT 3          INTEGER
!     ECPT ( 5) = SCALAR INDEX NUMBER FOR GRID POINT 4          INTEGER
!     ECPT ( 6) = SCALAR INDEX NUMBER FOR GRID POINT 5          INTEGER
!     ECPT ( 7) = SCALAR INDEX NUMBER FOR GRID POINT 6          INTEGER
!     ECPT ( 8) = THETA                                         REAL
!     ECPT ( 9) = MATERIAL ID 1                                 INTEGER
!     ECPT (10) = THICKNESS T1 AT GRID POINT G1
!     ECPT (11) = THICKNESS T3 AT GRID POINT G3
!     ECPT (12) = THICKNESS T5 AT GRID POINT G5
!     ECPT (13) = MATERIAL ID 2                                 INTEGER
!     ECPT (14) = THICKNESS TBEND1 FOR BENDING AT GRID POINT G1
!     ECPT (15) = THICKNESS TBEND3 FOR BENDING AT GRID POINT G3
!     ECPT (16) = THICKNESS TBEND5 FOR BENDING AT GRID POINT G5
!     ECPT (17) = MATERIAL ID 3                                 INTEGER
!     ECPT (18) = THICKNESS TSHR1 FOR TRANSVERSE SHEAR AT GRID POINT G1
!     ECPT (19) = THICKNESS TSHR3 FOR TRANSVERSE SHEAR AT GRID POINT G3
!     ECPT (20) = THICKNESS TSHR5 FOR TRANSVERSE SHEAR AT GRID POINT G5
!     ECPT (21) = NON-STRUCTURAL MASS                           REAL
!     ECPT (22) = DISTANCE Z11 FOR STRESS CALCULATION  AT GRID POINT G1
!     ECPT (23) = DISTANCE Z21 FOR STRESS CALCULATION  AT GRID POINT G1
!     ECPT (24) = DISTANCE Z13 FOR STRESS CALCULATION  AT GRID POINT G3
!     ECPT (25) = DISTANCE Z23 FOR STRESS CALCULATION  AT GRID POINT G3
!     ECPT (26) = DISTANCE Z15 FOR STRESS CALCULATION  AT GRID POINT G5
!     ECPT (27) = DISTANCE Z25 FOR STRESS CALCULATION  AT GRID POINT G5
!
!     X1,Y1,Z1 FOR ALL SIX POINTS ARE  IN NASTRAN BASIC SYSTEM
!
!     ECPT (28) = CO-ORDINATE SYSTEM ID FOR GRID A              INTEGER
!     ECPT (29) = CO-ORDINATE X1                                REAL
!     ECPT (30) = CO-ORDINATE Y1                                REAL
!     ECPT (31) = CO-ORDINATE Z1                                REAL
!     ECPT (32) = CO-ORDINATE SYSTEM ID FOR GRID B              INTEGER
!     ECPT (33) = CO-ORDINATE X1                                REAL
!     ECPT (34) = CO-ORDINATE Y1                                REAL
!     ECPT (35) = CO-ORDINATE Z1                                REAL
!     ECPT (36) = CO-ORDINATE SYSTEM ID FOR GRID C              INTEGER
!     ECPT (37) = CO-ORDINATE X1                                REAL
!     ECPT (38) = CO-ORDINATE Y1                                REAL
!     ECPT (39) = CO-ORDINATE Z1                                REAL
!     ECPT (40) = CO-ORDINATE SYSTEM ID FOR GRID D              INTEGER
!     ECPT (41) = CO-ORDINATE X1                                REAL
!     ECPT (42) = CO-ORDINATE Y1                                REAL
!     ECPT (43) = CO-ORDINATE Z1                                REAL
!     ECPT (44) = CO-ORDINATE SYSTEM ID FOR GRID E              INTEGER
!     ECPT (45) = CO-ORDINATE X1                                REAL
!     ECPT (46) = CO-ORDINATE Y1                                REAL
!     ECPT (47) = CO-ORDINATE Z1                                REAL
!     ECPT (48) = CO-ORDINATE SYSTEM ID FOR GRID F              INTEGER
!     ECPT (49) = CO-ORDINATE X1                                REAL
!     ECPT (50) = CO-ORDINATE Y1                                REAL
!     ECPT (51) = CO-ORDINATE Z1                                REAL
!     EST  (52) = ELEMENT TEMPERATURE
!
   !>>>>EQUIVALENCE (c1,cc(1)) , (c2,cc(2)) , (c3,cc(3)) , (c4,cc(4)) , (c5,cc(5)) , (c6,cc(6)) , (c7,cc(7)) , (c8,cc(8)) , (c9,cc(9)) , &
!>>>>    & (c10,cc(10)) , (Iest(1),Est(1)) , (a,dista) , (b,distb) , (c,distc) , (thk1,tbend1) , (thk2,tbend3) , (thk3,tbend5)
   DATA xu/0 , 1 , 0 , 2 , 1 , 0 , 26*0/ , yu/0 , 0 , 1 , 0 , 1 , 2 , 26*0/ , xv/6*0 , 0 , 1 , 0 , 2 , 1 , 0 , 20*0/ , yv/6*0 , 0 , &
      & 0 , 1 , 0 , 1 , 2 , 20*0/ , xw/12*0 , 0 , 1 , 0 , 2 , 1 , 0 , 3 , 2 , 1 , 0 , 4 , 3 , 2 , 1 , 0 , 5 , 3 , 2 , 1 , 0/ ,      &
      & yw/12*0 , 0 , 0 , 1 , 0 , 1 , 2 , 0 , 1 , 2 , 3 , 0 , 1 , 2 , 3 , 4 , 0 , 2 , 3 , 4 , 5/
   DATA blank , name/4H     , 4HTRSH , 4HL   /
   DATA rk , sk/0 , 1 , 0 , 0 , 0 , 1/ , degra/0.0174532925/
   DATA xthk/0 , 1 , 0 , 2 , 1 , 0 , 3 , 2 , 1 , 0/ , ythk/0 , 0 , 1 , 0 , 1 , 2 , 0 , 1 , 2 , 3/
   DATA tl/0 , 1 , 0/ , ul/0 , 0 , 1/ , pt/0 , 1 , 0/ , qt/0 , 0 , 1/
!
!     COMPONENT CODE,ICODE,IS  111111  AND HAS A VALUE OF 63
!
   nots = .FALSE.
   idele = iest(1)
   DO i = 1 , 6
      nl(i) = iest(i+1)
   ENDDO
   thetam = Est(8)
   matid1 = iest(9)
   tmem1 = Est(10)
   tmem3 = Est(11)
   tmem5 = Est(12)
   matid2 = iest(13)
   tbend1 = (Est(14)*12.0)**0.3333333333
   tbend3 = (Est(15)*12.0)**0.3333333333
   tbend5 = (Est(16)*12.0)**0.3333333333
   matid3 = iest(17)
   tshr1 = Est(18)
   tshr3 = Est(19)
   tshr5 = Est(20)
   nsm = Est(21)
   j = 0
   DO i = 28 , 48 , 4
      j = j + 1
      ics(j) = iest(i)
      xc(j) = Est(i+1)
      yc(j) = Est(i+2)
      zc(j) = Est(i+3)
   ENDDO
!
!     IF TMEM3 OR TMEM5 EQUAL TO ZERO OR BLANK, THEY WILL BE
!     SET EQUAL TO TMEM1 SO ALSO FOR TSHR3,TSHR5,TBEND3 AND TBEND5
!
   t1prim = -Treal(2)
   t3prim = -Treal(2)
   t5prim = -Treal(2)
   IF ( tmem3==0.0 .OR. tmem3==blank ) tmem3 = tmem1
   IF ( tmem5==0.0 .OR. tmem5==blank ) tmem5 = tmem1
   IF ( tshr3==0.0 .OR. tshr3==blank ) tshr3 = tshr1
   IF ( tshr5==0.0 .OR. tshr5==blank ) tshr5 = tshr1
   IF ( tshr1==0.0 ) nots = .TRUE.
   IF ( tbend3==0.0 .OR. tbend3==blank ) tbend3 = tbend1
   IF ( tbend5==0.0 .OR. tbend5==blank ) tbend5 = tbend1
   IF ( t3prim==0.0 .OR. t3prim==blank ) t3prim = t1prim
   IF ( t5prim==0.0 .OR. t5prim==blank ) t5prim = t1prim
   Eltemp = Est(52)
   avthk = (tbend1+tbend3+tbend5)/3.0
   aviner = avthk**3/12.0
   theta1 = thetam*degra
   Sinth = sin(theta1)
   Costh = cos(theta1)
   IF ( abs(Sinth)<=1.0E-06 ) Sinth = 0.0
!
!     EVALUTE MATERIAL PROPERTIES
!
   Matflg = 2
   Matid = matid1
   IF ( Matid>0 ) THEN
      CALL mat(idele)
!
      g11 = Em(1)
      g12 = Em(2)
      g13 = Em(3)
      g22 = Em(4)
      g23 = Em(5)
      g33 = Em(6)
      gm1 = Em(1)*Alf(1) + Em(2)*Alf(2) + Em(3)*Alf(3)
      gm2 = Em(2)*Alf(1) + Em(4)*Alf(2) + Em(5)*Alf(3)
      gm3 = Em(3)*Alf(1) + Em(5)*Alf(2) + Em(6)*Alf(3)
      g11pr = 0.0
      g22pr = 0.0
      g33pr = 0.0
   ENDIF
   Matflg = 2
   Matid = matid2
   IF ( Matid>0 ) THEN
      CALL mat(idele)
      d11 = Em(1)
      d12 = Em(2)
      d13 = Em(3)
      d22 = Em(4)
      d23 = Em(5)
      d33 = Em(6)
      g(1) = Em(1)
      g(2) = Em(2)
      g(3) = Em(3)
      g(4) = Em(2)
      g(5) = Em(4)
      g(6) = Em(5)
      g(7) = Em(3)
      g(8) = Em(5)
      g(9) = Em(6)
!
!     IF  TINT(6).NE.1,G1 IS G AND T1PRIME IS ALPHA TIMES T1PRIME
!     IF  TINT(6).EQ.1,G1 IS G TIMES ALPHA AND T1PRIME IS T1PRIME
!
      IF ( Tint(6)/=1 ) THEN
         DO i = 1 , 9
            ge1(i) = g(i)*aviner
         ENDDO
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY
!
         ising = -1
         CALL invers(3,ge1(1),3,ts6(1),0,determ,ising,index)
         IF ( ising==2 ) THEN
            WRITE (Nout,99001) Ufm , iest(1)
            nogo = .TRUE.
            RETURN
         ELSE
            CALL gmmats(ge1,3,3,0,Treal(2),3,1,0,pl(1))
         ENDIF
      ELSE
!
!     G1 IS G TIMES ALFA
!
         CALL gmmats(g,3,3,0,Alf,3,1,0,g1)
!
         g11pr = g1(1)
         g22pr = g1(2)
         g33pr = g1(3)
      ENDIF
   ENDIF
!
!     CALCULATIONS FOR THE TRIANGLE
!
   CALL trif(xc,yc,zc,ivect,jvect,kvect,a,b,c,iest(1),name)
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
      qqq(i2,3) = 1.0
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
      qqq(i3,2) = -1.0
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
         q(i,j) = qqq(i1,j)
      ENDDO
   ENDDO
!
!     SET ISING = -1
!
   ising = -1
   CALL invers(6,q,6,ts6(1),0,determ,ising,Ind)
   IF ( ising==2 ) THEN
      WRITE (Nout,99002) Ufm , iest(1)
!
99002 FORMAT (A23,' 2416, MATRIX RELATING GENERALIZED PARAMETERS AND ','GRID POINT DISPLACEMENTS IS SINGULAR.',/26X,                &
             &'CHECK COORDINATES OF ELEMENT  TRSHL WITH ID =',I9,1H.)
      nogo = .TRUE.
   ELSE
!
!     FOURTH ARGUMENT IS A DUMMY LOCATION FOR INVERSE AND HENCE TS1(1) I
!
!     AGAIN SET ISING = -1
!
      ising = -1
      CALL invers(20,qqq,20,ts6(1),0,determ,ising,index)
!
!     ISING EQUAL TO 2 IMPLIES THAT QQQ IS SINGULAR
!
      IF ( ising==2 ) THEN
         WRITE (Nout,99001) Ufm , iest(1)
         nogo = .TRUE.
      ELSE
!
!     FIRST 18 COLUMNS OF QQQ INVERSE IS THE QQQINV FOR USE IN STIFFNESS
!     MATRIX CALCULATIONS
!
         DO i = 1 , 960
            qq(i) = 0.0
         ENDDO
         DO i = 1 , 6
            DO j = 1 , 6
               ij = (i-1)*30 + j
               ik = (i+5)*30 + j + 6
               qq(ij) = q(i,j)
               qq(ik) = q(i,j)
            ENDDO
         ENDDO
         DO i = 1 , 20
            DO j = 1 , 18
               ij = 372 + (i-1)*30 + j
               qq(ij) = qqq(i,j)
               ij1 = (i-1)*18 + j
               qqqinv(ij1) = qqq(i,j)
            ENDDO
         ENDDO
!
!     START EXECUTION FOR STIFFNESS MATRIX CALCULATION
!
!     CM IS STIFFNESS MATRIX IN ELEMENT COORDINATES
!
!     EVALUATE THE CONSTANTS C1,C2,AND C3 IN THE LINEAR EQUATION FOR
!     THICKNESS VARIATION - MEMBRANE
!
         CALL af(F,14,a,b,c,c1,c2,c3,tmem1,tmem3,tmem5,1)
         cab(1) = c1
         cab(2) = c2
         cab(3) = c3
         area = F(1,1)
         vol = c1*F(1,1) + c2*F(2,1) + c3*F(1,2)
         uniben = .FALSE.
         unimem = .FALSE.
         unitem = .FALSE.
         IF ( abs(c2)<=1.0E-06 .AND. abs(c3)<=1.0E-06 ) unimem = .TRUE.
!
         d334 = d33*4.0
         d132 = d13*2.0
         d232 = d23*2.0
!
!     DD(1) TO DD(3) ARE THE CONSTANTS IN LINEAR EQUATION FOR TEMP
!     GRADIENT. CURRENTLY ONLY UNIFORM TEMP GRADIENT IS PERMITTED IN
!     THE ELEMENT, THOUGH THE CODE IS WRITTEN FOR LINEAR VARIATION
!
         CALL af(F,14,a,b,c,dd(1),dd(2),dd(3),t1prim,t3prim,t5prim,1)
!
!     EL(1) TO EL(3) ARE THE CONSTANTS IN THE LINEAR EQUATION FOR
!     MEAN TEMP VARIATION
!
         el(1) = Treal(1) - Tref
         el(2) = 0.0
         el(3) = 0.0
!
!     A1,A2,A3 ARE THE COEFFICIENTS OF LINEAR EQUATION FOR VARIATION
!     OF BENDING THICKNESSES
!
         CALL af(F,14,a,b,c,a1,a2,a3,thk1,thk2,thk3,1)
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
         IF ( abs(a2)<=1.0E-06 .AND. abs(a3)<=1.0E-06 ) uniben = .TRUE.
         el2 = el(2)
         el3 = el(3)
         IF ( abs(el2)<=1.E-06 .AND. abs(el3)<=1.E-06 ) unitem = .TRUE.
!
!     AA1, AA2, AA3  ARE COEFFICIENTS IN THICKNESS VARIATION FOR
!     TRANSVERSE SHEAR
!
         CALL af(F,14,a,b,c,aa1,aa2,aa3,tshr1,tshr3,tshr5,1)
         h4 = q(4,1)*zc(1) + q(4,2)*zc(2) + q(4,3)*zc(3) + q(4,4)*zc(4) + q(4,5)*zc(5) + q(4,6)*zc(6)
         h5 = q(5,1)*zc(1) + q(5,2)*zc(2) + q(5,3)*zc(3) + q(5,4)*zc(4) + q(5,5)*zc(5) + q(5,6)*zc(6)
         h6 = q(6,1)*zc(1) + q(6,2)*zc(2) + q(6,3)*zc(3) + q(6,4)*zc(4) + q(6,5)*zc(5) + q(6,6)*zc(6)
         h4 = h4*2.0
         h6 = h6*2.0
!
!     H5 IS MULTIPLIED BY 2.0, SO THAT EXY=DU/DY + DV/DX - ZXY*W
!
         h5 = h5*2.0
!
!     CALCULATION OF THERMAL LOAD VECTOR
!
         DO i = 1 , 32
            ptem(i) = 0.0
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
            mx01 = mx - 1
            mx1 = mx + 1
            nx01 = nx - 1
            nx1 = nx + 1
            ptemp = 0.0
            IF ( i<=12 ) THEN
               SPAG_Loop_2_2: DO k = 1 , 3
                  ixr = ix + rk(k)
                  jxs = jx + sk(k)
                  kxr = kx + rk(k)
                  lxs = lx + sk(k)
                  SPAG_Loop_3_1: DO l = 1 , 3
                     ixrt = ixr + tl(l)
                     jxsu1 = jxs + ul(l) + 1
                     kxrt1 = kxr + tl(l) + 1
                     lxsu = lxs + ul(l)
                     ixrt1 = ixrt + 1
                     jxsu = jxsu1 - 1
                     kxrt = kxrt1 - 1
                     lxsu1 = lxsu + 1
                     mkr1 = mx + kx + rk(k) - 1
                     nls1 = nx + lx + sk(k) - 1
                     IF ( ixrt>0 ) ptemp = ptemp + cab(k)*el(l)*gm1*rix*F(ixrt,jxsu1)
                     IF ( lxsu>0 ) ptemp = ptemp + cab(k)*el(l)*gm2*rlx*F(kxrt1,lxsu)
                     IF ( jxsu>0 ) ptemp = ptemp + cab(k)*el(l)*gm3*rjx*F(ixrt1,jxsu)
                     IF ( kxrt>0 ) ptemp = ptemp + cab(k)*el(l)*gm3*rkx*F(kxrt,lxsu1)
                     IF ( mkr1>0 .AND. nls1>0 ) ptemp = ptemp - (g11pr*h4*el(l)*cab(k)*F(mkr1,nls1))                                &
                        & - (g22pr*h4*el(l)*cab(k)*F(mkr1,nls1)) - (g33pr*h4*el(l)*cab(k)*F(mkr1,nls1))
                     IF ( unitem ) EXIT SPAG_Loop_3_1
                  ENDDO SPAG_Loop_3_1
                  IF ( unimem ) EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_2_2
               ptem(i) = ptemp
            ELSE
               SPAG_Loop_2_4: DO k = 1 , 10
                  mx01x = mx01 + xthk(k)
                  nx1y = nx1 + ythk(k)
                  mx1x = mx1 + xthk(k)
                  nx01y = nx01 + ythk(k)
                  mxx = mx + xthk(k)
                  nxy = nx + ythk(k)
                  IF ( Tint(6)==1 ) THEN
                     SPAG_Loop_3_3: DO l = 1 , 3
                        mx01xp = mx01x + pt(l)
                        nx1yq = nx1y + qt(l)
                        mx1xp = mx1x + pt(l)
                        nx01yq = nx01y + qt(l)
                        mxxp = mxx + pt(l)
                        nxyq = nxy + qt(l)
                        IF ( mx01xp>0 .AND. nx1yq>0 ) ptemp = ptemp + cc(k)*dd(l)*g1(1)*rmx1*F(mx01xp,nx1yq)
                        IF ( mx1xp>0 .AND. nx01yq>0 ) ptemp = ptemp + cc(k)*dd(l)*g1(2)*rnx1*F(mx1xp,nx01yq)
                        IF ( mxxp>0 .AND. nxyq>0 ) ptemp = ptemp + cc(k)*dd(l)*g1(3)*rmnx*F(mxxp,nxyq)
                        IF ( unitem ) EXIT SPAG_Loop_3_3
                     ENDDO SPAG_Loop_3_3
                  ENDIF
                  IF ( Tint(6)/=1 ) THEN
                     IF ( mx01x>0 ) ptemp = ptemp + cc(k)*rmx1*(pl(1)*g(1)+pl(2)*g(2)+pl(3)*g(3))*F(mx01x,nx1y)
                     IF ( nx01y>0 ) ptemp = ptemp + cc(k)*rnx1*(pl(1)*g(4)+pl(2)*g(5)+pl(3)*g(6))*F(mx1x,nx01y)
                     IF ( mxx>0 .AND. nxy>0 ) ptemp = ptemp + cc(k)*rmnx*(pl(1)*g(7)+pl(2)*g(8)+pl(3)*g(9))*F(mxx,nxy)
                  ENDIF
                  IF ( uniben ) EXIT SPAG_Loop_2_4
               ENDDO SPAG_Loop_2_4
               ptem(i) = ptemp/12.0
            ENDIF
         ENDDO
         CALL gmmats(qq,32,30,+1,ptem,32,1,0,P6)
         DO i = 1 , 30
            ee(i) = 0.0
         ENDDO
         ee(1) = ivect(1)
         ee(2) = jvect(1)
         ee(3) = kvect(1)
         ee(6) = ivect(2)
         ee(7) = jvect(2)
         ee(8) = kvect(2)
         ee(11) = ivect(3)
         ee(12) = jvect(3)
         ee(13) = kvect(3)
         ee(19) = ivect(1)
         ee(20) = jvect(1)
         ee(24) = ivect(2)
         ee(25) = jvect(2)
         ee(29) = ivect(3)
         ee(30) = jvect(3)
         DO k = 1 , 6
            DO i = 1 , 2
               k1 = 6*(i-1) + k
               i1 = 5*(k-1) + i
               P7(i1) = P6(k1)
            ENDDO
         ENDDO
         DO k = 1 , 6
            DO i = 1 , 3
               i2 = 5*(k-1) + i + 2
               k2 = 12 + (k-1)*3 + i
               P7(i2) = P6(k2)
            ENDDO
         ENDDO
!
!     LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL (THAT IS
!     COORDINATE AT ANY GRID POINT IN WHICH DISPLACEMENT AND STRESSES
!     ARE ROTATED
!     - NOT NEEDED IF FIELD 7 IN GRID CARD IS ZERO)
!
!     TRANSFORM STIFFNESS MATRIX FROM ELEMENT COORDINATES TO BASIC
!     COORDINATES
!
!     TRANSFORM STIFFNESS MATRIX FROM BASIC COORDINAYES TO GLOBAL (DISP)
!     COORDINATES
!
         DO i = 1 , 6
            small(i) = i
         ENDDO
         DO i = 1 , 6
            sil1 = small(i)
            DO ii = 1 , 36
               balotr(ii) = 0.0
            ENDDO
            DO k = 1 , 5
               k1 = (sil1-1)*5 + k
               p8(k) = P7(k1)
            ENDDO
            CALL gmmats(ee,6,5,0,p8,5,1,0,p9)
!
!     TRANSFORM THE KSUB(36) FROM BASIC TO DISPLACEMENT COORDINATES
!
            IF ( nl(sil1)/=0 .AND. ics(sil1)/=0 ) THEN
               jj = 4*sil1 + 24
               CALL transs(iest(jj),trand)
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
               CALL gmmats(balotr(1),6,6,1,p9(1),6,1,0,P6(1))
               DO k = 1 , 6
                  p9(k) = P6(k)
               ENDDO
            ENDIF
            DO ii = 1 , 6
               i2 = iest(i+1) + ii - 1
               Pg(i2) = Pg(i2) + p9(ii)
            ENDDO
         ENDDO
      ENDIF
   ENDIF
99001 FORMAT (A23,' 2417, A SINGULAR MATERIAL MATRIX FOR ELEMENT ID =',I9,' HAS BEEN DETECTED BY SUBROUTINE TLODSL',/26X,'WHILE',   &
             &' TRYING TO COMPUTE THERMAL LOADS WITH TEMPP2 CARD DATA.')
END SUBROUTINE tlodsl
