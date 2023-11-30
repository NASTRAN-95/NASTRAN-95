
SUBROUTINE strsl1
   IMPLICIT NONE
   REAL A , A1 , A2 , A3 , Aa1 , Aa2 , Aa3 , Alf(3) , B , Balotr(36) , C , Cab(3) , Costh , D(9) , Dista , Distb , Distc , Dph1(9) ,&
      & E(18) , E1(18) , Ee(30) , Ee1(5,6) , Eltemp , Em(6) , Emod(9) , Eph1(15) , Est(100) , Forvec(24) , G(4) , Gph1(6) , Gsube , &
      & Ph1ben(9) , Ph1mem(6) , Ph1out(1200) , Ph1shr(6) , Ph2(18) , Ph3(12) , Ph4(90) , Pla34 , Q(6,6) , Qq , Qqq(20,20) ,         &
      & Qqqinv(360) , Rhoy , Rj11 , Rj12 , Rj22 , Si(9) , Sigcy , Sigsy , Sigty , Sinth , Tm(96) , Tmqq(90) , Trans(9) , Tref ,     &
      & Ts6(40) , Ts7(60) , V1(3) , V2(3) , V3(3) , X , Xc(6) , Y , Yc(6) , Z , Zc(6)
   INTEGER Ics(6) , Iest(42) , Ind(6,3) , Index(20,3) , Matflg , Matid , Nl(6) , Nph1ou(990)
   COMMON /matin / Matid , Matflg , Eltemp , Pla34 , Sinth , Costh
   COMMON /matout/ Em , Rhoy , Alf , Tref , Gsube , Sigty , Sigcy , Sigsy , Rj11 , Rj12 , Rj22
   COMMON /sdr2x5/ Est , Ph1out , Forvec , X , Y , Z , Dista , Distb , Distc , A1 , A2 , A3 , Aa1 , Aa2 , Aa3 , Qqqinv , Qq , Tm ,  &
                 & Tmqq , Ts6 , Ts7 , Q , Ee , Ee1 , Ph2 , Ph3 , Ph4 , E , E1 , Xc , Yc , Zc , Ph1mem , Ph1ben , Ph1shr , Dph1 ,    &
                 & Eph1 , Gph1 , G , D , Ics , Nl , Cab , Trans , Balotr , Emod , Si
   REAL a1sq , a2sq , a3sq , blank , c1 , c10 , c2 , c3 , c4 , c5 , c6 , c7 , c8 , c9 , d11 , d12 , d13 , d132 , d21 , d22 , d23 ,  &
      & d232 , d31 , d32 , d33 , d334 , degra , det , determ , f , g11 , g12 , g13 , g22 , g23 , g33 , h4 , h5 , h6 , ivect(3) ,    &
      & j11 , j12 , j22 , jvect(3) , kvect(3) , nsm , tbend1 , tbend3 , tbend5 , theta1 , thetam , thk , thk1 , tmb(60) , tmbq(54) ,&
      & tmem1 , tmem2 , tmem3 , tmem5 , tmm(3,12) , tmmm(36) , tshr1 , tshr3 , tshr5 , x2 , x2y , x2y2 , x2y3 , x3 , x3y , x3y2 ,   &
      & x4 , x5 , xy , xy2 , xy3 , xy4 , y2 , y3 , y4
   INTEGER i , i1 , i2 , i3 , idele , ii , ij , ij1 , ising , j , j1 , j2 , ji , jj , jst , kz , l , m , matid1 , matid2 , matid3 , &
         & mz , name(2)
   LOGICAL nots
   REAL y5
!
!     OUTPUTS FROM THIS PHASE FOR USE IN PHASE II ARE THE FOLLOWING
!
!     1)  ELEMENT ID              WORDS    1    STORAGE IN PH1OUT  1
!     2)  SIX SILS                WORDS    6                     2-7
!     3)  3 MEMBRANE  THICKNESES  WORDS    3                     8-10
!     4)  3 BENDING  THICKNESES   WORDS    3                    11-13
!     5)  8  STRESS DATA POINTS   WORDS    8                    14-21
!     6)  4 NOS. STRESS MATRICES (6-5X6 EACH) WORDS 720         22-741
!     7)  S SUB T MATRIX          WORDS  4X3                   742-753
!     8)  ELEMENT ID              WORD     1                   754
!     9)  SIX SILS                WORDS    6                   755-760
!     10) ELEMENT TEMPERATURE     WORD     1                   761
!     11) 4 NOS. MEMBRANE STRESS MATRICES 4(6-3X3)             762-1193
!
!     ECPT LISTS
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
!     ECPT (26) = DISTANCE 015 FOR STRESS CALCULATION  AT GRID POINT G5
!     ECPT (27) = DISTANCE Z25 FOR STRESS CALCULATION  AT GRID POINT G5
!
!     X1,Y1,Z1 FOR ALL SIX POINTS ARE IN NASTRAN BASIC SYSTEM
!
!     ECPT (28) = CO-ORDINATE SYSTEM ID FOR GRID A              INTEGER
!     ECPT (29) = CO-ORDINATE X1                                REAL
!     ECPT (30) = CO-ORDINATE Y1                                REAL
!     ECPT (31) = CO-ORDINATE Z1                                REAL
!     ECPT (32) = CO-ORDINATE SYSTEM ID FOR GRID B              INTEGER
!     ECPT (33) = CO-ORDINATE X1                                REAL
!     ECPT (34) = CO-ORDINATE Y1                                REAL
!     ECPT (35) = CO-ORDINATE Z1                                REAL
!     ECPT (36) = CO-ORDINATE SYSTEM ID FOR GRID C              INTEGE9
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
!     ECPT (52) = ELEMENT TEMPERATURE  AT GRID POINTS G1        REAL
!     ECPT (53) = ELEMENT TEMPERATURE  AT GRID POINTS G2        REAL
!     ECPT (54) = ELEMENT TEMPERATURE  AT GRID POINTS G3        REAL
!     ECPT (55) = ELEMENT TEMPERATURE  AT GRID POINTS G4        REAL
!     ECPT (56) = ELEMENT TEMPERATURE  AT GRID POINTS G5        REAL
!     ECPT (57) = ELEMENT TEMPERATURE  AT GRID POINTS G6        REAL
!
   !>>>>EQUIVALENCE (A,Dista) , (B,Distb) , (C,Distc) , (V1(1),Est(29)) , (V2(1),Est(37)) , (V3(1),Est(45)) , (Iest(1),Est(1)) ,         &
!>>>>    & (tmmm(1),tmm(1,1)) , (Ph1out(1),Qqq(1,1)) , (Ph1out(401),Index(1,1),Ind(1,1)) , (Nph1ou(1),Ph1out(1))
   DATA degra/0.0174532925/
   DATA blank , name/4H     , 4HTRSH , 4HL   /
!
   nots = .TRUE.
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
   IF ( tmem3==0.0 .OR. tmem3==blank ) tmem3 = tmem1
   IF ( tmem5==0.0 .OR. tmem5==blank ) tmem5 = tmem1
   IF ( tshr3==0.0 .OR. tshr3==blank ) tshr3 = tshr1
   IF ( tshr5==0.0 .OR. tshr5==blank ) tshr5 = tshr1
   IF ( tshr1==0.0 ) nots = .TRUE.
   IF ( tbend3==0.0 .OR. tbend3==blank ) tbend3 = tbend1
   IF ( tbend5==0.0 .OR. tbend5==blank ) tbend5 = tbend1
   DO i = 28 , 48 , 4
      j = j + 1
      Ics(j) = Iest(i)
      Xc(j) = Est(i+1)
      Yc(j) = Est(i+2)
      Zc(j) = Est(i+3)
   ENDDO
   Eltemp = Est(52)
   theta1 = thetam*degra
   Sinth = sin(theta1)
   Costh = cos(theta1)
   IF ( abs(Sinth)<=1.0E-06 ) Sinth = 0.0
!
!     EVALUATE MATERIAL PROPERTIES
!
   Matflg = 2
   Matid = matid1
   IF ( matid1/=0 ) THEN
      CALL mat(idele)
      g11 = Em(1)
      g12 = Em(2)
      g13 = Em(3)
      g22 = Em(4)
      g23 = Em(5)
      g33 = Em(6)
   ENDIF
!
   Matid = matid2
   IF ( matid2/=0 ) THEN
      Matflg = 2
      CALL mat(idele)
      d11 = Em(1)
      d12 = Em(2)
      d13 = Em(3)
      d21 = d12
      d22 = Em(4)
      d23 = Em(5)
      d31 = d13
      d32 = d23
      d33 = Em(6)
      D(1) = d11
      D(2) = d12
      D(3) = d13
      D(4) = d21
      D(5) = d22
      D(6) = d23
      D(7) = d13
      D(8) = d23
      D(9) = d33
      d334 = d33*4.0
      d132 = d13*2.0
      d232 = d23*2.0
      j11 = 0.0
      j12 = 0.0
      j22 = 0.0
      IF ( .NOT.(nots) ) CALL mat(idele)
   ENDIF
!
!     CALCULATIONS FOR THE TRIANGLE
!
   CALL trif(Xc,Yc,Zc,ivect,jvect,kvect,A,B,C,Iest(1),name)
!
!     EVALUATE THE CONSTANTS C1,C2,AND C3 IN THE LINEAR EQUATION FOR
!     THICKNESS VARIATION - MEMBRANE
!
   CALL af(f,1,A,B,C,c1,c2,c3,tmem1,tmem2,tmem3,1)
   Cab(1) = c1
   Cab(2) = c2
   Cab(3) = c3
!
!     A1,A2,A3 ARE THE COEFFICIENTS OF LINEAR EQUATION FOR VARIATION
!     OF BENDING THICKNESSES
!
   CALL af(f,1,A,B,C,A1,A2,A3,tbend1,tbend3,tbend5,1)
   a1sq = A1*A1
   a2sq = A2*A2
   a3sq = A3*A3
   c1 = a1sq*A1
   c2 = 3.0*a1sq*A2
   c3 = 3.0*a1sq*A3
   c4 = 3.0*A1*a2sq
   c5 = 6.0*A1*A2*A3
   c6 = 3.0*a3sq*A1
   c7 = a2sq*A2
   c8 = 3.0*a2sq*A3
   c9 = 3.0*A2*a3sq
   c10 = A3*a3sq
!
!     AA1, AA2, AA3  ARE COEFFICIENTS IN THICKNESS VARIATION FOR
!     TRANSVERSE SHEAR
!
   CALL af(f,1,A,B,C,Aa1,Aa2,Aa3,tshr1,tshr3,tshr5,1)
!
!
!     FILL E-MATRIX
!
   DO i = 1 , 18
      E(i) = 0.0
   ENDDO
   E(1) = kvect(1)
   E(4) = kvect(2)
   E(7) = kvect(3)
   E(11) = ivect(1)
   E(14) = ivect(2)
   E(17) = ivect(3)
   E(12) = jvect(1)
   E(15) = jvect(2)
   E(18) = jvect(3)
!
!     CALCULATIONS FOR QMATRIX (QQQ) AND ITS INVERSE
!
   DO i = 1 , 400
      Qqq(i,1) = 0.0
   ENDDO
   DO i = 1 , 6
      i1 = (i-1)*3 + 1
      i2 = (i-1)*3 + 2
      i3 = (i-1)*3 + 3
      Qqq(i1,1) = 1.0
      Qqq(i1,2) = Xc(i)
      Qqq(i1,3) = Yc(i)
      Qqq(i1,4) = Xc(i)*Xc(i)
      Qqq(i1,5) = Xc(i)*Yc(i)
      Qqq(i1,6) = Yc(i)*Yc(i)
      Qqq(i1,7) = Qqq(i1,4)*Xc(i)
      Qqq(i1,8) = Qqq(i1,4)*Yc(i)
      Qqq(i1,9) = Qqq(i1,5)*Yc(i)
      Qqq(i1,10) = Qqq(i1,6)*Yc(i)
      Qqq(i1,11) = Qqq(i1,7)*Xc(i)
      Qqq(i1,12) = Qqq(i1,7)*Yc(i)
      Qqq(i1,13) = Qqq(i1,8)*Yc(i)
      Qqq(i1,14) = Qqq(i1,9)*Yc(i)
      Qqq(i1,15) = Qqq(i1,10)*Yc(i)
      Qqq(i1,16) = Qqq(i1,11)*Xc(i)
      Qqq(i1,17) = Qqq(i1,12)*Yc(i)
      Qqq(i1,18) = Qqq(i1,13)*Yc(i)
      Qqq(i1,19) = Qqq(i1,14)*Yc(i)
      Qqq(i1,20) = Qqq(i1,15)*Yc(i)
      Qqq(i2,3) = 1.0
      Qqq(i2,5) = Xc(i)
      Qqq(i2,6) = Yc(i)*2.0
      Qqq(i2,8) = Qqq(i1,4)
      Qqq(i2,9) = Qqq(i1,5)*2.0
      Qqq(i2,10) = Qqq(i1,6)*3.0
      Qqq(i2,12) = Qqq(i1,7)
      Qqq(i2,13) = Qqq(i1,8)*2.0
      Qqq(i2,14) = Qqq(i1,9)*3.0
      Qqq(i2,15) = Qqq(i1,10)*4.0
      Qqq(i2,17) = Qqq(i1,12)*2.0
      Qqq(i2,18) = Qqq(i1,13)*3.0
      Qqq(i2,19) = Qqq(i1,14)*4.0
      Qqq(i2,20) = Qqq(i1,15)*5.0
      Qqq(i3,2) = -1.0
      Qqq(i3,4) = -2.0*Xc(i)
      Qqq(i3,5) = -Yc(i)
      Qqq(i3,7) = -Qqq(i1,4)*3.0
      Qqq(i3,8) = -Qqq(i1,5)*2.0
      Qqq(i3,9) = -Qqq(i1,6)
      Qqq(i3,11) = -Qqq(i1,7)*4.0
      Qqq(i3,12) = -Qqq(i1,8)*3.0
      Qqq(i3,13) = -Qqq(i1,9)*2.0
      Qqq(i3,14) = -Qqq(i1,10)
      Qqq(i3,16) = -Qqq(i1,11)*5.0
      Qqq(i3,17) = -Qqq(i1,13)*3.0
      Qqq(i3,18) = -Qqq(i1,14)*2.0
      Qqq(i3,19) = -Qqq(i1,15)
   ENDDO
!
   Qqq(19,16) = 5.0*A**4*C
   Qqq(19,17) = 3.0*A**2*C**3 - 2.0*A**4*C
   Qqq(19,18) = -2.0*A*C**4 + 3.0*A**3*C**2
   Qqq(19,19) = C**5 - 4.0*A**2*C**3
   Qqq(19,20) = 5.0*A*C**4
   Qqq(20,16) = 5.0*B**4*C
   Qqq(20,17) = 3.0*B**2*C**3 - 2.0*B**4*C
   Qqq(20,18) = 2.0*B*C**4 - 3.0*B**3*C**2
   Qqq(20,19) = C**5 - 4.0*B**2*C**3
   Qqq(20,20) = -5.0*B*C**4
   DO i = 1 , 6
      DO j = 1 , 6
         i1 = 3*(i-1) + 1
         Q(i,j) = Qqq(i1,j)
      ENDDO
   ENDDO
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
   ising = -1
   CALL invers(6,Q,6,Ts6(1),0,det,ising,Ind)
!
!     FOURTH ARGUMENT IS A DUMMY LOCATION FOR INVERSE AND HENCE TS1(1)
!     IS U
!
!     SET ISING = -1 AGAIN.
   ising = -1
   CALL invers(20,Qqq,20,Ts6(1),0,determ,ising,Index)
!
!     ISING EQUAL TO 2 IMPLIES THAT QQQ IS SINGULAR
!
!
!     FIRST 18 COLUMNS OF QQQ INVERSE IS THE QQQINV FOR USE IN STIFFNESS
!     MATRIX CALCULATIONS
!
   h4 = Q(4,1)*Zc(1) + Q(4,2)*Zc(2) + Q(4,3)*Zc(3) + Q(4,4)*Zc(4) + Q(4,5)*Zc(5) + Q(4,6)*Zc(6)
   h5 = Q(5,1)*Zc(1) + Q(5,2)*Zc(2) + Q(5,3)*Zc(3) + Q(5,4)*Zc(4) + Q(5,5)*Zc(5) + Q(5,6)*Zc(6)
   h6 = Q(6,1)*Zc(1) + Q(6,2)*Zc(2) + Q(6,3)*Zc(3) + Q(6,4)*Zc(4) + Q(6,5)*Zc(5) + Q(6,6)*Zc(6)
   h4 = h4*2.0
   h6 = h6*2.0
!
!     H5 IS MULTIPLIED BY 2.0, SO THAT EXY=DU/DY + DV/DX - ZXY * W
!
   h5 = h5*2.0
   DO i = 1 , 20
      DO j = 1 , 18
         ij = (i-1)*18 + j
         Qqqinv(ij) = Qqq(i,j)
      ENDDO
   ENDDO
   DO i = 1 , 36
      Balotr(i) = 0.0
   ENDDO
!
   DO i = 1 , 7
      Nph1ou(i) = Iest(i)
   ENDDO
   IF ( matid2==0 ) THEN
      DO i = 2 , 7
         Nph1ou(i) = 0
      ENDDO
   ENDIF
   Ph1out(8) = tmem1
   Ph1out(9) = tmem3
   Ph1out(10) = tmem5
   Ph1out(11) = tbend1
   Ph1out(12) = tbend3
   Ph1out(13) = tbend5
   Ph1out(14) = Est(22)
   Ph1out(15) = Est(23)
   Ph1out(16) = Est(24)
   Ph1out(17) = Est(25)
   Ph1out(18) = Est(26)
   Ph1out(19) = Est(27)
   Emod(1) = g11
   Emod(2) = g12
   Emod(3) = g13
   Emod(4) = g12
   Emod(5) = g22
   Emod(6) = g23
   Emod(7) = g13
   Emod(8) = g23
   Emod(9) = g33
   DO i = 1 , 30
      Ee(i) = 0.0
   ENDDO
   Ee(1) = ivect(1)
   Ee(2) = jvect(1)
   Ee(3) = kvect(1)
   Ee(6) = ivect(2)
   Ee(7) = jvect(2)
   Ee(8) = kvect(2)
   Ee(11) = ivect(3)
   Ee(12) = jvect(3)
   Ee(13) = kvect(3)
   Ee(19) = ivect(1)
   Ee(20) = jvect(1)
   Ee(24) = ivect(2)
   Ee(25) = jvect(2)
   Ee(29) = ivect(3)
   Ee(30) = jvect(3)
   DO jj = 1 , 4
      j = 2*jj - 1
      IF ( jj==4 ) THEN
         X = (Xc(1)+Xc(3)+Xc(5))/3.0
         Y = (Yc(1)+Yc(3)+Yc(5))/3.0
         Ph1out(20) = (A1+A2*X+A3*Y)/2.0
         Ph1out(21) = -Ph1out(20)
      ELSE
         X = Xc(j)
         Y = Yc(j)
      ENDIF
      IF ( matid2/=0 ) THEN
         DO i = 1 , 60
            Ts7(i) = 0.0
         ENDDO
         thk = A1 + A2*X + A3*Y
         thk1 = thk**3/12.0
         D(1) = d11*thk1
         D(2) = d12*thk1
         D(3) = d13*thk1
         D(4) = D(2)
         D(5) = d22*thk1
         D(6) = d23*thk1
         D(7) = D(3)
         D(8) = D(6)
         D(9) = d33*thk1
         x2 = X*X
         xy = X*Y
         y2 = Y*Y
         x3 = x2*X
         x2y = x2*Y
         xy2 = X*y2
         y3 = y2*Y
         Ts7(4) = 2.0
         Ts7(7) = 6.0*X
         Ts7(8) = 2.0*Y
         Ts7(11) = 12.0*x2
         Ts7(12) = 6.0*xy
         Ts7(13) = 2.0*y2
         Ts7(16) = 20.0*x3
         Ts7(17) = 6.0*xy2
         Ts7(18) = 2.0*y3
         Ts7(26) = 2.0
         Ts7(29) = 2.0*X
         Ts7(30) = 6.0*Y
         Ts7(33) = 2.0*x2
         Ts7(34) = Ts7(12)
         Ts7(35) = 12.0*y2
         Ts7(37) = 2.0*x3
         Ts7(38) = 6.0*x2y
         Ts7(39) = 12.0*xy2
         Ts7(40) = 20.0*y3
         Ts7(45) = 2.0
         Ts7(48) = 4.0*X
         Ts7(49) = 4.0*Y
         Ts7(52) = 6.0*x2
         Ts7(53) = 8.0*xy
         Ts7(54) = 6.0*y2
         Ts7(57) = 12.0*x2y
         Ts7(58) = Ts7(39)
         Ts7(59) = 8.0*y3
         CALL gmmats(Ts7,3,20,0,Qqqinv,20,18,0,Ph4(1))
         CALL strslv(Ts6,nots)
         CALL gmmats(Ts6,2,20,0,Qqqinv,20,18,0,Ph4(55))
      ENDIF
!
      IF ( matid1/=0 ) THEN
         DO i = 1 , 36
            tmmm(i) = 0.0
         ENDDO
         DO j = 1 , 6
            j1 = (j-1)*2 + 1
            j2 = j1 + 1
            tmm(1,j1) = Q(2,j) + 2.0*X*Q(4,j) + Y*Q(5,j)
            tmm(2,j2) = Q(3,j) + X*Q(5,j) + 2.0*Y*Q(6,j)
            tmm(3,j1) = tmm(2,j2)
            tmm(3,j2) = tmm(1,j1)
         ENDDO
         x4 = x3*X
         x3y = x3*Y
         x2y2 = x2*y2
         xy3 = X*y3
         y4 = Y*y3
         x5 = x4*X
         x3y2 = x3*y2
         x2y3 = x2*y3
         xy4 = X*y4
         y5 = Y*y4
         tmb(1) = -h4
         tmb(2) = -h4*X
         tmb(3) = -h4*Y
         tmb(4) = -h4*x2
         tmb(5) = -h4*xy
         tmb(6) = -h4*y2
         tmb(7) = -h4*x3
         tmb(8) = -h4*x2y
         tmb(9) = -h4*xy2
         tmb(10) = -h4*y3
         tmb(11) = -h4*x4
         tmb(12) = -h4*x3y
         tmb(13) = -h4*x2y2
         tmb(14) = -h4*xy3
         tmb(15) = -h4*y4
         tmb(16) = -h4*x5
         tmb(17) = -h4*x3y2
         tmb(18) = -h4*x2y3
         tmb(19) = -h4*xy4
         tmb(20) = -h4*y5
         tmb(21) = -h6
         tmb(22) = -h6*X
         tmb(23) = -h6*Y
         tmb(24) = -h6*x2
         tmb(25) = -h6*xy
         tmb(26) = -h6*y2
         tmb(27) = -h6*x3
         tmb(28) = -h6*x2y
         tmb(29) = -h6*xy2
         tmb(30) = -h6*y3
         tmb(31) = -h6*x4
         tmb(32) = -h6*x3y
         tmb(33) = -h6*x2y2
         tmb(34) = -h6*xy3
         tmb(35) = -h6*y4
         tmb(36) = -h6*x5
         tmb(37) = -h6*x3y2
         tmb(38) = -h6*x2y3
         tmb(39) = -h6*xy4
         tmb(40) = -h6*y5
         tmb(41) = -h5
         tmb(42) = -h5*X
         tmb(43) = -h5*Y
         tmb(44) = -h5*x2
         tmb(45) = -h5*xy
         tmb(46) = -h5*y2
         tmb(47) = -h5*x3
         tmb(48) = -h5*x2y
         tmb(49) = -h5*xy2
         tmb(50) = -h5*y3
         tmb(51) = -h5*x4
         tmb(52) = -h5*x3y
         tmb(53) = -h5*x2y2
         tmb(54) = -h5*xy3
         tmb(55) = -h5*y4
         tmb(56) = -h5*x5
         tmb(57) = -h5*x3y2
         tmb(58) = -h5*x2y3
         tmb(59) = -h5*xy4
         tmb(60) = -h5*y5
         CALL gmmats(tmb,3,20,0,Qqqinv,20,18,0,tmbq)
      ENDIF
!
      DO ii = 1 , 6
         IF ( Ics(ii)==0 ) THEN
            DO i = 1 , 3
               DO j = 1 , 6
                  i1 = (i-1)*6 + j
                  j1 = (j-1)*3 + i
                  E1(i1) = E(j1)
               ENDDO
            ENDDO
         ELSE
            CALL transs(Iest(4*ii+24),Trans)
            DO j = 1 , 3
               l = 6*(j-1) + 1
               m = 3*(j-1) + 1
               Balotr(l) = Trans(m)
               Balotr(l+1) = Trans(m+1)
               Balotr(l+2) = Trans(m+2)
               Balotr(l+21) = Trans(m)
               Balotr(l+22) = Trans(m+1)
               Balotr(l+23) = Trans(m+2)
            ENDDO
            CALL gmmats(E,6,3,+1,Balotr,6,6,0,E1)
         ENDIF
         IF ( matid2/=0 ) THEN
            kz = (ii-1)*3 + 1
            Ph1ben(1) = Ph4(kz)
            Ph1ben(2) = Ph4(kz+1)
            Ph1ben(3) = Ph4(kz+2)
            Ph1ben(4) = Ph4(kz+18)
            Ph1ben(5) = Ph4(kz+19)
            Ph1ben(6) = Ph4(kz+20)
            Ph1ben(7) = Ph4(kz+36)
            Ph1ben(8) = Ph4(kz+37)
            Ph1ben(9) = Ph4(kz+38)
            CALL gmmats(D,3,3,0,Ph1ben,3,3,0,Dph1)
            CALL gmmats(Dph1,3,3,0,E1,3,6,0,Ph2)
            mz = (ii-1)*3 + 55
            Ph1shr(1) = Ph4(mz)
            Ph1shr(2) = Ph4(mz+1)
            Ph1shr(3) = Ph4(mz+2)
            Ph1shr(4) = Ph4(mz+18)
            Ph1shr(5) = Ph4(mz+19)
            Ph1shr(6) = Ph4(mz+20)
            IF ( nots ) THEN
               Gph1(1) = Ph1shr(1)
               Gph1(2) = Ph1shr(2)
               Gph1(3) = Ph1shr(3)
               Gph1(4) = Ph1shr(4)
               Gph1(5) = Ph1shr(5)
               Gph1(6) = Ph1shr(6)
            ELSE
               thk = Aa1 + Aa2*X + Aa3*Y
               G(1) = Em(6)*thk
               G(2) = 0.0
               G(3) = 0.0
               G(4) = G(1)
               CALL gmmats(G,2,2,0,Ph1shr,2,3,0,Gph1)
            ENDIF
            CALL gmmats(Gph1,2,3,0,E1,3,6,0,Ph3)
            DO i = 1 , 3
               DO j = 1 , 6
                  i1 = (i-1)*6 + j
                  i2 = i1 + 18
                  j1 = (ii-1)*30 + (jj-1)*180 + i1 + 21
                  j2 = j1 + 18
                  Ph1out(j1) = Ph2(i1)
                  IF ( i/=3 ) Ph1out(j2) = Ph3(i1)
               ENDDO
            ENDDO
         ENDIF
!
         IF ( matid1/=0 ) THEN
            DO i = 1 , 3
               DO j = 1 , 2
                  ji = (i-1)*5 + j
                  ij = (j-1)*3 + i + (ii-1)*6
                  Tm(ji) = tmmm(ij)
               ENDDO
            ENDDO
            DO i = 1 , 3
               DO j = 1 , 3
                  ji = (i-1)*5 + j + 2
                  ij = (i-1)*18 + j + (ii-1)*3
                  Tm(ji) = tmbq(ij)
               ENDDO
            ENDDO
            IF ( Ics(ii)/=0 ) CALL gmmats(Ee,6,5,+1,Balotr,6,6,0,Ee1)
            ij1 = (jj-1)*108 + (ii-1)*18 + 762
            CALL gmmats(Emod,3,3,0,Tm(1),3,5,0,Eph1)
            IF ( Ics(ii)==0 ) CALL gmmats(Eph1,3,5,0,Ee,6,5,+1,Ph1out(ij1))
            IF ( Ics(ii)/=0 ) CALL gmmats(Eph1,3,5,0,Ee1,5,6,0,Ph1out(ij1))
         ENDIF
      ENDDO
   ENDDO
!
   jst = 742 + (jj-1)*3
   IF ( matid2/=0 ) CALL gmmats(D,3,3,0,Alf(1),3,1,0,Ph1out(jst))
   IF ( matid1/=0 ) CALL gmmats(Emod,3,3,0,Alf(1),3,1,0,Ph1out(1194))
   IF ( matid1==0 ) THEN
      DO i = 1 , 7
         Nph1ou(753+i) = 0
      ENDDO
   ELSE
      DO i = 1 , 7
         Nph1ou(753+i) = Iest(i)
      ENDDO
   ENDIF
   Ph1out(761) = Tref
END SUBROUTINE strsl1