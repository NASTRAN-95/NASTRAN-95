
SUBROUTINE tlodt1(Treal,Tint)
   IMPLICIT NONE
   REAL A , A1 , A2 , A3 , Alf(3) , B , B1 , B2 , B3 , Balotr(36) , C , C1 , C10 , C2 , C3 , C4 , C5 , C6 , C7 , C8 , C9 , Cc(10) , &
      & Costh , D(3) , D1 , D2 , D3 , Dd(3) , Dista , Distb , Distc , E(18) , Eltemp , Em(6) , Est(100) , G(9) , G1(3) , Gsube ,    &
      & Ivect(3) , Jvect(3) , Kvect(3) , Pg(1) , Pla34 , Psub(3) , Psubt(6) , Psubt1(6) , Ptele(18) , Ptem(20) , Ptglb(36) , Rhoy , &
      & Rj11 , Rj12 , Rj22 , Sigcy , Sigsy , Sigty , Sinth , Sysbuf , Tl(3) , Tref , Ts6(40) , X , Y , Z
   INTEGER Iest(42) , Index(20,3) , Iout , Matflg , Matid , Name(2) , Nl(6)
   CHARACTER*23 Ufm
   COMMON /matin / Matid , Matflg , Eltemp , Pla34 , Sinth , Costh
   COMMON /matout/ Em , Rhoy , Alf , Tref , Gsube , Sigty , Sigcy , Sigsy , Rj11 , Rj12 , Rj22
   COMMON /ssgwrk/ X , Y , Z , Dista , Distb , Distc , A1 , A2 , A3 , B1 , B2 , B3 , G1 , D , E , Ivect , Jvect , Kvect , Cc , G ,  &
                 & Ptem , Ptele , Ptglb , Psub , Psubt , Psubt1 , Ts6 , Name , Index , Nl , Tl , Balotr
   COMMON /system/ Sysbuf , Iout
   COMMON /trimex/ Est
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Pg
   INTEGER Tint(6)
   REAL Treal(6)
   REAL a1sq , a2sq , a3sq , aviner , avthk , blank , degra , determ , distab , f(10,10) , ge1(9) , ptemp , qqinv(360) , qqq(20,20) &
      & , rmnx , rmx , rmx1 , rnx , rnx1 , t1prim , t3prim , t5prim , temp1 , temp3 , temp5 , theta1 , thetam , thk1 , thk2 , thk3 ,&
      & tmem1 , tmem3 , tmem5 , trand(9) , ts1(60) , ts2(60) , tshr1 , tshr3 , tshr5 , xc(6) , yc(6) , zc(6)
   INTEGER i , i1 , i2 , i3 , ics(6) , idele , ii , ij , ising , j , jj , k , k1 , l , m , matid1 , mx , mx01 , mx01x , mx01xp ,    &
         & mx1 , mx1x , mx1xp , mxx , mxxp , nam(2) , nx , nx01 , nx01y , nx01yq , nx1 , nx1y , nx1yq , nxy , nxyq , pt(3) , qt(3) ,&
         & sil(6) , sil1 , xpower(20) , xthk(10) , ypower(20) , ythk(10)
   LOGICAL nogo , nots , uniben , unitem
!
!     THERMAL LOAD VECTOR FOR TRPLT1 (HIGHER ORDER PLATE BENDING ELEMENT
!
!     ECPT ENTRIES
!     AS IN STIFFNESS ROUTINE KTRPL1
!
!
!     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
!
!     EQUIVALENCE IECPT WITH ECPT IN COMMON BLOCK /SMA1ET/ SINCE ECPT IS
!     A MIXED INTEGER AND REAL ARRAY
!
   EQUIVALENCE (thk1,tmem1) , (thk2,tmem3) , (thk3,tmem5) , (A,Dista) , (B,Distb) , (C,Distc) , (Iest(1),Est(1)) , (C1,Cc(1)) ,     &
    & (C2,Cc(2)) , (C3,Cc(3)) , (C4,Cc(4)) , (C5,Cc(5)) , (C6,Cc(6)) , (C7,Cc(7)) , (C8,Cc(8)) , (C9,Cc(9)) , (C10,Cc(10)) ,        &
    & (D(1),D1) , (D(2),D2) , (D(3),D3) , (Dd(1),D(1))
   DATA blank , nam/4H     , 4HTRPL , 4HT1  /
   DATA xpower/0 , 1 , 0 , 2 , 1 , 0 , 3 , 2 , 1 , 0 , 4 , 3 , 2 , 1 , 0 , 5 , 3 , 2 , 1 , 0/
   DATA ypower/0 , 0 , 1 , 0 , 1 , 2 , 0 , 1 , 2 , 3 , 0 , 1 , 2 , 3 , 4 , 0 , 2 , 3 , 4 , 5/
   DATA xthk/0 , 1 , 0 , 2 , 1 , 0 , 3 , 2 , 1 , 0/
   DATA ythk/0 , 0 , 1 , 0 , 1 , 2 , 0 , 1 , 2 , 3/
   DATA pt/0 , 1 , 0/ , qt/0 , 0 , 1/
   DATA degra/0.0174532925/
!
!
   nots = .FALSE.
   idele = Iest(1)
   DO i = 1 , 6
      Nl(i) = Iest(i+1)
   ENDDO
   thetam = Est(8)
   matid1 = Iest(9)
   tmem1 = (Est(10)*12.0)**0.333333333333
   tmem3 = (Est(11)*12.0)**0.333333333333
   tmem5 = (Est(12)*12.0)**0.333333333333
   tshr1 = Est(14)
   tshr3 = Est(15)
   tshr5 = Est(16)
   j = 0
   DO i = 24 , 44 , 4
      j = j + 1
      ics(j) = Iest(i)
      xc(j) = Est(i+1)
      yc(j) = Est(i+2)
      zc(j) = Est(i+3)
   ENDDO
   temp1 = Treal(1)
   temp3 = Treal(1)
   temp5 = Treal(1)
   t1prim = -Treal(2)
   t3prim = -Treal(2)
   t5prim = -Treal(2)
!
!     IF TMEM3 OR TMEM5 EQUAL TO ZERO OR BLANK,THEY WILL BE SET EQUAL TO
!     SO ALSO FOR TEMP3 AND TEMP5
!
   IF ( tmem3==0.0 .OR. tmem3==blank ) tmem3 = tmem1
   IF ( tmem5==0.0 .OR. tmem5==blank ) tmem5 = tmem1
   IF ( temp3==0.0 .OR. temp3==blank ) temp3 = temp1
   IF ( temp5==0.0 .OR. temp5==blank ) temp5 = temp1
   IF ( t3prim==.0 .OR. t3prim==blank ) t3prim = t1prim
   IF ( t5prim==.0 .OR. t5prim==blank ) t5prim = t1prim
   IF ( tshr3==0.0 .OR. tshr3==blank ) tshr3 = tshr1
   IF ( tshr5==0.0 .OR. tshr5==blank ) tshr5 = tshr1
   Eltemp = Est(48)
   avthk = (tmem1+tmem3+tmem5)/3.0
   aviner = avthk**3/12.0
   IF ( tshr1==0.0 ) nots = .TRUE.
   theta1 = thetam*degra
   Sinth = sin(theta1)
   Costh = cos(theta1)
   IF ( abs(Sinth)<=1.0E-06 ) Sinth = 0.0
!
!     EVALUATE MATERIAL PROPERTIES
!
   Matflg = 2
   Matid = matid1
   CALL mat(idele)
!
   G(1) = Em(1)
   G(2) = Em(2)
   G(3) = Em(3)
   G(4) = Em(2)
   G(5) = Em(4)
   G(6) = Em(5)
   G(7) = Em(3)
   G(8) = Em(5)
   G(9) = Em(6)
!
!     IF TINT(6).NE.1,G1 IS G AND T1PRIME IS ALPHA TIMES T1PRIME
!     IF TINT(6).EQ.1,G1 IS G TIMES ALPHA AND T1PRIME IS T1PRIME
!
   IF ( Tint(6)/=1 ) THEN
      DO i = 1 , 9
         ge1(i) = G(i)*aviner
      ENDDO
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
      ising = -1
      CALL invers(3,ge1(1),3,ts1(1),0,determ,ising,Index)
      IF ( ising==2 ) THEN
!
         WRITE (Iout,99001) Ufm , Iest(1)
99001    FORMAT (A23,' 2412, A SINGULAR MATERIAL MATRIX FOR ELEMENT ID =',I9,' HAS BEEN DETECTED BY SUBROUTINE TLODT1',/26X,'WHILE',&
                &' TRYING TO COMPUTE THERMAL LOADS WITH TEMPP2 CARD DATA.')
         nogo = .TRUE.
         GOTO 99999
      ELSE
         CALL gmmats(ge1,3,3,0,Treal(2),3,1,0,Tl(1))
      ENDIF
   ELSE
!
!     G1 IS G TIMES ALPHA
!
      CALL gmmats(G,3,3,0,Alf,3,1,0,G1)
   ENDIF
!
!     CALCULATIONS FOR THE TRIANGLE
!
   CALL trif(xc,yc,zc,Ivect,Jvect,Kvect,A,B,C,Iest(1),nam)
!
!     FILL E-MATRIX
!
   DO i = 1 , 18
      E(i) = 0.0
   ENDDO
   E(1) = Kvect(1)
   E(4) = Kvect(2)
   E(7) = Kvect(3)
   E(11) = Ivect(1)
   E(14) = Ivect(2)
   E(17) = Ivect(3)
   E(12) = Jvect(1)
   E(15) = Jvect(2)
   E(18) = Jvect(3)
!
!     EVALUATE CONSTANTS D1,D2,D3 IN THE LINEAR EQUATION FOR TEMPERATURE
!     GRADIENT VARIATION OVER THE ELEMENT
!
   CALL af(f,10,A,B,C,D1,D2,D3,thk1,thk2,thk3,1)
   unitem = .FALSE.
   IF ( abs(D2)<=1.0E-06 .AND. abs(D3)<=1.0E-06 ) unitem = .TRUE.
!
   distab = Dista + Distb
   A1 = (thk1*Dista+thk2*Distb)/distab
   A2 = (thk2-thk1)/distab
   A3 = (thk3-A1)/Distc
   a1sq = A1*A1
   a2sq = A2*A2
   a3sq = A3*A3
   C1 = a1sq*A1
   C2 = 3.0*a1sq*A2
   C3 = 3.0*a1sq*A3
   C4 = 3.0*A1*a2sq
   C5 = 6.0*A1*A2*A3
   C6 = 3.0*a3sq*A1
   C7 = a2sq*A2
   C8 = 3.0*a2sq*A3
   C9 = 3.0*A2*a3sq
   C10 = A3*a3sq
   CALL af(f,10,A,B,C,B1,B2,B3,tshr1,tshr3,tshr5,1)
   uniben = .FALSE.
   IF ( abs(A2)<=1.0E-06 .AND. abs(A3)<=1.0E-06 ) uniben = .TRUE.
!
!     COMPUTE THE AREA INTEGRATION FUNCTION F
!
   CALL af(f,10,A,B,C,0,0,0,0,0,0,-1)
!
!     CALCULATIONS FOR QMATRIX (QQQ) AND ITS INVERSE
!
   DO i = 1 , 400
      qqq(i,1) = 0.0
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
!
!     IF NO TRANSVERSE SHEAR GO TO 113
!
      IF ( .NOT.(nots) ) THEN
         X = xc(i)
         Y = yc(i)
         CALL tlodt3(Ts6,nots)
         DO jj = 1 , 20
            qqq(i2,jj) = qqq(i2,jj) - Ts6(20+jj)
            qqq(i3,jj) = qqq(i3,jj) + Ts6(jj)
         ENDDO
      ENDIF
   ENDDO
!
   qqq(19,16) = 5.0*A**4*C
   qqq(19,17) = 3.0*A**2*C**3 - 2.0*A**4*C
   qqq(19,18) = -2.0*A*C**4 + 3.0*A**3*C**2
   qqq(19,19) = C**5 - 4.0*A**2*C**3
   qqq(19,20) = 5.0*A*C**4
   qqq(20,16) = 5.0*B**4*C
   qqq(20,17) = 3.0*B**2*C**3 - 2.0*B**4*C
   qqq(20,18) = 2.0*B*C**4 - 3.0*B**3*C**2
   qqq(20,19) = C**5 - 4.0*B**2*C**3
   qqq(20,20) = -5.0*B*C**4
!
!     FOURTH ARGUMENT IS A DUMMY LOCATION FOR INVERSE AND HENCE TS1(1)
!     IS U
!
!     AGAIN SET ISING = -1
!
   ising = -1
   CALL invers(20,qqq,20,ts1(1),0,determ,ising,Index)
!
!     ISING EQUAL TO 2 IMPLIES THAT QQQ IS SINGULAR
!
!     FIRST 18 COLUMNS OF QQQ INVERSE IS THE QQQINV FOR USE IN STIFFNESS
!     MATRIX CALCULATIONS
!
   DO i = 1 , 20
      DO j = 1 , 18
         ij = (i-1)*18 + j
         qqinv(ij) = qqq(i,j)
      ENDDO
   ENDDO
!
   DO i = 1 , 20
      mx = xpower(i)
      rmx = mx
      nx = ypower(i)
      rnx = nx
      rmnx = rmx*rnx
      rmx1 = rmx*(rmx-1.0D0)
      rnx1 = rnx*(rnx-1.0D0)
      ptemp = 0.0
      mx01 = mx - 1
      mx1 = mx + 1
      nx01 = nx - 1
      nx1 = nx + 1
      DO k = 1 , 10
         mx01x = mx01 + xthk(k)
         nx1y = nx1 + ythk(k)
         mx1x = mx1 + xthk(k)
         nx01y = nx01 + ythk(k)
         mxx = mx + xthk(k)
         nxy = nx + ythk(k)
         IF ( Tint(6)==1 ) THEN
            DO l = 1 , 3
               mx01xp = mx01x + pt(l)
               nx1yq = nx1y + qt(l)
               mx1xp = mx1x + pt(l)
               nx01yq = nx01y + qt(l)
               mxxp = mxx + pt(l)
               nxyq = nxy + qt(l)
               IF ( mx01xp>0 .AND. nx1yq>0 ) ptemp = ptemp + Cc(k)*Dd(l)*G1(1)*rmx1*f(mx01xp,nx1yq)
               IF ( mx1xp>0 .AND. nx01yq>0 ) ptemp = ptemp + Cc(k)*Dd(l)*G1(2)*rnx1*f(mx1xp,nx01yq)
               IF ( mxxp>0 .AND. nxyq>0 ) ptemp = ptemp + Cc(k)*Dd(l)*G1(3)*rmnx*f(mxxp,nxyq)
               IF ( unitem ) EXIT
            ENDDO
         ENDIF
!
         IF ( Tint(6)/=1 ) THEN
            IF ( mx01x>0 ) ptemp = ptemp + Cc(k)*rmx1*(Tl(1)*G(1)+Tl(2)*G(2)+Tl(3)*G(3))*f(mx01x,nx1y)
            IF ( nx01y>0 ) ptemp = ptemp + Cc(k)*rnx1*(Tl(1)*G(4)+Tl(2)*G(5)+Tl(3)*G(6))*f(mx1x,nx01y)
            IF ( mxx>0 .AND. nxy>0 ) ptemp = ptemp + Cc(k)*rmnx*(Tl(1)*G(7)+Tl(2)*G(8)+Tl(3)*G(9))*f(mxx,nxy)
         ENDIF
         IF ( uniben ) EXIT
      ENDDO
!
      Ptem(i) = ptemp/12.0
   ENDDO
!
!     IF NO TRANSVERSE SHEAR GO TO 230
!
!     IF TSHR EQUAL TO ZERO OR MATID3 EQUAL TO ZERO, SKIP THESE
!     CALCULATIONS
!
   IF ( .NOT.(nots) ) THEN
!
      CALL tlodt2(ts1,ts2)
      DO i = 1 , 20
         Ptem(i) = Ptem(i) + ts2(i)
      ENDDO
   ENDIF
!
!     (QQQINV) TRANSPOSE (KTR3)  (QQQINV)
!
   CALL gmmats(qqinv,20,18,+1,Ptem,20,1,0,Ptele)
!
!     LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL (THAT IS
!     COORDINATE AT ANY GRID POINT IN WHICH DISPLACEMENT AND STRESSES
!     ARE R - NOT NEEDED IF FIELD 7 IN GRID CARD IS ZERO)
!
   DO i = 1 , 36
      Ptglb(i) = 0.0
   ENDDO
   DO i = 1 , 6
      sil(i) = i
   ENDDO
   DO i = 1 , 6
      DO ii = 1 , 36
         Balotr(ii) = 0.0D0
      ENDDO
      sil1 = sil(i)
      DO k = 1 , 3
         k1 = (sil1-1)*3 + k
         Psub(k) = Ptele(k1)
      ENDDO
      CALL gmmats(E,6,3,0,Psub,3,1,0,Psubt)
!
!     TRANSFORM THE PSUBT(6) FROM BASIC TO DISPLACEMENT COORDINATES
!
      IF ( Nl(i)/=0 .AND. ics(i)/=0 ) THEN
         jj = 4*i + 20
         CALL transs(Iest(jj),trand)
         DO jj = 1 , 3
            l = 6*(jj-1) + 1
            m = 3*(jj-1) + 1
            Balotr(l) = trand(m)
            Balotr(l+1) = trand(m+1)
            Balotr(l+2) = trand(m+2)
            Balotr(l+21) = trand(m)
            Balotr(l+22) = trand(m+1)
            Balotr(l+23) = trand(m+2)
         ENDDO
         CALL gmmats(Balotr(1),6,6,1,Psubt,6,1,0,Psubt1)
         DO k = 1 , 6
            Psubt(k) = Psubt1(k)
         ENDDO
      ENDIF
!
!     INSERT PTGLB IN PG
!
      DO ii = 1 , 6
         i1 = (i-1)*6 + ii
         i2 = Iest(i+1) + ii - 1
         Ptglb(i1) = Psubt(ii)
         Pg(i2) = Pg(i2) + Psubt(ii)
      ENDDO
   ENDDO
99999 RETURN
END SUBROUTINE tlodt1
