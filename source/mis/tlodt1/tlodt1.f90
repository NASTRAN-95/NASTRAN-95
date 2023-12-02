!*==tlodt1.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE tlodt1(Treal,Tint)
   IMPLICIT NONE
   USE c_matin
   USE c_matout
   USE c_ssgwrk
   USE c_system
   USE c_trimex
   USE c_xmssg
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: Treal
   INTEGER , DIMENSION(6) :: Tint
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , a1sq , a2sq , a3sq , aviner , avthk , b , c , c1 , c10 , c2 , c3 , c4 , c5 , c6 , c7 , c8 , c9 , d1 , d2 , d3 ,      &
         & determ , distab , ptemp , rmnx , rmx , rmx1 , rnx , rnx1 , t1prim , t3prim , t5prim , temp1 , temp3 , temp5 , theta1 ,   &
         & thetam , thk1 , thk2 , thk3 , tmem1 , tmem3 , tmem5 , tshr1 , tshr3 , tshr5
   REAL , SAVE :: blank , degra
   REAL , DIMENSION(3) :: dd
   REAL , DIMENSION(10,10) :: f
   REAL , DIMENSION(9) :: ge1 , trand
   INTEGER :: i , i1 , i2 , i3 , idele , ii , ij , ising , j , jj , k , k1 , l , m , matid1 , mx , mx01 , mx01x , mx01xp , mx1 ,    &
            & mx1x , mx1xp , mxx , mxxp , nx , nx01 , nx01y , nx01yq , nx1 , nx1y , nx1yq , nxy , nxyq , sil1
   INTEGER , DIMENSION(6) :: ics , sil
   INTEGER , DIMENSION(42) :: iest
   INTEGER , DIMENSION(2) , SAVE :: nam
   LOGICAL :: nogo , nots , uniben , unitem
   INTEGER , DIMENSION(3) , SAVE :: pt , qt
   REAL , DIMENSION(360) :: qqinv
   REAL , DIMENSION(20,20) :: qqq
   REAL , DIMENSION(60) :: ts1 , ts2
   REAL , DIMENSION(6) :: xc , yc , zc
   INTEGER , DIMENSION(20) , SAVE :: xpower , ypower
   INTEGER , DIMENSION(10) , SAVE :: xthk , ythk
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
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
   !>>>>EQUIVALENCE (thk1,tmem1) , (thk2,tmem3) , (thk3,tmem5) , (A,Dista) , (B,Distb) , (C,Distc) , (Iest(1),Est(1)) , (C1,Cc(1)) ,     &
!>>>>    & (C2,Cc(2)) , (C3,Cc(3)) , (C4,Cc(4)) , (C5,Cc(5)) , (C6,Cc(6)) , (C7,Cc(7)) , (C8,Cc(8)) , (C9,Cc(9)) , (C10,Cc(10)) ,        &
!>>>>    & (D(1),D1) , (D(2),D2) , (D(3),D3) , (Dd(1),D(1))
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
   idele = iest(1)
   DO i = 1 , 6
      nl(i) = iest(i+1)
   ENDDO
   thetam = est(8)
   matid1 = iest(9)
   tmem1 = (est(10)*12.0)**0.333333333333
   tmem3 = (est(11)*12.0)**0.333333333333
   tmem5 = (est(12)*12.0)**0.333333333333
   tshr1 = est(14)
   tshr3 = est(15)
   tshr5 = est(16)
   j = 0
   DO i = 24 , 44 , 4
      j = j + 1
      ics(j) = iest(i)
      xc(j) = est(i+1)
      yc(j) = est(i+2)
      zc(j) = est(i+3)
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
   eltemp = est(48)
   avthk = (tmem1+tmem3+tmem5)/3.0
   aviner = avthk**3/12.0
   IF ( tshr1==0.0 ) nots = .TRUE.
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
   g(1) = em(1)
   g(2) = em(2)
   g(3) = em(3)
   g(4) = em(2)
   g(5) = em(4)
   g(6) = em(5)
   g(7) = em(3)
   g(8) = em(5)
   g(9) = em(6)
!
!     IF TINT(6).NE.1,G1 IS G AND T1PRIME IS ALPHA TIMES T1PRIME
!     IF TINT(6).EQ.1,G1 IS G TIMES ALPHA AND T1PRIME IS T1PRIME
!
   IF ( Tint(6)/=1 ) THEN
      DO i = 1 , 9
         ge1(i) = g(i)*aviner
      ENDDO
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
      ising = -1
      CALL invers(3,ge1(1),3,ts1(1),0,determ,ising,index)
      IF ( ising==2 ) THEN
!
         WRITE (iout,99001) ufm , iest(1)
99001    FORMAT (A23,' 2412, A SINGULAR MATERIAL MATRIX FOR ELEMENT ID =',I9,' HAS BEEN DETECTED BY SUBROUTINE TLODT1',/26X,'WHILE',&
                &' TRYING TO COMPUTE THERMAL LOADS WITH TEMPP2 CARD DATA.')
         nogo = .TRUE.
         GOTO 99999
      ELSE
         CALL gmmats(ge1,3,3,0,Treal(2),3,1,0,tl(1))
      ENDIF
   ELSE
!
!     G1 IS G TIMES ALPHA
!
      CALL gmmats(g,3,3,0,alf,3,1,0,g1)
   ENDIF
!
!     CALCULATIONS FOR THE TRIANGLE
!
   CALL trif(xc,yc,zc,ivect,jvect,kvect,a,b,c,iest(1),nam)
!
!     FILL E-MATRIX
!
   DO i = 1 , 18
      e(i) = 0.0
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
!     EVALUATE CONSTANTS D1,D2,D3 IN THE LINEAR EQUATION FOR TEMPERATURE
!     GRADIENT VARIATION OVER THE ELEMENT
!
   CALL af(f,10,a,b,c,d1,d2,d3,thk1,thk2,thk3,1)
   unitem = .FALSE.
   IF ( abs(d2)<=1.0E-06 .AND. abs(d3)<=1.0E-06 ) unitem = .TRUE.
!
   distab = dista + distb
   a1 = (thk1*dista+thk2*distb)/distab
   a2 = (thk2-thk1)/distab
   a3 = (thk3-a1)/distc
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
   CALL af(f,10,a,b,c,b1,b2,b3,tshr1,tshr3,tshr5,1)
   uniben = .FALSE.
   IF ( abs(a2)<=1.0E-06 .AND. abs(a3)<=1.0E-06 ) uniben = .TRUE.
!
!     COMPUTE THE AREA INTEGRATION FUNCTION F
!
   CALL af(f,10,a,b,c,0,0,0,0,0,0,-1)
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
         x = xc(i)
         y = yc(i)
         CALL tlodt3(ts6,nots)
         DO jj = 1 , 20
            qqq(i2,jj) = qqq(i2,jj) - ts6(20+jj)
            qqq(i3,jj) = qqq(i3,jj) + ts6(jj)
         ENDDO
      ENDIF
   ENDDO
!
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
!     FOURTH ARGUMENT IS A DUMMY LOCATION FOR INVERSE AND HENCE TS1(1)
!     IS U
!
!     AGAIN SET ISING = -1
!
   ising = -1
   CALL invers(20,qqq,20,ts1(1),0,determ,ising,index)
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
               IF ( mx01xp>0 .AND. nx1yq>0 ) ptemp = ptemp + cc(k)*dd(l)*g1(1)*rmx1*f(mx01xp,nx1yq)
               IF ( mx1xp>0 .AND. nx01yq>0 ) ptemp = ptemp + cc(k)*dd(l)*g1(2)*rnx1*f(mx1xp,nx01yq)
               IF ( mxxp>0 .AND. nxyq>0 ) ptemp = ptemp + cc(k)*dd(l)*g1(3)*rmnx*f(mxxp,nxyq)
               IF ( unitem ) EXIT
            ENDDO
         ENDIF
!
         IF ( Tint(6)/=1 ) THEN
            IF ( mx01x>0 ) ptemp = ptemp + cc(k)*rmx1*(tl(1)*g(1)+tl(2)*g(2)+tl(3)*g(3))*f(mx01x,nx1y)
            IF ( nx01y>0 ) ptemp = ptemp + cc(k)*rnx1*(tl(1)*g(4)+tl(2)*g(5)+tl(3)*g(6))*f(mx1x,nx01y)
            IF ( mxx>0 .AND. nxy>0 ) ptemp = ptemp + cc(k)*rmnx*(tl(1)*g(7)+tl(2)*g(8)+tl(3)*g(9))*f(mxx,nxy)
         ENDIF
         IF ( uniben ) EXIT
      ENDDO
!
      ptem(i) = ptemp/12.0
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
         ptem(i) = ptem(i) + ts2(i)
      ENDDO
   ENDIF
!
!     (QQQINV) TRANSPOSE (KTR3)  (QQQINV)
!
   CALL gmmats(qqinv,20,18,+1,ptem,20,1,0,ptele)
!
!     LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL (THAT IS
!     COORDINATE AT ANY GRID POINT IN WHICH DISPLACEMENT AND STRESSES
!     ARE R - NOT NEEDED IF FIELD 7 IN GRID CARD IS ZERO)
!
   DO i = 1 , 36
      ptglb(i) = 0.0
   ENDDO
   DO i = 1 , 6
      sil(i) = i
   ENDDO
   DO i = 1 , 6
      DO ii = 1 , 36
         balotr(ii) = 0.0D0
      ENDDO
      sil1 = sil(i)
      DO k = 1 , 3
         k1 = (sil1-1)*3 + k
         psub(k) = ptele(k1)
      ENDDO
      CALL gmmats(e,6,3,0,psub,3,1,0,psubt)
!
!     TRANSFORM THE PSUBT(6) FROM BASIC TO DISPLACEMENT COORDINATES
!
      IF ( nl(i)/=0 .AND. ics(i)/=0 ) THEN
         jj = 4*i + 20
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
         CALL gmmats(balotr(1),6,6,1,psubt,6,1,0,psubt1)
         DO k = 1 , 6
            psubt(k) = psubt1(k)
         ENDDO
      ENDIF
!
!     INSERT PTGLB IN PG
!
      DO ii = 1 , 6
         i1 = (i-1)*6 + ii
         i2 = iest(i+1) + ii - 1
         ptglb(i1) = psubt(ii)
         pg(i2) = pg(i2) + psubt(ii)
      ENDDO
   ENDDO
99999 END SUBROUTINE tlodt1
