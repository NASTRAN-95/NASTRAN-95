!*==tlodm6.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE tlodm6(Ti)
   IMPLICIT NONE
   USE C_MATIN
   USE C_MATOUT
   USE C_TRIMEX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: Ti
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , b , c , c1 , c2 , c3 , d1 , d2 , d3 , determ , dista , distb , distc , nsm , ptemp , rix , rjx , rkx , rlx , t1bar , &
         & t2bar , t3bar , t5bar , theta1 , thetam , tmem1 , tmem3 , tmem5 , to1 , to3 , to5
   REAL , SAVE :: blank , degra
   REAL , DIMENSION(3) :: cc , dd , g1 , ivect , jvect , kvect , psubt , psubt1
   REAL , DIMENSION(6) :: e , xc , yc , zc
   REAL , DIMENSION(5,5) :: f
   REAL , DIMENSION(9) :: g , trans
   INTEGER :: i , i1 , i2 , idele , ii , ising , ix , ixr , ixrt , ixrt1 , j , jx , jxs , jxsu , jxsu1 , k , k1 , kx , kxr , kxrt , &
            & kxrt1 , l , lx , lxs , lxsu , lxsu1 , matid1 , sil1
   INTEGER , DIMENSION(6) :: ics , nl , sil
   INTEGER , DIMENSION(45) :: iest
   INTEGER , DIMENSION(6,3) :: ind
   REAL , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(2) :: psub
   REAL , DIMENSION(12) :: ptele , ptem
   REAL , DIMENSION(18) :: ptglb
   REAL , DIMENSION(6,6) :: q
   REAL , DIMENSION(36) :: qinv
   INTEGER , DIMENSION(3) , SAVE :: rk , sk , tl , ul
   LOGICAL :: unimem , unitem
   INTEGER , DIMENSION(12) , SAVE :: xu , xv , yu , yv
!
! End of declarations rewritten by SPAG
!
!
!     THERMAL LOAD VECTOR FOR TRIM6 (LINEAR STRAIN MEMBRANE TRIANGLE)
!     ELEMENT
!
!     EST ENTRIES
!
!     EST ( 1) = ELEMENT ID                              INTEGER
!     EST ( 2) = SCALAR INDEX NUMBER FOR GRID POINT 1    INTEGER
!     EST ( 3) = SCALAR INDEX NUMBER FOR GRID POINT 2    INTEGER
!     EST ( 4) = SCALAR INDEX NUMBER FOR GRID POINT 3    INTEGER
!     EST ( 5) = SCALAR INDEX NUMBER FOR GRID POINT 4    INTEGER
!     EST ( 6) = SCALAR INDEX NUMBER FOR GRID POINT 5    INTEGER
!     EST ( 7) = SCALAR INDEX NUMBER FOR GRID POINT 6    INTEGER
!     EST ( 8) = THETA                                   REAL
!     EST ( 9) = MATERIAL IDENTIFICATION NUMBER          INTEGER
!     EST (10) = THICKNESS T1 AT GRID POINT 1            REAL
!     EST (11) = THICKNESS T3 AT GRID POINT 3            REAL
!     EST (12) = THICKNESS T5 AT GRID POINT 5            REAL
!     EST (13) = NON-STRUCTURAL MASS                     REAL
!     X1,Y1,Z1 FOR ALL SIX POINTS ARE IN NASTRAN BASIC SYSTEM
!
!     EST (14) = CO-ORDINATE SYSTEM ID FOR GRID POINT 1  INTEGER
!     EST (15) = CO-ORDINATE X1                          REAL
!     EST (16) = CO-ORDINATE Y1                          REAL
!     EST (17) = CO-ORDINATE Z1                          REAL
!     EST (18) = CO-ORDINATE SYSTEM ID FOR GRID POINT 2  INTEGER
!     EST (19) = CO-ORDINATE X2                          REAL
!     EST (20) = CO-ORDINATE Y2                          REAL
!     EST (21) = CO-ORDINATE Z2                          REAL
!     EST (22) = CO-ORDINATE SYSTEM ID FOR GRID POINT 3  INTEGER
!     EST (23) = CO-ORDINATE X3                          REAL
!     EST (24) = CO-ORDINATE Y3                          REAL
!     EST (25) = CO-ORDINATE Z3                          REAL
!     EST (26) = CO-ORDINATE SYSTEM ID FOR GRID POINT 4  INTEGER
!     EST (27) = CO-ORDINATE X4                          REAL
!     EST (28) = CO-ORDINATE Y4                          REAL
!     EST (29) = CO-ORDINATE Z4                          REAL
!     EST (30) = CO-ORDINATE SYSTEM ID FOR GRID POINT 5  INTEGER
!     EST (31) = CO-ORDINATE X5                          REAL
!     EST (32) = CO-ORDINATE Y5                          REAL
!     EST (33) = CO-ORDINATE Z5                          REAL
!     EST (34) = CO-ORDINATE SYSTEM ID FOR GRID POINT 6  INTEGER
!     EST (35) = CO-ORDINATE X6                          REAL
!     EST (36) = CO-ORDINATE Y6                          REAL
!     EST (37) = CO-ORDINATE Z6                          REAL
!     EST (38) TO EST (43)  -  ELEMENT TEMPERATURES AT SIX GRID POINTS
!
!
!     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
!
!     EQUIVALENCE  IEST WITH EST IN COMMON BLOCK /EMGEST/ SINCE EST IS
!     A MIXED INTEGER AND REAL ARRAY
!
   !>>>>EQUIVALENCE (Iest(1),Est(1))
   !>>>>EQUIVALENCE (a,dista) , (b,distb) , (c,distc) , (cc(1),c1) , (cc(2),c2) , (cc(3),c3) , (dd(1),d1) , (dd(2),d2) , (dd(3),d3)
   DATA xu/0 , 1 , 0 , 2 , 1 , 0 , 6*0/ , yu/0 , 0 , 1 , 0 , 1 , 2 , 6*0/
   DATA xv/6*0 , 0 , 1 , 0 , 2 , 1 , 0/ , yv/6*0 , 0 , 0 , 1 , 0 , 1 , 2/
   DATA rk/0 , 1 , 0/ , sk/0 , 0 , 1/
   DATA tl/0 , 1 , 0/ , ul/0 , 0 , 1/
   DATA blank/4H    / , name/4HTRIM , 4H6   /
   DATA degra/0.0174532925/
!
!     ALLOCATE EST VALUES TO RESPECTIVE  LOCAL  VARIABLES
!
   idele = iest(1)
   DO i = 1 , 6
      nl(i) = iest(i+1)
   ENDDO
   thetam = Est(8)
   matid1 = iest(9)
   tmem1 = Est(10)
   tmem3 = Est(11)
   tmem5 = Est(12)
!
!     IF TMEM3 OR TMEM5 IS 0.0 OR BLANK,IT WILL BE SET EQUAL TO TMEM1
!
   IF ( tmem3==0.0 .OR. tmem3==blank ) tmem3 = tmem1
   IF ( tmem5==0.0 .OR. tmem5==blank ) tmem5 = tmem1
!
   nsm = Est(13)
!
   j = 0
   DO i = 14 , 34 , 4
      j = j + 1
      ics(j) = iest(i)
      xc(j) = Est(i+1)
      yc(j) = Est(i+2)
      zc(j) = Est(i+3)
   ENDDO
!
!     TEMPERATURE AT THE THREE GRID POINTS ARE  DENOTED BY TO1,TO3 AND
!     TO5
!
   to1 = Ti(1)
   to3 = Ti(3)
   to5 = Ti(5)
!
   Eltemp = (Est(38)+Est(39)+Est(40)+Est(41)+Est(42)+Est(43))/6.0
   theta1 = thetam*degra
   Sinth = sin(theta1)
   Costh = cos(theta1)
   IF ( abs(Sinth)<=1.0E-06 ) Sinth = 0.0
!
!     CALCULATIONS FOR THE  TRIANGLE
!
   CALL trif(xc,yc,zc,ivect,jvect,kvect,a,b,c,iest(1),name)
!
!     COMPUTE THE AREA INTEGRATION FUNCTION F, AND
!     EVALUATE THE CONSTANTS C1,C2,AND C3 IN THE LINEAR EQUATION FOR
!     THICKNESS VARIATION
!
   CALL af(f,5,a,b,c,c1,c2,c3,tmem1,tmem3,tmem5,0)
   unimem = .FALSE.
   IF ( abs(c2)<=1.0E-06 .AND. abs(c3)<=1.0E-06 ) unimem = .TRUE.
!
!     CALCULATIONS FOR  Q MATRIX AND ITS INVERSE
!
   DO i = 1 , 6
      DO j = 1 , 6
         q(i,j) = 0.0
      ENDDO
   ENDDO
   DO i = 1 , 6
      q(i,1) = 1.0
      q(i,2) = xc(i)
      q(i,3) = yc(i)
      q(i,4) = xc(i)*xc(i)
      q(i,5) = xc(i)*yc(i)
      q(i,6) = yc(i)*yc(i)
   ENDDO
!
!     FIND INVERSE OF Q MATRIX
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
   ising = -1
   CALL invers(6,q,6,qinv(1),0,determ,ising,ind)
!
!     ISING EQUAL TO 2 IMPLIES THAT Q MATRIX IS SINGULAR
!
!     EVALUATE  MATERIAL PROPERTIES AND FILL IN G MATRIX
!
   Matflg = 2
   Matid = matid1
   CALL mat(idele)
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
!     G1 IS G TIMES ALFA
!
   CALL gmmats(g,3,3,0,Alf,3,1,0,g1)
!
!     CALCULATION OF THERMAL LOAD VECTOR
!
!     EVALUATE THE CONSTANTS D1,D2,D3 IN THE LINEAR EQUATION FOR
!     TEMPERATURE VARIATION OVER THE ELEMENT
!
   t1bar = to1 - Tref
   t3bar = to3 - Tref
   t5bar = to5 - Tref
!
   CALL af(f,5,a,b,c,d1,d2,d3,t1bar,t2bar,t3bar,1)
   unitem = .FALSE.
   IF ( abs(d2)<=1.0E-06 .AND. abs(d3)<=1.0E-06 ) unitem = .TRUE.
   DO i = 1 , 12
      ix = xu(i)
      rix = ix
      jx = yu(i)
      rjx = jx
      kx = xv(i)
      rkx = kx
      lx = yv(i)
      rlx = lx
      ptemp = 0.0
      DO k = 1 , 3
         ixr = ix + rk(k)
         jxs = jx + sk(k)
         kxr = kx + rk(k)
         lxs = lx + sk(k)
         DO l = 1 , 3
            ixrt = ixr + tl(l)
            jxsu1 = jxs + ul(l) + 1
            kxrt1 = kxr + tl(l) + 1
            lxsu = lxs + ul(l)
            ixrt1 = ixrt + 1
            jxsu = jxsu1 - 1
            kxrt = kxrt1 - 1
            lxsu1 = lxsu + 1
            IF ( ixrt>0 ) ptemp = ptemp + cc(k)*dd(l)*g1(1)*rix*f(ixrt,jxsu1)
            IF ( lxsu>0 ) ptemp = ptemp + cc(k)*dd(l)*g1(2)*rlx*f(kxrt1,lxsu)
            IF ( jxsu>0 ) ptemp = ptemp + cc(k)*dd(l)*g1(3)*rjx*f(ixrt1,jxsu)
            IF ( kxrt>0 ) ptemp = ptemp + cc(k)*dd(l)*g1(3)*rkx*f(kxrt,lxsu1)
            IF ( unitem ) EXIT
         ENDDO
         IF ( unimem ) EXIT
      ENDDO
      ptem(i) = ptemp
   ENDDO
!
   CALL gmmats(q,6,6,0,ptem(1),6,1,0,ptele(1))
   CALL gmmats(q,6,6,0,ptem(7),6,1,0,ptele(7))
!
!     REORDER THE THERMAL LOAD VEC SO THAT THE DISPLACEMENTS OF A GRID
!     POINT ARE ARRANGED CONSECUTIVELY
!
   DO k = 1 , 6
      DO i = 1 , 2
         k1 = 6*(i-1) + k
         i1 = 2*(k-1) + i
         ptem(i1) = ptele(k1)
      ENDDO
   ENDDO
!
!     TRANSFORM THE THERMAL LOAD VECTOR PTEM FROM ELEMENT CO-ORDINATES
!     TO BASIC CO-ORDINATES
!
   e(1) = ivect(1)
   e(2) = jvect(1)
   e(3) = ivect(2)
   e(4) = jvect(2)
   e(5) = ivect(3)
   e(6) = jvect(3)
   DO i = 1 , 18
      ptglb(i) = 0.0
   ENDDO
   DO i = 1 , 6
      sil(i) = i
   ENDDO
   DO i = 1 , 6
      sil1 = sil(i)
      DO k = 1 , 2
         k1 = (sil1-1)*2 + k
         psub(k) = ptem(k1)
      ENDDO
      CALL gmmats(e,3,2,0,psub,2,1,0,psubt)
!
!     TRANSFORM THE PSUBT ROM BASIC TO GLOBAL CO-ORDINATES
!
      IF ( nl(sil1)/=0 .AND. ics(sil1)/=0 ) THEN
         k = 4*sil1 + 10
         CALL transs(iest(k),trans)
         CALL gmmats(trans(1),3,3,1,psubt,3,1,0,psubt1)
         DO k = 1 , 3
            psubt(k) = psubt1(k)
         ENDDO
      ENDIF
!
!     INSERT PTGLB IN GLOBAL LOAD VECTOR PG
!
      DO ii = 1 , 3
         i1 = (i-1)*3 + ii
         i2 = iest(i+1) + ii - 1
         ptglb(i1) = psubt(ii)
         Pg(i2) = Pg(i2) + psubt(ii)
      ENDDO
   ENDDO
END SUBROUTINE tlodm6
