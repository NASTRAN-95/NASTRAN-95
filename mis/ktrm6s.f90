
SUBROUTINE ktrm6s
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alf(3) , Costh , Dumy(12) , Eltemp , Em(6) , Est(45) , Gsube , Pla34 , Rhoy , Rj11 , Rj12 , Rj22 , Sigcy , Sigsy , Sigty ,  &
      & Sinth , Tref
   INTEGER Elid , Eltype , Estid , Iest(45) , Ioutpt , Iprec , Ixtra , Izr , Kmbgg(3) , Ksystm(63) , Ldict , Matflg , Matid ,       &
         & Nlocs , Nok , Nom , Nzr
   LOGICAL Nogo
   CHARACTER*23 Ufm
   COMMON /blank / Nok , Nom
   COMMON /emgdic/ Eltype , Ldict , Nlocs , Elid , Estid
   COMMON /emgest/ Est
   COMMON /emgprm/ Ixtra , Izr , Nzr , Dumy , Kmbgg , Iprec , Nogo
   COMMON /matin / Matid , Matflg , Eltemp , Pla34 , Sinth , Costh
   COMMON /matout/ Em , Rhoy , Alf , Tref , Gsube , Sigty , Sigcy , Sigsy , Rj11 , Rj12 , Rj22
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm
!
! Local variable declarations
!
   REAL a , amass , area , b , balotr(9) , blank , c , c1 , c2 , c3 , cc(3) , degra , determ , dista , distb , distc , e(6) , f(6,6)&
      & , g11 , g12 , g13 , g22 , g23 , g33 , gk11(6,6) , gk12(6,6) , gk22(6,6) , gkt(12,12) , gktrm(12,12) , ivect(3) , jvect(3) , &
      & krt(9) , krt1(9) , ksub(2,2) , ksubt(3,2) , ktr(3,3) , ktr1(3,3) , ktrm(12,12) , ktrmg(324) , kvect(3) , nsm , q(6,6) ,     &
      & qinv(36) , rho , st , st1 , theta1 , thetam , tmem1 , tmem3 , tmem5 , trand(9) , vol , xc(6) , yc(6) , zc(6)
   INTEGER dict(11) , i , i1 , i1j1 , icode , ics(6) , idele , ii , ind(6,3) , ipass , isi , isil , ising , j , j1 , j1i1 , jj , k ,&
         & k1 , kr , ks , l , l1 , matid1 , mi , mimj , minj , mipj , miqj , mj , name(2) , ndof , ni , nimj , ninj , nipj , niqj , &
         & nj , nl(6) , nsq , pi , pimj , pinj , pipj , piqj , pj , qi , qimj , qinj , qipj , qiqj , qj , rk(3) , save(6) , sil(6) ,&
         & sil1 , sil2 , sk(3) , xu(12) , xv(12) , yu(12) , yv(12)
   LOGICAL imass , unimem
!
! End of declarations
!
!
!     STIFFNESS MATRIX FOR TRIANGULAR MEMBRANE ELEMENT  TRIM6
!     SINGLEPRECISION VERSION
!
!     EST ENTRIES
!
!     EST( 1) = ELEMENT ID                              INTEGER
!     EST( 2) = SCALAR INDEX NUMBER FOR GRID POINT 1    INTEGER
!     EST( 3) = SCALAR INDEX NUMBER FOR GRID POINT 2    INTEGER
!     EST( 4) = SCALAR INDEX NUMBER FOR GRID POINT 3    INTEGER
!     EST( 5) = SCALAR INDEX NUMBER FOR GRID POINT 4    INTEGER
!     EST( 6) = SCALAR INDEX NUMBER FOR GRID POINT 5    INTEGER
!     EST( 7) = SCALAR INDEX NUMBER FOR GRID POINT 6    INTEGER
!     EST( 8) = THETA                                   REAL
!     EST( 9) = MATERIAL IDENTIFICATION NUMBER          INTEGER
!     EST(10) = THICKNESS T1 AT GRID POINT 1            REAL
!     EST(11) = THICKNESS T3 AT GRID POINT 3            REAL
!     EST(12) = THICKNESS T5 AT GRID POINT 5            REAL
!     EST(13) = NON-STRUCTURAL MASS                     REAL
!
!     X1,Y1,Z1 FOR ALL SIX POINTS ARE IN NASTRAN BASIC SYSTEM
!
!     EST(14) = COORDINATE SYSTEM ID FOR GRID POINT 1   INTEGER
!     EST(15) = COORDINATE X1                           REAL
!     EST(16) = COORDINATE Y1                           REAL
!     EST(17) = COORDINATE Z1                           REAL
!     EST(18) = COORDINATE SYSTEM ID FOR GRID POINT 2   INTEGER
!     EST(19) = COORDINATE X2                           REAL
!     EST(20) = COORDINATE Y2                           REAL
!     EST(21) = COORDINATE Z2                           REAL
!     EST(22) = COORDINATE SYSTEM ID FOR GRID POINT 3   INTEGER
!     EST(23) = COORDINATE X3                           REAL
!     EST(24) = COORDINATE Y3                           REAL
!     EST(25) = COORDINATE Z3                           REAL
!     EST(26) = COORDINATE SYSTEM ID FOR GRID POINT 4   INTEGER
!     EST(27) = COORDINATE X4                           REAL
!     EST(28) = COORDINATE Y4                           REAL
!     EST(29) = COORDINATE Z4                           REAL
!     EST(30) = COORDINATE SYSTEM ID FOR GRID POINT 5   INTEGER
!     EST(31) = COORDINATE X5                           REAL
!     EST(32) = COORDINATE Y5                           REAL
!     EST(33) = COORDINATE Z5                           REAL
!     EST(34) = COORDINATE SYSTEM ID FOR GRID POINT 6   INTEGER
!     EST(35) = COORDINATE X6                           REAL
!     EST(36) = COORDINATE Y6                           REAL
!     EST(37) = COORDINATE Z6                           REAL
!     EST(38) TO EST (43) = ELEMENT TEMPERATURES AT SIX GRID POINTS
!
   EQUIVALENCE (Ksystm(2),Ioutpt) , (gkt(1,1),gktrm(1,1)) , (Est(1),Iest(1)) , (a,dista) , (b,distb) , (c,distc) , (cc(1),c1) ,     &
    & (cc(2),c2) , (cc(3),c3) , (krt(1),ktr(1,1)) , (krt1(1),ktr1(1,1))
   DATA xu/0 , 1 , 0 , 2 , 1 , 0 , 6*0/ , yu/0 , 0 , 1 , 0 , 1 , 2 , 6*0/
   DATA xv/6*0 , 0 , 1 , 0 , 2 , 1 , 0/ , yv/6*0 , 0 , 0 , 1 , 0 , 1 , 2/
   DATA rk/0 , 1 , 0/ , sk/0 , 0 , 1/
   DATA degra/0.0174532925/ , blank/4H    /
   DATA name/4HTRIM , 4H6   /
!
!     COMPONENT CODE,ICODE,IS  000111  AND HAS A VALUE OF 7
!
   icode = 7
   ndof = 18
   nsq = ndof**2
   dict(1) = Estid
   dict(2) = 1
   dict(3) = ndof
   dict(4) = icode
   dict(5) = Gsube
   ipass = 1
   imass = .FALSE.
   IF ( Nom>0 ) imass = .TRUE.
!
!     ALLOCATE EST VALUES TO RESPECTIVE  LOCAL  VARIABLES
!
   idele = Iest(1)
   DO i = 1 , 6
      nl(i) = Iest(i+1)
   ENDDO
   thetam = Est(8)
   matid1 = Iest(9)
   tmem1 = Est(10)
   tmem3 = Est(11)
   tmem5 = Est(12)
!
!     IF  TMEM3 OR TMEM5 IS 0.0 OR BLANK,IT WILL BE SET EQUAL TO TMEM1
!
   IF ( tmem3==0.0 .OR. tmem3==blank ) tmem3 = tmem1
   IF ( tmem5==0.0 .OR. tmem5==blank ) tmem5 = tmem1
!
   nsm = Est(13)
   j = 0
   DO i = 14 , 34 , 4
      j = j + 1
      ics(j) = Iest(i)
      xc(j) = Est(i+1)
      yc(j) = Est(i+2)
      zc(j) = Est(i+3)
   ENDDO
   Eltemp = (Est(38)+Est(39)+Est(40)+Est(41)+Est(42)+Est(43))/6.0
   theta1 = thetam*degra
   Sinth = sin(theta1)
   Costh = cos(theta1)
   IF ( abs(Sinth)<=1.0E-06 ) Sinth = 0.0
!
!     START ELEMENT CALCULATIONS FOR STIFFNESS MATRIX
!
!     EVALUATE  MATERIAL PROPERTIES
!
   Matflg = 2
   Matid = matid1
   CALL mat(idele)
!
!     CALCULATIONS FOR THE  TRIANGLE
!
   CALL trif(xc,yc,zc,ivect,jvect,kvect,a,b,c,Iest(1),name)
!
!     FILL THE E-MATRIX
!
   e(1) = ivect(1)
   e(2) = jvect(1)
   e(3) = ivect(2)
   e(4) = jvect(2)
   e(5) = ivect(3)
   e(6) = jvect(3)
!
!     COMPUTE THE F FUCTION, AND CONSTANTS C1, C2, AND C3 IN THE LINEAR
!     EQUS. FOR THICKNESS VARIATION
!
   CALL af(f,6,a,b,c,c1,c2,c3,tmem1,tmem3,tmem5,0)
   area = f(1,1)
   vol = c1*f(1,1) + c2*f(2,1) + c3*f(1,2)
   unimem = .FALSE.
   IF ( abs(c2)<=1.0E-06 .AND. abs(c3)<=1.0E-06 ) unimem = .TRUE.
!
!     CALCULATIONS FOR  Q MATRIX AND ITS INVERSE
!
   DO i = 1 , 36
      q(i,1) = 0.0
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
   IF ( ising==2 ) GOTO 300
!
!     GKTRM IS STIFFNESS MATRIX IN GENERALIZED CO-ORDINATES.
!     KTRM  IS STIFFNESS MATRIX IN ELEMENT CO-ORDINATES.
!     START EXECUTION FOR STIFFNESS MATRIX CALCULATIONS
!
   g11 = Em(1)
   g12 = Em(2)
   g13 = Em(3)
   g22 = Em(4)
   g23 = Em(5)
   g33 = Em(6)
!
!     FORMULATION OF THE STIFFNESS MATRIX (FROM PROG. MANUAL,
!     PAGE 8.24-7)
!
   DO i = 1 , 12
      mi = xu(i)
      ni = yu(i)
      pi = xv(i)
      qi = yv(i)
      DO j = i , 12
         mj = xu(j)
         nj = yu(j)
         pj = xv(j)
         qj = yv(j)
         mimj = mi*mj
         minj = mi*nj
         mipj = mi*pj
         miqj = mi*qj
         nimj = ni*mj
         ninj = ni*nj
         nipj = ni*pj
         niqj = ni*qj
         pimj = pi*mj
         pinj = pi*nj
         pipj = pi*pj
         piqj = pi*qj
         qimj = qi*mj
         qinj = qi*nj
         qipj = qi*pj
         qiqj = qi*qj
         st1 = 0.0
         DO k = 1 , 3
            kr = rk(k)
            ks = sk(k)
            st = 0.0
            IF ( mimj>0 ) st = st + g11*mimj*f(mi+mj+kr-1,ni+nj+ks+1)
            IF ( qiqj>0 ) st = st + g22*qiqj*f(pi+pj+kr+1,qi+qj+ks-1)
            IF ( ninj>0 ) st = st + g33*ninj*f(mi+mj+kr+1,ni+nj+ks-1)
            IF ( pipj>0 ) st = st + g33*pipj*f(pi+pj+kr-1,qi+qj+ks+1)
            IF ( pimj>0 ) st = st + g13*pimj*f(pi+mj+kr-1,qi+nj+ks+1)
            IF ( mipj>0 ) st = st + g13*mipj*f(mi+pj+kr-1,ni+qj+ks+1)
            IF ( niqj>0 ) st = st + g23*niqj*f(mi+pj+kr+1,ni+qj+ks-1)
            IF ( qinj>0 ) st = st + g23*qinj*f(pi+mj+kr+1,qi+nj+ks-1)
            IF ( nipj+miqj>0 ) st = st + (g33*nipj+g12*miqj)*f(mi+pj+kr,ni+qj+ks)
            IF ( pinj+qimj>0 ) st = st + (g33*pinj+g12*qimj)*f(pi+mj+kr,qi+nj+ks)
            IF ( nimj+minj>0 ) st = st + g13*(nimj+minj)*f(mi+mj+kr,ni+nj+ks)
            IF ( piqj+qipj>0 ) st = st + g23*(piqj+qipj)*f(pi+pj+kr,qi+qj+ks)
            st1 = st1 + st*cc(k)
            IF ( unimem ) EXIT
         ENDDO
         gkt(i,j) = st1
         gkt(j,i) = st1
      ENDDO
   ENDDO
!
   IF ( ipass==1 ) GOTO 200
 100  rho = Rhoy
   DO i = 1 , 12
      DO j = i , 12
         mimj = xu(i) + xu(j)
         ninj = yu(i) + yu(j)
         gkt(i,j) = nsm*f(mimj+1,ninj+1)
         DO k = 1 , 3
            kr = rk(k)
            ks = sk(k)
            gkt(i,j) = gkt(i,j) + rho*cc(k)*f(mimj+kr+1,ninj+ks+1)
         ENDDO
         gkt(j,i) = gkt(i,j)
      ENDDO
   ENDDO
!
 200  DO i = 1 , 6
      DO j = 1 , 6
         gk11(i,j) = gktrm(j,i)
         gk12(i,j) = gktrm(j+6,i)
         gk22(i,j) = gktrm(j+6,i+6)
      ENDDO
   ENDDO
   CALL gmmats(q,6,6,0,gk11,6,6,0,qinv)
   CALL gmmats(qinv,6,6,0,q,6,6,1,gk11)
   CALL gmmats(q,6,6,0,gk12,6,6,0,qinv)
   CALL gmmats(qinv,6,6,0,q,6,6,1,gk12)
   CALL gmmats(q,6,6,0,gk22,6,6,0,qinv)
   CALL gmmats(qinv,6,6,0,q,6,6,1,gk22)
   DO i = 1 , 6
      DO j = 1 , 6
         gktrm(i,j) = gk11(i,j)
         gktrm(i,j+6) = gk12(i,j)
         gktrm(i+6,j) = gk12(j,i)
         gktrm(i+6,j+6) = gk22(i,j)
      ENDDO
   ENDDO
!
!     REORDER THE STIFFNESS MATRIX SO THAT THE DISPLACEMENTS OF A GRID
!     POINT ARE ARRANGED CONSECUTIVELY
!
   DO k = 1 , 6
      DO i = 1 , 2
         k1 = 6*(i-1) + k
         i1 = 2*(k-1) + i
         DO j = 1 , 12
            ktrm(i1,j) = gktrm(k1,j)
         ENDDO
      ENDDO
   ENDDO
   DO k = 1 , 6
      DO i = 1 , 2
         k1 = 6*(i-1) + k
         i1 = 2*(k-1) + i
         DO j = 1 , 12
            gktrm(j,i1) = ktrm(j,k1)
         ENDDO
      ENDDO
   ENDDO
   DO
!
      DO i = 1 , 324
         ktrmg(i) = 0.0
      ENDDO
      IF ( ipass<=2 ) THEN
!
!     TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM ELEMENT CO-ORDINATES
!     TO BASIC CO-ORDINATES
!
         DO i = 1 , 6
            save(i) = nl(i)
         ENDDO
         DO i = 1 , 6
            sil(i) = i
            isil = nl(i)
            DO j = 1 , 6
               IF ( isil>nl(j) ) THEN
                  sil(i) = j
                  isil = nl(j)
               ENDIF
            ENDDO
            isi = sil(i)
            nl(isi) = 1000000
         ENDDO
         DO i = 1 , 6
            nl(i) = save(i)
         ENDDO
         DO i = 1 , 6
            sil1 = sil(i)
            DO j = i , 6
               sil2 = sil(j)
               DO ii = 1 , 9
                  balotr(ii) = 0.0
               ENDDO
               DO k = 1 , 2
                  k1 = (sil1-1)*2 + k
                  DO l = 1 , 2
                     l1 = (sil2-1)*2 + l
                     ksub(k,l) = gktrm(k1,l1)
                  ENDDO
               ENDDO
               CALL gmmats(e,3,2,0,ksub,2,2,0,ksubt)
               CALL gmmats(ksubt,3,2,0,e,3,2,1,ktr)
               DO k = 1 , 3
                  DO l = 1 , 3
                     k1 = (k-1)*3 + l
                     l1 = (l-1)*3 + k
                     krt1(l1) = krt(k1)
                  ENDDO
               ENDDO
!
!     TRANSFORM THE KTR1 FROM BASIC TO GLOBAL CO-ORDINATES
!
               IF ( nl(sil1)/=0 .AND. ics(sil1)/=0 ) THEN
                  i1 = 4*sil1 + 10
                  CALL transs(Iest(i1),trand)
                  CALL gmmats(trand(1),3,3,1,ktr1,3,3,0,ktr)
                  DO k = 1 , 9
                     krt1(k) = krt(k)
                  ENDDO
               ENDIF
               IF ( nl(sil2)/=0 .AND. ics(sil2)/=0 ) THEN
                  IF ( j/=i ) THEN
                     j1 = 4*sil2 + 10
                     CALL transs(Iest(j1),trand)
                  ENDIF
                  CALL gmmats(ktr1,3,3,0,trand,3,3,0,ktr)
                  DO k = 1 , 9
                     krt1(k) = krt(k)
                  ENDDO
               ENDIF
               DO ii = 1 , 3
                  DO jj = 1 , 3
                     i1 = (i-1)*3 + ii
                     j1 = (j-1)*3 + jj
                     i1j1 = (i1-1)*18 + j1
                     j1i1 = (j1-1)*18 + i1
                     ktrmg(j1i1) = ktr1(jj,ii)
                     ktrmg(i1j1) = ktr1(jj,ii)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ELSE
!
!     LUMPED MASS MATRIX, IN THREE DOFS, NOT TWO
!     (SINCE LUMPED MASS IS AN INVARIANT, TRANSFORMATION IS NOT NEEDED)
!
         rho = Rhoy
         amass = (rho*vol+nsm*area)/6.
         DO i = 1 , 324 , 19
            ktrmg(i) = amass
         ENDDO
         ipass = 2
      ENDIF
!
!     CALL INSERTION ROUTINE
!
      CALL emgout(ktrmg(1),ktrmg(1),324,1,dict,ipass,Iprec)
      IF ( .NOT.imass .OR. ipass>=2 ) RETURN
!
!     GO TO 290 TO COMPUTE LUMPED  MASS MATRIX
!     GO TO 241 TO COMPUTE CONSIST.MASS MATRIX (THIS PATH DOES NOT WORK)
!
      ipass = 3
      IF ( ipass==1 ) GOTO 99999
      IF ( ipass==2 ) GOTO 100
      IF ( ipass/=3 ) EXIT
   ENDDO
!
!     ERRORS
!
 300  Nogo = .TRUE.
   WRITE (Ioutpt,99001) Ufm , Iest(1)
99001 FORMAT (A23,' 2407, MATRIX RELATING GENERALIZED PARAMETERS AND ','GRID POINT DISPLACEMENTS IS SINGULAR.',//26X,               &
             &'CHECK COORDINATES OF ELEMENT  TRIM6 WITH ID',I9,1H.)
99999 END SUBROUTINE ktrm6s
