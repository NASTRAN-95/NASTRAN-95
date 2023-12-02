!*==bound.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE bound(Fbrec,Afe,Nafe,Kge,Nkge)
   IMPLICIT NONE
   USE C_BLANK
   USE C_FLBPTR
   USE C_MATIN
   USE C_MATOUT
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(12) :: Fbrec
   REAL*8 , DIMENSION(48) :: Afe
   INTEGER :: Nafe
   REAL*8 , DIMENSION(144) :: Kge
   INTEGER :: Nkge
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 :: a , aa , aa2 , aeps , aflel , aflstr , akjcon , astrel , astria , bb , c1 , c2 , c3 , cc , conii , dd , dpoly ,        &
           & factii , fdet , fii , h , leps , mag , nn , nn1 , nx , nz , rhoxg , tria , x3 , x4 , y3 , y4 , zz , zz1
   REAL*8 , DIMENSION(3,4) :: akj , fl , pt , st
   REAL*8 , DIMENSION(4,7) :: c
   REAL*8 , SAVE :: d1 , d2 , dhalf , dlb , dub , dz , epslon , epso10 , x1 , x2 , y1 , y2 , z1 , z2 , z3 , z4
   REAL*8 , DIMENSION(3,2) :: e
   REAL*8 , DIMENSION(2) :: eps
   REAL*8 , DIMENSION(3,7) :: f
   INTEGER , DIMENSION(2,4) , SAVE :: fedge
   INTEGER , DIMENSION(2,4) :: fledge
   REAL :: g
   INTEGER :: gf1 , gf2 , gf3 , gf4 , gs1 , gs2 , gs3 , gs4 , gsi , gsj , i , i1 , i2 , icol , iloc , irow , it , itria , j , jloc ,&
            & k , k1loc , kk1 , kk2 , kloc , lgrav , loc , m , n , ngridf , ngrids , npoly , nrow
   LOGICAL :: grav
   INTEGER , DIMENSION(3,4) , SAVE :: grid
   REAL*8 , DIMENSION(3) :: in , jn , kn , ks , ksb , r12 , r13 , r14 , r24 , vtemp , y
   INTEGER , DIMENSION(1) :: iz
   REAL*8 , DIMENSION(3) , SAVE :: kident
   REAL*8 , DIMENSION(144) :: kii
   REAL*8 , DIMENSION(9) :: kik , ss
   REAL*8 , DIMENSION(2,3) :: ktemp
   REAL*8 , DIMENSION(2,2) :: ktwo
   INTEGER , DIMENSION(4) :: locfos , locsof
   INTEGER , DIMENSION(3) :: loctof
   REAL*8 , DIMENSION(2,7) :: p
   REAL*8 , DIMENSION(48) :: s
   INTEGER , DIMENSION(2,3) , SAVE :: stedge
   REAL*8 , DIMENSION(3,3) :: t , tfst , tr
!
! End of declarations rewritten by SPAG
!
!
!     COMPUTES AREA FACTOR AND GRAVITIONAL STIFFNESS MATRICES FOR A FACE
!     OF A INDIVIDUAL FLUID ELEMENT
!
!
!     OPEN CORE
!
!
!     CORE POINTERS
!
!
!     MATERIAL PROPERTIES
!
!
!     MODULE PARAMETERS
!
!
!     NASTRAN PARAMETERS
!
   !>>>>EQUIVALENCE (tfst(1,1),in(1)) , (x1,fl(1,1)) , (x2,fl(1,2)) , (tfst(1,2),jn(1)) , (y1,fl(2,1)) , (y2,fl(2,2)) , (tfst(1,3),kn(1))&
!>>>>    & , (z1,fl(3,1)) , (z2,fl(3,2)) , (ss(1),bb) , (x3,fl(1,3)) , (x4,fl(1,4)) , (ss(2),cc) , (y3,fl(2,3)) , (y4,fl(2,4)) ,         &
!>>>>    & (ss(3),zz) , (z3,fl(3,3)) , (z4,fl(3,4)) , (ss(4),nn) , (ss(5),nn1) , (ss(6),zz1) , (eps(1),aeps) , (eps(2),leps) , (fii,bb) ,&
!>>>>    & (factii,cc) , (conii,akjcon) , (Z(1),Iz(1))
!
!     GRID POINTS TO BE USED IN SUBDIVIDING QUADS INTO TRIANGLES
!
   DATA grid/1 , 2 , 3 , 2 , 3 , 4 , 3 , 4 , 1 , 4 , 1 , 2/
!
   DATA dz , d1 , d2 , dhalf/0.D0 , 1.D0 , 2.D0 , .5D0/
   DATA epslon , epso10/1.D-3 , 1.D-4/
   DATA dlb , dub/ - 1.D-3 , 1.001D0/
   DATA x1 , x2 , y1 , y2 , z1 , z2 , z3 , z4/8*0.D0/
!
   DATA fedge/1 , 2 , 2 , 3 , 3 , 4 , 4 , 1/
   DATA stedge/1 , 2 , 2 , 3 , 3 , 1/
   DATA kident/0.D0 , 0.D0 , 1.D0/
!
!
!     DETERMINE SIZES OF MATRIX PARTITIONS
!
   ngrids = 4
   IF ( Fbrec(6)<0 ) ngrids = 3
   ngridf = 4
   IF ( Fbrec(12)<0 ) ngridf = 3
!
   nrow = 3*ngrids
   Nafe = nrow*ngridf*2
   Nkge = 0
!
!     OBTAIN MATERIAL PROPERTY AND GRAVITY DATA IF GRAV ID IS
!     PRESENT
!
   grav = .FALSE.
   IF ( Fbrec(7)/=0 ) THEN
      Inflag = 11
      Matid = Fbrec(8)
      CALL mat(Fbrec(1))
!
      IF ( Ngrav/=0 ) THEN
         lgrav = Igrav + Ngrav - 1
         DO i = Igrav , lgrav , 6
            IF ( iz(i)==Fbrec(7) ) GOTO 50
!
         ENDDO
      ENDIF
      WRITE (Nout,99001) Ufm , Fbrec(1) , Fbrec(7)
!
99001 FORMAT (A23,' 8013, FLUID ELEMENT',I9,' ON A CFLSTR CARD ','REFERENCES UNDEFINED GRAVITY ID',I9)
      Error = .TRUE.
      GOTO 99999
!
 50   g = sqrt(Z(i+3)**2+Z(i+4)**2+Z(i+5)**2)
      g = g*Z(i+2)
      rhoxg = dble(Rho)*dble(g)
      Nkge = nrow*nrow*2
      Nograv = 1
      grav = .TRUE.
!
!     NORMILIZE THE GRAVITY VECTOR
!
      e(1,2) = dble(Z(i+3))
      e(2,2) = dble(Z(i+4))
      e(3,2) = dble(Z(i+5))
      CALL dnorm(e(1,2),mag)
      IF ( iz(i+1)/=0 ) THEN
!
!     TRANSFORM GRAVITY VECTOR TO BASIC
!
         j = iz(Ibgpdt)
         iz(Ibgpdt) = iz(i+1)
         CALL transd(iz(Ibgpdt),tr)
         iz(Ibgpdt) = j
         CALL gmmatd(tr,3,3,0,e(1,2),3,1,0,vtemp)
         DO j = 1 , 3
            e(j,2) = vtemp(j)
         ENDDO
      ENDIF
   ENDIF
!
!
!     COMPUTE NEW COORDINATES FOR FLUID FACE BASED ON FLUID COORDINATE
!     SYSTEM - PERFORM THIS ONLY IF THE FLUID FACE HAS CHANGED
!     THESE COMPUTATIONS INCLUDE --
!
!        IN,JN,KN  - NORMAL VECTORS TO DEFINE FLUID COORDINATE SYSTEM
!        X2,X3,X4  - X COORDINATES OF GRID POINTS IN NEW SYSTEM
!                    ( X1 = 0 )
!        Y3,Y4       Y COORDINATES OF GRID POINTS IN NEW SYSTEM
!                    ( Y1,Y2 = 0 )
!
!     NORMAL (UNIT) VECTORS STORED *COLUMN-WISE* IN U --
!           I IN U(L,1), J IN U(L,2), K IN U(L,3), L= 1,3
!        TRANSFORMED FLUID COORDINATES STORED IN FL
!
!
!     LOCATE GRID POINTS COORDINATES FOR THE FLUID GRID POINTS IN THE
!     BGPDT TABLE
!
   gf1 = Ibgpdt + (Fbrec(9)-1)*4
   gf2 = Ibgpdt + (Fbrec(10)-1)*4
   gf3 = Ibgpdt + (Fbrec(11)-1)*4
   gf4 = -1
   IF ( ngridf==4 ) gf4 = Ibgpdt + (Fbrec(12)-1)*4
!
   IF ( ngridf==4 ) THEN
!
!     QUADRATIC FLUID FACE
!
      DO i = 1 , 3
         r12(i) = Z(gf2+i) - Z(gf1+i)
         r13(i) = Z(gf3+i) - Z(gf1+i)
         r14(i) = Z(gf4+i) - Z(gf1+i)
         r24(i) = Z(gf4+i) - Z(gf2+i)
      ENDDO
!
      CALL daxb(r13,r24,kn)
      CALL dnorm(kn,mag)
!
      h = r12(1)*kn(1) + r12(2)*kn(2) + r12(3)*kn(3)
!
      DO i = 1 , 3
         in(i) = r12(i) - h*kn(i)
      ENDDO
      CALL dnorm(in,mag)
!
      x2 = mag
!
      CALL daxb(kn,in,jn)
!
      x3 = r13(1)*in(1) + r13(2)*in(2) + r13(3)*in(3)
      x4 = r14(1)*in(1) + r14(2)*in(2) + r14(3)*in(3)
      y3 = r13(1)*jn(1) + r13(2)*jn(2) + r13(3)*jn(3)
      y4 = r14(1)*jn(1) + r14(2)*jn(2) + r14(3)*jn(3)
   ELSE
!
!     TRIANGULAR FLUID FACE
!
      DO i = 1 , 3
         r12(i) = Z(gf2+i) - Z(gf1+i)
         in(i) = r12(i)
         r13(i) = Z(gf3+i) - Z(gf1+i)
      ENDDO
!
      CALL dnorm(in,mag)
      x2 = mag
!
      CALL daxb(r12,r13,kn)
      CALL dnorm(kn,mag)
!
      CALL daxb(kn,in,jn)
!
      x3 = r13(1)*in(1) + r13(2)*in(2) + r13(3)*in(3)
      y3 = r13(1)*jn(1) + r13(2)*jn(2) + r13(3)*jn(3)
   ENDIF
!
!     VARIOUS CALCULATIONS DEPENDENT ON FLUID FACE
!
!     INDICES FOR CORNERS OF FLUID ELEMENT
!
   DO n = 1 , 2
      DO j = 1 , ngridf
         fledge(n,j) = fedge(n,j)
      ENDDO
   ENDDO
   fledge(2,ngridf) = 1
!
!     SET UP FOR FLUID TRIANGLE
!
   c1 = (d1-fl(1,3)/fl(1,2))/fl(2,3)
   c2 = fl(1,3)/(fl(1,2)*fl(2,3))
   DO n = 1 , 3
      r12(n) = fl(n,2) - fl(n,1)
      r13(n) = fl(n,3) - fl(n,1)
   ENDDO
   CALL daxb(r12,r13,vtemp)
!
   IF ( ngridf/=3 ) THEN
!
!     SET UP FOR FLUID QUADRANGLE
!
      c1 = fl(2,3) - fl(2,4)
      c2 = fl(1,2)*fl(2,4)
      c3 = fl(1,2) - fl(1,3) + fl(1,4)
      aa = -fl(1,2)*c1
      aa2 = d2*aa
!
      DO n = 1 , 3
         r13(n) = fl(n,3) - fl(n,1)
         r24(n) = fl(n,4) - fl(n,2)
      ENDDO
      CALL daxb(r13,r24,vtemp)
   ENDIF
   aflel = dvmag(vtemp,dz)
!
!     ZERO OUT AREA FACTOR MATRIX
!     AND AREA COMMON TO FLUID AND STRUCTURE ELEMENTS (AFLSTR)
!
   DO i = 1 , 48
      Afe(i) = dz
      s(i) = 0.0D0
   ENDDO
   DO i = 1 , 144
      Kge(i) = 0.0D0
   ENDDO
   aflstr = 0.0
!
!     DETERMINE NUMBER OF STRUCTURAL TRIANGLES TO BE USED, ITRIA
!     AND CUMULATIVE AREA CONSTANT, TRIA
!        ITRIA= 4, TRIA= .5 WHEN STRUCTURE ELEMENT IS QUADRANGLE
!        ITRIA= 1, TRIA= 1. WHEN STRUCTURE ELEMENT IS TRIANGLE
!
   itria = 1
   tria = d1
   IF ( ngrids/=3 ) THEN
      itria = 4
      tria = dhalf
   ENDIF
!
!     TRANSFORM STRUCTURE COORDINATES TO FLUID COORDINATE SYSTEM
!
   gs1 = Ibgpdt + (Fbrec(3)-1)*4
   gs2 = Ibgpdt + (Fbrec(4)-1)*4
   gs3 = Ibgpdt + (Fbrec(5)-1)*4
   gs4 = -1
   IF ( ngrids==4 ) gs4 = Ibgpdt + (Fbrec(6)-1)*4
!
   DO n = 1 , 3
      pt(n,1) = Z(gs1+n) - Z(gf1+n)
      pt(n,2) = Z(gs2+n) - Z(gf1+n)
      pt(n,3) = Z(gs3+n) - Z(gf1+n)
      pt(n,4) = dz
      IF ( ngrids==4 ) pt(n,4) = Z(gs4+n) - Z(gf1+n)
      DO k = 1 , 4
         st(n,k) = dz
      ENDDO
   ENDDO
!
   DO k = 1 , ngrids
      DO n = 1 , 3
         DO m = 1 , 3
            st(n,k) = st(n,k) + pt(m,k)*tfst(m,n)
         ENDDO
      ENDDO
   ENDDO
   DO n = 1 , 2
      r12(n) = st(n,2) - st(n,1)
      r13(n) = st(n,3) - st(n,1)
      IF ( ngrids==4 ) r24(n) = st(n,4) - st(n,2)
   ENDDO
   CALL daxb(r12,r13,vtemp)
   IF ( ngrids==4 ) CALL daxb(r12,r24,vtemp)
   astrel = dvmag(vtemp,dz)
   aeps = dhalf*dmin1(aflel,astrel)
   leps = dz
   IF ( aeps>dz ) leps = epslon*dsqrt(aeps)
   aeps = epslon*aeps
!
!     LOCATE STRUCTURE ELEMENT GRIDS RELATIVE TO FLUID SURFACE
!     LOCSOF FLAGS STRUCTURE ON FLUID:
!            1= INSIDE, -1= OUTSIDE, 0= ON FLUID EDGE
!
   CALL locpt(ngrids,st,ngridf,fl,fledge,kident,eps,locsof)
!
!
!     LOOP THRU (INCREMENTAL) STRUCTURAL TRIANGLES (ITRIA IS 1 OR 4)
!
   DO it = 1 , itria
!
!     LOCATE COORDINATES OF CURRENT TRIANGLE
!
      gs1 = grid(1,it)
      gs2 = grid(2,it)
      gs3 = grid(3,it)
!
      loctof(1) = locsof(gs1)
      loctof(2) = locsof(gs2)
      loctof(3) = locsof(gs3)
!
!     TRANSFER COORDINATES OF CURRENT STRUCTURE TRIANGLE TO CONTIGUOUS
!     ARRAY, AND DO VARIOUS CALCULATIONS DEPENDENT ON THEM
!
      DO n = 1 , 3
         tr(n,1) = st(n,gs1)
         tr(n,2) = st(n,gs2)
         tr(n,3) = st(n,gs3)
         r12(n) = tr(n,2) - tr(n,1)
         r13(n) = tr(n,3) - tr(n,1)
      ENDDO
!
!     OBTAIN KS, UNIT VECTOR NORMAL TO (XY) PLANE OF CURRENT STRUCTURAL
!     TRIANGLE (IN SYSTEM LOCAL TO FLUID ELEMENT)
!
      CALL daxb(r12,r13,ks)
      astria = dvmag(ks,dz)
      CALL dnorm(ks,mag)
!
!     OBTAIN KSB, UNIT VECTOR NORMAL TO (XY) PLANE OF CURRENT STRUCTURE
!     TRIANGLE (IN NASTRAN BASIC COORD SYSTEM)
!
      DO n = 1 , 3
         r12(n) = pt(n,gs2) - pt(n,gs1)
         r13(n) = pt(n,gs3) - pt(n,gs1)
      ENDDO
!
      CALL daxb(r12,r13,ksb)
      CALL dnorm(ksb,mag)
!
!     CALCULATE EPSLON FUNCTIONS FOR SIGNIFICANCE TESTING
!
      leps = dz
      aeps = dhalf*dmin1(aflel,astria)*epslon
      IF ( aeps>dz ) leps = dsqrt(aeps)
!
!     DETERMINE POINTS DESCRIBING AREA POLYGON COMMON TO BOTH FLUID
!     ELEMENT AND (INCREMENTAL) STRUCTURAL TRIANGLE
!
!        POLYGON POINTS IN   P(2,I)    I .LE. 7
!        FLUID POINTS IN     FL(3,J)   J .LE. 4
!        TRIANGLE POINTS IN  TR(3,K)   K=1,3
!
!     DETERMINE POINTS DESCRIBING POLYGON OF COMMON AREA
!
!
!     LOCATE FLUID ELEMENT POINTS RELATIVE TO BOUNDRY OF THIS STRUCTURAL
!     TRIANGLE
!
      CALL locpt(ngridf,fl,3,tr,stedge,ks,eps,locfos)
      DO j = 1 , ngridf
         IF ( locfos(j)<0 ) GOTO 100
      ENDDO
!
!     FLUID ELEMENT IS COMMON AREA POLYGON WHEN NO FLUID POINTS ARE
!     OUTSIDE BOUNDRY OF THIS STRUCTURAL TRIANGLE
!
      npoly = ngridf
      DO n = 1 , 2
         DO j = 1 , ngridf
            p(n,j) = fl(n,j)
         ENDDO
      ENDDO
      GOTO 150
!
!     CALL POLYPT TO DETERMINE POINTS DESCRIBING THE COMMON AREA POLYGON
!
 100  CALL polypt(loctof,stedge,tr,ngridf,fledge,fl,locfos,eps,npoly,p)
!
!     SKIP TO NEXT (INCREMENTAL) STRUCTURAL TRIANGLE WHEN THIS TRIANGLE
!     IS DISJOINT FROM FLUID ELEMENT
!
      IF ( npoly<3 ) CYCLE
!
!     AREA OF COMMON POLYGON AND HALVED WHEN OVERLAPPING (INCREMENTAL)
!     STRUCTURE TRIANGLES USED CUMULATIVE AREA OF FLUID/STRUCTURAL
!     ELEMENT OVERLAP
!
 150  a = tria*dapoly(npoly,p)
      aflstr = aflstr + a
!
!     TERMS FOR LOAD FACTORS
!
      ss(1) = tr(1,1)*tr(2,2)
      ss(2) = -tr(1,1)*tr(2,3)
      ss(3) = tr(1,2)*tr(2,3)
      ss(4) = -tr(1,2)*tr(2,1)
      ss(5) = tr(1,3)*tr(2,1)
      ss(6) = -tr(1,3)*tr(2,2)
      fdet = dz
      DO m = 1 , 6
         fdet = fdet + ss(m)
      ENDDO
      ss(1) = ss(1) + ss(4)
      ss(2) = ss(2) + ss(5)
      ss(3) = ss(3) + ss(6)
      ss(4) = tr(2,2) - tr(2,3)
      ss(5) = tr(2,3) - tr(2,1)
      ss(6) = tr(2,1) - tr(2,2)
      ss(7) = tr(1,3) - tr(1,2)
      ss(8) = tr(1,1) - tr(1,3)
      ss(9) = tr(1,2) - tr(1,1)
!
!     GET LOAD DISTRIBUTION FACTORS, F(K,I)
!     - FROM -
!         I -- AREA POLYGON POINT -- P(N,I)
!         K -- STRUCTURE TRIANGLE POINT -- TR(N,K)
!
      DO i = 1 , npoly
         f(1,i) = p(1,i)*ss(4) + p(2,i)*ss(7) + ss(3)
         f(2,i) = p(1,i)*ss(5) + p(2,i)*ss(8) + ss(2)
         f(3,i) = p(1,i)*ss(6) + p(2,i)*ss(9) + ss(1)
      ENDDO
!
!     GET PRESSURE DISTRIBUTION FACTORS, C(J,I)
!     - FROM -
!         I -- AREA POLYGON POINT  -- P(N,I)
!         J -- FLUID ELEMENT POINT -- FL(N,J)
!
      IF ( ngridf==4 ) THEN
!
!     FLUID ELEMENT IS QUADRANGLE
!
         DO i = 1 , npoly
            bb = p(1,i)*c1 - c2 + p(2,i)*c3
            cc = p(1,i)*fl(2,4) - p(2,i)*fl(1,4)
            IF ( bb==dz .OR. dabs(aa)>dabs(bb*epslon) ) THEN
!
               dd = dsqrt(bb*bb-d2*aa2*cc)
               zz = (dd-bb)/aa2
               IF ( zz<=dlb .OR. zz>=dub ) zz = (-dd-bb)/aa2
            ELSE
               zz = -cc/bb
            ENDIF
!
            nn = p(2,i)/(fl(2,4)+zz*c1)
            IF ( nn<=dlb .OR. nn>=dub ) GOTO 200
!
            zz1 = d1 - zz
            nn1 = d1 - nn
            c(1,i) = zz1*nn1
            c(2,i) = zz*nn1
            c(3,i) = zz*nn
            c(4,i) = zz1*nn
         ENDDO
      ELSE
!
!     FLUID ELEMENT IS TRIANGLE
!
         DO i = 1 , npoly
            bb = p(1,i)/fl(1,2)
            c(1,i) = d1 - bb - p(2,i)*c1
            c(2,i) = bb - p(2,i)*c2
            c(3,i) = p(2,i)/fl(2,3)
         ENDDO
      ENDIF
!
!     CALCULATE AREA TERMS FOR THIS STRUCTURAL TRIANGLE AND INSERT IN
!     MATRIX
!
      dpoly = npoly
      akjcon = a/(fdet*dpoly)
      dpoly = npoly - 1
      factii = d1/dpoly
!
      DO j = 1 , ngridf
         jloc = 3*ngrids*(j-1)
!
         DO k = 1 , 3
            loc = jloc + 3*(grid(k,it)-1)
!
            akj(k,j) = dz
            DO i = 1 , npoly
               akj(k,j) = akj(k,j) + f(k,i)*c(j,i)
            ENDDO
            akj(k,j) = akjcon*akj(k,j)
!
            DO n = 1 , 3
               s(loc+n) = s(loc+n) + akj(k,j)*ksb(n)
            ENDDO
         ENDDO
      ENDDO
!
      IF ( grav ) THEN
!
!     CALCULATE GRAVITATIONAL STIFFNESS TERMS FOR THIS TRIANGLE
!     AND INSERT INTO MATRIX
!
         DO n = 1 , 3
            e(n,1) = dz
         ENDDO
         CALL daxb(e(1,2),ksb,y)
         mag = dadotb(y,y)
         IF ( mag>dz ) mag = dsqrt(mag)
         IF ( mag>=epso10 ) THEN
!
            CALL daxb(e(1,2),y,e)
            CALL dnorm(e,mag)
         ENDIF
!
         nx = 0.D0
         nz = 0.D0
         DO n = 1 , 3
            nx = nx + e(n,1)*ksb(n)
            nz = nz + e(n,2)*ksb(n)
         ENDDO
         conii = rhoxg*akjcon/(d2*fdet)
         ktwo(1,1) = dz
!
         ktwo(2,1) = nx
         ktwo(1,2) = ktwo(2,1)
         ktwo(2,2) = nz
         CALL gmmatd(e,2,3,1,ktwo,2,2,0,ktemp)
         CALL gmmatd(ktemp,3,2,0,e,2,3,0,kik)
!
         DO kk1 = 1 , 3
            k1loc = 9*ngrids*(grid(kk1,it)-1)
!
            DO kk2 = 1 , 3
               loc = k1loc + 9*(grid(kk2,it)-1)
!
               h = 0.D0
               DO i1 = 1 , npoly
                  DO i2 = 1 , npoly
                     fii = f(kk1,i1)*f(kk2,i2)
                     IF ( i1/=i2 ) fii = factii*fii
                     h = h + fii
                  ENDDO
               ENDDO
!
               DO n = 1 , 9
                  Kge(loc+n) = Kge(loc+n) - kik(n)*h*conii
               ENDDO
!
            ENDDO
         ENDDO
      ENDIF
!
!     END OF (INCREMENTAL) STRUCTURAL TRIANGLE LOOP
!
   ENDDO
!
!     WARNING MESSAGE WHEN FLUID AND STRUCTURE ELEMENTS ARE DISJOINT
!
   IF ( aflstr<=dz ) THEN
      WRITE (Nout,99002) Uwm , Fbrec(1) , Fbrec(2)
!
99002 FORMAT (A25,' 8014, FLUID ELEMENT',I9,' AND STRUCTURE ELEMENT',I9,' ARE DISJOINT. CHECK CFLSTR CARDS.')
      GOTO 99999
   ELSE
!
!     TRANSFORM THE AREA AND STIFFNESS MATRICES TO GLOBAL COORDINATES IF
!     REQUIRED
!
      DO irow = 1 , ngrids
         gsi = Ibgpdt + (Fbrec(irow+2)-1)*4
         CALL transd(Z(gsi),t)
!
!     AREA FACTOR MATRIX
!
         jloc = 3*(irow-1)
!
         DO icol = 1 , ngridf
            iloc = 3*ngrids*(icol-1) + jloc
!
            IF ( iz(gsi)==0 ) THEN
!
               DO i = 1 , 3
                  Afe(iloc+i) = s(iloc+i)
               ENDDO
            ELSE
               CALL gmmatd(t,3,3,1,s(iloc+1),3,1,0,Afe(iloc+1))
            ENDIF
!
         ENDDO
         IF ( grav ) THEN
!
!     GRAVITATIONAL STIFFNESS MATRIX
!
            jloc = 9*(irow-1)
!
            DO icol = 1 , ngrids
               iloc = 9*ngrids*(icol-1) + jloc
!
               IF ( iz(gsi)==0 ) THEN
!
                  kloc = iloc
                  DO i = 1 , 9
                     kik(i) = Kge(kloc+i)
                  ENDDO
               ELSE
                  CALL gmmatd(t,3,3,1,Kge(iloc+1),3,3,0,kik)
               ENDIF
!
               gsj = Ibgpdt + (Fbrec(icol+2)-1)*4
               IF ( iz(gsj)==0 ) THEN
!
                  kloc = iloc
                  DO i = 1 , 9
                     kii(kloc+i) = kik(i)
                  ENDDO
               ELSE
                  CALL transd(Z(gsj),t)
                  CALL gmmatd(kik,3,3,0,t,3,3,0,kii(iloc+1))
               ENDIF
            ENDDO
         ENDIF
      ENDDO
!
!     REARANGE THE STORAGE OF THE GRAVITATIONAL STIFFNESS MATRIX
!     TO COLUMNWISE FOR THE USE WITH THE ASSEMBLER
!
      IF ( .NOT.grav ) RETURN
!
      DO icol = 1 , ngrids
         jloc = 9*ngrids*(icol-1)
!
         DO irow = 1 , ngrids
            iloc = jloc + 9*(irow-1)
            kloc = jloc + 3*(irow-1)
!
            DO i = 1 , 3
               Kge(kloc+1) = kii(iloc+1)
               Kge(kloc+2) = kii(iloc+4)
               Kge(kloc+3) = kii(iloc+7)
               kloc = kloc + 3*ngrids
               iloc = iloc + 1
            ENDDO
         ENDDO
      ENDDO
      RETURN
   ENDIF
!
!     ERROR CONDITIONS
!
 200  WRITE (Nout,99003) Ufm , Fbrec(2)
!
99003 FORMAT (A23,' 8005. BAD GEOMETRY DEFINED FOR STRUCTURAL ELEMENT ',I8)
   Error = .TRUE.
99999 END SUBROUTINE bound
