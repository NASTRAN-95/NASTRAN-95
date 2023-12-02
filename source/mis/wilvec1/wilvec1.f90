!*==wilvec1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE wilvec1(D,O,Val,Vloc,V,F,P,Q,R,Vec,Nx,Svec)
   USE c_givn
   USE c_mgivxx
   USE c_packx
   USE c_reigkr
   USE c_system
   USE c_unpakx
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nx
   REAL , DIMENSION(1) :: D
   REAL , DIMENSION(1) :: O
   REAL , DIMENSION(1) :: Val
   INTEGER , DIMENSION(1) :: Vloc
   REAL , DIMENSION(1) :: V
   REAL , DIMENSION(1) :: F
   REAL , DIMENSION(1) :: P
   REAL , DIMENSION(1) :: Q
   REAL , DIMENSION(1) :: R
   REAL , DIMENSION(Nx,1) :: Vec
   INTEGER , DIMENSION(1) :: Svec
!
! Local variable declarations rewritten by SPAG
!
   REAL :: deps , rmult , rrmult , sft , sftinv , value , vmult , w , x , y , z
   INTEGER :: i , i1 , ibuf1 , ibuf2 , iclos , ifin , im , im1 , im2 , ist , iter , iterm , itime , j , k , kk , kkk , l , l1 , l2 ,&
            & lm1 , loc , m , max , maxitr , min , mt , mul1 , mul2 , mulp1 , mulp2 , mulp3 , n , n1 , n2 , n2m1 , n2m2 , nm1 ,     &
            & nm2 , nrigid , nv , nv1 , nv2 , nver , nz , path , phia , pv , v1 , vector , vv
   INTEGER , DIMENSION(7) , SAVE :: mcb , mcb1
   INTEGER , SAVE :: mgiv , mul3
   REAL , SAVE :: one , zero
   EXTERNAL bckrec , close , gopen , klock , korsz , makmcb , pack , tmtogo , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     WILKINSON EIGENVECTOR SOLUTION FOR LARGE SYM MATRICES
!
   !>>>>EQUIVALENCE (N,Vcom(1)) , (Pv,Vcom(5)) , (Nv,Vcom(7)) , (Nrigid,Vcom(10)) , (Phia,Vcom(12)) , (Nver,Vcom(13)) , (Maxitr,Vcom(15))&
!>>>>    & , (Iterm,Vcom(16))
   DATA mul3 , mcb1 , mcb/0 , 0 , 0 , 0 , 2 , 2 , 0 , 0 , 7*0/
   DATA zero , one/0.0D+0 , 1.0D+0/
   DATA mgiv/4HMGIV/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     D        = DIAGONAL     TERMS OF THE TRIDIAGONAL MATRIX (N)
!     O        = OFF-DIAGONAL TERMS OF THE TRIDIAGONAL MATRIX (N)
!     VAL      = EIGENVALUES (NV)
!     VLOC     = ORIGINAL ORDERING OF THE EIGENVALUES (NV)
!     V,F,P,Q,R= N DIMENSIONAL ARRAYS
!     VEC      = THE REST OF OPEN CORE
!
!     MT       = TRANSFORMATION TAPE
!     N        = ORDER  OF PROBLEM
!     NV       = NUMBER OF EIGENVECTORS
!     RSTRT
!     V2       = NUMBER   OF EIGENVECTORS ALREADY CLLCULATED
!     VV       = POINTER  TO CURRENT VECTOR IN CORE VEC(1,VV)
!     NM2X     = MIDPOINT OF PROBLEM (SWITCH SINE SAVE TAPES)
!
!
!     INITALIZE VARIABLES
!
         deps = 1.0 - 35
         sft = 1.0 + 20
         sftinv = 1.0 + 0/sft
         vmult = 1.0 - 02
         nz = korsz(Svec)
         ibuf1 = nz - sysbuf + 1
         ibuf2 = ibuf1 - sysbuf
         im = 1
         CALL makmcb(mcb1,phia,n,2,iprec)
         im1 = 2
         nz = ibuf2 - 1
         path = 0
         nv1 = (nz-1)/(n+n)
         im2 = 2
         nm1 = n - 1
         nm2 = n - 2
         nver = 0
         v2 = nrigid
!
!     REARRANGE EIGENVALUES AND EXTRACTION ORDER FOR MULTIPLE ROOTS
!     TO GUARANTEE THAT THEY ARE IN SUBMATRIX ORDER FOR PURPOSES
!     OF TRIAL VECTOR AND ORTHOGONOLIZATION COMPUTATIONS
!
         rmult = vmult
         rrmult = vmult/100.0
         iclos = 0
         i = nrigid + 1
         spag_nextblock_1 = 2
      CASE (2)
         IF ( abs(Val(i))+abs(Val(i+1))>=rmult ) THEN
            IF ( Val(i)==zero ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( abs(one-Val(i)/Val(i+1))>rrmult ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( iclos==0 ) iclos = i
         spag_nextblock_1 = 5
      CASE (3)
         DO i1 = iclos , i
            min = Vloc(i1)
            value = Val(i1)
            k = i1
            DO j = i1 , i
               IF ( Vloc(j)<min ) THEN
                  k = j
                  min = Vloc(j)
                  value = Val(j)
               ENDIF
            ENDDO
            Vloc(k) = Vloc(i1)
            Val(k) = Val(i1)
            Vloc(i1) = min
            Val(i1) = value
         ENDDO
         iclos = 0
         spag_nextblock_1 = 4
      CASE (4)
         IF ( iclos/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         i = i + 1
         IF ( i<nv ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iclos/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     START LOOP FOR CORE LOADS OF VECTORS
!
         CALL klock(ist)
         v1 = v2 + 1
         v2 = v2 + nv1
         mul2 = mul3
         mulp2 = 0
         mul3 = 0
         IF ( nv<v2 ) THEN
            v2 = nv
         ELSEIF ( nv/=v2 ) THEN
!
!     SEARCH FOR MULTIPLICITIES OF EIGENVALUES V2 AND V2+1.
!
            vv = v2
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (7)
         IF ( abs(Val(v2))+abs(Val(v2+1))>=rmult ) THEN
            IF ( abs(one-Val(v2)/Val(v2+1))>rmult ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         l1 = Vloc(v2)
         l2 = Vloc(v2+1)
         n1 = min0(l1,l2)
         n2 = max0(l1,l2) - 1
         DO k = n1 , n2
            IF ( O(k)==zero ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         v2 = v2 - 1
         IF ( v2+6>n1 .AND. v2>v1 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         v2 = vv
         mul3 = 1
         spag_nextblock_1 = 8
      CASE (8)
!
!     FIND EIGENVECTORS V1 - V2.
!
         n1 = 0
         n2 = 0
         nv2 = v2 - v1 + 1
         DO vv = 1 , nv2
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  vector = v1 + vv - 1
                  value = Val(vector)
!
!     FOR MGIV METHOD, USE ORIGINAL LAMBDA COMPUTED BY QRITER
!     IN EIGENVECTOR COMPUTATIONS
!
                  IF ( ioptn==mgiv ) value = 1.0/(value+dlmdas)
                  loc = Vloc(vector)
                  IF ( loc<n1 .OR. loc>n2 ) THEN
!
!     SEARCH FOR A DECOUPLED SUBMATRIX.
!
                     mul1 = 0
                     IF ( loc/=1 ) THEN
                        DO k = 2 , loc
                           n1 = loc - k + 2
                           IF ( O(n1-1)==zero ) GOTO 2
                        ENDDO
                     ENDIF
                     n1 = 1
 2                   IF ( loc/=n ) THEN
                        DO k = loc , nm1
                           IF ( O(k)==zero ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                        ENDDO
                     ENDIF
                     n2 = n
                  ENDIF
                  spag_nextblock_2 = 3
               CASE (2)
                  n2 = k
                  spag_nextblock_2 = 3
               CASE (3)
                  IF ( mul1==0 .AND. mul2==0 ) THEN
                     DO i = 1 , n
                        V(i) = zero
                     ENDDO
                     IF ( n1==n2 ) THEN
                        V(loc) = one
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDIF
                  n2m1 = n2 - 1
                  n2m2 = n2 - 2
!
!     SET UP SIMULTANEOUS EQUATIONS
!
                  x = D(n1) - value
                  y = O(n1)
                  DO k = n1 , n2m1
                     IF ( x==zero ) THEN
                        F(k) = -sft*O(k)
                     ELSE
                        F(k) = -O(k)/x
                     ENDIF
                     IF ( abs(x)<abs(O(k)) ) THEN
!
!     PIVOT.
!
                        P(k) = O(k)
                        Q(k) = D(k+1) - value
                        R(k) = O(k+1)
                        z = -x/P(k)
                        x = z*Q(k) + y
                        y = z*R(k)
                        CYCLE
                     ELSEIF ( abs(x)==abs(O(k)) ) THEN
!
!     DO NOT PIVOT.
!
                        IF ( x==zero ) x = sftinv
                     ENDIF
                     P(k) = x
                     Q(k) = y
                     R(k) = zero
                     x = D(k+1) - (value+O(k)*(y/x))
                     y = O(k+1)
                  ENDDO
                  IF ( mul1==0 .AND. mul2==0 ) THEN
                     DO k = n1 , n2m1
                        V(k) = one
                     ENDDO
                     w = one/sqrt(float(n2-n1+1))
                     V(n2) = one
                  ENDIF
!
!     SOLVE FOR AN EIGENVECTOR OF THE TRIDIAGONAL MATRIX.
!
                  mul2 = 0
                  maxitr = 3
                  SPAG_Loop_2_1: DO iter = 1 , maxitr
!
!     BACK SUBSTITUTION
!
                     IF ( x==zero ) THEN
                        V(n2) = V(n2)*sft
                     ELSE
                        V(n2) = V(n2)/x
                     ENDIF
                     V(n2-1) = (V(n2-1)-Q(n2-1)*V(n2))/P(n2-1)
                     max = n2
                     IF ( abs(V(n2))<abs(V(n2-1)) ) max = n2m1
                     IF ( n2m2>=n1 ) THEN
                        DO k = n1 , n2m2
                           l = n2m2 - (k-n1)
                           V(l) = (V(l)-Q(l)*V(l+1)-R(l)*V(l+2))/P(l)
                           IF ( abs(V(l))>abs(V(max)) ) max = l
                        ENDDO
                     ENDIF
!
!     NORMALIZE THE VECTOR.
!
                     y = abs(V(max))
                     z = zero
                     DO i = n1 , n2
                        V(i) = V(i)/y
                        IF ( abs(V(i))>=deps ) z = z + V(i)*V(i)
                     ENDDO
                     z = sqrt(z)
                     DO i = n1 , n2
                        V(i) = V(i)/z
                     ENDDO
!
!     CHECK CONVERGENCE OF THE LARGEST COMPONENT OF THE VECTOR.
!
                     y = abs(V(max))
                     IF ( w==y ) EXIT SPAG_Loop_2_1
                     IF ( iter/=maxitr ) THEN
                        w = y
!
!     PIVOT V.
!
                        DO i = n1 , n2m1
                           IF ( P(i)==O(i) ) THEN
                              z = V(i+1)
                              V(i+1) = V(i) + z/F(i)
                              V(i) = z
                           ELSE
                              V(i+1) = V(i+1) + V(i)*F(i)
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDDO SPAG_Loop_2_1
                  spag_nextblock_2 = 4
               CASE (4)
!
!     TOO MANY ITERATIONS.
!
!     THE ACCURACY OF EIGENVECTOR XXXX CORRESPONDING TO THE EIGENVALUE
!     XXXXXXX  IS IN DOUBT.
!
                  DO i = 1 , n
                     Vec(i,vv) = V(i)
                  ENDDO
!
!     CHECK MULTIPLICITY OF THE NEXT EIGENVALUE IF IT IS IN THE SAME
!     SUBMATRIX AS THIS ONE.
!
                  IF ( vector/=v2 ) THEN
!
!     FOR MGIV METHOD, USE ADJUSTED LAMBDA COMING OUT OF QRITER
!     IN THE FOLLOWING CHECKS
!
                     IF ( abs(Val(vector+1))+abs(Val(vector))>=rmult ) THEN
                        IF ( abs(Val(vector+1)-Val(vector))>rmult*abs(Val(vector+1)) ) THEN
                           spag_nextblock_2 = 5
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
                     l1 = Vloc(vector+1)
                     IF ( l1>=n1 .AND. l1<=n2 ) THEN
!
!     A MULTIPLICITY DOES EXIT...THE INITIAL APPROXIMATION OF THE NEXT
!     EIGENVECTOR SHOULD BE ORTHOGONAL TO THE ONE JUST CALCULATED.
!
                        IF ( mul1==0 ) mul1 = vv
                        mulp2 = mulp2 + 1
                        mulp3 = mulp2 + mul1 - 1
                        DO kkk = n1 , n2
                           V(kkk) = one
                        ENDDO
                        DO jjj = mul1 , mulp3
                           z = zero
                           DO kk = n1 , n2
                              DO ii = n1 , n2
                                 z = z + Vec(ii,jjj)*V(ii)
                              ENDDO
                              V(kk) = V(kk) - z*Vec(kk,jjj)
                           ENDDO
                        ENDDO
                        CYCLE
                     ENDIF
                  ENDIF
                  spag_nextblock_2 = 5
               CASE (5)
!
!     DOES THIS EIGENVALUE = PREVIOUS ONE(S) IN THIS SUBMATRIX
!
                  IF ( mul1/=0 ) THEN
!
!     A MULTIPLICITY OF EIGENVALUES OCCURRED...IMPROVE THE ORTHOGONALITY
!     OF THE CORRESPONDING EIGENVECTORS.
!
                     mulp1 = mul1 + 1
                     DO l = mulp1 , vv
                        DO i = n1 , n2
                           P(i) = Vec(i,l)
                           Q(i) = zero
                        ENDDO
                        lm1 = l - 1
                        DO k = mul1 , lm1
                           z = zero
                           DO i = n1 , n2
                              z = z + P(i)*Vec(i,k)
                           ENDDO
                           DO i = n1 , n2
                              Q(i) = Q(i) + z*Vec(i,k)
                           ENDDO
                        ENDDO
                        z = zero
                        DO k = n1 , n2
                           Q(k) = P(k) - Q(k)
                           IF ( abs(Q(k))>=deps ) z = z + Q(k)*Q(k)
                        ENDDO
                        z = sqrt(z)
                        DO k = n1 , n2
                           Vec(k,l) = Q(k)/z
                        ENDDO
                     ENDDO
                     mul1 = 0
                     mulp2 = 0
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
!
!     CORE IS NOW FULL OF EIGENVECTORS OF THE TRIDIAGONAL MATRIX.
!     CONVERT THEM TO EIGENVECTORS OF THE ORIGINAL MATRIX.
!
         it1 = iprec
         it2 = iprec
         jj = n
         incr = 1
!
!     IS THE ORIGINAL MATRIX A 2X2
!
         IF ( nm2/=0 ) THEN
            mt = mt1
            IF ( path==0 ) mt = mo
            CALL gopen(mt,Svec(ibuf1),im2)
            IF ( path==0 .AND. v2/=nv ) CALL gopen(mt1,Svec(ibuf2),1)
            it3 = iprec
            jjj = n
            incr1 = 1
            DO m = 1 , nm2
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     l1 = n - m
                     iii = l1 + 1
                     IF ( path==0 ) CALL bckrec(mt)
                     CALL unpack(*4,mt,P)
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
 4                   DO i = 1 , m
                        P(i) = zero
                     ENDDO
                     spag_nextblock_3 = 2
                  CASE (2)
                     IF ( path==0 .AND. v2/=nv ) THEN
                        ii = l1 + 1
                        CALL pack(P,mt1,mcb)
                     ENDIF
                     IF ( path==0 ) CALL bckrec(mt)
                     DO k = 1 , m
                        l2 = n - k + 1
                        i = m - k + 1
                        y = P(i)
                        IF ( y/=zero ) THEN
                           x = zero
                           IF ( abs(y)<one ) x = sqrt(one-y**2)
                           DO vv = 1 , nv2
                              z = x*Vec(l1,vv) - y*Vec(l2,vv)
                              Vec(l2,vv) = x*Vec(l2,vv) + y*Vec(l1,vv)
                              Vec(l1,vv) = z
                           ENDDO
                        ENDIF
                     ENDDO
                     EXIT SPAG_DispatchLoop_3
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
            ENDDO
            CALL close(mt,1)
            IF ( path==0 ) THEN
               IF ( v2/=nv ) WRITE (iotpe,99001) uim , n , nv , nv1
99001          FORMAT (A29,' 2016A, WILVEC EIGENVECTOR COMPUTATIONS.',/37X,'PROBLEM SIZE IS',I6,', NUMBER OF EIGENVECTORS TO BE ',  &
                      &'RECOVERED IS',I6,/37X,'SPILL WILL OCCUR FOR THIS ','CORE AT RECOVERY OF',I6,' EIGENVECTORS.')
               path = 1
               CALL close(mt1,1)
               im2 = 0
            ENDIF
         ENDIF
!
!     WRITE THE EIGENVECTORS ONTO PHIA
!
         CALL gopen(phia,Svec(ibuf1),im)
         ii = 1
         it2 = iprec
         IF ( im==1 .AND. nrigid>0 ) THEN
!
!     PUT OUT ZERO VECTORS FOR RIGID BODY MODES
!
            jj = 1
            DO vv = 1 , nrigid
               CALL pack(zero,phia,mcb1)
            ENDDO
            jj = n
         ENDIF
         im = 3
         IF ( n/=1 ) THEN
            DO vv = 1 , nv2
               CALL pack(Vec(1,vv),phia,mcb1)
            ENDDO
         ENDIF
         IF ( v2==nv ) im1 = 1
         CALL close(phia,im1)
         xentry = -entry
!
!     ANY TIME LEFT TO FIND MORE
!
         CALL tmtogo(itime)
         CALL klock(ifin)
         IF ( 2*(ifin-ist)>=itime ) THEN
!
!     MAX TIME
!
            iterm = 3
         ELSEIF ( v2/=nv ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL wrttrl(mcb1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE wilvec1
