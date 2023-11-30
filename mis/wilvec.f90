
SUBROUTINE wilvec(D,O,Val,Vloc,V,F,P,Q,R,Vec,Nx,Svec)
   IMPLICIT NONE
   DOUBLE PRECISION Dlmdas
   INTEGER Entry , Ii , Iii , Incr , Incr1 , Ioptn , Iotpe , Iprec , It1 , It2 , It3 , Iterm , Jj , Jjj , Ksys(52) , Maxitr , Mo ,  &
         & Mr , Mt1 , Mv1 , N , Nrigid , Nv , Nver , Phia , Pv , Sysbuf , V2 , Xentry
   REAL Rstrt , T12(5) , T131(20) , T19 , T21(80) , T3 , T6 , T8(3) , Title(1) , Vcom(30)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /givn  / Title , Mo , T3 , Mr , Mt1 , T6 , Mv1 , T8 , Entry , T12 , Rstrt , V2 , T19 , Xentry , T21 , Vcom , T131
   COMMON /mgivxx/ Dlmdas
   COMMON /packx / It1 , It2 , Ii , Jj , Incr
   COMMON /reigkr/ Ioptn
   COMMON /system/ Sysbuf , Iotpe , Ksys , Iprec
   COMMON /unpakx/ It3 , Iii , Jjj , Incr1
   COMMON /xmssg / Ufm , Uwm , Uim
   INTEGER Nx
   DOUBLE PRECISION D(1) , F(1) , O(1) , P(1) , Q(1) , R(1) , V(1) , Val(1) , Vec(Nx,1)
   INTEGER Svec(1) , Vloc(1)
   DOUBLE PRECISION deps , one , rmult , rrmult , sft , sftinv , value , vmult , w , x , y , z , zero
   INTEGER i , i1 , ibuf1 , ibuf2 , iclos , ifin , im , im1 , im2 , ist , iter , itime , j , k , kk , kkk , l , l1 , l2 , lm1 ,     &
         & loc , m , max , mcb(7) , mcb1(7) , mgiv , min , mt , mul1 , mul2 , mul3 , mulp1 , mulp2 , mulp3 , n1 , n2 , n2m1 , n2m2 ,&
         & nm1 , nm2 , nv1 , nv2 , nz , path , v1 , vector , vv
   INTEGER korsz
!
!     WILKINSON EIGENVECTOR SOLUTION FOR LARGE SYM MATRICES
!
   EQUIVALENCE (N,Vcom(1)) , (Pv,Vcom(5)) , (Nv,Vcom(7)) , (Nrigid,Vcom(10)) , (Phia,Vcom(12)) , (Nver,Vcom(13)) , (Maxitr,Vcom(15))&
    & , (Iterm,Vcom(16))
   DATA mul3 , mcb1 , mcb/0 , 0 , 0 , 0 , 2 , 2 , 0 , 0 , 7*0/
   DATA zero , one/0.0D+0 , 1.0D+0/
   DATA mgiv/4HMGIV/
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
   deps = 1.0D-35
   sft = 1.0D+20
   sftinv = 1.0D+0/sft
   vmult = 1.0D-02
   nz = korsz(Svec)
   ibuf1 = nz - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   im = 1
   CALL makmcb(mcb1,Phia,N,2,Iprec)
   im1 = 2
   nz = ibuf2 - 1
   path = 0
   nv1 = (nz-1)/(N+N)
   im2 = 2
   nm1 = N - 1
   nm2 = N - 2
   Nver = 0
   V2 = Nrigid
!
!     REARRANGE EIGENVALUES AND EXTRACTION ORDER FOR MULTIPLE ROOTS
!     TO GUARANTEE THAT THEY ARE IN SUBMATRIX ORDER FOR PURPOSES
!     OF TRIAL VECTOR AND ORTHOGONOLIZATION COMPUTATIONS
!
   rmult = vmult
   rrmult = vmult/100.0D0
   iclos = 0
   i = Nrigid + 1
 100  IF ( dabs(Val(i))+dabs(Val(i+1))>=rmult ) THEN
      IF ( Val(i)==zero ) GOTO 400
      IF ( dabs(one-Val(i)/Val(i+1))>rrmult ) GOTO 300
   ENDIF
   IF ( iclos==0 ) iclos = i
   GOTO 400
 200  DO i1 = iclos , i
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
 300  IF ( iclos/=0 ) GOTO 200
 400  i = i + 1
   IF ( i<Nv ) GOTO 100
   IF ( iclos/=0 ) GOTO 200
!
!     START LOOP FOR CORE LOADS OF VECTORS
!
 500  CALL klock(ist)
   v1 = V2 + 1
   V2 = V2 + nv1
   mul2 = mul3
   mulp2 = 0
   mul3 = 0
   IF ( Nv<V2 ) THEN
      V2 = Nv
   ELSEIF ( Nv/=V2 ) THEN
!
!     SEARCH FOR MULTIPLICITIES OF EIGENVALUES V2 AND V2+1.
!
      vv = V2
      GOTO 600
   ENDIF
   GOTO 700
 600  IF ( dabs(Val(V2))+dabs(Val(V2+1))>=rmult ) THEN
      IF ( dabs(one-Val(V2)/Val(V2+1))>rmult ) GOTO 700
   ENDIF
   l1 = Vloc(V2)
   l2 = Vloc(V2+1)
   n1 = min0(l1,l2)
   n2 = max0(l1,l2) - 1
   DO k = n1 , n2
      IF ( O(k)==zero ) GOTO 700
   ENDDO
   V2 = V2 - 1
   IF ( V2+6>n1 .AND. V2>v1 ) GOTO 600
   V2 = vv
   mul3 = 1
!
!     FIND EIGENVECTORS V1 - V2.
!
 700  n1 = 0
   n2 = 0
   nv2 = V2 - v1 + 1
   DO vv = 1 , nv2
      vector = v1 + vv - 1
      value = Val(vector)
!
!     FOR MGIV METHOD, USE ORIGINAL LAMBDA COMPUTED BY QRITER
!     IN EIGENVECTOR COMPUTATIONS
!
      IF ( Ioptn==mgiv ) value = 1.0D0/(value+Dlmdas)
      loc = Vloc(vector)
      IF ( loc<n1 .OR. loc>n2 ) THEN
!
!     SEARCH FOR A DECOUPLED SUBMATRIX.
!
         mul1 = 0
         IF ( loc/=1 ) THEN
            DO k = 2 , loc
               n1 = loc - k + 2
               IF ( O(n1-1)==zero ) GOTO 720
            ENDDO
         ENDIF
         n1 = 1
 720     IF ( loc/=N ) THEN
            DO k = loc , nm1
               IF ( O(k)==zero ) GOTO 750
            ENDDO
         ENDIF
         n2 = N
      ENDIF
      GOTO 800
 750  n2 = k
 800  IF ( mul1==0 .AND. mul2==0 ) THEN
         DO i = 1 , N
            V(i) = zero
         ENDDO
         IF ( n1==n2 ) THEN
            V(loc) = one
            GOTO 850
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
         IF ( dabs(x)<dabs(O(k)) ) THEN
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
         ELSEIF ( dabs(x)==dabs(O(k)) ) THEN
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
         w = one/dsqrt(dble(float(n2-n1+1)))
         V(n2) = one
      ENDIF
!
!     SOLVE FOR AN EIGENVECTOR OF THE TRIDIAGONAL MATRIX.
!
      mul2 = 0
      Maxitr = 3
      DO iter = 1 , Maxitr
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
         IF ( dabs(V(n2))<dabs(V(n2-1)) ) max = n2m1
         IF ( n2m2>=n1 ) THEN
            DO k = n1 , n2m2
               l = n2m2 - (k-n1)
               V(l) = (V(l)-Q(l)*V(l+1)-R(l)*V(l+2))/P(l)
               IF ( dabs(V(l))>dabs(V(max)) ) max = l
            ENDDO
         ENDIF
!
!     NORMALIZE THE VECTOR.
!
         y = dabs(V(max))
         z = zero
         DO i = n1 , n2
            V(i) = V(i)/y
            IF ( dabs(V(i))>=deps ) z = z + V(i)*V(i)
         ENDDO
         z = dsqrt(z)
         DO i = n1 , n2
            V(i) = V(i)/z
         ENDDO
!
!     CHECK CONVERGENCE OF THE LARGEST COMPONENT OF THE VECTOR.
!
         y = dabs(V(max))
         IF ( sngl(w)==sngl(y) ) EXIT
         IF ( iter/=Maxitr ) THEN
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
      ENDDO
!
!     TOO MANY ITERATIONS.
!
!     THE ACCURACY OF EIGENVECTOR XXXX CORRESPONDING TO THE EIGENVALUE
!     XXXXXXX  IS IN DOUBT.
!
 850  DO i = 1 , N
         Vec(i,vv) = V(i)
      ENDDO
!
!     CHECK MULTIPLICITY OF THE NEXT EIGENVALUE IF IT IS IN THE SAME
!     SUBMATRIX AS THIS ONE.
!
      IF ( vector/=V2 ) THEN
!
!     FOR MGIV METHOD, USE ADJUSTED LAMBDA COMING OUT OF QRITER
!     IN THE FOLLOWING CHECKS
!
         IF ( dabs(Val(vector+1))+dabs(Val(vector))>=rmult ) THEN
            IF ( dabs(Val(vector+1)-Val(vector))>rmult*dabs(Val(vector+1)) ) GOTO 900
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
            DO Jjj = mul1 , mulp3
               z = zero
               DO kk = n1 , n2
                  DO Ii = n1 , n2
                     z = z + Vec(Ii,Jjj)*V(Ii)
                  ENDDO
                  V(kk) = V(kk) - z*Vec(kk,Jjj)
               ENDDO
            ENDDO
            CYCLE
         ENDIF
      ENDIF
!
!     DOES THIS EIGENVALUE = PREVIOUS ONE(S) IN THIS SUBMATRIX
!
 900  IF ( mul1/=0 ) THEN
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
               IF ( dabs(Q(k))>=deps ) z = z + Q(k)*Q(k)
            ENDDO
            z = dsqrt(z)
            DO k = n1 , n2
               Vec(k,l) = Q(k)/z
            ENDDO
         ENDDO
         mul1 = 0
         mulp2 = 0
      ENDIF
   ENDDO
!
!     CORE IS NOW FULL OF EIGENVECTORS OF THE TRIDIAGONAL MATRIX.
!     CONVERT THEM TO EIGENVECTORS OF THE ORIGINAL MATRIX.
!
   It1 = 2
   It2 = 2
   Jj = N
   Incr = 1
!
!     IS THE ORIGINAL MATRIX A 2X2
!
   IF ( nm2/=0 ) THEN
      mt = Mt1
      IF ( path==0 ) mt = Mo
      CALL gopen(mt,Svec(ibuf1),im2)
      IF ( path==0 .AND. V2/=Nv ) CALL gopen(Mt1,Svec(ibuf2),1)
      It3 = 2
      Jjj = N
      Incr1 = 1
      DO m = 1 , nm2
         l1 = N - m
         Iii = l1 + 1
         IF ( path==0 ) CALL bckrec(mt)
         CALL unpack(*920,mt,P)
         GOTO 940
 920     DO i = 1 , m
            P(i) = zero
         ENDDO
 940     IF ( path==0 .AND. V2/=Nv ) THEN
            Ii = l1 + 1
            CALL pack(P,Mt1,mcb)
         ENDIF
         IF ( path==0 ) CALL bckrec(mt)
         DO k = 1 , m
            l2 = N - k + 1
            i = m - k + 1
            y = P(i)
            IF ( y/=zero ) THEN
               x = zero
               IF ( dabs(y)<one ) x = dsqrt(one-y**2)
               DO vv = 1 , nv2
                  z = x*Vec(l1,vv) - y*Vec(l2,vv)
                  Vec(l2,vv) = x*Vec(l2,vv) + y*Vec(l1,vv)
                  Vec(l1,vv) = z
               ENDDO
            ENDIF
         ENDDO
      ENDDO
      CALL close(mt,1)
      IF ( path==0 ) THEN
         IF ( V2/=Nv ) WRITE (Iotpe,99001) Uim , N , Nv , nv1
99001    FORMAT (A29,' 2016A, WILVEC EIGENVECTOR COMPUTATIONS.',/37X,'PROBLEM SIZE IS',I6,', NUMBER OF EIGENVECTORS TO BE ',        &
                &'RECOVERED IS',I6,/37X,'SPILL WILL OCCUR FOR THIS ','CORE AT RECOVERY OF',I6,' EIGENVECTORS.')
         path = 1
         CALL close(Mt1,1)
         im2 = 0
      ENDIF
   ENDIF
!
!     WRITE THE EIGENVECTORS ONTO PHIA
!
   CALL gopen(Phia,Svec(ibuf1),im)
   Ii = 1
   It2 = Iprec
   IF ( im==1 .AND. Nrigid>0 ) THEN
!
!     PUT OUT ZERO VECTORS FOR RIGID BODY MODES
!
      Jj = 1
      DO vv = 1 , Nrigid
         CALL pack(zero,Phia,mcb1)
      ENDDO
      Jj = N
   ENDIF
   im = 3
   IF ( N/=1 ) THEN
      DO vv = 1 , nv2
         CALL pack(Vec(1,vv),Phia,mcb1)
      ENDDO
   ENDIF
   IF ( V2==Nv ) im1 = 1
   CALL close(Phia,im1)
   Xentry = -Entry
!
!     ANY TIME LEFT TO FIND MORE
!
   CALL tmtogo(itime)
   CALL klock(ifin)
   IF ( 2*(ifin-ist)>=itime ) THEN
!
!     MAX TIME
!
      Iterm = 3
   ELSEIF ( V2/=Nv ) THEN
      GOTO 500
   ENDIF
   CALL wrttrl(mcb1)
   RETURN
END SUBROUTINE wilvec
