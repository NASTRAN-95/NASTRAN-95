
SUBROUTINE ateig(M,A,Rr,Ri,Iana,Ia,B,Rra,Rri)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Ia , M
   DOUBLE PRECISION A(1) , Ri(1) , Rr(1)
   REAL B(1) , Rra(1) , Rri(1)
   INTEGER Iana(1)
!
! Local variable declarations
!
   DOUBLE PRECISION alpha , cap , d , delta , e10 , e6 , e7 , eps , eta , g1 , g2 , g3 , pan , pan1 , pri(2) , prr(2) , psi1 ,      &
                  & psi2 , r , rmod , s , t , u , v
   INTEGER i , ii , ii1 , iip , ij , in , in1 , ip , ip2j , ipi , ipip , ipip2 , it , j , ji , jip , jip2 , k , maxit , n , n1 ,    &
         & n1n , n1n1 , n1n2 , n2 , nn , nn1 , np , p , p1 , q
!
! End of declarations
!
!
!     ..................................................................
!
!        SUBROUTINE ATEIG
!
!        PURPOSE
!           COMPUTE THE EIGENVALUES OF A REAL ALMOST TRIANGULAR MATRIX
!
!        USAGE
!           CALL ATEIG(M,A,RR,RI,IANA,IA)
!
!        DESCRIPTION OF THE PARAMETERS
!           M      ORDER OF THE MATRIX
!           A      THE INPUT MATRIX, M BY M
!           RR     VECTOR CONTAINING THE REAL PARTS OF THE EIGENVALUES
!                  ON RETURN
!           RI     VECTOR CONTAINING THE IMAGINARY PARTS OF THE EIGEN-
!                  VALUES ON RETURN
!           IANA   VECTOR WHOSE DIMENSION MUST BE GREATER THAN OR EQUAL
!                  TO M, CONTAINING ON RETURN INDICATIONS ABOUT THE WAY
!                  THE EIGENVALUES APPEARED (SEE MATH. DESCRIPTION)
!           IA     SIZE OF THE FIRST DIMENSION ASSIGNED TO THE ARRAY A
!                  IN THE CALLING PROGRAM WHEN THE MATRIX IS IN DOUBLE
!                  SUBSCRIPTED DATA STORAGE MODE.
!                  IA=M WHEN THE MATRIX IS IN SSP VECTOR STORAGE MODE.
!
!        REMARKS
!           THE ORIGINAL MATRIX IS DESTROYED
!           THE DIMENSION OF RR AND RI MUST BE GREATER OR EQUAL TO M
!
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!           NONE
!
!        METHOD
!           QR DOUBLE ITERATION
!
!        REFERENCES
!           J.G.F. FRANCIS - THE QR TRANSFORMATION---THE COMPUTER
!           JOURNAL, VOL. 4, NO. 3, OCTOBER 1961, VOL. 4, NO. 4, JANUARY
!           1962.  J. H. WILKINSON - THE ALGEBRAIC EIGENVALUE PROBLEM -
!           CLARENDON PRESS, OXFORD, 1965.
!
!     ..................................................................
!
!
   e7 = 1.0E-8
   e6 = 1.0E-6
   e10 = 1.0E-10
   delta = 0.5
   maxit = 30
!
!        INITIALIZATION
!
   n = M
 100  n1 = n - 1
   in = n1*Ia
   nn = in + n
   IF ( n1==0 ) GOTO 400
   np = n + 1
!
!        ITERATION COUNTER
!
   it = 0
!
!        ROOTS OF THE 2ND ORDER MAIN SUBMATRIX AT THE PREVIOUS
!        ITERATION
!
   DO i = 1 , 2
      prr(i) = 0.0
      pri(i) = 0.0
   ENDDO
!
!        LAST TWO SUBDIAGONAL ELEMENTS AT THE PREVIOUS ITERATION
!
   pan = 0.0
   pan1 = 0.0
!
!        ORIGIN SHIFT
!
   r = 0.0
   s = 0.0
!
!        ROOTS OF THE LOWER MAIN 2 BY 2 SUBMATRIX
!
   n2 = n1 - 1
   in1 = in - Ia
   nn1 = in1 + n
   n1n = in + n1
   n1n1 = in1 + n1
 200  t = A(n1n1) - A(nn)
   u = t*t
   v = 4.0D0*A(n1n)*A(nn1)
   IF ( dabs(v)>u*e7 ) THEN
      t = u + v
      IF ( dabs(t)<=dmax1(u,dabs(v))*e6 ) t = 0.0
      u = (A(n1n1)+A(nn))/2.0D0
      v = dsqrt(dabs(t))/2.0D0
      IF ( t<0 ) THEN
         Rr(n1) = u
         Rr(n) = u
         Ri(n1) = v
         Ri(n) = -v
         GOTO 300
      ELSEIF ( u<0 ) THEN
         Rr(n1) = u - v
         Rr(n) = u + v
      ELSE
         Rr(n1) = u + v
         Rr(n) = u - v
      ENDIF
   ELSEIF ( t<0 ) THEN
      Rr(n1) = A(nn)
      Rr(n) = A(n1n1)
   ELSE
      Rr(n1) = A(n1n1)
      Rr(n) = A(nn)
   ENDIF
   Ri(n) = 0.0
   Ri(n1) = 0.0
 300  IF ( n2>0 ) THEN
!
!        TESTS OF CONVERGENCE
!
      n1n2 = n1n1 - Ia
      rmod = Rr(n1)*Rr(n1) + Ri(n1)*Ri(n1)
      eps = e10*dsqrt(rmod)
      IF ( dabs(A(n1n2))>eps ) THEN
         IF ( dabs(A(nn1))<=e10*dabs(A(nn)) ) GOTO 400
         IF ( dabs(pan1-A(n1n2))>dabs(A(n1n2))*e6 ) THEN
            IF ( dabs(pan-A(nn1))<=dabs(A(nn1))*e6 ) GOTO 340
            IF ( it>=maxit ) GOTO 340
!
!        COMPUTE THE SHIFT
!
            j = 1
            DO i = 1 , 2
               k = np - i
               IF ( dabs(Rr(k)-prr(i))+dabs(Ri(k)-pri(i))<delta*(dabs(Rr(k))+dabs(Ri(k))) ) j = j + i
            ENDDO
            IF ( j==2 .OR. j==3 ) THEN
               j = n + 2 - j
               r = Rr(j)*Rr(j)
               s = Rr(j) + Rr(j)
            ELSEIF ( j==4 ) THEN
               r = Rr(n)*Rr(n1) - Ri(n)*Ri(n1)
               s = Rr(n) + Rr(n1)
            ELSE
               r = 0.0
               s = 0.0
            ENDIF
!
!        SAVE THE LAST TWO SUBDIAGONAL TERMS AND THE ROOTS OF THE
!        SUBMATRIX BEFORE ITERATION
!
            pan = A(nn1)
            pan1 = A(n1n2)
            DO i = 1 , 2
               k = np - i
               prr(i) = Rr(k)
               pri(i) = Ri(k)
            ENDDO
!
!        SEARCH FOR A PARTITION OF THE MATRIX, DEFINED BY P AND Q
!
            p = n2
            ipi = n1n2
            DO j = 2 , n2
               ipi = ipi - Ia - 1
               IF ( dabs(A(ipi))<=eps ) EXIT
               ipip = ipi + Ia
               ipip2 = ipip + Ia
               d = A(ipip)*(A(ipip)-s) + A(ipip2)*A(ipip+1) + r
               IF ( d/=0 ) THEN
                  IF ( dabs(A(ipi)*A(ipip+1))*(dabs(A(ipip)+A(ipip2+1)-s)+dabs(A(ipip2+2)))<=dabs(d)*eps ) GOTO 310
               ENDIF
               p = n1 - j
            ENDDO
            q = p
            GOTO 320
 310        p1 = p - 1
            q = p
            DO i = 1 , p1
               ipi = ipi - Ia - 1
               IF ( dabs(A(ipi))<=eps ) EXIT
               q = q - 1
            ENDDO
!
!        QR DOUBLE ITERATION
!
 320        ii = (p-1)*Ia + p
            DO i = p , n1
               ii1 = ii - Ia
               iip = ii + Ia
               IF ( i/=p ) THEN
                  g1 = A(ii1)
                  g2 = A(ii1+1)
                  IF ( i<=n2 ) THEN
                     g3 = A(ii1+2)
                  ELSE
                     g3 = 0.0
                  ENDIF
               ELSE
                  ipi = ii + 1
                  ipip = iip + 1
!
!        INITIALIZATION OF THE TRANSFORMATION
!
                  g1 = A(ii)*(A(ii)-s) + A(iip)*A(ipi) + r
                  g2 = A(ipi)*(A(ipip)+A(ii)-s)
                  g3 = A(ipi)*A(ipip+1)
                  A(ipi+1) = 0.0
               ENDIF
               cap = dsqrt(g1*g1+g2*g2+g3*g3)
               IF ( cap/=0 ) THEN
                  IF ( g1<0 ) cap = -cap
                  t = g1 + cap
                  psi1 = g2/t
                  psi2 = g3/t
                  alpha = 2.0D0/(1.0D0+psi1*psi1+psi2*psi2)
               ELSE
                  alpha = 2.0
                  psi1 = 0.0
                  psi2 = 0.0
               ENDIF
               IF ( i/=q ) THEN
                  IF ( i/=p ) THEN
                     A(ii1) = -cap
                  ELSE
                     A(ii1) = -A(ii1)
                  ENDIF
               ENDIF
!
!        ROW OPERATION
!
               ij = ii
               DO j = i , n
                  t = psi1*A(ij+1)
                  IF ( i<n1 ) THEN
                     ip2j = ij + 2
                     t = t + psi2*A(ip2j)
                  ENDIF
                  eta = alpha*(t+A(ij))
                  A(ij) = A(ij) - eta
                  A(ij+1) = A(ij+1) - psi1*eta
                  IF ( i<n1 ) A(ip2j) = A(ip2j) - psi2*eta
                  ij = ij + Ia
               ENDDO
!
!        COLUMN OPERATION
!
               IF ( i<n1 ) THEN
                  k = i + 2
               ELSE
                  k = n
               ENDIF
               ip = iip - i
               DO j = q , k
                  jip = ip + j
                  ji = jip - Ia
                  t = psi1*A(jip)
                  IF ( i<n1 ) THEN
                     jip2 = jip + Ia
                     t = t + psi2*A(jip2)
                  ENDIF
                  eta = alpha*(t+A(ji))
                  A(ji) = A(ji) - eta
                  A(jip) = A(jip) - eta*psi1
                  IF ( i<n1 ) A(jip2) = A(jip2) - eta*psi2
               ENDDO
               IF ( i<n2 ) THEN
                  ji = ii + 3
                  jip = ji + Ia
                  jip2 = jip + Ia
                  eta = alpha*psi2*A(jip2)
                  A(ji) = -eta
                  A(jip) = -eta*psi1
                  A(jip2) = A(jip2) - eta*psi2
               ENDIF
               ii = iip + 1
            ENDDO
            it = it + 1
            GOTO 200
         ENDIF
!
!        END OF ITERATION
!
 340     IF ( dabs(A(nn1))<dabs(A(n1n2)) ) GOTO 400
      ENDIF
   ENDIF
!
!        TWO EIGENVALUES HAVE BEEN FOUND
!
   Iana(n) = 0
   Iana(n1) = 2
   n = n2
   IF ( n2>0 ) GOTO 100
   GOTO 500
!
!        ONE EIGENVALUE HAS BEEN FOUND
!
 400  Rr(n) = A(nn)
   Ri(n) = 0.0
   Iana(n) = 1
   IF ( n1>0 ) THEN
      n = n1
      GOTO 100
   ENDIF
 500  k = 0
   DO i = 1 , M
      Rra(i) = Rr(i)
      Rri(i) = Ri(i)
      DO j = 1 , M
         k = k + 1
         B(k) = A(k)
      ENDDO
   ENDDO
END SUBROUTINE ateig
