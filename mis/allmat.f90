
SUBROUTINE allmat(A,Lambda,H,Hl,Vect,Mult,Inth,Int,M,Ncal,Iopt1)
   IMPLICIT NONE
   INTEGER Iopt1 , M , Ncal
   COMPLEX A(M,M) , H(M,M) , Hl(M,M) , Lambda(2) , Mult(1) , Vect(1)
   INTEGER Int(1)
   LOGICAL Inth(1)
   REAL abig , abssq , aid , ain , ard , arn , big , eps , rho , sum , te , tem , term1 , term2 , term3
   COMPLEX cos , shift(3) , sin , temp , temp1 , temp2
   INTEGER i , icount , im1 , index , inter , ip1 , j , k , l , mn1 , n , n1i , ni1 , nm1 , nm2 , nvec , r , rp1 , rp2
   LOGICAL twice
!
!     SUBROUTINE ALLMAT (A,LAMBDA,M,IA,NCAL)
!
!     A ON ENTRY      = MATRIX TO BE ITERATED
!     A    ON RETURN  = EIGENVECTORS  (OPTIONAL)
!     LAMBDA          = EIGENVALUES
!     M               = FIRST DIMENSION OF (A)
!     NCAL ON ENTRY   = FLAG .NE. 0. COMPUTE VECTORS
!                            .EQ. 0. NO VECTORS
!     NCAL ON RETURN  = NUMBER OF EIGENVALUES
!
!
!     PROG. AUTHORS JOHN RINZEL AND R.E.FUNDERLIC, UNION CARBIDE CORP.
!     NUCLEAR DIVISION,CENTRAL DATA PROCESSING FACILITY,
!     OAK RIDGE, TENNESSEE
!
!
   nvec = Ncal
   n = M
   Ncal = n
   IF ( n/=1 ) THEN
      icount = 0
      shift(1) = 0.
      IF ( n/=2 ) THEN
!
!     REDUCE MATRIX A TO HESSENBERG FORM
!
         nm2 = n - 2
         DO r = 1 , nm2
            rp1 = r + 1
            rp2 = r + 2
            abig = 0.
            Int(r) = rp1
            DO i = rp1 , n
               abssq = real(A(i,r))**2 + aimag(A(i,r))**2
               IF ( abssq>abig ) THEN
                  Int(r) = i
                  abig = abssq
               ENDIF
            ENDDO
            IF ( abig/=0. ) THEN
               inter = Int(r)
               IF ( inter/=rp1 ) THEN
                  DO i = r , n
                     temp = A(rp1,i)
                     A(rp1,i) = A(inter,i)
                     A(inter,i) = temp
                  ENDDO
                  DO i = 1 , n
                     temp = A(i,rp1)
                     A(i,rp1) = A(i,inter)
                     A(i,inter) = temp
                  ENDDO
               ENDIF
               DO i = rp2 , n
                  Mult(i) = A(i,r)/A(rp1,r)
                  A(i,r) = Mult(i)
               ENDDO
               DO i = 1 , rp1
                  temp = 0.
                  DO j = rp2 , n
                     temp = temp + A(i,j)*Mult(j)
                  ENDDO
                  A(i,rp1) = A(i,rp1) + temp
               ENDDO
               DO i = rp2 , n
                  temp = 0.
                  DO j = rp2 , n
                     temp = temp + A(i,j)*Mult(j)
                  ENDDO
                  A(i,rp1) = A(i,rp1) + temp - Mult(i)*A(rp1,rp1)
               ENDDO
               DO i = rp2 , n
                  DO j = rp2 , n
                     A(i,j) = A(i,j) - Mult(i)*A(rp1,j)
                  ENDDO
               ENDDO
            ENDIF
         ENDDO
!
!     CALCULATE EPSILON
!
         eps = 0.
         DO i = 1 , n
            eps = eps + cabs(A(1,i))
         ENDDO
         DO i = 2 , n
            sum = 0.
            im1 = i - 1
            DO j = im1 , n
               sum = sum + cabs(A(i,j))
            ENDDO
            IF ( sum>eps ) eps = sum
         ENDDO
         eps = sqrt(float(n))*eps*1.E-12
         IF ( eps==0. ) eps = 1.E-12
         DO i = 1 , n
            DO j = 1 , n
               H(i,j) = A(i,j)
            ENDDO
         ENDDO
         GOTO 200
      ENDIF
   ELSE
      Lambda(1) = A(1,1)
      A(1,1) = 1.
      GOTO 99999
   ENDIF
 100  temp = (A(1,1)+A(2,2)+csqrt((A(1,1)+A(2,2))*(A(1,1)+A(2,2))-4.*(A(2,2)*A(1,1)-A(2,1)*A(1,2))))/2.
   IF ( real(temp)/=0. .OR. aimag(temp)/=0. ) THEN
      Lambda(M) = temp + shift(1)
      Lambda(M-1) = (A(2,2)*A(1,1)-A(2,1)*A(1,2))/(Lambda(M)-shift(1)) + shift(1)
   ELSE
      Lambda(M) = shift(1)
      Lambda(M-1) = A(1,1) + A(2,2) + shift(1)
   ENDIF
   GOTO 600
 200  IF ( n==1 ) THEN
      Lambda(M) = A(1,1) + shift(1)
      GOTO 600
   ENDIF
 300  IF ( n==2 ) GOTO 100
 400  mn1 = M - n + 1
   ard = real(A(n,n))
   aid = aimag(A(n,n))
   arn = real(A(n,n-1))
   ain = aimag(A(n,n-1))
   IF ( ard/=0.0 .OR. aid/=0.0 ) THEN
      term1 = abs(ard*arn+aid*ain)
      term2 = abs(ard*ain-aid*arn)
      term3 = ard*ard + aid*aid
      IF ( (term1+term2)<=1.0E-9*term3 ) GOTO 500
   ENDIF
   IF ( (abs(arn)+abs(ain))>=eps ) THEN
!
!     DETERMINE SHIFT
!
      shift(2) = (A(n-1,n-1)+A(n,n)+csqrt((A(n-1,n-1)+A(n,n))*(A(n-1,n-1)+A(n,n))-4.*(A(n,n)*A(n-1,n-1)-A(n,n-1)*A(n-1,n))))/2.
      IF ( real(shift(2))/=0. .OR. aimag(shift(2))/=0. ) THEN
         shift(3) = (A(n,n)*A(n-1,n-1)-A(n,n-1)*A(n-1,n))/shift(2)
      ELSE
         shift(3) = A(n-1,n-1) + A(n,n)
      ENDIF
      IF ( cabs(shift(2)-A(n,n))<cabs(shift(3)-A(n,n)) ) THEN
         index = 2
      ELSE
         index = 3
      ENDIF
      IF ( cabs(A(n-1,n-2))>=eps ) THEN
         shift(1) = shift(1) + shift(index)
         DO i = 1 , n
            A(i,i) = A(i,i) - shift(index)
         ENDDO
!
!     PERFORM GIVENS ROTATIONS, QR ITERATES
!
         IF ( icount<=20 ) THEN
            nm1 = n - 1
            temp1 = A(1,1)
            temp2 = A(2,1)
            DO r = 1 , nm1
               rp1 = r + 1
               rho = sqrt(real(temp1)**2+aimag(temp1)**2+real(temp2)**2+aimag(temp2)**2)
               IF ( rho/=0. ) THEN
                  cos = temp1/rho
                  sin = temp2/rho
                  index = max0(r-1,1)
                  DO i = index , n
                     temp = conjg(cos)*A(r,i) + conjg(sin)*A(rp1,i)
                     A(rp1,i) = -sin*A(r,i) + cos*A(rp1,i)
                     A(r,i) = temp
                  ENDDO
                  temp1 = A(rp1,rp1)
                  temp2 = A(r+2,r+1)
                  DO i = 1 , r
                     temp = cos*A(i,r) + sin*A(i,rp1)
                     A(i,rp1) = -conjg(sin)*A(i,r) + conjg(cos)*A(i,rp1)
                     A(i,r) = temp
                  ENDDO
                  index = min0(r+2,n)
                  DO i = rp1 , index
                     A(i,r) = sin*A(i,rp1)
                     A(i,rp1) = conjg(cos)*A(i,rp1)
                  ENDDO
               ENDIF
            ENDDO
            icount = icount + 1
            GOTO 400
         ELSE
            Ncal = M - n
            GOTO 600
         ENDIF
      ELSE
         Lambda(mn1) = shift(2) + shift(1)
         Lambda(mn1+1) = shift(3) + shift(1)
         icount = 0
         n = n - 2
         GOTO 200
      ENDIF
   ENDIF
 500  Lambda(mn1) = A(n,n) + shift(1)
   icount = 0
   n = n - 1
   GOTO 300
!
!     CALCULATE VECTORS
!
 600  IF ( Ncal/=0 .AND. nvec/=0 ) THEN
      n = M
      nm1 = n - 1
      IF ( n==2 ) THEN
         eps = amax1(cabs(Lambda(1)),cabs(Lambda(2)))*1.E-8
         IF ( eps==0. ) eps = 1.E-12
         H(1,1) = A(1,1)
         H(1,2) = A(1,2)
         H(2,1) = A(2,1)
         H(2,2) = A(2,2)
      ENDIF
      DO l = 1 , Ncal
         DO i = 1 , n
            DO j = 1 , n
               Hl(i,j) = H(i,j)
            ENDDO
            Hl(i,i) = Hl(i,i) - Lambda(l)
         ENDDO
         DO i = 1 , nm1
            Mult(i) = 0.
            Inth(i) = .FALSE.
            ip1 = i + 1
            IF ( cabs(Hl(i+1,i))>cabs(Hl(i,i)) ) THEN
               Inth(i) = .TRUE.
               DO j = i , n
                  temp = Hl(i+1,j)
                  Hl(i+1,j) = Hl(i,j)
                  Hl(i,j) = temp
               ENDDO
            ENDIF
            IF ( real(Hl(i,i))/=0. .OR. aimag(Hl(i,i))/=0. ) THEN
               Mult(i) = -Hl(i+1,i)/Hl(i,i)
               DO j = ip1 , n
                  Hl(i+1,j) = Hl(i+1,j) + Mult(i)*Hl(i,j)
               ENDDO
            ENDIF
         ENDDO
         DO i = 1 , n
            Vect(i) = 1.
         ENDDO
         twice = .FALSE.
         DO
            IF ( real(Hl(n,n))==0. .AND. aimag(Hl(n,n))==0. ) Hl(n,n) = eps
            Vect(n) = Vect(n)/Hl(n,n)
            DO i = 1 , nm1
               k = n - i
               DO j = k , nm1
                  Vect(k) = Vect(k) - Hl(k,j+1)*Vect(j+1)
               ENDDO
               IF ( real(Hl(k,k))==0. .AND. aimag(Hl(k,k))==0. ) Hl(k,k) = eps
               Vect(k) = Vect(k)/Hl(k,k)
            ENDDO
            big = 0.
            DO i = 1 , n
               sum = abs(real(Vect(i))) + abs(aimag(Vect(i)))
               IF ( sum>big ) big = sum
            ENDDO
            DO i = 1 , n
               Vect(i) = Vect(i)/big
            ENDDO
            IF ( twice ) THEN
               IF ( n/=2 ) THEN
                  nm2 = n - 2
                  DO i = 1 , nm2
                     n1i = n - 1 - i
                     ni1 = n - i + 1
                     DO j = ni1 , n
                        Vect(j) = H(j,n1i)*Vect(n1i+1) + Vect(j)
                     ENDDO
                     index = Int(n1i)
                     temp = Vect(n1i+1)
                     Vect(n1i+1) = Vect(index)
                     Vect(index) = temp
                  ENDDO
               ENDIF
               DO i = 1 , n
                  A(i,l) = Vect(i)
               ENDDO
               EXIT
            ELSE
               DO i = 1 , nm1
                  IF ( Inth(i) ) THEN
                     temp = Vect(i)
                     Vect(i) = Vect(i+1)
                     Vect(i+1) = temp
                  ENDIF
                  Vect(i+1) = Vect(i+1) + Mult(i)*Vect(i)
               ENDDO
               twice = .TRUE.
            ENDIF
         ENDDO
      ENDDO
      DO j = 1 , Ncal
         te = 0.
         DO i = 1 , n
            tem = cabs(A(i,j))
            IF ( te<=tem ) THEN
               l = i
               te = tem
            ENDIF
         ENDDO
         temp1 = A(l,j)
         DO i = 1 , n
            A(i,j) = A(i,j)/temp1
         ENDDO
      ENDDO
   ENDIF
99999 RETURN
END SUBROUTINE allmat
