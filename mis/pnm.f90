
SUBROUTINE pnm(M,N,X,Ir,V)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Ir , M , N
   REAL V , X
!
! Local variable declarations
!
   REAL abxx , cf , gamma(81) , w , y , z
   INTEGER i , i2 , ii , j , j2 , nmm , npnn1
!
! End of declarations
!
   IF ( N<M ) THEN
      V = 0.0
      RETURN
   ELSEIF ( N==0 ) THEN
      V = 1.0
      RETURN
   ELSE
      z = 1.0
      w = z
      IF ( N/=M ) THEN
         nmm = N - M
         DO i = 1 , nmm
            z = X*z
         ENDDO
      ENDIF
      gamma(1) = 1.0
      npnn1 = N + N + 1
      DO i = 2 , npnn1
         gamma(i) = w*gamma(i-1)
         w = w + 1.0
      ENDDO
      w = 1.0
      abxx = abs(X)
      IF ( abxx<0.001 ) THEN
         i = (N-M)/2
         i2 = 2*i
         nmm = N - M
         IF ( i2/=nmm ) THEN
            V = 0.0
            RETURN
         ELSE
            V = gamma(M+N+1)/(gamma(i+1)*gamma(M+i+1))
            IF ( Ir==0 ) V = V*(-1.0)**i
         ENDIF
      ELSE
         y = w/(X*X)
         IF ( Ir==0 ) THEN
            y = -y
            w = -w
         ENDIF
         j = 3
         V = 0.0
         DO i = 1 , 22
            ii = (N-M+2)/2
            IF ( ii<i ) EXIT
            V = V + gamma(N+N-i-i+3)*z/(gamma(i)*gamma(N-i+2)*gamma(N-i-i-M+j))
            z = z*y
         ENDDO
      ENDIF
      z = 1.0
      DO i = 1 , N
         z = z + z
      ENDDO
      V = V/z
      IF ( Ir/=0 ) THEN
         ii = N/4
         i = N - 4*ii
         IF ( i>1 ) V = -V
      ENDIF
      IF ( M==0 ) RETURN
      j = M/2
      cf = w + X*X
      z = abs(cf)
      j2 = j + j
      IF ( M/=j2 ) THEN
         z = sqrt(z)
         j = M
      ENDIF
      IF ( j<1 ) j = 1
      DO i = 1 , j
         V = V*z
      ENDDO
   ENDIF
END SUBROUTINE pnm
