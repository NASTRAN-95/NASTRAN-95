
SUBROUTINE gauss2(A,N,N2)
   IMPLICIT NONE
   INTEGER N , N2
   COMPLEX A(20,1)
   DOUBLE COMPLEX da(20,30)
   INTEGER i , j , k , l , m
!
!
   DO i = 1 , N
      DO j = 1 , N2
         da(i,j) = A(i,j)
      ENDDO
   ENDDO
   DO i = 1 , N
      k = i + 1
      DO j = k , N2
         da(i,j) = da(i,j)/da(i,i)
      ENDDO
      DO m = 1 , N
         IF ( m/=i ) THEN
            DO l = k , N2
               da(m,l) = da(m,l) - da(m,i)*da(i,l)
            ENDDO
         ENDIF
      ENDDO
   ENDDO
   DO i = 1 , N
      DO j = 1 , N2
         A(i,j) = da(i,j)
      ENDDO
   ENDDO
END SUBROUTINE gauss2
