
DOUBLE PRECISION FUNCTION dapoly(N,P)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER N
   DOUBLE PRECISION P(2,1)
!
! Local variable declarations
!
   INTEGER i , j , k(2,10) , k1 , k2 , kedge(2,10) , nn
!
! End of declarations
!
!
!     CALCULATES AREA OF A POLYGON DESCRIBED BY N POINTS (P)
!        ( N .LE. 10 )
!
!        AREA= -1* LINE INTEGRAL OF Y*DX
!
!     AREA CONTRIBUTION FROM SIDE WHOSE ENDS ARE P(I), P(J):
!        A(I,J)= 0.5 * (Y(I)+Y(J)) * (X(I)-X(J))
!
!
   DATA kedge/1 , 2 , 2 , 3 , 3 , 4 , 4 , 5 , 5 , 6 , 6 , 7 , 7 , 8 , 8 , 9 , 9 , 10 , 10 , 1/
!
   DO i = 1 , 2
      DO j = 1 , N
         k(i,j) = kedge(i,j)
      ENDDO
   ENDDO
   k(2,N) = 1
   dapoly = 0.0
!
   DO nn = 1 , N
      k1 = k(1,nn)
      k2 = k(2,nn)
      dapoly = dapoly + 5.D-1*(P(2,k1)+P(2,k2))*(P(1,k1)-P(1,k2))
   ENDDO
END FUNCTION dapoly
