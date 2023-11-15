
SUBROUTINE alg29(Y,X,Fxy,N)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER N
   REAL Fxy(3) , X(3) , Y(3)
!
! Local variable declarations
!
   INTEGER j , n2
   REAL x1 , x2
!
! End of declarations
!
!
!
   x1 = (X(3)+X(2))*(Y(3)-Y(2))/(X(3)-X(2))
   Fxy(2) = x1/(X(3)-X(1))
   n2 = N - 2
   DO j = 3 , n2
      x2 = (X(j+1)+X(j))*(Y(j+1)-Y(j))/(X(j+1)-X(j))
      Fxy(j) = (x2-x1)/(X(j+1)-X(j-1))
      x1 = x2
   ENDDO
   Fxy(N-1) = -x1/(X(N)-X(N-2))
   Fxy(1) = Fxy(2) - (Fxy(3)-Fxy(2))/(X(3)-X(2))*(X(2)-X(1))
   Fxy(N) = Fxy(N-1) + (Fxy(N-1)-Fxy(N-2))/(X(N-1)-X(N-2))*(X(N)-X(N-1))
END SUBROUTINE alg29
