
SUBROUTINE csub(X,Y,Z,A,B)
   IMPLICIT NONE
   REAL Aaa
   INTEGER Ncol
   COMMON /cinvpx/ Aaa , Ncol
   DOUBLE PRECISION A(2) , B(2) , X(2) , Y(2) , Z(1)
   DOUBLE PRECISION dum
   INTEGER i , ncol2
!*******
!     CSUB WILL FORM Z = A*X - B*Y WHERE A AND B ARE SCALAR
!     MULTIPLIERS FOR THE COMPLEX VECTORS X AND Y
!*******
   ncol2 = Ncol + Ncol
   DO i = 1 , ncol2 , 2
      dum = X(i)*A(1) - X(i+1)*A(2) - Y(i)*B(1) + Y(i+1)*B(2)
      Z(i+1) = X(i)*A(2) + X(i+1)*A(1) - Y(i+1)*B(1) - Y(i)*B(2)
      Z(i) = dum
   ENDDO
END SUBROUTINE csub
