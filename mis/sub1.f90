
SUBROUTINE sub1(X,Y,A,B)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ncol
   REAL Xx
   COMMON /invpwx/ Xx , Ncol
!
! Dummy argument declarations
!
   DOUBLE PRECISION A , B
   REAL X(1) , Y(1)
!
! Local variable declarations
!
   REAL a1 , b1
   INTEGER i
!
! End of declarations
!
!     SUBROUTINE SUB(X,Y,A,B)
!*******
!     SUB WILL FORM Y = A*X - B*Y  WHERE A AND B ARE SCALAR MULTIPLIERS
!     FOR THE VECTORS X AND Y
!*******
!     DOUBLE PRECISION   X(1)      ,Y(1)     ,A        ,B
   a1 = A
   b1 = B
   DO i = 1 , Ncol
      Y(i) = X(i)*a1 - Y(i)*b1
   ENDDO
END SUBROUTINE sub1