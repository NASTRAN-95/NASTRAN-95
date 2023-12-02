!*==sub1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sub1(X,Y,A,B)
USE C_INVPWX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: X
   REAL , DIMENSION(1) :: Y
   REAL(REAL64) :: A
   REAL(REAL64) :: B
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a1 , b1
   INTEGER :: i
!
! End of declarations rewritten by SPAG
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
