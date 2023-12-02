!*==xtrnsy.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xtrnsy(X,Y,Alpha)
USE C_INVPWX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: X
   REAL(REAL64) , DIMENSION(1) :: Y
   REAL(REAL64) :: Alpha
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
!
! End of declarations rewritten by SPAG
!
!*******
!     X TRNS Y  FORMS THE DOT PRODUCT X TRANSPOSE * Y = ALPHA
!*******
   Alpha = 0.D0
   DO i = 1 , Ncol
      Alpha = Alpha + X(i)*Y(i)
   ENDDO
END SUBROUTINE xtrnsy
