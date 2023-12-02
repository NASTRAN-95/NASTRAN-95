!*==xtrny1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xtrny1(X,Y,Alpha1)
   USE c_invpwx
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: X
   REAL , DIMENSION(1) :: Y
   REAL(REAL64) :: Alpha1
!
! Local variable declarations rewritten by SPAG
!
   REAL :: alpha
   INTEGER :: i
!
! End of declarations rewritten by SPAG
!
!     SUBROUTINE X TRNS Y (X,Y,ALPHA)
!*******
!     X TRNS Y  FORMS THE DOT PRODUCT X TRANSPOSE * Y = ALPHA
!*******
!     DOUBLE PRECISION   X(1)      ,Y(1)     ,ALPHA
   alpha = 0.0
   DO i = 1 , ncol
      alpha = alpha + X(i)*Y(i)
   ENDDO
   Alpha1 = alpha
END SUBROUTINE xtrny1
