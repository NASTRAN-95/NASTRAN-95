
SUBROUTINE xtrny1(X,Y,Alpha1)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Aaa
   INTEGER Ncol
   COMMON /invpwx/ Aaa , Ncol
!
! Dummy argument declarations
!
   DOUBLE PRECISION Alpha1
   REAL X(1) , Y(1)
!
! Local variable declarations
!
   REAL alpha
   INTEGER i
!
! End of declarations
!
!     SUBROUTINE X TRNS Y (X,Y,ALPHA)
!*******
!     X TRNS Y  FORMS THE DOT PRODUCT X TRANSPOSE * Y = ALPHA
!*******
!     DOUBLE PRECISION   X(1)      ,Y(1)     ,ALPHA
   alpha = 0.0
   DO i = 1 , Ncol
      alpha = alpha + X(i)*Y(i)
   ENDDO
   Alpha1 = alpha
END SUBROUTINE xtrny1
