
SUBROUTINE xtrnsy(X,Y,Alpha)
   IMPLICIT NONE
   REAL Aaa
   INTEGER Ncol
   COMMON /invpwx/ Aaa , Ncol
   DOUBLE PRECISION Alpha
   DOUBLE PRECISION X(1) , Y(1)
   INTEGER i
!*******
!     X TRNS Y  FORMS THE DOT PRODUCT X TRANSPOSE * Y = ALPHA
!*******
   Alpha = 0.D0
   DO i = 1 , Ncol
      Alpha = Alpha + X(i)*Y(i)
   ENDDO
END SUBROUTINE xtrnsy