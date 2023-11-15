
SUBROUTINE norm1(X,Div)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Filek(7)
   INTEGER Ncol
   COMMON /invpwx/ Filek
!
! Dummy argument declarations
!
   DOUBLE PRECISION Div
   DOUBLE PRECISION X(1)
!
! Local variable declarations
!
   INTEGER i , ind , ind1
   DOUBLE PRECISION max
   REAL xi , xx
!
! End of declarations
!
!*******
!     NORM WILL NORMALIZE X TO MAXIMUM ELEMENT EQUAL TO ONE AND STORE TH
!     DIVISOR IN MAX
!*******
   EQUIVALENCE (Ncol,Filek(2))
   DATA ind1/1/
   max = 0.D0
   DO i = 1 , Ncol
      Div = dabs(X(i))
      IF ( Div>max ) THEN
         max = Div
         ind = i
      ENDIF
   ENDDO
   IF ( X(ind)<0.D0 ) ind = -ind
   i = iabs(ind1)
   xx = X(i)
   Div = sign(1.,xx)*float(isign(1,ind1))*max
   xx = Div
   ind1 = ind*ifix(sign(1.,xx))
   max = 1.D0/Div
   DO i = 1 , Ncol
      xi = X(i)*max
      IF ( abs(xi)<1.E-36 ) xi = 0.
      X(i) = xi
   ENDDO
END SUBROUTINE norm1
