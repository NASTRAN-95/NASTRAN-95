
SUBROUTINE norm11(X,Div)
   IMPLICIT NONE
   REAL Filek(7)
   INTEGER Ncol
   COMMON /invpwx/ Filek
   DOUBLE PRECISION Div
   REAL X(1)
   INTEGER i , ind , ind1
   REAL max , xi , xx
!
   !>>>>EQUIVALENCE (Ncol,Filek(2))
   DATA ind1/1/
!
   max = 0.0
   DO i = 1 , Ncol
      xx = abs(X(i))
      IF ( xx>max ) THEN
         max = xx
         ind = i
      ENDIF
   ENDDO
   IF ( X(ind)<0.0 ) ind = -ind
   i = iabs(ind1)
   xx = X(i)
   Div = sign(1.,xx)*float(isign(1,ind1))*max
   xx = Div
   ind1 = ind*ifix(sign(1.,xx))
   max = 1.0/Div
   DO i = 1 , Ncol
      xi = X(i)*max
      IF ( abs(xi)<1.E-36 ) xi = 0.0
      X(i) = xi
   ENDDO
END SUBROUTINE norm11