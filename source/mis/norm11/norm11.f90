!*==norm11.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE norm11(X,Div)
USE C_INVPWX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: X
   REAL(REAL64) :: Div
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ind , ncol
   INTEGER , SAVE :: ind1
   REAL :: max , xi , xx
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Ncol,Filek(2))
   DATA ind1/1/
!
   max = 0.0
   DO i = 1 , ncol
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
   DO i = 1 , ncol
      xi = X(i)*max
      IF ( abs(xi)<1.E-36 ) xi = 0.0
      X(i) = xi
   ENDDO
END SUBROUTINE norm11
