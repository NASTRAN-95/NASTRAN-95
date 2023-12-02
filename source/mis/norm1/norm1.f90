!*==norm1.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE norm1(X,Div)
   USE c_invpwx
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: X
   REAL(REAL64) :: Div
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ind , ncol
   INTEGER , SAVE :: ind1
   REAL(REAL64) :: max
   REAL :: xi , xx
!
! End of declarations rewritten by SPAG
!
!*******
!     NORM WILL NORMALIZE X TO MAXIMUM ELEMENT EQUAL TO ONE AND STORE TH
!     DIVISOR IN MAX
!*******
   !>>>>EQUIVALENCE (Ncol,Filek(2))
   DATA ind1/1/
   max = 0.D0
   DO i = 1 , ncol
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
   DO i = 1 , ncol
      xi = X(i)*max
      IF ( abs(xi)<1.E-36 ) xi = 0.
      X(i) = xi
   ENDDO
END SUBROUTINE norm1
