!*==cnorm.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cnorm(X,Div,Y)
   USE c_cinvpx
   USE c_cinvxx
   USE c_system
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: X
   REAL(REAL64) , DIMENSION(2) :: Div
   REAL(REAL64) , DIMENSION(1) :: Y
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: cosang , d , max , r , ri , sign , temp , xo
   INTEGER :: i , idiag , ind , ncol , ncol2
   EXTERNAL mesage , sswtch
!
! End of declarations rewritten by SPAG
!
!
!     CNORM WILL NORMALIZE X TO THE MAXIMUM ELEMENT EQUAL TO A MODULUS
!     OF ONE AND STORE THE DIVISOR IN MAX (X MAY BE COMPLEX)
!
   !>>>>EQUIVALENCE (Ncol,Filek(2))
!
   ncol2 = ncol + ncol
   max = 0.D0
   sign = 1.0D0
   ind = 0
   DO i = 1 , ncol2 , 2
      IF ( X(i)**2+X(i+1)**2>max ) THEN
         max = X(i)**2 + X(i+1)**2
         ind = i
      ENDIF
   ENDDO
   IF ( ind==0 ) THEN
!
      WRITE (nout,99001)
99001 FORMAT (//5X,37HCONOR  RECEIVED A VECTOR OF ALL ZEROS)
      CALL mesage(-37,0,0)
      RETURN
   ELSEIF ( iter/=1 ) THEN
      IF ( ind/=ind1 ) THEN
         CALL sswtch(12,idiag)
         IF ( idiag/=0 ) THEN
            WRITE (6,99002) ind , ind1
99002       FORMAT (10H CHANGE   ,2I5)
         ENDIF
      ENDIF
      d = X(ind)**2 + X(ind+1)**2
      r = (X(ind1)*X(ind)+X(ind1+1)*X(ind+1))/d
      ri = (X(ind1+1)*X(ind)-X(ind1)*X(ind+1))/d
      cosang = xo*r/dsqrt(r**2+ri**2)
      IF ( dabs(cosang+1.D0)<=0.1D0 ) sign = -1.0D0
   ENDIF
   i = ind
   Div(1) = X(i)*sign
   Div(2) = X(i+1)*sign
   ind1 = ind
   max = 1.0D0/max
   DO i = 1 , ncol2 , 2
      temp = (X(i)*Div(1)+X(i+1)*Div(2))*max
      X(i+1) = (X(i+1)*Div(1)-X(i)*Div(2))*max
      X(i) = temp
   ENDDO
   xo = X(ind)
END SUBROUTINE cnorm
