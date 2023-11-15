
SUBROUTINE cnorm(X,Div,Y)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum(30) , Filek(7)
   INTEGER Ibuf , Ind1 , Iter , Ncol , Nout
   COMMON /cinvpx/ Filek
   COMMON /cinvxx/ Dum , Ind1 , Iter
   COMMON /system/ Ibuf , Nout
!
! Dummy argument declarations
!
   DOUBLE PRECISION Div(2) , X(1) , Y(1)
!
! Local variable declarations
!
   DOUBLE PRECISION cosang , d , max , r , ri , sign , temp , xo
   INTEGER i , idiag , ind , ncol2
!
! End of declarations
!
!
!     CNORM WILL NORMALIZE X TO THE MAXIMUM ELEMENT EQUAL TO A MODULUS
!     OF ONE AND STORE THE DIVISOR IN MAX (X MAY BE COMPLEX)
!
   EQUIVALENCE (Ncol,Filek(2))
!
   ncol2 = Ncol + Ncol
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
      WRITE (Nout,99001)
99001 FORMAT (//5X,37HCONOR  RECEIVED A VECTOR OF ALL ZEROS)
      CALL mesage(-37,0,0)
      GOTO 99999
   ELSEIF ( Iter/=1 ) THEN
      IF ( ind/=Ind1 ) THEN
         CALL sswtch(12,idiag)
         IF ( idiag/=0 ) THEN
            WRITE (6,99002) ind , Ind1
99002       FORMAT (10H CHANGE   ,2I5)
         ENDIF
      ENDIF
      d = X(ind)**2 + X(ind+1)**2
      r = (X(Ind1)*X(ind)+X(Ind1+1)*X(ind+1))/d
      ri = (X(Ind1+1)*X(ind)-X(Ind1)*X(ind+1))/d
      cosang = xo*r/dsqrt(r**2+ri**2)
      IF ( dabs(cosang+1.D0)<=0.1D0 ) sign = -1.0D0
   ENDIF
   i = ind
   Div(1) = X(i)*sign
   Div(2) = X(i+1)*sign
   Ind1 = ind
   max = 1.0D0/max
   DO i = 1 , ncol2 , 2
      temp = (X(i)*Div(1)+X(i+1)*Div(2))*max
      X(i+1) = (X(i+1)*Div(1)-X(i)*Div(2))*max
      X(i) = temp
   ENDDO
   xo = X(ind)
   RETURN
99999 END SUBROUTINE cnorm
