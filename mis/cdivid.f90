
SUBROUTINE cdivid(A,B,D,Ncol)
   IMPLICIT NONE
   INTEGER Ncol
   DOUBLE PRECISION A(1) , B(1) , D(2)
   DOUBLE PRECISION denm , dtemp
   INTEGER i
!
!     THIS ROUTINE DIVIDES THE VECTOR A BY D AND STORE RESULT IN B
!
   denm = D(1)**2 + D(2)**2
   DO i = 1 , Ncol , 2
      dtemp = (A(i)*D(1)+A(i+1)*D(2))/denm
      B(i+1) = (A(i+1)*D(1)-A(i)*D(2))/denm
      B(i) = dtemp
   ENDDO
END SUBROUTINE cdivid
