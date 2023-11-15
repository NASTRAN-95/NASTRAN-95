
SUBROUTINE cnorm1(X,N)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ibuf , Nout
   COMMON /system/ Ibuf , Nout
!
! Dummy argument declarations
!
   INTEGER N
   DOUBLE PRECISION X(1)
!
! Local variable declarations
!
   DOUBLE PRECISION div(2) , dum , max
   INTEGER i , index , name(2) , nn
!
! End of declarations
!
!
!     CNORM1 WILL SEARCH A VECTOR FOR THE LARGEST VALUE AND NORMALIZE
!     THE VECTOR TO LARGEST ELEMENT EQUAL TO ONE
!
   DATA name/4HCNOR , 4HM1  /
!
   nn = N + N
   max = 0.D0
   index = 0
   DO i = 1 , nn , 2
      dum = X(i)*X(i) + X(i+1)*X(i+1)
      IF ( dum>max ) THEN
         max = dum
         index = i
      ENDIF
   ENDDO
   IF ( index==0 ) THEN
!
      WRITE (Nout,99001)
99001 FORMAT (//5X,37HCNORM1 RECEIVED A VECTOR OF ALL ZEROS)
      CALL mesage(-37,0,name)
      GOTO 99999
   ENDIF
   div(1) = X(index)
   div(2) = X(index+1)
   max = div(1)*div(1) + div(2)*div(2)
   DO i = 1 , nn , 2
      dum = (X(i)*div(1)+X(i+1)*div(2))/max
      X(i+1) = (X(i+1)*div(1)-X(i)*div(2))/max
      X(i) = dum
   ENDDO
   RETURN
99999 END SUBROUTINE cnorm1
