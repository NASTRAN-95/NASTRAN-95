!*==cnorm1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cnorm1(X,N)
   USE c_system
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: X
   INTEGER :: N
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: div
   REAL(REAL64) :: dum , max
   INTEGER :: i , index , nn
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL mesage
!
! End of declarations rewritten by SPAG
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
      WRITE (nout,99001)
99001 FORMAT (//5X,37HCNORM1 RECEIVED A VECTOR OF ALL ZEROS)
      CALL mesage(-37,0,name)
      RETURN
   ENDIF
   div(1) = X(index)
   div(2) = X(index+1)
   max = div(1)*div(1) + div(2)*div(2)
   DO i = 1 , nn , 2
      dum = (X(i)*div(1)+X(i+1)*div(2))/max
      X(i+1) = (X(i+1)*div(1)-X(i)*div(2))/max
      X(i) = dum
   ENDDO
END SUBROUTINE cnorm1
