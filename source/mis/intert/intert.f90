!*==intert.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE intert(Nl,Nl1,Nl2,Nm,Ajj,Ta)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nl
   INTEGER :: Nl1
   INTEGER :: Nl2
   INTEGER :: Nm
   REAL , DIMENSION(1) :: Ajj
   REAL , DIMENSION(1) :: Ta
!
! Local variable declarations rewritten by SPAG
!
   REAL :: fract , t , t1 , t2
   INTEGER :: i , n , n1 , n2
!
! End of declarations rewritten by SPAG
!
!
!
   t1 = Ta(Nl1)
   t2 = Ta(Nl2)
   t = Ta(Nl)
   n1 = Nm*(Nl1-1)
   n2 = Nm*(Nl2-1)
   n = Nm*(Nl-1)
   fract = (t-t1)/(t2-t1)
   DO i = 1 , Nm
      Ajj(i+n) = Ajj(i+n1) + fract*(Ajj(i+n2)-Ajj(i+n1))
   ENDDO
END SUBROUTINE intert
