!*==zeroc.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE zeroc(Iz,N)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: N
   INTEGER , DIMENSION(N) :: Iz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
!
! End of declarations rewritten by SPAG
!
!
!     SET AND ARRAY TO ZERO
!
!
   DO i = 1 , N
      Iz(i) = 0
   ENDDO
END SUBROUTINE zeroc
