
SUBROUTINE zeroc(Iz,N)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER N
   INTEGER Iz(N)
!
! Local variable declarations
!
   INTEGER i
!
! End of declarations
!
!
!     SET AND ARRAY TO ZERO
!
!
   DO i = 1 , N
      Iz(i) = 0
   ENDDO
END SUBROUTINE zeroc
