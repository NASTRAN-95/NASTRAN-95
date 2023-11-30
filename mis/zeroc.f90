
SUBROUTINE zeroc(Iz,N)
   IMPLICIT NONE
   INTEGER N
   INTEGER Iz(N)
   INTEGER i
!
!     SET AND ARRAY TO ZERO
!
!
   DO i = 1 , N
      Iz(i) = 0
   ENDDO
END SUBROUTINE zeroc
