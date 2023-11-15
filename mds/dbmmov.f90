
SUBROUTINE dbmmov(Index1,Index2,No)
   IMPLICIT NONE
   INCLUDE 'ZZZZZZ.COM'
!
! Dummy argument declarations
!
   INTEGER Index1 , Index2 , No
!
! Local variable declarations
!
   INTEGER i
!
! End of declarations
!
   DO i = 1 , No
      Mem(Index2+i-1) = Mem(Index1+i-1)
   ENDDO
END SUBROUTINE dbmmov
