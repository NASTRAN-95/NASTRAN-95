!*==dbmmov.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dbmmov(Index1,Index2,No)
   USE I_ZZZZZZ
   IMPLICIT NONE
   INCLUDE 'ZZZZZZ.COM'
   INTEGER Index1 , Index2 , No
   INTEGER i
   DO i = 1 , No
      mem(Index2+i-1) = mem(Index1+i-1)
   ENDDO
END SUBROUTINE dbmmov
