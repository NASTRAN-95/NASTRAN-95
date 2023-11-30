
SUBROUTINE dbmmov(Index1,Index2,No)
   IMPLICIT NONE
   INCLUDE 'ZZZZZZ.COM'
   INTEGER Index1 , Index2 , No
   INTEGER i
   DO i = 1 , No
      Mem(Index2+i-1) = Mem(Index1+i-1)
   ENDDO
END SUBROUTINE dbmmov
