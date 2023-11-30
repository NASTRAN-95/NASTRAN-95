
SUBROUTINE ifp4g(Ibit,File)
   IMPLICIT NONE
   INTEGER Two(32)
   COMMON /two   / Two
   INTEGER File , Ibit
   INTEGER i1 , i2 , trail(7)
   INTEGER orf
   EXTERNAL orf
!
!     TURNS ON BIT -IBIT- IN TRAILER FOR DATA BLOCK -FILE-
!
!
   trail(1) = File
   CALL rdtrl(trail)
   i1 = (Ibit-1)/16 + 2
   i2 = Ibit - (i1-2)*16 + 16
   trail(i1) = orf(trail(i1),Two(i2))
   trail(1) = File
   CALL wrttrl(trail)
END SUBROUTINE ifp4g
