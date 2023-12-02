!*==ifp4g.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp4g(Ibit,File)
   IMPLICIT NONE
   USE C_TWO
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ibit
   INTEGER :: File
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i1 , i2
   INTEGER , DIMENSION(7) :: trail
   EXTERNAL orf , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
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
