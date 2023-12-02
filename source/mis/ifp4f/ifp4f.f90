!*==ifp4f.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp4f(Ibit,File,Bit)
   IMPLICIT NONE
   USE C_TWO
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ibit
   INTEGER :: File
   LOGICAL :: Bit
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i1 , i2
   INTEGER , DIMENSION(7) :: trail
   EXTERNAL andf , rdtrl
!
! End of declarations rewritten by SPAG
!
!
!     TEST BIT -IBIT- IN TRAILER OF DATA BLOCK -FILE-
!
!
   trail(1) = File
   CALL rdtrl(trail)
   i1 = (Ibit-1)/16 + 2
   i2 = Ibit - (i1-2)*16 + 16
   IF ( andf(trail(i1),Two(i2))/=0 ) THEN
      Bit = .TRUE.
      RETURN
   ELSE
      Bit = .FALSE.
   ENDIF
END SUBROUTINE ifp4f
