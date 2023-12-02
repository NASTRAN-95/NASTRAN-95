!*==ddr1b.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddr1b(In1,In2,Iout)
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: In1
   INTEGER :: In2
   INTEGER :: Iout
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , nd , nz
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL close , cyct2b , gopen , korsz , rdtrl , skprec , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE REPLACES DISPLACEMNTS ON IN1 WITH DISPLACEMENTS ON
!     IN2  AND WRITES ON  IOUT
!
!
!
   nz = korsz(Z) - Sysbuf
   CALL gopen(In1,Z(nz+1),0)
   nz = nz - Sysbuf
   CALL gopen(In2,Z(nz+1),0)
   nz = nz - Sysbuf
   CALL gopen(Iout,Z(nz+1),1)
   mcb(1) = In1
   CALL rdtrl(mcb)
   mcb(1) = Iout
   nd = mcb(2)/3
   Itc = mcb(5)
   Incr = 1
   mcb(2) = 0
   mcb(6) = 0
   mcb(7) = 0
   DO i = 1 , nd
      CALL skprec(In1,1)
      CALL cyct2b(In2,Iout,1,Z,mcb)
      CALL cyct2b(In1,Iout,2,Z,mcb)
   ENDDO
   CALL wrttrl(mcb)
   CALL close(In1,1)
   CALL close(In2,1)
   CALL close(Iout,1)
END SUBROUTINE ddr1b
