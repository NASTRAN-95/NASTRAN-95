!*==ddr1b.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddr1b(In1,In2,Iout)
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
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
   nz = korsz(z) - sysbuf
   CALL gopen(In1,z(nz+1),0)
   nz = nz - sysbuf
   CALL gopen(In2,z(nz+1),0)
   nz = nz - sysbuf
   CALL gopen(Iout,z(nz+1),1)
   mcb(1) = In1
   CALL rdtrl(mcb)
   mcb(1) = Iout
   nd = mcb(2)/3
   itc = mcb(5)
   incr = 1
   mcb(2) = 0
   mcb(6) = 0
   mcb(7) = 0
   DO i = 1 , nd
      CALL skprec(In1,1)
      CALL cyct2b(In2,Iout,1,z,mcb)
      CALL cyct2b(In1,Iout,2,z,mcb)
   ENDDO
   CALL wrttrl(mcb)
   CALL close(In1,1)
   CALL close(In2,1)
   CALL close(Iout,1)
END SUBROUTINE ddr1b
