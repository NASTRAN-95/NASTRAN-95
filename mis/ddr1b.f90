
SUBROUTINE ddr1b(In1,In2,Iout)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ii , Incr , Itc , Jj , Sysbuf
   REAL Z(1)
   COMMON /system/ Sysbuf
   COMMON /unpakx/ Itc , Ii , Jj , Incr
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER In1 , In2 , Iout
!
! Local variable declarations
!
   INTEGER i , mcb(7) , nd , nz
   INTEGER korsz
!
! End of declarations
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
