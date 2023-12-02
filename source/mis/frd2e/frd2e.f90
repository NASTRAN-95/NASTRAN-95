!*==frd2e.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frd2e(In,Io,Nload,Nfreq)
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: In
   INTEGER :: Io
   INTEGER :: Nload
   INTEGER :: Nfreq
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ib1 , ib2 , j
   INTEGER , DIMENSION(7) :: ma , mb
   EXTERNAL close , cyct2b , gopen , korsz , makmcb , rdtrl , rewind , skprec , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!
!     MAKE UHDF FROM IN
!
   incr1 = 1
   ma(1) = In
   mb(1) = Io
   ib1 = korsz(z) - isys
   ib2 = ib1 - isys
   CALL rdtrl(ma)
   iout = ma(5)
   CALL gopen(In,z(ib1),0)
   CALL gopen(Io,z(ib2),1)
   CALL makmcb(mb,Io,ma(3),ma(4),iout)
   DO j = 1 , Nload
      CALL skprec(In,j-1)
      DO i = 1 , Nfreq
         CALL cyct2b(In,Io,1,z,mb)
         IF ( i/=Nfreq ) CALL skprec(In,Nload-1)
      ENDDO
      CALL rewind(In)
      CALL skprec(In,1)
   ENDDO
   CALL close(In,1)
   CALL close(Io,1)
   CALL wrttrl(mb)
END SUBROUTINE frd2e
