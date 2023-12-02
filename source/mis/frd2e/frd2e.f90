!*==frd2e.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frd2e(In,Io,Nload,Nfreq)
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
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
   Incr1 = 1
   ma(1) = In
   mb(1) = Io
   ib1 = korsz(Z) - Isys
   ib2 = ib1 - Isys
   CALL rdtrl(ma)
   Iout = ma(5)
   CALL gopen(In,Z(ib1),0)
   CALL gopen(Io,Z(ib2),1)
   CALL makmcb(mb,Io,ma(3),ma(4),Iout)
   DO j = 1 , Nload
      CALL skprec(In,j-1)
      DO i = 1 , Nfreq
         CALL cyct2b(In,Io,1,Z,mb)
         IF ( i/=Nfreq ) CALL skprec(In,Nload-1)
      ENDDO
      CALL rewind(In)
      CALL skprec(In,1)
   ENDDO
   CALL close(In,1)
   CALL close(Io,1)
   CALL wrttrl(mb)
END SUBROUTINE frd2e
