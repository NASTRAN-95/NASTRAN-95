!*==frd2d.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frd2d(In,Io,Ip)
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: In
   INTEGER :: Io
   INTEGER :: Ip
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ib1 , ib2 , n , nc
   INTEGER , DIMENSION(7) :: ma , mb
   EXTERNAL close , cyct2b , dmpfil , gopen , korsz , makmcb , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!
!     ADD IN TO END OF IO
!
   Incr1 = 1
   ma(1) = In
   mb(1) = Io
   CALL rdtrl(ma)
   Iout = ma(5)
   nc = korsz(Z)
   ib1 = nc - Sysbuf
   ib2 = ib1 - Sysbuf
   CALL gopen(In,Z(ib1),0)
   IF ( Ip/=0 ) THEN
      CALL gopen(Io,Z(ib2),3)
      CALL rdtrl(mb)
   ELSE
      CALL gopen(Io,Z(ib2),1)
      CALL makmcb(mb,Io,ma(3),2,Iout)
   ENDIF
   n = ma(2)
   CALL cyct2b(In,Io,n,Z,mb)
   CALL close(In,1)
   CALL close(Io,3)
   CALL wrttrl(mb)
   CALL dmpfil(-In,Z,nc)
END SUBROUTINE frd2d
