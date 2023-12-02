!*==frd2d.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frd2d(In,Io,Ip)
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
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
   incr1 = 1
   ma(1) = In
   mb(1) = Io
   CALL rdtrl(ma)
   iout = ma(5)
   nc = korsz(z)
   ib1 = nc - sysbuf
   ib2 = ib1 - sysbuf
   CALL gopen(In,z(ib1),0)
   IF ( Ip/=0 ) THEN
      CALL gopen(Io,z(ib2),3)
      CALL rdtrl(mb)
   ELSE
      CALL gopen(Io,z(ib2),1)
      CALL makmcb(mb,Io,ma(3),2,iout)
   ENDIF
   n = ma(2)
   CALL cyct2b(In,Io,n,z,mb)
   CALL close(In,1)
   CALL close(Io,3)
   CALL wrttrl(mb)
   CALL dmpfil(-In,z,nc)
END SUBROUTINE frd2d
