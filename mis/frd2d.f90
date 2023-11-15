
SUBROUTINE frd2d(In,Io,Ip)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Incr1 , Inn , Iout , Mnn , Sysbuf
   REAL Z(1)
   COMMON /system/ Sysbuf
   COMMON /unpakx/ Iout , Inn , Mnn , Incr1
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER In , Io , Ip
!
! Local variable declarations
!
   INTEGER ib1 , ib2 , ma(7) , mb(7) , n , nc
   INTEGER korsz
!
! End of declarations
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
