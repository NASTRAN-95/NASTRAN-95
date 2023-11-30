
SUBROUTINE frd2e(In,Io,Nload,Nfreq)
   IMPLICIT NONE
   INTEGER Incr1 , Inn , Iout , Isys , Nnn
   REAL Z(1)
   COMMON /system/ Isys
   COMMON /unpakx/ Iout , Inn , Nnn , Incr1
   COMMON /zzzzzz/ Z
   INTEGER In , Io , Nfreq , Nload
   INTEGER i , ib1 , ib2 , j , ma(7) , mb(7)
   INTEGER korsz
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