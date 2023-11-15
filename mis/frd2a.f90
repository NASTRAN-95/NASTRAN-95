
SUBROUTINE frd2a(Nqhl,Qhr,Qhi,Ih,Nfreq)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ii , Incr , Incr1 , Inn , Iout , Iti , Ito , Nn , Nnn , Sysbuf
   REAL Z(1)
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /system/ Sysbuf
   COMMON /unpakx/ Iout , Inn , Nnn , Incr1
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Ih , Nfreq , Nqhl , Qhi , Qhr
!
! Local variable declarations
!
   REAL flag
   INTEGER i , ibuf1 , ibuf2 , j , mcb(7) , nwc , nz , thi(7) , thr(7)
   INTEGER korsz
!
! End of declarations
!
!
!
!     FIND COLUMN OF NQHL AND COPY REAL TO QHR AND IMAG TO QHI
!
   nz = korsz(Z) - Sysbuf
   mcb(1) = Nqhl
   CALL rdtrl(mcb)
   IF ( mcb(2)==0 ) GOTO 300
   Iout = mcb(5)
   Iti = 1
   IF ( Iout==4 ) Iti = 2
   Ito = Iti
   Nnn = mcb(3)
   Inn = 1
   Incr1 = 1
   Ii = 1
   Nn = Ih
   Incr = 2
   nwc = 2
   IF ( Iout==4 ) nwc = 4
   ibuf1 = nz
   ibuf2 = ibuf1 - Sysbuf
   CALL open(*300,Nqhl,Z(ibuf1),0)
   CALL read(*300,*300,Nqhl,Z(1),-2,1,flag)
   CALL makmcb(thr,Qhr,Ih,mcb(4),Ito)
   CALL makmcb(thi,Qhi,Ih,mcb(4),Ito)
   CALL skprec(Nqhl,Nfreq-1)
   CALL unpack(*100,Nqhl,Z(1))
   GOTO 200
 100  CALL zeroc(Z,Nnn*nwc)
 200  j = 1
   CALL close(Nqhl,1)
   CALL gopen(Qhr,Z(ibuf2),1)
   CALL gopen(Qhi,Z(ibuf1),1)
   DO i = 1 , Ih
      CALL pack(Z(j),Qhr,thr)
      CALL pack(Z(j+1),Qhi,thi)
      j = j + Ih*nwc
   ENDDO
   CALL close(Qhr,1)
   CALL close(Qhi,1)
   CALL wrttrl(thr)
   CALL wrttrl(thi)
   CALL dmpfil(-Qhr,Z,nz)
   CALL dmpfil(-Qhi,Z,nz)
   GOTO 99999
 300  CALL makmcb(thr,Qhr,0,0,0)
   CALL wrttrl(thr)
   thr(1) = Qhi
   CALL wrttrl(thr)
99999 END SUBROUTINE frd2a
