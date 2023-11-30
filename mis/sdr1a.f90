
SUBROUTINE sdr1a(Input,Iout)
   IMPLICIT NONE
   INTEGER Core(1100) , Ii , Ii1 , Incr , Incr1 , It1 , It2 , It3 , Jj , Jj1 , Ksystm(65) , Loadnn , Sysbuf
   REAL Corex(1)
   COMMON /blank / Loadnn
   COMMON /packx / It2 , It3 , Ii1 , Jj1 , Incr1
   COMMON /system/ Sysbuf , Ksystm
   COMMON /unpakx/ It1 , Ii , Jj , Incr
   COMMON /zzzzzz/ Corex
   INTEGER Input , Iout , Itran , Iuf , Iuf1 , Ps
   INTEGER bcd1(2) , i , ia(7) , ibf , ibf1 , k , mcb(7) , ncolps , nz
   INTEGER korsz
!
!     THIS ROUTINE MAKES PS AND IUF COMPATABLE TO COMPUTE QS IN
!     CASE OF TRANSIENT ANALYSIS
!
!
   !>>>>EQUIVALENCE (Core(1),Corex(1))
!
   DATA bcd1/4HSDR1 , 4HA   /
!
   nz = korsz(Core) - Sysbuf
   CALL open(*200,Input,Core(nz+1),0)
   CALL skprec(Input,1)
   nz = nz - Sysbuf
   Loadnn = max0(Loadnn,1)
   IF ( Loadnn==1 ) GOTO 300
   ia(1) = Iout
   CALL rdtrl(ia)
   IF ( ia(2)==0 ) GOTO 300
   ia(1) = Input
   CALL rdtrl(ia)
   IF ( ia(2)==0 ) CALL mesage(-7,0,bcd1)
!
!     POSITION TO END
!
   CALL gopen(Iout,Core(nz+1),0)
   CALL skpfil(Iout,+1)
   CALL skpfil(Iout,-1)
   CALL close(Iout,+2)
   ia(1) = Iout
   CALL rdtrl(ia)
   ia(7) = 0
   CALL gopen(Iout,Core(nz+1),3)
 100  mcb(1) = Input
   CALL rdtrl(mcb)
   k = mcb(2)
   It1 = mcb(5)
   It2 = It1
   It3 = It2
   Incr = 1
   Incr1 = 1
   DO i = 1 , k
      Ii = 0
      CALL unpack(*150,Input,Core)
      Ii1 = Ii
      Jj1 = Jj
      CALL pack(Core,Iout,ia)
      CYCLE
 150  Ii1 = 1
      Jj1 = 1
      Core(1) = 0
      Core(2) = 0
      Core(3) = 0
      Core(4) = 0
      CALL pack(Core,Iout,ia)
   ENDDO
   CALL close(Input,1)
   CALL close(Iout,1)
   CALL wrttrl(ia)
 200  RETURN
!
!     FIRST TIME
!
 300  CALL gopen(Iout,Core(nz+1),1)
   ia(1) = Input
   CALL rdtrl(ia)
   ia(2) = 0
   ia(6) = 0
   ia(7) = 0
   ia(1) = Iout
   GOTO 100
!
!     SDR1D -
!
   ENTRY sdr1d(Ps,Iuf,Iuf1,Itran)
!     ===============================
!
   IF ( Itran/=0 ) THEN
      Itran = 1
      mcb(1) = Ps
      CALL rdtrl(mcb)
      IF ( mcb(1)<=0 ) GOTO 99999
      ncolps = mcb(2)
      mcb(1) = Iuf
      CALL rdtrl(mcb)
      IF ( ncolps==mcb(2) ) RETURN
!
!     THIS IS A TRANSIENT PROBLEM
      Itran = 0
   ENDIF
!
   mcb(1) = Iuf
   CALL rdtrl(mcb)
   ncolps = mcb(2)/3
   ibf = korsz(Core) - Sysbuf
   CALL gopen(Iuf,Core(ibf),0)
   ibf1 = ibf - Sysbuf
   CALL gopen(Iuf1,Core(ibf1),1)
   It1 = mcb(5)
   It2 = It1
   It3 = It2
   Incr = 1
   Incr1 = 1
   mcb(1) = Iuf1
   mcb(2) = 0
   mcb(6) = 0
   mcb(7) = 0
   DO i = 1 , ncolps
      Ii = 0
      CALL unpack(*350,Iuf,Core)
      Ii1 = Ii
      Jj1 = Jj
      GOTO 400
 350  Core(1) = 0
      Core(2) = 0
      Core(3) = 0
      Core(4) = 0
      Ii1 = 1
      Jj1 = 1
 400  CALL skprec(Iuf,2)
      CALL pack(Core,Iuf1,mcb)
   ENDDO
   CALL close(Iuf1,1)
   CALL close(Iuf,1)
   CALL wrttrl(mcb)
99999 RETURN
END SUBROUTINE sdr1a