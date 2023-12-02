!*==sdr1a.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr1a(Input,Iout)
   USE C_BLANK
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
   IMPLICIT NONE
   INTEGER core(1100) , Ii , Ii1 , Incr , Incr1 , It1 , It2 , It3 , Jj , Jj1 , Ksystm(65) , Loadnn , Sysbuf
   REAL Corex(1)
   COMMON /blank / Loadnn
   COMMON /packx / It2 , It3 , Ii1 , Jj1 , Incr1
   COMMON /system/ Sysbuf , Ksystm
   COMMON /unpakx/ It1 , Ii , Jj , Incr
   COMMON /zzzzzz/ Corex
   INTEGER Input , Iout , Itran , Iuf , Iuf1 , Ps
   INTEGER bcd1(2) , i , ia(7) , ibf , ibf1 , k , mcb(7) , ncolps , nz
   INTEGER korsz
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS ROUTINE MAKES PS AND IUF COMPATABLE TO COMPUTE QS IN
!     CASE OF TRANSIENT ANALYSIS
!
!
   !>>>>EQUIVALENCE (Core(1),Corex(1))
!
   DATA bcd1/4HSDR1 , 4HA   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nz = korsz(core) - Sysbuf
         CALL open(*20,Input,core(nz+1),0)
         CALL skprec(Input,1)
         nz = nz - Sysbuf
         Loadnn = max0(Loadnn,1)
         IF ( Loadnn==1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ia(1) = Iout
         CALL rdtrl(ia)
         IF ( ia(2)==0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ia(1) = Input
         CALL rdtrl(ia)
         IF ( ia(2)==0 ) CALL mesage(-7,0,bcd1)
!
!     POSITION TO END
!
         CALL gopen(Iout,core(nz+1),0)
         CALL skpfil(Iout,+1)
         CALL skpfil(Iout,-1)
         CALL close(Iout,+2)
         ia(1) = Iout
         CALL rdtrl(ia)
         ia(7) = 0
         CALL gopen(Iout,core(nz+1),3)
         spag_nextblock_1 = 2
      CASE (2)
         mcb(1) = Input
         CALL rdtrl(mcb)
         k = mcb(2)
         It1 = mcb(5)
         It2 = It1
         It3 = It2
         Incr = 1
         Incr1 = 1
         DO i = 1 , k
            Ii = 0
            CALL unpack(*10,Input,core)
            Ii1 = Ii
            Jj1 = Jj
            CALL pack(core,Iout,ia)
            CYCLE
 10         Ii1 = 1
            Jj1 = 1
            core(1) = 0
            core(2) = 0
            core(3) = 0
            core(4) = 0
            CALL pack(core,Iout,ia)
         ENDDO
         CALL close(Input,1)
         CALL close(Iout,1)
         CALL wrttrl(ia)
 20      RETURN
      CASE (3)
!
!     FIRST TIME
!
         CALL gopen(Iout,core(nz+1),1)
         ia(1) = Input
         CALL rdtrl(ia)
         ia(2) = 0
         ia(6) = 0
         ia(7) = 0
         ia(1) = Iout
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
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
            IF ( mcb(1)<=0 ) RETURN
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
         ibf = korsz(core) - Sysbuf
         CALL gopen(Iuf,core(ibf),0)
         ibf1 = ibf - Sysbuf
         CALL gopen(Iuf1,core(ibf1),1)
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
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  Ii = 0
                  CALL unpack(*22,Iuf,core)
                  Ii1 = Ii
                  Jj1 = Jj
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 22               core(1) = 0
                  core(2) = 0
                  core(3) = 0
                  core(4) = 0
                  Ii1 = 1
                  Jj1 = 1
                  spag_nextblock_2 = 2
               CASE (2)
                  CALL skprec(Iuf,2)
                  CALL pack(core,Iuf1,mcb)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL close(Iuf1,1)
         CALL close(Iuf,1)
         CALL wrttrl(mcb)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sdr1a
