!*==frd2a.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frd2a(Nqhl,Qhr,Qhi,Ih,Nfreq)
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nqhl
   INTEGER :: Qhr
   INTEGER :: Qhi
   INTEGER :: Ih
   INTEGER :: Nfreq
!
! Local variable declarations rewritten by SPAG
!
   REAL :: flag
   INTEGER :: i , ibuf1 , ibuf2 , j , nwc , nz
   INTEGER , DIMENSION(7) :: mcb , thi , thr
   EXTERNAL close , dmpfil , gopen , korsz , makmcb , open , pack , rdtrl , read , skprec , unpack , wrttrl , zeroc
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     FIND COLUMN OF NQHL AND COPY REAL TO QHR AND IMAG TO QHI
!
         nz = korsz(z) - sysbuf
         mcb(1) = Nqhl
         CALL rdtrl(mcb)
         IF ( mcb(2)==0 ) GOTO 40
         iout = mcb(5)
         iti = 1
         IF ( iout==4 ) iti = 2
         ito = iti
         nnn = mcb(3)
         inn = 1
         incr1 = 1
         ii = 1
         nn = Ih
         incr = 2
         nwc = 2
         IF ( iout==4 ) nwc = 4
         ibuf1 = nz
         ibuf2 = ibuf1 - sysbuf
         CALL open(*40,Nqhl,z(ibuf1),0)
         CALL read(*40,*40,Nqhl,z(1),-2,1,flag)
         CALL makmcb(thr,Qhr,Ih,mcb(4),ito)
         CALL makmcb(thi,Qhi,Ih,mcb(4),ito)
         CALL skprec(Nqhl,Nfreq-1)
         CALL unpack(*20,Nqhl,z(1))
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      CALL zeroc(z,nnn*nwc)
         spag_nextblock_1 = 2
      CASE (2)
         j = 1
         CALL close(Nqhl,1)
         CALL gopen(Qhr,z(ibuf2),1)
         CALL gopen(Qhi,z(ibuf1),1)
         DO i = 1 , Ih
            CALL pack(z(j),Qhr,thr)
            CALL pack(z(j+1),Qhi,thi)
            j = j + Ih*nwc
         ENDDO
         CALL close(Qhr,1)
         CALL close(Qhi,1)
         CALL wrttrl(thr)
         CALL wrttrl(thi)
         CALL dmpfil(-Qhr,z,nz)
         CALL dmpfil(-Qhi,z,nz)
         RETURN
 40      CALL makmcb(thr,Qhr,0,0,0)
         CALL wrttrl(thr)
         thr(1) = Qhi
         CALL wrttrl(thr)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE frd2a
