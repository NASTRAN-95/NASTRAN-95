!*==dpd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpd
   IMPLICIT NONE
   USE C_BLANK
   USE C_DPDCOM
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ineq
   EXTERNAL dpd1 , dpd2 , dpd3 , dpd4 , dpd5 , korsz , mesage , rdtrl
!
! End of declarations rewritten by SPAG
!
!
!     DPD IS MAIN CONTROL PROGRAM FOR THE DYNAMICS POOL DISTRIBUTOR.
!
   !>>>>EQUIVALENCE (Z(1),Zz(1)) , (Buf(1),Bufr(1)) , (Msg(2),Ngrid)
!
!     INITIALIZE CONTROL PARAMETERS.
!
   Notfl = -1
   Nodlt = -1
   Nopsdl = -1
   Nofrl = -1
   Nonlft = -1
   Notrl = -1
   Noeed = -1
   Nosdt = -1
   Noue = -1
   Nogo = 0
   ineq = 0
   DO i = 1 , 7
      Mcb(i) = 0
   ENDDO
!
!     PERFORM BUFFER ALLOCATION
!
   Buf1 = korsz(Z) - Sysbuf - 2
   Buf2 = Buf1 - Sysbuf
   Buf3 = Buf2 - Sysbuf
   Buf4 = Buf3 - Sysbuf
!
!     IF DYNAMICS POOL IS PURGED, EXIT. OTHERWISE, EXECUTE THE PHASES
!     OF DPD
!
   Buf(1) = Dpool
   CALL rdtrl(Buf)
   IF ( Buf(1)/=Dpool ) RETURN
   CALL dpd1
   CALL dpd2
   CALL dpd3
   CALL dpd4
   CALL dpd5
   IF ( Nogo/=0 ) CALL mesage(-61,0,0)
END SUBROUTINE dpd
