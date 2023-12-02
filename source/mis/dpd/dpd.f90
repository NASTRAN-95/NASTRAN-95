!*==dpd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpd
   USE c_blank
   USE c_dpdcom
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
   notfl = -1
   nodlt = -1
   nopsdl = -1
   nofrl = -1
   nonlft = -1
   notrl = -1
   noeed = -1
   nosdt = -1
   noue = -1
   nogo = 0
   ineq = 0
   DO i = 1 , 7
      mcb(i) = 0
   ENDDO
!
!     PERFORM BUFFER ALLOCATION
!
   buf1 = korsz(z) - sysbuf - 2
   buf2 = buf1 - sysbuf
   buf3 = buf2 - sysbuf
   buf4 = buf3 - sysbuf
!
!     IF DYNAMICS POOL IS PURGED, EXIT. OTHERWISE, EXECUTE THE PHASES
!     OF DPD
!
   buf(1) = dpool
   CALL rdtrl(buf)
   IF ( buf(1)/=dpool ) RETURN
   CALL dpd1
   CALL dpd2
   CALL dpd3
   CALL dpd4
   CALL dpd5
   IF ( nogo/=0 ) CALL mesage(-61,0,0)
END SUBROUTINE dpd
