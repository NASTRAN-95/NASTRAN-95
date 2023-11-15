
SUBROUTINE dpd
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf(24) , Buf1 , Buf2 , Buf3 , Buf4 , Dload(2) , Dlt , Dpool , Eed , Eigb(2) , Eigc(2) , Eigr(2) , Epoint(2) , Eqdyn ,   &
         & Freq(2) , Freq1(2) , Frl , Gpl , Gpld , Ineqc , Kn , L , Loads(32) , Luset , Lusetd , Mcb(7) , Msg(3) , Nam(2) , Neqdyn ,&
         & Ngrid , Nlft , Nodlt , Noeed , Nofrl , Nogo , Nolin(21) , Nonlft , Nopsdl , Nosdt , Notfl , Notrl , Noue , Psd(2) ,      &
         & Psdl , Scr1 , Scr2 , Scr3 , Scr4 , Sdt , Seqep(2) , Sil , Sild , Sysbuf , Tf(2) , Tfl , Tic(2) , Trl , Tstep(2) , Uset , &
         & Usetd , Z(1)
   REAL Bufr(20) , Zz(1)
   COMMON /blank / Luset , Lusetd , Notfl , Nodlt , Nopsdl , Nofrl , Nonlft , Notrl , Noeed , Nosdt , Noue
   COMMON /dpdcom/ Dpool , Gpl , Sil , Uset , Gpld , Sild , Usetd , Dlt , Frl , Nlft , Tfl , Trl , Psdl , Eed , Scr1 , Scr2 , Scr3 ,&
                 & Scr4 , Buf , Buf1 , Buf2 , Buf3 , Buf4 , Epoint , Seqep , L , Kn , Neqdyn , Loads , Dload , Freq1 , Freq ,       &
                 & Nolin , Nogo , Msg , Tic , Tstep , Tf , Psd , Eigr , Eigb , Eigc , Mcb , Nam , Eqdyn , Sdt , Ineqc
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER i , ineq
   INTEGER korsz
!
! End of declarations
!
!
!     DPD IS MAIN CONTROL PROGRAM FOR THE DYNAMICS POOL DISTRIBUTOR.
!
   EQUIVALENCE (Z(1),Zz(1)) , (Buf(1),Bufr(1)) , (Msg(2),Ngrid)
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
