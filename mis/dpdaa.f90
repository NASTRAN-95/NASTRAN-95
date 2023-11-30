
SUBROUTINE dpdaa
   IMPLICIT NONE
   INTEGER Buf(24) , Buf1 , Buf2 , Buf3 , Buf4 , Dload(2) , Dlt , Dpool , Eed , Eigb(2) , Eigc(2) , Eigr(2) , Epoint(2) , Eqdyn ,   &
         & Freq(2) , Freq1(2) , Frl , Gpl , Gpld , Ineq , Kn , L , Loads(32) , Mcb(7) , Msg(3) , Nam(2) , Neqdyn , Ngrid , Nlft ,   &
         & Nogo , Nolin(21) , Psd(2) , Psdl , Scr1 , Scr2 , Scr3 , Scr4 , Seqep(2) , Sil , Sild , Tf(2) , Tfl , Tic(2) , Trl ,      &
         & Tstep(2) , Uset , Usetd , Z(1)
   REAL Bufr(20) , Sdt , Zz(1)
   COMMON /dpdcom/ Dpool , Gpl , Sil , Uset , Gpld , Sild , Usetd , Dlt , Frl , Nlft , Tfl , Trl , Psdl , Eed , Scr1 , Scr2 , Scr3 ,&
                 & Scr4 , Buf , Buf1 , Buf2 , Buf3 , Buf4 , Epoint , Seqep , L , Kn , Neqdyn , Loads , Dload , Freq1 , Freq ,       &
                 & Nolin , Nogo , Msg , Tic , Tstep , Tf , Psd , Eigr , Eigb , Eigc , Mcb , Nam , Eqdyn , Sdt , Ineq
   COMMON /zzzzzz/ Z
   INTEGER k , khi , klo
!*****
! DPDAA PERFORMS A BINARY SEARCH IN EQDYN AND CONVERTS THE GRID NO
! AND COMPONENT CODE TO AN SIL VALUE.
!*****
!
!
!
!
!
   EQUIVALENCE (Z(1),Zz(1)) , (Buf(1),Bufr(1)) , (Msg(2),Ngrid)
!
!*****
! IF EQDYN IS NOT IN CORE, READ IT IN AND SET FLAG.
!*****
   IF ( Ineq==0 ) THEN
      CALL gopen(Eqdyn,Z(Buf3),0)
      CALL fread(Eqdyn,Z,Neqdyn+1,1)
      CALL close(Eqdyn,1)
      Ineq = 1
   ENDIF
!*****
! PERFORM SEARCH.
!*****
   klo = 1
   khi = Kn
   Ngrid = Buf(L)
   k = (klo+khi+1)/2
   DO
      IF ( Ngrid<Z(2*k-1) ) THEN
         khi = k
      ELSEIF ( Ngrid==Z(2*k-1) ) THEN
         EXIT
      ELSE
         klo = k
      ENDIF
      IF ( khi-klo<1 ) THEN
         CALL mesage(30,Msg,Msg(2))
         Nogo = 1
         EXIT
      ELSEIF ( khi-klo==1 ) THEN
         IF ( k==klo ) THEN
            k = khi
         ELSE
            k = klo
         ENDIF
         klo = khi
      ELSE
         k = (klo+khi+1)/2
      ENDIF
   ENDDO
   Buf(L) = Z(2*k)
   IF ( Buf(L+1)/=0 ) Buf(L) = Buf(L) + Buf(L+1) - 1
END SUBROUTINE dpdaa
