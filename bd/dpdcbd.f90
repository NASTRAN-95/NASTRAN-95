
BLOCKDATA dpdcbd
   IMPLICIT NONE
   INTEGER Buf(24) , Buf1 , Buf2 , Buf3 , Buf4 , Dload(2) , Dlt , Dpool , Eed , Eigb(2) , Eigc(2) , Eigr(2) , Epoint(2) , Eqdyn ,   &
         & Freq(2) , Freq1(2) , Frl , Gpl , Gpld , Ineq , Kn , L , Loads(32) , Mcb(7) , Msg(3) , Nam(2) , Neqdyn , Nlft , Nogo ,    &
         & Nolin(21) , Psd(2) , Psdl , Scr1 , Scr2 , Scr3 , Scr4 , Sdt , Seqep(2) , Sil , Sild , Tf(2) , Tfl , Tic(2) , Trl ,       &
         & Tstep(2) , Uset , Usetd
   COMMON /dpdcom/ Dpool , Gpl , Sil , Uset , Gpld , Sild , Usetd , Dlt , Frl , Nlft , Tfl , Trl , Psdl , Eed , Scr1 , Scr2 , Scr3 ,&
                 & Scr4 , Buf , Buf1 , Buf2 , Buf3 , Buf4 , Epoint , Seqep , L , Kn , Neqdyn , Loads , Dload , Freq1 , Freq ,       &
                 & Nolin , Nogo , Msg , Tic , Tstep , Tf , Psd , Eigr , Eigb , Eigc , Mcb , Nam , Eqdyn , Sdt , Ineq
!DPDCBD
! BLOCK DATA PROGRAM FOR THE DYNAMICS POOL DISTRIBUTOR
!*****
!
!
!
!*****
! INPUT FILES
!*****
   DATA Dpool/101/ , Gpl/102/ , Sil/103/ , Uset/104/
!*****
! OUTPUT FILES
!*****
   DATA Gpld/201/ , Sild/202/ , Usetd/203/ , Tfl/204/ , Dlt/205/ , Psdl/206/ , Frl/207/ , Nlft/208/ , Trl/209/ , Eed/210/ ,         &
      & Eqdyn/211/ , Sdt/212/
!*****
! SCRATCH FILES
!*****
   DATA Scr1/301/ , Scr2/302/ , Scr3/303/ , Scr4/304/
!*****
! DATA DEFINING INPUT CARDS
!*****
   DATA Epoint/707 , 7/ , Seqep/5707 , 57/ , Loads/27 , 17 , 0 , 0 , 37 , 18 , 0 , 0 , 77 , 19 , 0 , 0 , 5107 , 51 , 6 , 0 , 5207 , &
      & 52 , 6 , 0 , 7107 , 71 , 5 , 0 , 7207 , 72 , 10 , 0 , 0 , 0 , 0 , 0/ , Dload/57 , 5/ , Freq1/1007 , 10/ , Freq/1307 , 13/
   DATA Nolin/3107 , 31 , 8 , 3207 , 32 , 8 , 3307 , 33 , 8 , 3407 , 34 , 8 , 3507 , 35 , 16 , 3607 , 36 , 5 , 3707 , 37 , 8/
   DATA Tic/6607 , 66/ , Tstep/8307 , 83/ , Tf/6207 , 62/ , Eigr/307 , 3/ , Eigb/107 , 1/ , Eigc/207 , 2/
!*****
! MISC DATA
!*****
   DATA Mcb/7*0/ , Nam/4HDPD  , 4H    /
END BLOCKDATA dpdcbd
