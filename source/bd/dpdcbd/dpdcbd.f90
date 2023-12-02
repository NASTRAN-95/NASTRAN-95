!*==dpdcbd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
BLOCKDATA dpdcbd
   IMPLICIT NONE
   USE C_DPDCOM
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
