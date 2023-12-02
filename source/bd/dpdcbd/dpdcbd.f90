!*==dpdcbd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
BLOCKDATA dpdcbd
   USE c_dpdcom
   IMPLICIT NONE
!DPDCBD
! BLOCK DATA PROGRAM FOR THE DYNAMICS POOL DISTRIBUTOR
!*****
!
!
!
!*****
! INPUT FILES
!*****
   DATA dpool/101/ , gpl/102/ , sil/103/ , uset/104/
!*****
! OUTPUT FILES
!*****
   DATA gpld/201/ , sild/202/ , usetd/203/ , tfl/204/ , dlt/205/ , psdl/206/ , frl/207/ , nlft/208/ , trl/209/ , eed/210/ ,         &
      & eqdyn/211/ , sdt/212/
!*****
! SCRATCH FILES
!*****
   DATA scr1/301/ , scr2/302/ , scr3/303/ , scr4/304/
!*****
! DATA DEFINING INPUT CARDS
!*****
   DATA epoint/707 , 7/ , seqep/5707 , 57/ , loads/27 , 17 , 0 , 0 , 37 , 18 , 0 , 0 , 77 , 19 , 0 , 0 , 5107 , 51 , 6 , 0 , 5207 , &
      & 52 , 6 , 0 , 7107 , 71 , 5 , 0 , 7207 , 72 , 10 , 0 , 0 , 0 , 0 , 0/ , dload/57 , 5/ , freq1/1007 , 10/ , freq/1307 , 13/
   DATA nolin/3107 , 31 , 8 , 3207 , 32 , 8 , 3307 , 33 , 8 , 3407 , 34 , 8 , 3507 , 35 , 16 , 3607 , 36 , 5 , 3707 , 37 , 8/
   DATA tic/6607 , 66/ , tstep/8307 , 83/ , tf/6207 , 62/ , eigr/307 , 3/ , eigb/107 , 1/ , eigc/207 , 2/
!*****
! MISC DATA
!*****
   DATA mcb/7*0/ , nam/4HDPD  , 4H    /
END BLOCKDATA dpdcbd
