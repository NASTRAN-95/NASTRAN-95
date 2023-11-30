
BLOCKDATA vdrbd
   IMPLICIT NONE
   INTEGER Buf(50) , Buf1 , Buf2 , Buf3 , Casecc , Cei(2) , Direct(2) , Eqdyn , Frq(2) , Iaacc , Iacc , Iadisp , Iavel , Idisp ,    &
         & Idload , Ielf , Ifrout , Iloads , Ilsym , Infile , Ipnl , Ispcf , Istr , Ittl , Ivel , Masks(6) , Modal(2) , Nam(2) ,    &
         & Oeigs , Opnl1 , Outfle , Pnl , Pp , Scr1 , Scr2 , Trn(2) , Usetd , Xset0 , Xycdb
   REAL Vdrcom , Vdrreq
   COMMON /vdrcom/ Vdrcom , Idisp , Ivel , Iacc , Ispcf , Iloads , Istr , Ielf , Iadisp , Iavel , Iaacc , Ipnl , Ittl , Ilsym ,     &
                 & Ifrout , Idload , Casecc , Eqdyn , Usetd , Infile , Oeigs , Pp , Xycdb , Pnl , Outfle , Opnl1 , Scr1 , Scr2 ,    &
                 & Buf1 , Buf2 , Buf3 , Nam , Buf , Masks , Cei , Frq , Trn , Direct , Xset0 , Vdrreq , Modal
!VDRBD
! BLOCK DATA FOR THE VECTOR DATA RECOVERY MODULE (VDR).
!*****
!
!
!
! DATA DEFINING POSITION OF PARAMETERS IN CASE CONTROL RECORD.
!
   DATA Idisp/20/ , Ivel/32/ , Iacc/29/ , Ispcf/35/ , Iloads/17/ , Istr/23/ , Ielf/26/ , Iadisp/151/ , Iavel/154/ , Iaacc/157/ ,    &
      & Ipnl/10/ , Ittl/39/ , Ilsym/200/ , Ifrout/145/ , Idload/13/
!
! DATA DEFINING GINO FILE NAMES
!
   DATA Casecc/101/ , Eqdyn/102/ , Usetd/103/ , Infile/104/ , Oeigs/105/ , Pp/105/ , Xycdb/106/ , Pnl/107/ , Outfle/201/ ,          &
      & Opnl1/202/ , Scr1/301/ , Scr2/302/
!
! MISC DATA
!
   DATA Buf/50*0/ , Nam/4HVDR  , 4H    / , Masks/4 , 8 , 16 , 32 , 64 , 128/ , Xset0/100000000/
!
! DATA DEFINING RIGID FORMATS AND PROBLEM TYPES
!
   DATA Cei/4HCEIG , 4HEN  / , Frq/4HFREQ , 4HRESP/ , Trn/4HTRAN , 4HRESP/ , Modal/4HMODA , 4HL   / , Direct/4HDIRE , 4HCT  /
END BLOCKDATA vdrbd