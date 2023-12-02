!*==/home/marcusmae/nasa/nastran/SPAGged/C_VDRCOM.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_VDRCOM
   INTEGER, DIMENSION(50) :: Buf
   INTEGER :: Buf1, Buf2, Buf3, Casecc, Eqdyn, Iaacc, Iacc, Iadisp, Iavel, Idisp, Idload, Ielf, Ifrout, Iloads,       &
            & Ilsym, Infile, Ipnl, Ispcf, Istr, Ittl, Ivel, Oeigs, Opnl1, Outfle, Pnl, Pp, Scr1, Scr2, Usetd, Xset0,&
            & Xycdb
   INTEGER, DIMENSION(2) :: Cei, Direct, Frq, Modal, Nam, Trn
   INTEGER, DIMENSION(6) :: Masks
   REAL :: Vdrcom, Vdrreq
!VDRBD
! BLOCK DATA FOR THE VECTOR DATA RECOVERY MODULE (VDR).
!*****
!
!
!
! DATA DEFINING POSITION OF PARAMETERS IN CASE CONTROL RECORD.
!
   DATA idisp/20/ , ivel/32/ , iacc/29/ , ispcf/35/ , iloads/17/ , istr/23/ , ielf/26/ , iadisp/151/ , iavel/154/ , iaacc/157/ ,    &
      & ipnl/10/ , ittl/39/ , ilsym/200/ , ifrout/145/ , idload/13/
!
! DATA DEFINING GINO FILE NAMES
!
   DATA casecc/101/ , eqdyn/102/ , usetd/103/ , infile/104/ , oeigs/105/ , pp/105/ , xycdb/106/ , pnl/107/ , outfle/201/ ,          &
      & opnl1/202/ , scr1/301/ , scr2/302/
!
! MISC DATA
!
   DATA buf/50*0/ , nam/4HVDR  , 4H    / , masks/4 , 8 , 16 , 32 , 64 , 128/ , xset0/100000000/
!
! DATA DEFINING RIGID FORMATS AND PROBLEM TYPES
!
   DATA cei/4HCEIG , 4HEN  / , frq/4HFREQ , 4HRESP/ , trn/4HTRAN , 4HRESP/ , modal/4HMODA , 4HL   / , direct/4HDIRE , 4HCT  /
END MODULE c_vdrcom
