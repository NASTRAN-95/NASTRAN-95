!*==vdrbd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
BLOCKDATA vdrbd
   IMPLICIT NONE
   USE C_VDRCOM
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
