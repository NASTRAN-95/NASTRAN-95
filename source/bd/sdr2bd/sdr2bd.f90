!*==sdr2bd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
BLOCKDATA sdr2bd
!SDR2BD
   USE c_sdr2x1
   USE c_sdr2x2
   USE c_sdr2x4
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(40) , SAVE :: rfmts
!
! End of declarations rewritten by SPAG
!
!
!
!
!
!
   EQUIVALENCE (Sta(1),Rfmts(1))
!
!*****
!     DATA DEFINING POSITIONS OF PARAMETERS IN A CASE CONTROL RECORD.
!*****
   DATA ieigen/5/ , ieldef/6/ , itload/7/ , isymfl/16/ , iloads/17/ , idispl/20/ , istr/23/ , ielf/26/ , iacc/29/ , ivel/32/ ,      &
      & ispcf/35/ , ittl/39/ , ilsym/200/ , ifrout/145/ , isload/4/ , idload/13/ , isorc/136/
!*****
!     DATA DEFINING DATA BLOCK FILE NUMBERS.
!*****
   DATA casecc/101/ , cstm/102/ , mpt/103/ , dit/104/ , eqexin/105/ , sil/106/ , gptt/107/ , edt/108/ , bgpdt/109/ , pg/110/ ,      &
      & qg/111/ , ugv/112/ , est/113/ , phig/112/ , eigr/110/ , opg1/201/ , oqg1/202/ , ougv1/203/ , oes1/204/ , oef1/205/ ,        &
      & pugv1/206/ , oeigr/201/ , ophig/203/ , pphig/206/ , esta/301/ , gptta/302/ , harms/137/ , xycdb/114/ , scr3/303/ ,          &
      & pcomps/116/ , oes1l/207/ , oef1l/208/
!*****
!     DATA DEFINING RIGID FORMATS.
!*****
   DATA nrigds/10/ , rfmts/4HSTAT , 4HICS  , 4HREIG , 4HEN   , 4HDS0  , 4H     , 4HDS1  , 4H     , 4HFREQ , 4H     , 4HTRAN ,       &
      & 4HSNT  , 4HBKL0 , 4H     , 4HBKL1 , 4H     , 4HCEIG , 4HEN   , 4HPLA  , 4H     , 20*0/
!*****
!     MISC. DATA.
!*****
   DATA nam/4HSDR2 , 4H    / , end/4HEND / , dtype/2 , 3 , 1 , 5 , 4 , 6 , 7 , 8/ , mset/1001/ , isopl8/0/
!
END BLOCKDATA sdr2bd
