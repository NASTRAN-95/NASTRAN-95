!*==sdr2bd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
BLOCKDATA sdr2bd
!SDR2BD
   IMPLICIT NONE
   USE C_SDR2X1
   USE C_SDR2X2
   USE C_SDR2X4
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
   !>>>>EQUIVALENCE (Sta(1),Rfmts(1))
!
!*****
!     DATA DEFINING POSITIONS OF PARAMETERS IN A CASE CONTROL RECORD.
!*****
   DATA Ieigen/5/ , Ieldef/6/ , Itload/7/ , Isymfl/16/ , Iloads/17/ , Idispl/20/ , Istr/23/ , Ielf/26/ , Iacc/29/ , Ivel/32/ ,      &
      & Ispcf/35/ , Ittl/39/ , Ilsym/200/ , Ifrout/145/ , Isload/4/ , Idload/13/ , Isorc/136/
!*****
!     DATA DEFINING DATA BLOCK FILE NUMBERS.
!*****
   DATA Casecc/101/ , Cstm/102/ , Mpt/103/ , Dit/104/ , Eqexin/105/ , Sil/106/ , Gptt/107/ , Edt/108/ , Bgpdt/109/ , Pg/110/ ,      &
      & Qg/111/ , Ugv/112/ , Est/113/ , Phig/112/ , Eigr/110/ , Opg1/201/ , Oqg1/202/ , Ougv1/203/ , Oes1/204/ , Oef1/205/ ,        &
      & Pugv1/206/ , Oeigr/201/ , Ophig/203/ , Pphig/206/ , Esta/301/ , Gptta/302/ , Harms/137/ , Xycdb/114/ , Scr3/303/ ,          &
      & Pcomps/116/ , Oes1l/207/ , Oef1l/208/
!*****
!     DATA DEFINING RIGID FORMATS.
!*****
   DATA Nrigds/10/ , rfmts/4HSTAT , 4HICS  , 4HREIG , 4HEN   , 4HDS0  , 4H     , 4HDS1  , 4H     , 4HFREQ , 4H     , 4HTRAN ,       &
      & 4HSNT  , 4HBKL0 , 4H     , 4HBKL1 , 4H     , 4HCEIG , 4HEN   , 4HPLA  , 4H     , 20*0/
!*****
!     MISC. DATA.
!*****
   DATA Nam/4HSDR2 , 4H    / , End/4HEND / , Dtype/2 , 3 , 1 , 5 , 4 , 6 , 7 , 8/ , Mset/1001/ , Isopl8/0/
!
END BLOCKDATA sdr2bd
