
BLOCKDATA sdr2bd
!SDR2BD
   IMPLICIT NONE
   INTEGER Acc , All , Any , Axic , Bgpdt , Bk0(2) , Bk1(2) , Branch , Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Casecc , Cei(2) , Cstm ,  &
         & Ddrmm , Deform , Displ , Dit , Ds0(2) , Ds1(2) , Dtype(8) , Edt , Eigr , Eldef , End , Eqexin , Est , Esta , File ,      &
         & Force , Frq(2) , Gptt , Gptta , Harms , Iacc , Icb(7) , Icstm , Idispl , Idload , Ieigen , Ieldef , Ielf , Ifrout ,      &
         & Iloads , Ilsym , Isload , Isopl , Isopl8 , Isorc , Ispcf , Istr , Isymfl , Itload , Ittl , Ivec , Ivecn , Ivel , Knset , &
         & Ktype , Kwdcc , Kwdedt , Kwdest , Kwdgpt , Loads , Mcb(7) , Mpt , Mset , Nam(2) , Ncstm , Nharms , Nrigds , Nrings ,     &
         & Ocb(7) , Oef1 , Oef1l , Oeigr , Oes1 , Oes1l
   INTEGER Opg1 , Ophig , Oqg1 , Ougv1 , Pcomps , Pg , Phig , Pla(22) , Pphig , Pugv1 , Qg , Rei(2) , Rfmts(40) , Scr3 , Sil ,      &
         & Spcf , Sta(2) , Stress , Strspt , Symflg , Temp , Tloads , Trn(2) , Ugv , Vel , Xycdb
   COMMON /sdr2x1/ Ieigen , Ieldef , Itload , Isymfl , Iloads , Idispl , Istr , Ielf , Iacc , Ivel , Ispcf , Ittl , Ilsym , Ifrout ,&
                 & Isload , Idload , Isorc
   COMMON /sdr2x2/ Casecc , Cstm , Mpt , Dit , Eqexin , Sil , Gptt , Edt , Bgpdt , Pg , Qg , Ugv , Est , Phig , Eigr , Opg1 , Oqg1 ,&
                 & Ougv1 , Oes1 , Oef1 , Pugv1 , Oeigr , Ophig , Pphig , Esta , Gptta , Harms , Xycdb , Scr3 , Pcomps , Oes1l ,     &
                 & Oef1l
   COMMON /sdr2x4/ Nam , End , Mset , Icb , Ocb , Mcb , Dtype , Icstm , Ncstm , Ivec , Ivecn , Temp , Deform , File , Buf1 , Buf2 , &
                 & Buf3 , Buf4 , Buf5 , Any , All , Tloads , Eldef , Symflg , Branch , Ktype , Loads , Spcf , Displ , Vel , Acc ,   &
                 & Stress , Force , Kwdest , Kwdedt , Kwdgpt , Kwdcc , Nrigds , Sta , Rei , Ds0 , Ds1 , Frq , Trn , Bk0 , Bk1 ,     &
                 & Cei , Pla , Nrings , Nharms , Axic , Knset , Isopl , Strspt , Ddrmm , Isopl8
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
   DATA Nrigds/10/ , Rfmts/4HSTAT , 4HICS  , 4HREIG , 4HEN   , 4HDS0  , 4H     , 4HDS1  , 4H     , 4HFREQ , 4H     , 4HTRAN ,       &
      & 4HSNT  , 4HBKL0 , 4H     , 4HBKL1 , 4H     , 4HCEIG , 4HEN   , 4HPLA  , 4H     , 20*0/
!*****
!     MISC. DATA.
!*****
   DATA Nam/4HSDR2 , 4H    / , End/4HEND / , Dtype/2 , 3 , 1 , 5 , 4 , 6 , 7 , 8/ , Mset/1001/ , Isopl8/0/
!
END BLOCKDATA sdr2bd
