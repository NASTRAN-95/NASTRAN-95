!*==/home/marcusmae/nasa/nastran/SPAGged/C_SDR2X4.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_SDR2X4
   INTEGER :: Acc, All, Any, Axic, Branch, Buf1, Buf2, Buf3, Buf4, Buf5, Ddrmm, Deform, Displ, Eldef, End, File,    &
            & Force, Icstm, Isopl, Isopl8, Ivec, Ivecn, Knset, Ktype, Kwdcc, Kwdedt, Kwdest, Kwdgpt, Loads, Mset,     &
            & Ncstm, Nharms, Nrigds, Nrings, Spcf, Stress, Strspt, Symflg, Temp, Tloads, Vel
   INTEGER, DIMENSION(2) :: Bk0, Bk1, Cei, Ds0, Ds1, Frq, Nam, Rei, Sta, Trn
   INTEGER, DIMENSION(8) :: Dtype
   INTEGER, DIMENSION(7) :: Icb, Mcb, Ocb
   INTEGER, DIMENSION(22) :: Pla
!
   INTEGER , DIMENSION(40) , SAVE :: rfmts
!
   EQUIVALENCE (Sta(1),Rfmts(1))
!*****
!     DATA DEFINING RIGID FORMATS.
!*****
   DATA nrigds/10/ , rfmts/4HSTAT , 4HICS  , 4HREIG , 4HEN   , 4HDS0  , 4H     , 4HDS1  , 4H     , 4HFREQ , 4H     , 4HTRAN ,       &
      & 4HSNT  , 4HBKL0 , 4H     , 4HBKL1 , 4H     , 4HCEIG , 4HEN   , 4HPLA  , 4H     , 20*0/
!*****
!     MISC. DATA.
!*****
   DATA nam/4HSDR2 , 4H    / , end/4HEND / , dtype/2 , 3 , 1 , 5 , 4 , 6 , 7 , 8/ , mset/1001/ , isopl8/0/

END MODULE C_SDR2X4
