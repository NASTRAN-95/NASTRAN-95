!*==/home/marcusmae/nasa/nastran/SPAGged/C_VDRCOM.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_VDRCOM
   INTEGER, DIMENSION(50) :: Buf
   REAL :: Buf1, Buf2, Buf3, Eqdyn, Oeigs, Pp, Scr1, Scr2, Usetd, Vdrcom, Xset0, Xycdb
   INTEGER :: Casecc, Iaacc, Iacc, Iadisp, Iavel, Idisp, Idload, Ielf, Ifrout, Iloads, Ilsym, Infile, Ipnl, Ispcf,    &
            & Istr, Ittl, Ivel, Opnl1, Outfle, Pnl, Vdrreq
   REAL, DIMENSION(2) :: Cei, Direct, Frq
   INTEGER, DIMENSION(6) :: Masks
   INTEGER, DIMENSION(2) :: Modal, Nam, Trn
END MODULE C_VDRCOM
