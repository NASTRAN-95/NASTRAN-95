!*==/home/marcusmae/nasa/nastran/SPAGged/C_VDRCOM.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_VDRCOM
   INTEGER, DIMENSION(50) :: Buf
   INTEGER :: Buf1, Buf2, Buf3, Casecc, Iaacc, Iacc, Iadisp, Iavel, Idisp, Idload, Ielf, Ifrout, Iloads, Ilsym,       &
            & Infile, Ipnl, Ispcf, Istr, Ittl, Ivel, Scr1, Scr3, Vdrreq, Xset0, Xycdb
   REAL, DIMENSION(2) :: Cei, Direct
   REAL :: Eqdyn, Oeigs, Opnl1, Outfle, Pnl, Pp, Usetd
   INTEGER, DIMENSION(2) :: Frq, Modal, Nam, Trn
   INTEGER, DIMENSION(6) :: Masks
   INTEGER, DIMENSION(1) :: Vdrcom
END MODULE C_VDRCOM
