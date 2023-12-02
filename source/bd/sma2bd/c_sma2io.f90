!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA2IO.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_SMA2IO
   INTEGER :: Clsnrw, Clsrw, Eor, Idum1, Idum2, Idum3, Idum4, Idum5, Ifbgg, Ifcstm, Ifdit, Ifecpt, Ifgpct, Ifmgg, Ifmpt,&
            & Igbgg, Igecpt, Iggpct, Igmgg, Inrw, Neor, Outrw
   INTEGER, DIMENSION(7) :: Mcbbgg, Mcbmgg
!SMA2BD
!
!
!     SMA2 VARIABLE CORE BOOKKEEPING PARAMETERS
!
!
!     SMA2 PROGRAM CONTROL PARAMETERS
!
!
!     ECPT COMMON BLOCK
!
!
   DATA ifcstm , ifmpt , ifecpt , ifgpct , ifdit/101 , 102 , 103 , 104 , 105/
   DATA ifmgg , ifbgg/201 , 202/
   DATA inrw , clsrw , clsnrw , eor , neor , outrw/0 , 1 , 2 , 1 , 0 , 1/

END MODULE C_SMA2IO
