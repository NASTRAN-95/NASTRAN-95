!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA1IO.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_SMA1IO
!SMA1BD
!
!
!     SMA1 VARIABLE CORE BOOKKEEPING PARAMETERS
!
!
!     SMA1 PROGRAM CONTROL PARAMETERS
!
!
!     ECPT COMMON BLOCK
!
   INTEGER :: Clsnrw, Clsrw, Eor, Idum1, If4gg, Ifcstm, Ifdit, Ifecpt, Ifgei, Ifgpct, Ifgpst, Ifkgg, Ifmpt, Ig4gg,       &
            & Igecpt, Iggei, Iggpct, Iggpst, Igkgg, Inrw, Neor, Outrw
   INTEGER, DIMENSION(7) :: Mcb4gg, Mcbkgg
!
   DATA ifcstm , ifmpt , ifecpt , ifgpct , ifdit/101 , 102 , 103 , 104 , 105/
   DATA ifkgg , if4gg , ifgpst/201 , 202 , 203/
   DATA inrw , clsrw , clsnrw , eor , neor , outrw/0 , 1 , 2 , 1 , 0 , 1/

END MODULE C_SMA1IO
