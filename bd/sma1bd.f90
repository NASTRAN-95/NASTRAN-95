
BLOCKDATA sma1bd
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Clsnrw , Clsrw , Eor , I6x64 , I6x6k , Icstm , Idetck , Idum1 , If4gg , Ifcstm , Ifdit , Ifecpt , Ifgei , Ifgpct ,       &
         & Ifgpst , Ifkgg , Ifmpt , Ig4gg , Igecpt , Iggei , Iggpct , Iggpst , Igkgg , Igpct , Inrw , Iopt4 , Ipoint , Jmax ,       &
         & K4ggsw , Left , Link(10) , Lrowic , Mcb4gg(7) , Mcbkgg(7) , N6x64 , N6x6k , Ncstm , Neor , Ngpct , Nlinks , Nogoo ,      &
         & Npoint , Npvt , Nrowsc , Outrw
   REAL Dodet , Dummy(200) , Ecpt(200) , Frowic , Tnrows
   DOUBLE PRECISION Dpdum(514)
   COMMON /sma1bk/ Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6k , N6x6k , I6x64 , N6x64
   COMMON /sma1cl/ Iopt4 , K4ggsw , Npvt , Left , Frowic , Lrowic , Nrowsc , Tnrows , Jmax , Nlinks , Link , Idetck , Dodet ,       &
                 & Nogoo , Dummy
   COMMON /sma1dp/ Dpdum
   COMMON /sma1et/ Ecpt
   COMMON /sma1io/ Ifcstm , Ifmpt , Ifdit , Idum1 , Ifecpt , Igecpt , Ifgpct , Iggpct , Ifgei , Iggei , Ifkgg , Igkgg , If4gg ,     &
                 & Ig4gg , Ifgpst , Iggpst , Inrw , Outrw , Clsnrw , Clsrw , Neor , Eor , Mcbkgg , Mcb4gg
!
! End of declarations
!
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
!
!
   DATA Nlinks/10/
   DATA Nogoo/0/
   DATA Ifcstm , Ifmpt , Ifecpt , Ifgpct , Ifdit/101 , 102 , 103 , 104 , 105/
   DATA Ifkgg , If4gg , Ifgpst/201 , 202 , 203/
   DATA Inrw , Clsrw , Clsnrw , Eor , Neor , Outrw/0 , 1 , 2 , 1 , 0 , 1/
END BLOCKDATA sma1bd
