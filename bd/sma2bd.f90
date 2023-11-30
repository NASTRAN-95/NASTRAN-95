
BLOCKDATA sma2bd
   IMPLICIT NONE
   REAL Bggind , Dummy(202) , Ecpt(200) , Frowic , Tnrows
   INTEGER Clsnrw , Clsrw , Eor , I6x6b , I6x6m , Icstm , Idum1 , Idum2 , Idum3 , Idum4 , Idum5 , Ifbgg , Ifcstm , Ifdit , Ifecpt , &
         & Ifgpct , Ifmgg , Ifmpt , Igbgg , Igecpt , Iggpct , Igmgg , Igpct , Inrw , Iopt4 , Ipoint , Jmax , Left , Link(10) ,      &
         & Lrowic , Mcbbgg(7) , Mcbmgg(7) , N6x6b , N6x6m , Ncstm , Neor , Ngpct , Nlinks , Nogo , Npoint , Npvt , Nrowsc , Outrw
   COMMON /sma2bk/ Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6m , N6x6m , I6x6b , N6x6b
   COMMON /sma2cl/ Iopt4 , Bggind , Npvt , Left , Frowic , Lrowic , Nrowsc , Tnrows , Jmax , Nlinks , Link , Nogo , Dummy
   COMMON /sma2et/ Ecpt
   COMMON /sma2io/ Ifcstm , Ifmpt , Ifdit , Idum1 , Ifecpt , Igecpt , Ifgpct , Iggpct , Idum2 , Idum3 , Ifmgg , Igmgg , Ifbgg ,     &
                 & Igbgg , Idum4 , Idum5 , Inrw , Outrw , Clsnrw , Clsrw , Neor , Eor , Mcbmgg , Mcbbgg
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
   DATA Nlinks/10/
   DATA Nogo/0/
   DATA Ifcstm , Ifmpt , Ifecpt , Ifgpct , Ifdit/101 , 102 , 103 , 104 , 105/
   DATA Ifmgg , Ifbgg/201 , 202/
   DATA Inrw , Clsrw , Clsnrw , Eor , Neor , Outrw/0 , 1 , 2 , 1 , 0 , 1/
END BLOCKDATA sma2bd