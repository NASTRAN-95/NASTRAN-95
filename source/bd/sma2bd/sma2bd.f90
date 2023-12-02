!*==sma2bd.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
BLOCKDATA sma2bd
   IMPLICIT NONE
   USE C_SMA2BK
   USE C_SMA2CL
   USE C_SMA2ET
   USE C_SMA2IO
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
