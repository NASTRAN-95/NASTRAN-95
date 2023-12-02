!*==sma1bd.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
BLOCKDATA sma1bd
   IMPLICIT NONE
   USE C_SMA1BK
   USE C_SMA1CL
   USE C_SMA1DP
   USE C_SMA1ET
   USE C_SMA1IO
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
