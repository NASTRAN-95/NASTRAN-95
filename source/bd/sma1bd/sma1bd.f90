!*==sma1bd.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
BLOCKDATA sma1bd
   IMPLICIT NONE
   USE c_sma1bk
   USE c_sma1cl
   USE c_sma1dp
   USE c_sma1et
   USE c_sma1io
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
   DATA nlinks/10/
   DATA nogoo/0/
   DATA ifcstm , ifmpt , ifecpt , ifgpct , ifdit/101 , 102 , 103 , 104 , 105/
   DATA ifkgg , if4gg , ifgpst/201 , 202 , 203/
   DATA inrw , clsrw , clsnrw , eor , neor , outrw/0 , 1 , 2 , 1 , 0 , 1/
END BLOCKDATA sma1bd
