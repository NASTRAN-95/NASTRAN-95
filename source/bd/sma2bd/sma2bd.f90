!*==sma2bd.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
BLOCKDATA sma2bd
   IMPLICIT NONE
   USE c_sma2bk
   USE c_sma2cl
   USE c_sma2et
   USE c_sma2io
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
   DATA nlinks/10/
   DATA nogo/0/
   DATA ifcstm , ifmpt , ifecpt , ifgpct , ifdit/101 , 102 , 103 , 104 , 105/
   DATA ifmgg , ifbgg/201 , 202/
   DATA inrw , clsrw , clsnrw , eor , neor , outrw/0 , 1 , 2 , 1 , 0 , 1/
END BLOCKDATA sma2bd
