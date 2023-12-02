!*==flbbd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
BLOCKDATA flbbd
   IMPLICIT NONE
   USE C_FLBFIL
!FLBBD
!     FLBBD - BLOCK DATA FOR MODULE FLBMG
!
!
!     GINO FILES
!
!
!     INPUT DATA BLOCKS
!
   DATA Geom2 , Ect , Bgpdt , Sil , Mpt , Geom3 , Cstm , Uset , Eqexin/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108 , 109/
!
!     OUTPUT DATA BLOCKS
!
   DATA Usetf , Usets , Af , Dkgg/201 , 202 , 203 , 204/
!
!     INTERNAL SCRATCH FILES
!
   DATA Fbelm , Frelm , Conect , Afmat , Afdict , Kgmat , Kgdict/301 , 302 , 303 , 304 , 305 , 306 , 307/
!
END BLOCKDATA flbbd
