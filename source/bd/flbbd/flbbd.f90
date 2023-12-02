!*==flbbd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
BLOCKDATA flbbd
   USE c_flbfil
   IMPLICIT NONE
!FLBBD
!     FLBBD - BLOCK DATA FOR MODULE FLBMG
!
!
!     GINO FILES
!
!
!     INPUT DATA BLOCKS
!
   DATA geom2 , ect , bgpdt , sil , mpt , geom3 , cstm , uset , eqexin/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108 , 109/
!
!     OUTPUT DATA BLOCKS
!
   DATA usetf , usets , af , dkgg/201 , 202 , 203 , 204/
!
!     INTERNAL SCRATCH FILES
!
   DATA fbelm , frelm , conect , afmat , afdict , kgmat , kgdict/301 , 302 , 303 , 304 , 305 , 306 , 307/
!
END BLOCKDATA flbbd
