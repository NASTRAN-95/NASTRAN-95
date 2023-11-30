
BLOCKDATA flbbd
   IMPLICIT NONE
   INTEGER Af , Afdict , Afmat , Bgpdt , Conect , Cstm , Dkgg , Ect , Eqexin , Fbelm , Frelm , Geom2 , Geom3 , Kgdict , Kgmat ,     &
         & Mpt , Sil , Uset , Usetf , Usets
   COMMON /flbfil/ Geom2 , Ect , Bgpdt , Sil , Mpt , Geom3 , Cstm , Uset , Eqexin , Usetf , Usets , Af , Dkgg , Fbelm , Frelm ,     &
                 & Conect , Afmat , Afdict , Kgmat , Kgdict
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