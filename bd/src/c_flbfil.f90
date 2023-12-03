!*==/home/marcusmae/nasa/nastran/SPAGged/C_FLBFIL.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_FLBFIL
   INTEGER :: Af, Afdict, Afmat, Bgpdt, Conect, Cstm, Dkgg, Ect, Eqexin, Fbelm, Frelm, Geom2, Geom3, Kgdict, Kgmat,  &
            & Mpt, Sil, Uset, Usetf, Usets
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
END MODULE c_flbfil
