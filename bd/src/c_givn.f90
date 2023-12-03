!*==/home/marcusmae/nasa/nastran/SPAGged/C_GIVN.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_GIVN
   REAL :: G1
   REAL, DIMENSION(8) :: G2
   REAL, DIMENSION(2) :: G3, G5, G7
   REAL, DIMENSION(82) :: G4
   REAL, DIMENSION(4) :: G6
   INTEGER :: Lgama, M1, M2, M3, M4, Max, Md, Mo, Mr1, Ncol, Oeigs, Order, Phia, Rstrt
   REAL, DIMENSION(35) :: X

   DATA order , rstrt , ncol , max/ - 2 , 0 , 0 , 253/
   DATA mo , md , mr1 , m1 , m2 , m3 , m4 , lgama , oeigs , phia/301 , 304 , 202 , 303 , 307 , 308 , 309 , 201 , 204 , 305/

END MODULE C_GIVN
