!*==/home/marcusmae/nasa/nastran/SPAGged/C_TRIMEX.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_TRIMEX
   REAL, DIMENSION(3) :: Alphas, H, Pt, Rtside
   REAL, DIMENSION(12) :: Bt
   REAL :: Determ, Dumarg, T, Tempav, Th, Ttemp, X1, X2, X3, X4, X5, X6, X7, X8, Y1, Y2, Y3, Y4, Y5, Y6, Y7,   &
         & Y8, Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8
   REAL, DIMENSION(16) :: Dnc, Dnl, Save, Xx
   REAL, DIMENSION(9) :: G
   INTEGER :: Id1, Isys1, Isys2, Isys3, Isys4, Isys5, Isys6, Isys7, Isys8, Matid1
   INTEGER, DIMENSION(1) :: Necpt
   INTEGER, DIMENSION(8) :: Ngrid
   REAL, DIMENSION(4) :: Xjb
   REAL, DIMENSION(2, 2) :: Xxjb
END MODULE C_TRIMEX
