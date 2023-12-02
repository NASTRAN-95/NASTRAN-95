!*==/home/marcusmae/nasa/nastran/SPAGged/C_DS1ADP.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_DS1ADP
   USE ISO_FORTRAN_ENV
   REAL(REAL64), DIMENSION(12) :: B, Bt
   REAL(REAL64) :: Determ, Dumarg, Gsube
   REAL(REAL64), DIMENSION(16) :: Dnc, Dnl, Xx
   REAL(REAL64), DIMENSION(9) :: G, Tb
   REAL(REAL64), DIMENSION(3) :: H, Pt
   REAL(REAL64), DIMENSION(36) :: Kij
   REAL(REAL64), DIMENSION(72) :: Tsave
   REAL(REAL64), DIMENSION(4) :: Xjb
   REAL(REAL64), DIMENSION(2, 2) :: Xxjb
END MODULE C_DS1ADP
