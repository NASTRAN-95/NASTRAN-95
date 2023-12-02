!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA2DP.f90  created by SPAG 8.01RF at 14:47 on  2 Dec 2023
MODULE C_SMA2DP
   USE ISO_FORTRAN_ENV
   REAL(REAL64), DIMENSION(225) :: A
   REAL(REAL64) :: Aij, Bij, Determ, Fi, Fij, Fj, Fj2, Sizero, Temp, Xbsq, Xcsq, Xcyc, Xprodi, Xsubb, Xsubc, Ycsq,  &
                 & Yprodj, Ysubc
   REAL, DIMENSION(59) :: Dummy
   REAL(REAL64), DIMENSION(9) :: E, Prod9, Temp9
   INTEGER :: Ising
END MODULE C_SMA2DP
