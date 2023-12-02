!*==/home/marcusmae/nasa/nastran/SPAGged/C_PLA42D.f90  created by SPAG 8.01RF at 14:47 on  2 Dec 2023
MODULE C_PLA42D
   USE ISO_FORTRAN_ENV
   REAL(REAL64), DIMENSION(225) :: A
   REAL(REAL64) :: Area, Determ, Px2, Pxy2, Py2, Temp, Xbar, Xbar3, Xbsq, Xcsq, Xcyc, Xsubb, Xsubc, Ybar, Ybar2,     &
                 & Ybar3, Ycsq, Ysubc
   REAL, DIMENSION(4) :: Dumdum
   REAL, DIMENSION(30) :: Dummy
   REAL(REAL64), DIMENSION(18) :: E
   INTEGER :: Ising, Nbegin, Npivot, Nsized, Nsubc, Subsca, Subscb
   REAL(REAL64), DIMENSION(9) :: Prod9, Temp9
   REAL :: Theta
END MODULE C_PLA42D
