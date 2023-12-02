!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA1DP.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_SMA1DP
   USE
   REAL*8, DIMENSION(225) :: A
   REAL*8 :: Area, Determ, Px2, Pxy2, Py2, Temp, Xbar, Xbar3, Xbsq, Xcsq, Xcyc, Xsubb, Xsubc, Ybar, Ybar2, Ybar3,      &
           & Ycsq, Ysubc
   REAL, DIMENSION(4) :: Dumdum
   REAL, DIMENSION(30) :: Dummy
   REAL*8, DIMENSION(18) :: E
   INTEGER :: Ising, Nbegin, Npivot, Nsized, Nsubc, Subsca, Subscb
   REAL*8, DIMENSION(9) :: Prod9, Temp9
   REAL :: Theta
END MODULE C_SMA1DP
