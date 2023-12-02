!*==/home/marcusmae/nasa/nastran/SPAGged/C_DS1ADP.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_DS1ADP
   USE ISO_FORTRAN_ENV
   REAL(REAL64), DIMENSION(3) :: A1, D1, D2, Ivect, Jvect, Kvect
   REAL :: Cosang, Sinang, Theta
   REAL(REAL64), DIMENSION(1) :: Dpdum
   REAL(REAL64), DIMENSION(43) :: Dpdum2
   REAL(REAL64), DIMENSION(18) :: E, Temp18, Tite, Tjte
   REAL(REAL64) :: H, Sigx, Sigxy, Sigy, Temp, U1, U2, Xsubb, Xsubc, Ysubc
   INTEGER :: Ipvt, Ising, Jnot, Km, Nbegin, Npivot, Npoint, Nsubc, Subsca, Subscb, Subscc
   REAL(REAL64), DIMENSION(36) :: Kout, Ksum
   REAL(REAL64), DIMENSION(9) :: Prod9, T, Temp9
   REAL(REAL64), DIMENSION(2, 4) :: R
   REAL, DIMENSION(2) :: Sp1
   REAL(REAL64), DIMENSION(2) :: V, Vv
END MODULE C_DS1ADP
