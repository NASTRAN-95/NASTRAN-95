!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA2DP.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_SMA2DP
   USE ISO_FORTRAN_ENV
   REAL(REAL64), DIMENSION(3) :: A1, D1, D2, Ivect, Jvect, Kvect
   REAL :: Cosang, Sinang, Theta
   REAL(REAL64), DIMENSION(27) :: Dpdum1
   REAL(REAL64), DIMENSION(10) :: Dpdum2
   REAL(REAL64), DIMENSION(9) :: E, Prod9, T, Temp9, Tite
   REAL(REAL64) :: H, Iiz, Miz, Ptmass, Sign, Temp, U1, U2, Xsubb, Xsubc, Ysubc
   INTEGER :: Ising, Jnot, Km, Nbegin, Npivot, Npoint, Nsubc, Subsca, Subscb, Subscc
   REAL(REAL64), DIMENSION(36) :: M6x6, Mout, Temp36, Tjte
   REAL(REAL64), DIMENSION(2, 4) :: R
   REAL, DIMENSION(33) :: Sp1
   REAL, DIMENSION(20) :: Spdum
   REAL(REAL64), DIMENSION(2) :: V, Vv
END MODULE C_SMA2DP
