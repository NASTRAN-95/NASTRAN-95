!*==/home/marcusmae/nasa/nastran/SPAGged/C_PLA42D.f90  created by SPAG 8.01RF at 14:47 on  2 Dec 2023
MODULE C_PLA42D
   USE ISO_FORTRAN_ENV
   REAL(REAL64), DIMENSION(3) :: A1, D1, D2, Dpdum2, Ivect, Jvect, Kvect
   REAL :: Cosang, Sinang, Theta
   REAL(REAL64), DIMENSION(54) :: Dpdum1
   REAL(REAL64), DIMENSION(18) :: E, Temp18, Tite, Tjte
   REAL(REAL64) :: H, Temp, U1, U2, Xsubb, Xsubc, Ysubc
   INTEGER :: Ising, Jnot, Km, Nbegin, Npivot, Npoint, Nsubc, Subsca, Subscb, Subscc
   REAL(REAL64), DIMENSION(36) :: Kout, Ksum
   REAL(REAL64), DIMENSION(9) :: Prod9, T, Temp9
   REAL(REAL64), DIMENSION(2, 4) :: R
   REAL, DIMENSION(28) :: Sp1
   REAL, DIMENSION(2) :: Sp2
   REAL(REAL64), DIMENSION(2) :: V, Vv
END MODULE C_PLA42D
