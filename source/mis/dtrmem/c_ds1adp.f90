!*==/home/marcusmae/nasa/nastran/SPAGged/C_DS1ADP.f90  created by SPAG 8.01RF at 14:47 on  2 Dec 2023
MODULE C_DS1ADP
   USE ISO_FORTRAN_ENV
   REAL(REAL64) :: Areat, Delta, Gamma1, Gamma2, Gamma3, Lamda, Mu, Sigx, Sigxy, Sigy, Temp, Xsubb, Xsubc, Ysubc
   REAL(REAL64), DIMENSION(54) :: C
   REAL(REAL64), DIMENSION(9) :: Disp, E, G, T
   REAL(REAL64), DIMENSION(12) :: Dumdp
   INTEGER :: Icstm1, Idum, Npivot
   REAL(REAL64), DIMENSION(36) :: Kd
   REAL(REAL64), DIMENSION(18) :: Temp1, Temp2
   REAL :: Theta
END MODULE C_DS1ADP
