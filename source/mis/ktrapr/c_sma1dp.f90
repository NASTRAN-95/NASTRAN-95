!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA1DP.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_SMA1DP
   USE ISO_FORTRAN_ENV
   REAL(REAL64), DIMENSION(64) :: Ak, D, Gambq
   REAL(REAL64) :: Cosg, Del, Dgama, Dgamr, Er, Et, Ez, Grz, Sing, Vrt, Vrz, Vtr, Vtz, Vzr, Vzt, Zmin
   REAL(REAL64), DIMENSION(12) :: Delint
   REAL(REAL64), DIMENSION(16) :: Ee, Teo
   INTEGER, DIMENSION(4) :: Ics, Igp
   REAL(REAL64), DIMENSION(4) :: R, Z
   REAL, DIMENSION(24) :: Sp
   REAL :: Tempe
END MODULE C_SMA1DP
