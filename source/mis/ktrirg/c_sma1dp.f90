!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA1DP.f90  created by SPAG 8.01RF at 14:47 on  2 Dec 2023
MODULE C_SMA1DP
   USE ISO_FORTRAN_ENV
   REAL(REAL64), DIMENSION(36) :: Ak, D, Gambq
   REAL(REAL64) :: Area, Cosg, Del, Dgama, Dgamr, Dr, Dz, Er, Et, Ez, Grz, Ra, Rh, Sing, Vrt, Vrz, Vtr, Vtz, Vzr,&
                 & Vzt, Za, Zh, Zmin
   REAL(REAL64), DIMENSION(8) :: Delint
   REAL(REAL64), DIMENSION(16) :: Ee, Teo
   INTEGER, DIMENSION(3) :: Ics, Igp
   REAL(REAL64), DIMENSION(3) :: R, Z
   REAL, DIMENSION(18) :: Sp
   REAL :: Tempe
END MODULE C_SMA1DP
