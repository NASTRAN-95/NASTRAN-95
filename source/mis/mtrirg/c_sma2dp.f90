!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA2DP.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_SMA2DP
   USE ISO_FORTRAN_ENV
   REAL(REAL64), DIMENSION(36) :: Ak, Aki, D, Gambq
   REAL(REAL64), DIMENSION(9) :: Akt
   REAL(REAL64) :: Area, Dgama, Dr, Dz, Ra, Rh, Rhod, Twopi, Za, Zh, Zmin
   REAL(REAL64), DIMENSION(8) :: Delint
   INTEGER, DIMENSION(3) :: Ics, Igp
   REAL(REAL64), DIMENSION(3) :: R, Z
   REAL, DIMENSION(18) :: Sp
   REAL :: Tempe
END MODULE C_SMA2DP
