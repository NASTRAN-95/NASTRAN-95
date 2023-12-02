!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA2DP.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_SMA2DP
   USE ISO_FORTRAN_ENV
   REAL(REAL64) :: Emass, Ptmass
   INTEGER, DIMENSION(4) :: Iloc
   INTEGER :: Itest, J1, Jloc, Kpt, M1, Mfirst, Nel, Npts, Nrow
   REAL(REAL64), DIMENSION(36) :: Mge
   REAL(REAL64), DIMENSION(3, 3) :: R
END MODULE C_SMA2DP
