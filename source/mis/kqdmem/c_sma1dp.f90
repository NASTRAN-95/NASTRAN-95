!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA1DP.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_SMA1DP
   USE
   REAL, DIMENSION(16) :: Coord
   REAL :: Cosang, Sinang, U1, U2, Vecl
   REAL, DIMENSION(156) :: Dum7
   REAL, DIMENSION(248) :: Dumm8
   REAL, DIMENSION(3) :: Ivec, Jvec, Kvec, Pvec, Si, V, Vsubk
   REAL*8, DIMENSION(36) :: Kij, Ksum
   INTEGER :: Mi, Mpoint, Npivot, Nsubsc
   INTEGER, DIMENSION(4) :: Ngrid
   REAL*8 :: Temp
END MODULE C_SMA1DP
