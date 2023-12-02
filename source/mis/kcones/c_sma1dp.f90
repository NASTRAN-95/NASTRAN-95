!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA1DP.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_SMA1DP
   USE ISO_FORTRAN_ENV
   REAL :: A, B, Cp, Cp2, Cp2e22, Cpe12, Cpe22, D11, D12, D22, D33, Dl, E11, E12, E22, E33, L2, N, N2, N2e22,   &
         & N2e33, Ncp, Nsp, Opi, Oq, Piovb, Ra, Rasq, Rb, Rbsq, Sign, Sl, Sp, Sp2, Sp2e22, Sp2e33, Spe12, Spe22,  &
         & Spe33, T, Td, Temp, Temp1, Temp2, Temp3, Temp4, Temp5, Temp6, Temp7, Tn, Tnsp, Ts, Twod33, Za, Zb
   REAL, DIMENSION(100) :: Huq
   REAL, DIMENSION(10) :: Hyq, Hyqf
   REAL, DIMENSION(28) :: Integ
   REAL, DIMENSION(36) :: Kij
   REAL(REAL64), DIMENSION(36) :: Kijd
   REAL(REAL64) :: Qq1, Qq2, Qq3, Qq4, Sum
   REAL, DIMENSION(60) :: Temp60
END MODULE C_SMA1DP
