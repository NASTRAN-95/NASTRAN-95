!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA2DP.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_SMA2DP
   USE
   REAL*8, DIMENSION(81) :: A
   REAL*8, DIMENSION(9) :: Arr9, Array9, E, Prod9, T, Temp9
   REAL*8 :: C1, C2, Determ, L1, L2, S1, S2, Temp, Temp1, Temp2, U1, U2, X1, X2, Xsubb, Xsubc, Y1, Y2, Ysubc
   REAL :: Cosang, Sinang, Theta
   REAL, DIMENSION(20) :: Dumtwo
   REAL, DIMENSION(54) :: Dumx
   REAL*8, DIMENSION(36) :: Hinv, M6x6
   INTEGER :: Ising, Km, Npivot, Npoint, Npt1, Nsubc, Subsca, Subscb, Subscc
   REAL*8, DIMENSION(3) :: Ivect, Jvect, Kvect
   REAL*8, DIMENSION(18) :: S
   REAL*8, DIMENSION(2) :: V, Vv
END MODULE C_SMA2DP
