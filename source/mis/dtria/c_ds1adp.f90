!*==/home/marcusmae/nasa/nastran/SPAGged/C_DS1ADP.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_DS1ADP
   USE
   REAL*8, DIMENSION(54) :: A
   REAL*8, DIMENSION(9) :: Arr9, Array9, Prod9, T, Temp9
   REAL*8 :: C1, C2, Determ, L1, L2, S1, S2, Sigx, Sigxy, Sigy, Temp, Temp1, Temp2, U1, U2, X1, X2, Xsubb, Xsubc,   &
           & Y1, Y2, Ysubc
   REAL :: Cosang, Sinang, Theta
   REAL*8, DIMENSION(2) :: Dumtwo, V, Vv
   REAL*8, DIMENSION(18) :: E, S
   REAL*8, DIMENSION(36) :: Hinv
   INTEGER :: Ipvt, Ising, Km, Npivot, Npoint, Npt1, Nsubb, Nsubc, Subsca, Subscb, Subscc
   REAL*8, DIMENSION(3) :: Ivect, Jvect, Kvect, Stres
   REAL*8, DIMENSION(63) :: Ksum
   REAL*8, DIMENSION(2, 4) :: R
END MODULE C_DS1ADP
