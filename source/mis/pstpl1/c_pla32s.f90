!*==/home/marcusmae/nasa/nastran/SPAGged/C_PLA32S.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_PLA32S
   REAL, DIMENSION(45) :: A
   REAL :: C1, C2, Cosang, Determ, Dum10, Dum11, Dum8, Dum9, L1, L2, S1, S2, Sinang, Temp, Temp1, Temp2, Theta, U1,  &
         & U2, X1, X2, Xc, Xsubb, Xsubc, Y1, Y2, Yc, Ysubc
   REAL, DIMENSION(3) :: D1, D2, Ivect, Jvect, Kvect
   REAL, DIMENSION(29) :: Dum12
   REAL, DIMENSION(18) :: E, Habc, S
   REAL, DIMENSION(36) :: Hinv
   INTEGER :: Ising, Km, Npoint, Nsubc, Subsca, Subscb, Subscc
   REAL, DIMENSION(12) :: Prod12
   REAL, DIMENSION(9) :: Prod9, T, Temp9
   REAL, DIMENSION(2, 4) :: R
   REAL, DIMENSION(60) :: Ssum
   REAL, DIMENSION(2) :: Vv1, Vv2
END MODULE C_PLA32S
