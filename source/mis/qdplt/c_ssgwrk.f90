!*==/home/marcusmae/nasa/nastran/SPAGged/C_SSGWRK.f90  created by SPAG 8.01RF at 14:47 on  2 Dec 2023
MODULE C_SSGWRK
   REAL, DIMENSION(45) :: A
   REAL, DIMENSION(3) :: A1, D1, D2, Ivect, Jvect, Kvect
   REAL :: Cosang, H, Sinang, Temp, Theta, U1, U2, Xc, Xsubb, Xsubc, Yc, Ysubc
   REAL, DIMENSION(18) :: E, Spdum1, Tite
   INTEGER :: Npoint, Nsubc, Subsca, Subscb, Subscc
   REAL, DIMENSION(15) :: Prod15, Spdum3, Temp15
   REAL, DIMENSION(2, 5) :: R
   REAL, DIMENSION(5) :: Spdum2, Spdum6
   REAL, DIMENSION(1) :: Spdum4
   REAL, DIMENSION(2) :: Spdum5, Vv1, Vv2
   REAL, DIMENSION(60) :: Ssum
   REAL, DIMENSION(9) :: T
   REAL, DIMENSION(25) :: V
END MODULE C_SSGWRK
