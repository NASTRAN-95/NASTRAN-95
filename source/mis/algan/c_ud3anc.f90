!*==/home/marcusmae/nasa/nastran/SPAGged/C_UD3ANC.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_UD3ANC
   REAL, DIMENSION(21, 10) :: Alpha, Rsta, Theta, Xsta, Ycamb, Zcamb
   REAL, DIMENSION(21) :: B1, B2, Bs, Cord, Delx, Dely, Pp, Qq, Rle, S, Tc, Te, Temp1, Temp2, Temp3, Temp4, Zout, &
                         & Zr, Zz
   REAL, DIMENSION(80, 4) :: Epz
   INTEGER, DIMENSION(10) :: Ifangs
   INTEGER, DIMENSION(21) :: Kpts
   REAL, DIMENSION(10, 21) :: R, Tanphi
   REAL, DIMENSION(100) :: Rad, Sigma, Ss, X, Xtemp, Yprime
   REAL, DIMENSION(18) :: Title2
   REAL, DIMENSION(10) :: Xhere
   REAL, DIMENSION(21, 31) :: Xsemi, Xsemj, Ysemi, Ysemj, Zsemi, Zsemj
   REAL, DIMENSION(21, 70) :: Zp
END MODULE C_UD3ANC
