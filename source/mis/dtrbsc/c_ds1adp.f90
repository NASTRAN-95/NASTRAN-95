!*==/home/marcusmae/nasa/nastran/SPAGged/C_DS1ADP.f90  created by SPAG 8.01RF at 14:47 on  2 Dec 2023
MODULE C_DS1ADP
   USE ISO_FORTRAN_ENV
   REAL(REAL64) :: Ar, Eye, Px2, Pxy2, Py2, R, R2, S2, Sp, Sx, Sxy, Sy, T, T2, U, Xbar, Xbar3, Xbsq, Xcsq, Xcyc,&
                 & Xsubb, Xsubc, Ybar, Ybar2, Ybar3, Ycsq, Ysubc
   REAL(REAL64), DIMENSION(24, 3) :: C
   REAL(REAL64), DIMENSION(81) :: Dumdp
   REAL(REAL64), DIMENSION(9) :: G
   REAL(REAL64), DIMENSION(4) :: G2x2
END MODULE C_DS1ADP
