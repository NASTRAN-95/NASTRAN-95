!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA2DP.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_SMA2DP
   USE ISO_FORTRAN_ENV
   REAL(REAL64), DIMENSION(36) :: Balotr
   REAL(REAL64), DIMENSION(3) :: Cab
   REAL, DIMENSION(10) :: Cc
   INTEGER, DIMENSION(15) :: Dict
   REAL :: Fac
   INTEGER, DIMENSION(6) :: Ics, Nl, Save, Sil, Small
   INTEGER, DIMENSION(20, 3) :: Index
   REAL, DIMENSION(3) :: Ivect, Jvect, Kvect
   REAL(REAL64), DIMENSION(6, 6) :: Ksub, Ksubt
   REAL(REAL64), DIMENSION(9) :: Trand
   REAL, DIMENSION(6) :: Xc, Yc, Zc
END MODULE C_SMA2DP
