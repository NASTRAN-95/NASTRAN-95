!*==/home/marcusmae/nasa/nastran/SPAGged/C_SDR2X8.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_SDR2X8
   REAL :: A, B, Cosphi, P, Phi, Q, R, Rm, Rx, Rxy, Ry, Ryz, Rz, Rzx, S, Sa, Sb, Sc, Sn, So, Strspx, T, Temp,   &
         & V, X
   REAL, DIMENSION(3, 3) :: Dcos
   INTEGER, DIMENSION(32) :: Dummy
   INTEGER, DIMENSION(200) :: Equ
   INTEGER :: I, Iend, Ipts, J, K, Khi, Klo, Kx, L, Leqx, Mxleq, Neqx, Ngp, Sil
   INTEGER, DIMENSION(7) :: Itrl
   REAL, DIMENSION(6) :: Sig
END MODULE C_SDR2X8
