!*==/home/marcusmae/nasa/nastran/SPAGged/C_DETMX.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_DETMX
   USE ISO_FORTRAN_ENV
   REAL(REAL64), DIMENSION(4) :: Det1, Detx, P, Ps1
   REAL :: Fact1, Sml1
   INTEGER :: Iadd, Ic, Idet, Ifail, Ipdeta, Ips, Ipsav, Is, K, L2, N2ev, Nd, Nsmove, Prec, U1, U2
   INTEGER, DIMENSION(4) :: Ipdet1, Ipdetx
END MODULE C_DETMX
