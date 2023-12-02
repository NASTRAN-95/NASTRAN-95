!*==/home/marcusmae/nasa/nastran/SPAGged/C_DETMX.f90  created by SPAG 8.01RF at 14:47 on  2 Dec 2023
MODULE C_DETMX
   USE ISO_FORTRAN_ENV
   REAL(REAL64), DIMENSION(4) :: Det1, Detx, P, Ps1
   REAL :: Fact1, Sml1
   INTEGER :: Iadd, Ic, Idet, Ifail, Iffnd, Ipdeta, Ips, Ipsav, Is, Isng, Iterm, K, N2ev, Nd, Ndcmp, Nfail, Npole, &
            & Nsmove, Nstart, Prec
   INTEGER, DIMENSION(4) :: Ipdet1, Ipdetx
END MODULE C_DETMX
