!*==/home/marcusmae/nasa/nastran/SPAGged/C_DETMX.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_DETMX
   USE
   REAL*8, DIMENSION(4) :: Det1, Detx, P, Ps1
   REAL :: Fact1, Sml1
   INTEGER :: Iadd, Ic, Idet, Ifail, Iffnd, Ipdeta, Ips, Ipsav, Is, Isng, K, L1, L2, N2ev, Nd, Nfail, Npole, Nstart, &
            & Prec, U2
   INTEGER, DIMENSION(4) :: Ipdet1, Ipdetx
END MODULE C_DETMX
