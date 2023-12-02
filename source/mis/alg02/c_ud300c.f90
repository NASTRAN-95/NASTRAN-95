!*==/home/marcusmae/nasa/nastran/SPAGged/C_UD300C.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_UD300C
   REAL, DIMENSION(30) :: Bblock, Bdist, Delf, Delh, Delt, Drdm2, Rim1, Speed, Wblock, Wwbl, Xim1
   REAL :: C1, Conmx, Contr, Ej, Fm2, G, Hmin, Pi, Plow, Pscale, Rconst, Rlow, Sclfac, Shape, Tolnce, Visk, Xmmax, &
         & Xscale
   REAL, DIMENSION(21) :: Cppg, Cr, Delw, Gama, Hkeep, Lami, Lamim1, Lamip1, Loss, Phi, Skeep, Sppg, Taneps, Vv,     &
                         & Vwkeep, Work, Xi
   REAL, DIMENSION(100) :: Data1, Data2, Data3, Data4, Data5, Data6, Data7, Data8, Data9, Datac, Delc, Delta
   REAL, DIMENSION(15, 4) :: Diff, Fdhub, Fdmid, Fdtip
   REAL, DIMENSION(11, 5, 2) :: Dm, Wfrac
   REAL, DIMENSION(10) :: Flow, Spdfac
   REAL, DIMENSION(21, 30) :: H, R, S, Tbeta, Vm, Vw, X, Xl
   INTEGER :: I, Icase, Ifail, Ifailo, Iffail, Iloss, Imid, Ipass, Iprint, Istag, Iter, Itub, Ivfail, Lnct, Log1,    &
            & Log2, Log3, Log4, Log5, Log6, Nbl, Ncase, Neqn, Nforce, Nmany, Nmax, Nmix, Npage, Nplot, Npunch,       &
            & Nread, Nset1, Nset2, Nsplit, Nstns, Nstplt, Nstrms, Ntrans
   INTEGER, DIMENSION(30) :: Is1, Is2, Is3, Nblade, Ncurve, Ndata, Ndel, Ndimen, Neval, Nl1, Nl2, Nliter, Nloss,      &
                            & Nmach, Nout1, Nout2, Nout3, Nspec, Nterp, Nwhich, Nwork
   INTEGER, DIMENSION(4) :: Ndiff
   INTEGER, DIMENSION(2) :: Nm, Nrad
   REAL, DIMENSION(150) :: Rstn, Xstn
   REAL, DIMENSION(5, 2) :: Terad
   REAL, DIMENSION(18) :: Title
END MODULE C_UD300C
