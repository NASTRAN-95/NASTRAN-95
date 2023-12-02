!*==/home/marcusmae/nasa/nastran/SPAGged/C_SDR2DE.f90  created by SPAG 8.01RF at 14:47 on  2 Dec 2023
MODULE C_SDR2DE
   INTEGER :: Buf6, Buf7, Buf8, Device, Elemid, Eltype, Eof, Estawd, Fdest, Flag, Forcex, Formt, Fsetno, I, Icc,     &
            & Icore, Idef, Iedt, Ielem, Iesta, Igptta, Ilist, Ipart, Irecx, Ireqx, Iretrn, Isave, Iseq, Isetf,        &
            & Isetnf, Isetno, Isets, Isymn, Ix, Ixsetf, Ixsets, J, Jany, Jforc, Jlist, Jstrs, K, Kcount, Kfrq, Khi, &
            & Klo, Kn, Ktype1, Ktypex, Kx, Lsym, M, Midvec, N, N1, N2, Ndef, Nesta, Ngptt, Nlist, Nlogic, Notset,  &
            & Nsetf, Nsets, Nvects, Nwdfor, Nwds, Nwdsa, Nwdstr, Nwords, Nx, Nxsetf, Nxsets, Ofile, Outfl, Retx,      &
            & Sdest, Setno, Sorc, Stresx, Tload, Tmprec, Ugvvec, Xsetnf, Xsetns
   REAL :: Coef, Deftmp, Diff, Diff1, Fn, Save
   LOGICAL :: Eofcc
   REAL, DIMENSION(33) :: Tgrd
   REAL, DIMENSION(4) :: Tgrid
END MODULE C_SDR2DE
