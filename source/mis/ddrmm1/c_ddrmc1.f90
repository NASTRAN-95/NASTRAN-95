!*==/home/marcusmae/nasa/nastran/SPAGged/C_DDRMC1.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_DDRMC1
   INTEGER, DIMENSION(6) :: Buff
   LOGICAL :: Col1, Frstid, Idout, Sort2, Trnsnt
   INTEGER :: Device, Dhsize, Entrys, File, Form, I1, I2, Icc, Ierror, Ilist, Infile, Ipass, Istlst, Itemp, Itype1,  &
            & Itype2, Jfile, Lsf, Lstlst, Ncc, Ncore, Nlambs, Nlist, Nptsf, Nsols, Nwds, Nwdsf, Nwords, Outfil,       &
            & Passes, Phase, Setid, Subcas, Uvsol
   INTEGER, DIMENSION(2) :: Filnam
   INTEGER, DIMENSION(146) :: Idrec
   REAL :: Lambda, Omega
   INTEGER, DIMENSION(7) :: Mcb, Scrt
   REAL, DIMENSION(150) :: Rbuf
   INTEGER, DIMENSION(5, 3) :: Sets
END MODULE C_DDRMC1
