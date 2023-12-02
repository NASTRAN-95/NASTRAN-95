!*==/home/marcusmae/nasa/nastran/SPAGged/C_PLA42C.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_PLA42C
   INTEGER :: Clsrw, Cstm, Dit, Ecpto, Ecpts, Eor, Gpct, I6x6k, Icstm, Igpct, Inrw, Ipass, Ipoint, Jmax, Kggnl,      &
            & Lrowic, Mpt, N6x6k, Ncstm, Neor, Ngpct, Nlinks, Nogo, Npoint, Npvt, Nrowsc, Outrw
   REAL :: Frowic, Gami, Gamip1
   INTEGER, DIMENSION(40) :: Iovrly, Link, Nwords
!PLA4BD
!
!
!
!
!    1         ROD       BEAM      TUBE      SHEAR     TWIST
!    2         TRIA1     TRBSC     TRPLT     TRMEM     CONROD
!    3         ELAS1     ELAS2     ELAS3     ELAS4     QDPLT
!    4         QDMEM     TRIA2     QUAD2     QUAD1     DAMP1
!    5         DAMP2     DAMP3     DAMP4     VISC      MASS1
!    6         MASS2     MASS3     MASS4     CONM1     CONM2
!    7         PLOTEL    REACT     QUAD3     BAR       CONE
!    8         TRIARG    TRAPRG    TORDRG    CORE      CAP
!
!
   DATA npvt , gami , gamip1 , ipass , icstm , ncstm/6*0/ , igpct , ngpct , ipoint , npoint , i6x6k , n6x6k/6*0/ , cstm , mpt ,     &
      & gpct , dit , kggnl , ecpto , ecpts/101 , 102 , 104 , 105 , 201 , 202 , 301/ , inrw , outrw , eor , neor , clsrw/0 , 1 , 1 , &
      & 0 , 1/ , jmax , frowic , lrowic , nrowsc , nlinks/4*0 , 1/ , nwords/26 , 0 , 25 , 0 , 0 , 42 , 0 , 0 , 36 , 26 , 0 , 0 , 0 ,&
      & 0 , 0 , 44 , 36 , 44 , 50 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 57 , 0 , 0 , 0 , 0 , 0 , 0/ ,           &
      & iovrly/40*1/ , nogo/0/
END MODULE c_pla42c
