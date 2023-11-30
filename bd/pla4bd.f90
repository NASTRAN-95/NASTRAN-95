
BLOCKDATA pla4bd
   IMPLICIT NONE
   INTEGER Clsrw , Cstm , Dit , Ecpto , Ecpts , Eor , Gpct , I6x6k , Icstm , Igpct , Inrw , Iovrly(40) , Ipass , Ipoint , Jmax ,    &
         & Kggnl , Link(40) , Lrowic , Mpt , N6x6k , Ncstm , Neor , Ngpct , Nlinks , Nogo , Npoint , Npvt , Nrowsc , Nwords(40) ,   &
         & Outrw
   REAL Frowic , Gami , Gamip1
   COMMON /pla42c/ Npvt , Gami , Gamip1 , Ipass , Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6k , N6x6k , Cstm , Mpt ,    &
                 & Ecpts , Gpct , Dit , Kggnl , Ecpto , Inrw , Outrw , Eor , Neor , Clsrw , Jmax , Frowic , Lrowic , Nrowsc ,       &
                 & Nlinks , Nwords , Iovrly , Link , Nogo
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
   DATA Npvt , Gami , Gamip1 , Ipass , Icstm , Ncstm/6*0/ , Igpct , Ngpct , Ipoint , Npoint , I6x6k , N6x6k/6*0/ , Cstm , Mpt ,     &
      & Gpct , Dit , Kggnl , Ecpto , Ecpts/101 , 102 , 104 , 105 , 201 , 202 , 301/ , Inrw , Outrw , Eor , Neor , Clsrw/0 , 1 , 1 , &
      & 0 , 1/ , Jmax , Frowic , Lrowic , Nrowsc , Nlinks/4*0 , 1/ , Nwords/26 , 0 , 25 , 0 , 0 , 42 , 0 , 0 , 36 , 26 , 0 , 0 , 0 ,&
      & 0 , 0 , 44 , 36 , 44 , 50 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 57 , 0 , 0 , 0 , 0 , 0 , 0/ ,           &
      & Iovrly/40*1/ , Nogo/0/
END BLOCKDATA pla4bd
