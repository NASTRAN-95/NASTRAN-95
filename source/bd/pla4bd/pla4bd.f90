!*==pla4bd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
BLOCKDATA pla4bd
   USE c_pla42c
   IMPLICIT NONE
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
END BLOCKDATA pla4bd
