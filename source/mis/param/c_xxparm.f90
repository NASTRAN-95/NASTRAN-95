!*==/home/marcusmae/nasa/nastran/SPAGged/C_XXPARM.f90  created by SPAG 8.01RF at 14:47 on  2 Dec 2023
MODULE C_XXPARM
   INTEGER, DIMENSION(3) :: Axis, Daxis
   INTEGER :: Bframs, Camera, Color, Direct, For, Fpltit, Fscale, Fvp, Icntvl, Lasset, Layer, Ncntr, Norg, Npens,     &
            & Org, Pltbuf, Prject, Tapden, Where
   REAL, DIMENSION(50) :: Cntr
   REAL :: D02, D03, Data, Defmax, Flag, Maxdef, Penpap, S0s, Subcas
   REAL, DIMENSION(11, 4) :: Edge
   INTEGER, DIMENSION(11) :: Origin
   REAL, DIMENSION(2) :: Papsiz, Scale
   INTEGER, DIMENSION(2) :: Paptyp, Pltmod
   INTEGER, DIMENSION(8, 2) :: Penclr
   INTEGER, DIMENSION(8) :: Pensiz
   INTEGER, DIMENSION(17) :: Pltitl
   REAL, DIMENSION(6) :: Skpvue
   REAL, DIMENSION(3) :: Vangle
   REAL, DIMENSION(5) :: Vanpnt
   REAL, DIMENSION(11, 3) :: Xy
END MODULE C_XXPARM
