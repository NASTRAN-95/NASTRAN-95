!*==/home/marcusmae/nasa/nastran/SPAGged/C_SDR2X8.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_SDR2X8
   REAL, DIMENSION(39) :: Delta
   REAL, DIMENSION(6) :: Deltat, Epslnm, Epslnt, Tdelta
   REAL :: Detg2, Gdum, Offset, T3ov12, Tbar
   REAL, DIMENSION(6, 4) :: Epscmi, Epscsi, Epsumi, Epsusi
   REAL, DIMENSION(8) :: Epsln
   INTEGER, DIMENSION(6) :: Extrnl
   REAL, DIMENSION(3, 3) :: G2
   REAL, DIMENSION(3, 4) :: G2alfb
   INTEGER, DIMENSION(3) :: Idr
   INTEGER, DIMENSION(4) :: Igrid
   INTEGER, DIMENSION(3, 3) :: Indxg2
   REAL, DIMENSION(2, 4) :: Qveci, Z12
   REAL, DIMENSION(3) :: Stempd
   REAL, DIMENSION(9) :: Tes, Uem, Ues
   REAL, DIMENSION(4) :: Thikns, Ves
   REAL, DIMENSION(36) :: U
   REAL, DIMENSION(2) :: Vxvy
END MODULE C_SDR2X8
