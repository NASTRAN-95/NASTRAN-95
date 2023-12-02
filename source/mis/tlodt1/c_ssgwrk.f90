!*==/home/marcusmae/nasa/nastran/SPAGged/C_SSGWRK.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_SSGWRK
   REAL :: A1, A2, A3, B1, B2, B3, Dista, Distb, Distc, X, Y, Z
   REAL, DIMENSION(36) :: Balotr, Ptglb
   REAL, DIMENSION(10) :: Cc
   REAL, DIMENSION(3) :: D, G1, Ivect, Jvect, Kvect, Psub, Tl
   REAL, DIMENSION(18) :: E, Ptele
   REAL, DIMENSION(9) :: G
   INTEGER, DIMENSION(20, 3) :: Index
   INTEGER, DIMENSION(2) :: Name
   INTEGER, DIMENSION(6) :: Nl
   REAL, DIMENSION(6) :: Psubt, Psubt1
   REAL, DIMENSION(20) :: Ptem
   REAL, DIMENSION(40) :: Ts6
END MODULE C_SSGWRK
