!*==/home/marcusmae/nasa/nastran/SPAGged/C_SMA1DP.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_SMA1DP
   USE
   REAL*8 :: A, A2, A3, A4, A5, B, B2, B3, B4, B5, C, C2, C23, C3, C4, C5, Cep1, Cep2, D, D2, D3, D4, D5,       &
           & Dampc, Dpcon, E, Ep, F, G, Nu, Nuc, Pa, Sa, T, Temp, Term, Term1, Term2, Term3, Term4, Term5, V12dk,   &
           & Vjl, Vkl, Vp12l, X1, X2, X3, X4, Xl, Xl13, Xl24, Xp, Xq, Y1, Y2, Y3, Y4, Yp, Z
   REAL*8, DIMENSION(4) :: Avec, P, Smallu, Smallv
   REAL*8, DIMENSION(36) :: Ke
   REAL*8, DIMENSION(9) :: Ti
   REAL*8, DIMENSION(3) :: V12, V41, Vd1, Vd2, Vi, Vj, Vk, Vkn, Vp12
   REAL*8, DIMENSION(6) :: Vleft, Vright
END MODULE C_SMA1DP
