!*==/home/marcusmae/nasa/nastran/SPAGged/C_DS1ADP.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_DS1ADP
   USE ISO_FORTRAN_ENV
   REAL(REAL64) :: A, A2, A3, A4, A5, B, B2, B3, B4, B5, C, C2, C23, C3, C4, C5, Cep1, Cep2, D, D2, D3, D4,   &
                 & D5, Dpterm, E, Ep, F, F13, F24, Fxx, G, Nu, Nuc, Pa, Sa, Sum, T, Temp, Term, Term1, Term2,    &
                 & Term3, Term4, Term5, V12dk, Vjl, Vkl, Vp12l, X1, X2, X3, X4, Xl, Xl13, Xl24, Xp, Xq, Y1, Y2,   &
                 & Y3, Y4, Yp, Z
   REAL(REAL64), DIMENSION(4) :: Avec, P, Smallu, Smallv
   REAL(REAL64), DIMENSION(9) :: J3x3, K3x3, Ti
   REAL(REAL64), DIMENSION(3) :: Jj, Ui, V12, V41, Vd1, Vd2, Vi, Vj, Vk, Vkn, Vp12
   REAL(REAL64), DIMENSION(36) :: Ke
   REAL(REAL64), DIMENSION(6) :: Vleft
END MODULE C_DS1ADP
