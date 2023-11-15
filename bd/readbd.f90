
BLOCKDATA readbd
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Eps , Epsi , G1 , G2(8) , G3(2) , G4(82) , G5(2) , G6(4) , G7(2) , Lmax , Lmin , Rmax , Rmin , Rminr , X(35)
   INTEGER Ibuck , Idump , Iev(7) , Ifilk(7) , Ifillm(7) , Ifilm(7) , Ifilvc(7) , Ik(7) , Im(7) , Iscr1 , Iscr2 , Iscr3 , Iscr4 ,   &
         & Iscr5 , Iscr6 , Iscr7 , Iscr8 , Lama , Lcore , Lgama , M1 , M2 , M3 , M4 , Max , Md , Mo , Mr1 , Mz , Ncol , Ndmnus ,    &
         & Ndplus , Ne , Nev , Nevm , Nfound , Nit , Noest , Novect , Nsym , Oeigs , Order , Phia , Rstrt , Scr1 , Scr2 , Scr3 ,    &
         & Scr4 , Scr5 , Scr6 , Scr7
   COMMON /givn  / G1 , Mo , Md , Mr1 , M1 , M2 , M3 , M4 , G2 , Rstrt , Ncol , G3 , G4 , Order , G5 , Lgama , G6 , Oeigs , Phia ,  &
                 & G7 , Max , X
   COMMON /invpwx/ Ifilk , Ifilm , Ifillm , Ifilvc , Iscr1 , Iscr2 , Iscr3 , Iscr4 , Iscr5 , Iscr6 , Iscr7 , Iscr8 , Idump , Lmin , &
                 & Lmax , Noest , Ndplus , Ndmnus , Eps , Novect
   COMMON /regean/ Im , Ik , Iev , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Lcore , Rmax , Rmin , Mz , Nev , Epsi , Rminr , Ne , Nit ,    &
                 & Nevm , Scr6 , Scr7 , Nfound , Lama , Ibuck , Nsym
!
! End of declarations
!
!READBD
   DATA Mo , Md , Mr1 , M1 , M2 , M3 , M4 , Lgama , Oeigs , Phia/301 , 304 , 202 , 303 , 307 , 308 , 309 , 201 , 204 , 305/
   DATA Order , Rstrt , Ncol , Max , Im , Ik , Iev/ - 2 , 0 , 0 , 253 , 102 , 6*0 , 101 , 6*0 , 302 , 0 , 0 , 2 , 1 , 0 , 0/
   DATA Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Lama , Scr6 , Scr7/306 , 307 , 303 , 304 , 305 , 301 , 308 , 204/
   DATA Rmax , Rmin , Epsi , Rminr/100.0 , .01 , 1.0E-11 , -.001/
   DATA Mz , Nev , Ne , Nit , Nevm , Nfound/0 , 9 , 4 , 30 , 5 , 0/
   DATA Ifilk , Ifilm , Ifillm , Ifilvc/101 , 6*0 , 102 , 6*0 , 201 , 6*0 , 202 , 6*0/
   DATA Iscr1 , Iscr2 , Iscr3 , Iscr4 , Iscr5 , Iscr6 , Iscr7 , Iscr8 , Idump/301 , 302 , 303 , 304 , 305 , 306 , 307 , 308 , 204/
   DATA Noest , Ndplus , Ndmnus , Eps , Novect , Lmin , Lmax , Nsym/5 , 5 , 0 , .0001 , 0 , 0. , 60. , 0/
END BLOCKDATA readbd
