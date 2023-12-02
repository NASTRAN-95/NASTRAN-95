!*==readbd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
BLOCKDATA readbd
   IMPLICIT NONE
   USE C_GIVN
   USE C_INVPWX
   USE C_REGEAN
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
