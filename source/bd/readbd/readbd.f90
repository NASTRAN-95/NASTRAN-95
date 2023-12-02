!*==readbd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
BLOCKDATA readbd
   USE c_givn
   USE c_invpwx
   USE c_regean
   IMPLICIT NONE
!READBD
   DATA mo , md , mr1 , m1 , m2 , m3 , m4 , lgama , oeigs , phia/301 , 304 , 202 , 303 , 307 , 308 , 309 , 201 , 204 , 305/
   DATA order , rstrt , ncol , max , im , ik , iev/ - 2 , 0 , 0 , 253 , 102 , 6*0 , 101 , 6*0 , 302 , 0 , 0 , 2 , 1 , 0 , 0/
   DATA scr1 , scr2 , scr3 , scr4 , scr5 , lama , scr6 , scr7/306 , 307 , 303 , 304 , 305 , 301 , 308 , 204/
   DATA rmax , rmin , epsi , rminr/100.0 , .01 , 1.0E-11 , -.001/
   DATA mz , nev , ne , nit , nevm , nfound/0 , 9 , 4 , 30 , 5 , 0/
   DATA ifilk , ifilm , ifillm , ifilvc/101 , 6*0 , 102 , 6*0 , 201 , 6*0 , 202 , 6*0/
   DATA iscr1 , iscr2 , iscr3 , iscr4 , iscr5 , iscr6 , iscr7 , iscr8 , idump/301 , 302 , 303 , 304 , 305 , 306 , 307 , 308 , 204/
   DATA noest , ndplus , ndmnus , eps , novect , lmin , lmax , nsym/5 , 5 , 0 , .0001 , 0 , 0. , 60. , 0/
END BLOCKDATA readbd
