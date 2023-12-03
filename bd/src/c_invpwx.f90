!*==/home/marcusmae/nasa/nastran/SPAGged/C_INVPWX.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_INVPWX
   REAL :: Eps, Lmax, Lmin
   INTEGER :: Idump, Iscr1, Iscr2, Iscr3, Iscr4, Iscr5, Iscr6, Iscr7, Iscr8, Ndmnus, Ndplus, Noest, Novect
   INTEGER, DIMENSION(7) :: Ifilk, Ifillm, Ifilm, Ifilvc

   DATA iscr1 , iscr2 , iscr3 , iscr4 , iscr5 , iscr6 , iscr7 , iscr8 , idump/301 , 302 , 303 , 304 , 305 , 306 , 307 , 308 , 204/
   DATA noest , ndplus , ndmnus , eps , novect , lmin , lmax , nsym/5 , 5 , 0 , .0001 , 0 , 0. , 60. , 0/
   DATA ifilk , ifilm , ifillm , ifilvc/101 , 6*0 , 102 , 6*0 , 201 , 6*0 , 202 , 6*0/

END MODULE C_INVPWX
