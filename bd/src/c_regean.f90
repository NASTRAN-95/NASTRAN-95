!*==/home/marcusmae/nasa/nastran/SPAGged/C_REGEAN.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_REGEAN
   REAL :: Epsi, Rmax, Rmin, Rminr
   INTEGER :: Ibuck, Lama, Lcore, Mz, Ne, Nev, Nevm, Nfound, Nit, Nsym, Scr1, Scr2, Scr3, Scr4, Scr5, Scr6, Scr7
   INTEGER, DIMENSION(7) :: Iev, Ik, Im

   DATA rmax , rmin , epsi , rminr/100.0 , .01 , 1.0E-11 , -.001/
   DATA mz , nev , ne , nit , nevm , nfound/0 , 9 , 4 , 30 , 5 , 0/
   DATA scr1 , scr2 , scr3 , scr4 , scr5 , lama , scr6 , scr7/306 , 307 , 303 , 304 , 305 , 301 , 308 , 204/
   DATA im , ik , iev/ 102 , 6*0 , 101 , 6*0 , 302 , 0 , 0 , 2 , 1 , 0 , 0/

END MODULE C_REGEAN
