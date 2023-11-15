!
! COMMON variable declarations
!
   INTEGER Iax , Iax2 , Ibrow , Ibx , Idx , Idx2 , Idx4 , Ifile , Ipass , Ircol1 , Ircoln , Irfile , Irpos(3) , Itype , Lasind ,    &
         & Lasindm , Lasmem , Method , Nastor , Nbstor , Ncolpp , Nwdb , Nwdd , Nwddnbr , Nwddndr
   REAL Ofile , Sign
   COMMON /mmacom/ Ifile , Lasind , Sign , Itype , Iax , Idx , Idx2 , Idx4 , Ibrow , Nwdd , Nwdb , Ncolpp , Ofile , Ipass , Nastor ,&
                 & Nbstor , Nwddndr , Ibx , Nwddnbr , Irfile , Ircol1 , Ircoln , Lasmem , Irpos , Iax2 , Lasindm , Method
!
! End of declarations
!
