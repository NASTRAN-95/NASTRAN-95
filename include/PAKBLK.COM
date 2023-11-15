!
! COMMON variable declarations
!
   INTEGER Iblka(15) , Iblkb(15) , Iblkc(15) , Iblkd(15) , Itrail , Itypi , Itypo
   COMMON /pakblk/ Itypi , Itypo , Itrail , Iblka , Iblkb , Iblkc , Iblkd
!
! End of declarations
!
 
