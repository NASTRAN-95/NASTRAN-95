!
! COMMON variable declarations
!
   CHARACTER*72 Dic , Dirtry , In12 , Inp1 , Inp2 , Input , Log , Nptp , Optp , Out11 , Output , Plot , Punch , Rdic , Rfdir
   CHARACTER*80 Dsnames(89)
   COMMON /dosnam/ Dirtry , Rfdir , Input , Output , Log , Punch , Plot , Nptp , Dic , Optp , Rdic , In12 , Out11 , Inp1 , Inp2
   COMMON /dsname/ Dsnames
!
! End of declarations
!
