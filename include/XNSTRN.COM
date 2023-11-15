!
! COMMON variable declarations
!
   INTEGER Ibase(700000)
   COMMON /zzzzzz/ Ibase
!
! End of declarations
!
