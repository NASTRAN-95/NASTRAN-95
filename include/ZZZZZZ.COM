!
! COMMON variable declarations
!
   INTEGER Mem(10)
   COMMON /zzzzzz/ Mem
!
! End of declarations
!
