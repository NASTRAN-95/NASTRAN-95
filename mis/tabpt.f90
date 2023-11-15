
SUBROUTINE tabpt
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Irc , Iwd
   REAL Op(2)
   COMMON /blank / Op , Irc , Iwd
!
! Local variable declarations
!
   REAL blank
   INTEGER i , in(5) , itrl(7)
!
! End of declarations
!
!
!     MODULE DRIVER TO PRINT TABLES
!
   DATA in/101 , 102 , 103 , 104 , 105/ , blank/4H    /
!
   DO i = 1 , 5
      itrl(1) = in(i)
      CALL rdtrl(itrl(1))
      IF ( itrl(1)>0 ) CALL tabprt(in(i))
   ENDDO
   Op(1) = blank
   Op(2) = blank
   Irc = 0
   Iwd = 0
END SUBROUTINE tabpt
