!*==tabpt.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tabpt
   IMPLICIT NONE
   USE C_BLANK
!
! Local variable declarations rewritten by SPAG
!
   REAL , SAVE :: blank
   INTEGER :: i
   INTEGER , DIMENSION(5) , SAVE :: in
   INTEGER , DIMENSION(7) :: itrl
   EXTERNAL rdtrl , tabprt
!
! End of declarations rewritten by SPAG
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
