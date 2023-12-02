!*==sofsiz.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION sofsiz(Dum)
   IMPLICIT NONE
   USE C_SYS
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: sofsiz
   REAL :: Dum
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL chkopn
!
! End of declarations rewritten by SPAG
!
!*****
!     RETURNS THE REMAINING NUMBER OF AVAILABLE WORDS ON THE SOF.
!*****
   DATA nmsbr/4HSOFS , 4HIZ  /
!*****
   CALL chkopn(nmsbr(1))
   sofsiz = Blksiz*Avblks
END FUNCTION sofsiz
