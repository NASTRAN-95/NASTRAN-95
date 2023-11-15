
INTEGER FUNCTION sofsiz(Dum)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Avblks , Blksiz
   REAL Dirsiz , Supsiz
   COMMON /sys   / Blksiz , Dirsiz , Supsiz , Avblks
!
! Dummy argument declarations
!
   REAL Dum
!
! Local variable declarations
!
   INTEGER nmsbr(2)
!
! End of declarations
!
!*****
!     RETURNS THE REMAINING NUMBER OF AVAILABLE WORDS ON THE SOF.
!*****
   DATA nmsbr/4HSOFS , 4HIZ  /
!*****
   CALL chkopn(nmsbr(1))
   sofsiz = Blksiz*Avblks
END FUNCTION sofsiz
