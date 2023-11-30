
INTEGER FUNCTION sofsiz(Dum)
   IMPLICIT NONE
   INTEGER Avblks , Blksiz
   REAL Dirsiz , Supsiz
   COMMON /sys   / Blksiz , Dirsiz , Supsiz , Avblks
   REAL Dum
   INTEGER nmsbr(2)
!*****
!     RETURNS THE REMAINING NUMBER OF AVAILABLE WORDS ON THE SOF.
!*****
   DATA nmsbr/4HSOFS , 4HIZ  /
!*****
   CALL chkopn(nmsbr(1))
   sofsiz = Blksiz*Avblks
END FUNCTION sofsiz