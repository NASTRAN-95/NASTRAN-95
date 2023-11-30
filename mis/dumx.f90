
SUBROUTINE dumx
   IMPLICIT NONE
   INTEGER Ibuf , Nout
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /system/ Ibuf , Nout
   COMMON /xmssg / Ufm , Uwm
   INTEGER Icore
   INTEGER ii(9) , j , jj , kk(9)
!
!     DELETE ANY OF THE FOLLOW ENTRY POINT IF A SUBROUTINE OF THE SAME
!     NAME ALREADY EXISTS
!
   DATA ii/9*0/ , jj/4H DUM/ , kk/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9/
!
   GOTO 99999
!
!
   ENTRY dum9(Icore)
!     ==========
!
   j = 9
   GOTO 100
!
!
   ENTRY dum8(Icore)
!     ==========
!
   j = 8
   GOTO 100
!
!
   ENTRY dum7(Icore)
!     ==========
!
   j = 7
   GOTO 100
!
!
   ENTRY dum6(Icore)
!     ==========
!
   j = 6
   GOTO 100
!
!
   ENTRY dum5(Icore)
!     ==========
!
   j = 5
   GOTO 100
!
!
   ENTRY dum4(Icore)
!     ==========
!
   j = 4
   GOTO 100
!
!
   ENTRY dum3(Icore)
!     ==========
!
   j = 3
   GOTO 100
!
!
   ENTRY dum2(Icore)
!     ==========
!
   j = 2
   GOTO 100
!
!
   ENTRY dum1(Icore)
!     ==========
!
   j = 1
!     GO TO 10
!
 100  IF ( ii(j)==0 ) THEN
      ii(j) = 1
      WRITE (Nout,99001) Uwm , jj , kk(j)
99001 FORMAT (A25,' 2182, SUBROUTINE ',2A4,' IS DUMMY.  ONLY ONE OF ','THESE MESSAGES WILL APPEAR PER OVERLAY OF THIS DECK.')
   ENDIF
99999 RETURN
END SUBROUTINE dumx
