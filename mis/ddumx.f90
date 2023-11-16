
SUBROUTINE ddumx
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ibuf , Nout
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /system/ Ibuf , Nout
   COMMON /xmssg / Ufm , Uwm
!
! Local variable declarations
!
   INTEGER ii(9) , j , jj , kk(9)
!
! End of declarations
!
!
!     DELETE ANY OF THE FOLLOW ENTRY POINT IF A SUBROUTINE OF THE SAME
!     NAME ALREADY EXISTS
!
   DATA ii/9*0/ , jj/4HDDUM/ , kk/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9/
!
   GOTO 99999
!
!
   ENTRY ddum9
!     ===========
!
   j = 9
   GOTO 100
!
!
   ENTRY ddum8
!     ===========
!
   j = 8
   GOTO 100
!
!
   ENTRY ddum7
!     ===========
!
   j = 7
   GOTO 100
!
!
   ENTRY ddum6
!     ===========
!
   j = 6
   GOTO 100
!
!
   ENTRY ddum5
!     ===========
!
   j = 5
   GOTO 100
!
!
   ENTRY ddum4
!     ===========
!
   j = 4
   GOTO 100
!
!
   ENTRY ddum3
!     ===========
!
   j = 3
   GOTO 100
!
!
   ENTRY ddum2
!     ===========
!
   j = 2
   GOTO 100
!
!
   ENTRY ddum1
!     ===========
!
   j = 1
!     GO TO 10
!
 100  IF ( ii(j)==0 ) THEN
      ii(j) = 1
      WRITE (Nout,99001) Uwm , jj , kk(j)
99001 FORMAT (A25,' 2182, SUBROUTINE ',2A4,' IS DUMMY.  ONLY ONE OF ','THESE MESSAGES WILL APPEAR PER  OVERLAY OF THIS DECK.')
   ENDIF
99999 RETURN
END SUBROUTINE ddumx
