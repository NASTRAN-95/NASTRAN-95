!*==sdumx1.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sdumx1
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_XMSSG
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(9) , SAVE :: ii , kk
   INTEGER :: j
   INTEGER , SAVE :: jj
!
! End of declarations rewritten by SPAG
!
!
!     DELETE ANY OF THE FOLLOW ENTRY POINT IF A SUBROUTINE OF THE SAME
!     NAME ALREADY EXISTS
!
   DATA ii/9*0/ , jj/4HSDUM/ , kk/2H11 , 2H21 , 2H31 , 2H41 , 2H51 , 2H61 , 2H71 , 2H81 , 2H91/
!
   GOTO 99999
!
!
   ENTRY sdum91
!     ============
!
   j = 9
   GOTO 100
!
!
   ENTRY sdum81
!     ============
!
   j = 8
   GOTO 100
!
!
   ENTRY sdum71
!     ============
!
   j = 7
   GOTO 100
!
!
   ENTRY sdum61
!     ============
!
   j = 6
   GOTO 100
!
!
   ENTRY sdum51
!     ============
!
   j = 5
   GOTO 100
!
!
   ENTRY sdum41
!     ============
!
   j = 4
   GOTO 100
!
!
   ENTRY sdum31
!     ============
!
   j = 3
   GOTO 100
!
!
   ENTRY sdum21
!     ============
!
   j = 2
   GOTO 100
!
!
   ENTRY sdum11
!     ============
!
   j = 1
!     GO TO 10
!
 100  IF ( ii(j)==0 ) THEN
      ii(j) = 1
      WRITE (Nout,99001) Uwm , jj , kk(j)
99001 FORMAT (A25,' 2182, SUBROUTINE ',2A4,' IS DUMMY.  ONLY ONE OF ','THESE MESSAGES WILL APPEAR PER OVERLAY OF THIS DECK.')
   ENDIF
99999 END SUBROUTINE sdumx1
