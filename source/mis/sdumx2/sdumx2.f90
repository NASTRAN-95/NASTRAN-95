!*==sdumx2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sdumx2
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
   DATA ii/9*0/ , jj/4HSDUM/ , kk/2H12 , 2H22 , 2H32 , 2H42 , 2H52 , 2H62 , 2H72 , 2H82 , 2H92/
!
   GOTO 99999
!
!
   ENTRY sdum92
!     ============
!
   j = 9
   GOTO 100
!
!
   ENTRY sdum82
!     ============
!
   j = 8
   GOTO 100
!
!
   ENTRY sdum72
!     ============
!
   j = 7
   GOTO 100
!
!
   ENTRY sdum62
!     ============
!
   j = 6
   GOTO 100
!
!
   ENTRY sdum52
!     ============
!
   j = 5
   GOTO 100
!
!
   ENTRY sdum42
!     ============
!
   j = 4
   GOTO 100
!
!
   ENTRY sdum32
!     ============
!
   j = 3
   GOTO 100
!
!
   ENTRY sdum22
!     ============
!
   j = 2
   GOTO 100
!
!
   ENTRY sdum12
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
99999 END SUBROUTINE sdumx2
