!*==sdumx2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdumx2
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
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
   RETURN
!
!
   ENTRY sdum92
!     ============
!
   j = 9
   CALL spag_block_1
   RETURN
!
!
   ENTRY sdum82
!     ============
!
   j = 8
   CALL spag_block_1
   RETURN
!
!
   ENTRY sdum72
!     ============
!
   j = 7
   CALL spag_block_1
   RETURN
!
!
   ENTRY sdum62
!     ============
!
   j = 6
   CALL spag_block_1
   RETURN
!
!
   ENTRY sdum52
!     ============
!
   j = 5
   CALL spag_block_1
   RETURN
!
!
   ENTRY sdum42
!     ============
!
   j = 4
   CALL spag_block_1
   RETURN
!
!
   ENTRY sdum32
!     ============
!
   j = 3
   CALL spag_block_1
   RETURN
!
!
   ENTRY sdum22
!     ============
!
   j = 2
   CALL spag_block_1
   RETURN
!
!
   ENTRY sdum12
!     ============
!
   j = 1
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!     GO TO 10
!
      IF ( ii(j)==0 ) THEN
         ii(j) = 1
         WRITE (nout,99001) uwm , jj , kk(j)
99001    FORMAT (A25,' 2182, SUBROUTINE ',2A4,' IS DUMMY.  ONLY ONE OF ','THESE MESSAGES WILL APPEAR PER OVERLAY OF THIS DECK.')
      ENDIF
   END SUBROUTINE spag_block_1
END SUBROUTINE sdumx2
