!*==mdumx.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mdumx
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
   DATA ii/9*0/ , jj/4HMDUM/ , kk/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9/
!
   RETURN
!
!
   ENTRY mdum9
!     ===========
!
   j = 9
   CALL spag_block_1
   RETURN
!
!
   ENTRY mdum8
!     ==========
!
   j = 8
   CALL spag_block_1
   RETURN
!
!
   ENTRY mdum7
!     ==========
!
   j = 7
   CALL spag_block_1
   RETURN
!
!
   ENTRY mdum6
!     ==========
!
   j = 6
   CALL spag_block_1
   RETURN
!
!
   ENTRY mdum5
!     ==========
!
   j = 5
   CALL spag_block_1
   RETURN
!
!
   ENTRY mdum4
!     ==========
!
   j = 4
   CALL spag_block_1
   RETURN
!
!
   ENTRY mdum3
!     ==========
!
   j = 3
   CALL spag_block_1
   RETURN
!
!
   ENTRY mdum2
!     ==========
!
   j = 2
   CALL spag_block_1
   RETURN
!
!
   ENTRY mdum1
!     ==========
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
END SUBROUTINE mdumx
