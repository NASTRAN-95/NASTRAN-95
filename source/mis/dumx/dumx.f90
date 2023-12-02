!*==dumx.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dumx
   USE C_SYSTEM
   USE C_XMSSG
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
   RETURN
!
!
   ENTRY dum9(Icore)
!     ==========
!
   j = 9
   CALL spag_block_1
   RETURN
!
!
   ENTRY dum8(Icore)
!     ==========
!
   j = 8
   CALL spag_block_1
   RETURN
!
!
   ENTRY dum7(Icore)
!     ==========
!
   j = 7
   CALL spag_block_1
   RETURN
!
!
   ENTRY dum6(Icore)
!     ==========
!
   j = 6
   CALL spag_block_1
   RETURN
!
!
   ENTRY dum5(Icore)
!     ==========
!
   j = 5
   CALL spag_block_1
   RETURN
!
!
   ENTRY dum4(Icore)
!     ==========
!
   j = 4
   CALL spag_block_1
   RETURN
!
!
   ENTRY dum3(Icore)
!     ==========
!
   j = 3
   CALL spag_block_1
   RETURN
!
!
   ENTRY dum2(Icore)
!     ==========
!
   j = 2
   CALL spag_block_1
   RETURN
!
!
   ENTRY dum1(Icore)
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
         WRITE (Nout,99001) Uwm , jj , kk(j)
99001    FORMAT (A25,' 2182, SUBROUTINE ',2A4,' IS DUMMY.  ONLY ONE OF ','THESE MESSAGES WILL APPEAR PER OVERLAY OF THIS DECK.')
      ENDIF
   END SUBROUTINE spag_block_1
END SUBROUTINE dumx
