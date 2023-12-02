!*==dumx.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dumx
USE c_system
USE c_xmssg
USE C_SYSTEM
USE C_XMSSG
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   *0() :: 
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: Icore
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
      USE C_SYSTEM
      USE C_XMSSG
!     GO TO 10
!
      IF ( Ii(J)==0 ) THEN
         Ii(J) = 1
         WRITE (Nout,99001) Uwm , Jj , Kk(J)
99001    FORMAT (A25,' 2182, SUBROUTINE ',2A4,' IS DUMMY.  ONLY ONE OF ','THESE MESSAGES WILL APPEAR PER OVERLAY OF THIS DECK.')
      ENDIF
   END SUBROUTINE spag_block_1
END SUBROUTINE dumx
