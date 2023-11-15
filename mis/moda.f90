
SUBROUTINE moda
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Iparm(5) , Parm12 , Parm13
   REAL Parm(5) , Parm11
   COMMON /blank / Parm , Iparm , Parm11 , Parm12 , Parm13
!
! End of declarations
!
!
!*****
!
!     DUMMY DECK FOR MODULE MODA   SEE USERS MANUAL SECTION 5.3
!                                  FOR MODULE PROPERTIES CHECK XMPLBD
!                                  OR USE DIAG 29
!
!*****
!
!     INTEGER OUTFIL(4)
!
!
!     DATA OUTFIL /201,202,203,204/
!
END SUBROUTINE moda
