!*==timtst.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE timtst
   USE c_blank
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
   EXTERNAL timts1 , timts2
!
! End of declarations rewritten by SPAG
!
!
!     TIMETEST   /,/ C,N,N / C,N,M / C,N,T / C,N,O1 / C,N,O2 $
!
!
   IF ( o1<1 .OR. o1>2 ) THEN
!
!     ERROR MESSAGES
!
      WRITE (nout,99001) uwm
99001 FORMAT (A25,' 2195, ILLEGAL VALUE FOR P4 =',I7)
!
      WRITE (nout,99002)
99002 FORMAT ('0*** MODULE TIMETEST TERMINAL ERROR.')
      RETURN
   ELSEIF ( o1==2 ) THEN
!
      CALL timts2
   ELSE
!
      CALL timts1
   ENDIF
!
!
!
END SUBROUTINE timtst
