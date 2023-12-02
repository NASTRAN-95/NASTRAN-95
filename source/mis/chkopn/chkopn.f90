!*==chkopn.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE chkopn(Name)
   USE c_sofcom
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Name
   EXTERNAL mesage
!
! End of declarations rewritten by SPAG
!
!
!     CHECKS IF A CALL TO SOFOPN HAS BEEN MADE.
!
!
   IF ( .NOT.(opnsof) ) THEN
      WRITE (nout,99001) ufm , Name
99001 FORMAT (A23,' 6204, SUBROUTINE ',2A4,' - THE SUBROUTINE SOFOPN ','SHOULD BE CALLED PRIOR TO ANY OF THE SOF UTILITY ',         &
             &'SUBROUTINES.')
      CALL mesage(-61,0,0)
   ENDIF
END SUBROUTINE chkopn
