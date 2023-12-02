!*==chkopn.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE chkopn(Name)
   IMPLICIT NONE
   USE C_SOFCOM
   USE C_SYSTEM
   USE C_XMSSG
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
   IF ( .NOT.(Opnsof) ) THEN
      WRITE (Nout,99001) Ufm , Name
99001 FORMAT (A23,' 6204, SUBROUTINE ',2A4,' - THE SUBROUTINE SOFOPN ','SHOULD BE CALLED PRIOR TO ANY OF THE SOF UTILITY ',         &
             &'SUBROUTINES.')
      CALL mesage(-61,0,0)
   ENDIF
END SUBROUTINE chkopn
