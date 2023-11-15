
SUBROUTINE chkopn(Name)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Nbuff , Nout
   LOGICAL Opnsof
   REAL Sofdum(25)
   CHARACTER*23 Ufm
   COMMON /sofcom/ Sofdum , Opnsof
   COMMON /system/ Nbuff , Nout
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   INTEGER Name(2)
!
! End of declarations
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
