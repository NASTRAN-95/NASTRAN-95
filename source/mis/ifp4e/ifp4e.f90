!*==ifp4e.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp4e(Id)
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Id
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: nogo
!
! End of declarations rewritten by SPAG
!
!
!     IFP4E, CALLED BY IFP4, CHECKS TO SEE THAT ID IS WITHIN PERMISSABLE
!     RANGE OF FROM 1 TO 499999.
!
!
   IF ( Id>=1 ) THEN
      IF ( Id<=499999 ) RETURN
   ENDIF
!
!     ERROR
!
   nogo = .TRUE.
   WRITE (output,99001) ufm , Id
99001 FORMAT (A23,' 4041, ID =',I12,' IS OUT OF PERMISSIBLE RANGE OF 1',' TO 499999.')
END SUBROUTINE ifp4e
