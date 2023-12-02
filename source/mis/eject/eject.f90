!*==eject.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION eject(Lines)
   USE c_system
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: eject
   INTEGER :: Lines
   EXTERNAL page1
!
! End of declarations rewritten by SPAG
!
!
!     LINES = NUNBER OF LINES TO BE PRINTED.
!     RESULT = 1 IF NEW PAGE IS STARTED.
!
   eject = 0
   IF ( lincnt+Lines+2>maxlin ) THEN
      CALL page1
      eject = 1
   ENDIF
END FUNCTION eject
