!*==ifp5a.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp5a(Num)
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Num
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   EXTERNAL page2
!
! End of declarations rewritten by SPAG
!
!
!     IFP5A PRINTS MESSAGE NUMBER LINE ONLY.
!     CALLING SUBROUTINE PRINTS THE MESSAGE.
!
!
   CALL page2(4)
   i = Num + 4080
   WRITE (output,99001) ufm , i
99001 FORMAT (A23,I15,1H.)
   nogo = .TRUE.
END SUBROUTINE ifp5a
