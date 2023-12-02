!*==ifp5a.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp5a(Num)
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_XMSSG
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
   WRITE (Output,99001) Ufm , i
99001 FORMAT (A23,I15,1H.)
   Nogo = .TRUE.
END SUBROUTINE ifp5a
