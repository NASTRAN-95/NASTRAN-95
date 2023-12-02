!*==korsz.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION korsz(I)
   USE c_logout
   USE c_lstadd
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: korsz
   INTEGER :: I
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: l13
   EXTERNAL locfx , sswtch
!
! End of declarations rewritten by SPAG
!
   korsz = lastad - locfx(I) + 1
   CALL sswtch(13,l13)
   IF ( l13/=0 ) WRITE (lout,99001) korsz
99001 FORMAT (22X,' --- OPEN CORE =',I8,' WORDS (DECIMAL) ---')
END FUNCTION korsz
