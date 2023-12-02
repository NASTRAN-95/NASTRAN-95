!*==korsz.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION korsz(I)
   IMPLICIT NONE
   USE C_LOGOUT
   USE C_LSTADD
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
   korsz = Lastad - locfx(I) + 1
   CALL sswtch(13,l13)
   IF ( l13/=0 ) WRITE (Lout,99001) korsz
99001 FORMAT (22X,' --- OPEN CORE =',I8,' WORDS (DECIMAL) ---')
END FUNCTION korsz
