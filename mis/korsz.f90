
FUNCTION korsz(I)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Lastad , Lout
   COMMON /logout/ Lout
   COMMON /lstadd/ Lastad
!
! Dummy argument declarations
!
   INTEGER I
   INTEGER korsz
!
! Local variable declarations
!
   INTEGER l13
   INTEGER locfx
!
! End of declarations
!
   korsz = Lastad - locfx(I) + 1
   CALL sswtch(13,l13)
   IF ( l13/=0 ) WRITE (Lout,99001) korsz
99001 FORMAT (22X,' --- OPEN CORE =',I8,' WORDS (DECIMAL) ---')
   RETURN
END FUNCTION korsz
