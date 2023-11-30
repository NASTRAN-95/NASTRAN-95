
FUNCTION korsz(I)
   IMPLICIT NONE
   INTEGER Lastad , Lout
   COMMON /logout/ Lout
   COMMON /lstadd/ Lastad
   INTEGER I
   INTEGER korsz
   INTEGER l13
   INTEGER locfx
   korsz = Lastad - locfx(I) + 1
   CALL sswtch(13,l13)
   IF ( l13/=0 ) WRITE (Lout,99001) korsz
99001 FORMAT (22X,' --- OPEN CORE =',I8,' WORDS (DECIMAL) ---')
   RETURN
END FUNCTION korsz
