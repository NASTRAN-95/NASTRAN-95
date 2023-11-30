
FUNCTION numtyp(Ivalue)
   IMPLICIT NONE
   INTEGER Ivalue
   INTEGER numtyp
   CHARACTER*2 byte(4)
   INTEGER i
   CHARACTER*8 word
!
 
!
   !>>>>EQUIVALENCE (byte,word)
!
!      WRITE(6,40646) IVALUE
99001 FORMAT (' NUMTYP,IVALUE=',Z9)
   IF ( Ivalue==0 ) THEN
!
!     VALUE IS ZERO
!
      numtyp = 0
   ELSE
      WRITE (word,99002) Ivalue
!*****
99002 FORMAT (Z8)
      IF ( byte(1)=='  ' ) THEN
!
!     VALUE IS INTEGER
!
         numtyp = 1
      ELSEIF ( byte(1)=='00' ) THEN
         numtyp = 1
      ELSEIF ( (byte(1)=='07' .OR. byte(1)==' 7') .AND. byte(2)=='FF' .AND. byte(3)=='FF' .AND. byte(4)=='FF' ) THEN
         numtyp = 1
      ELSEIF ( byte(1)=='7F' .AND. byte(2)=='FF' .AND. byte(3)=='FF' .AND. byte(4)=='FF' ) THEN
         numtyp = 1
 
      ELSEIF ( byte(1)=='FF' ) THEN
         numtyp = 1
      ELSE
         DO i = 1 , 4
            IF ( byte(i)<'1F' .OR. byte(i)>'5E' ) GOTO 100
         ENDDO
!
!     VALUE IS ALPHA
!
         numtyp = 3
      ENDIF
   ENDIF
   GOTO 200
!
!     VALUE IS REAL
!
 100  numtyp = 2
!
 200  RETURN
!*****
END FUNCTION numtyp