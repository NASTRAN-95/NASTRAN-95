!*==numtyp.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION numtyp(Ivalue)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: numtyp
   INTEGER :: Ivalue
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(2) , DIMENSION(4) :: byte
   INTEGER :: i
   CHARACTER(8) :: word
!
! End of declarations rewritten by SPAG
!
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
            IF ( byte(i)<'1F' .OR. byte(i)>'5E' ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
         ENDDO
!
!     VALUE IS ALPHA
!
         numtyp = 3
      ENDIF
   ENDIF
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
!     VALUE IS REAL
!
      numtyp = 2
   END SUBROUTINE spag_block_1
!
!*****
END FUNCTION numtyp
