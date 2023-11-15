
INTEGER FUNCTION isft(Bf,Sft,J)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Bf , J , Sft
!
! Local variable declarations
!
   INTEGER lshift , rshift
   EXTERNAL lshift , rshift
!
! End of declarations
!
!
!
   IF ( J==4 ) THEN
      isft = lshift(Bf,Sft)
      GOTO 99999
   ENDIF
   isft = rshift(Bf,Sft)
   RETURN
99999 END FUNCTION isft
