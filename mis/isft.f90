
INTEGER FUNCTION isft(Bf,Sft,J)
   IMPLICIT NONE
   INTEGER Bf , J , Sft
   INTEGER lshift , rshift
   EXTERNAL lshift , rshift
!
!
   IF ( J==4 ) THEN
      isft = lshift(Bf,Sft)
      GOTO 99999
   ENDIF
   isft = rshift(Bf,Sft)
   RETURN
99999 RETURN
END FUNCTION isft
