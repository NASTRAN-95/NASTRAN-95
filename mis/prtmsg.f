      SUBROUTINE PRTMSG
C
      COMMON /ZZZZZZ/ BUF(1)
      COMMON /OUTPUT/ TITLE(32,6)
C
      DATA INPREW,MSG,BLANK / 0,101,4H    /
C
      CALL OPEN (*110,MSG,BUF,INPREW)
      CALL READ (*110,*110,MSG,0,0,1,J)
      DO 100 J = 4,6
      DO 100 I = 1,32
      TITLE(I,J) = BLANK
  100 CONTINUE
      CALL WRTMSG (MSG)
  110 RETURN
      END
