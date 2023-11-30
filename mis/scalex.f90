
SUBROUTINE scalex(Ilval,Code,L)
   IMPLICIT NONE
   INTEGER Code , Ilval
   INTEGER L(1)
   INTEGER expnd(6) , i , id , ii , inv , j , k
   DO i = 1 , 6
      L(i) = 0
   ENDDO
   IF ( Code<=0 ) THEN
      L(1) = Ilval
   ELSE
      id = Code
      DO i = 1 , 6
         inv = 7 - i
         expnd(inv) = mod(id,10)
         id = id/10
      ENDDO
      j = 0
      DO i = 1 , 6
         IF ( expnd(i)/=0 ) THEN
            IF ( i>=2 ) THEN
               ii = i - 1
               DO k = 1 , ii
                  IF ( expnd(k)==expnd(i) ) GOTO 50
               ENDDO
            ENDIF
            j = j + 1
            L(j) = expnd(i)
         ENDIF
 50   ENDDO
      i = 0
      DO
         i = i + 1
         L(i) = Ilval + L(i) - 1
         IF ( i>=j ) EXIT
      ENDDO
   ENDIF
END SUBROUTINE scalex
