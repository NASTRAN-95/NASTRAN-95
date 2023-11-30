
SUBROUTINE permut(Ia,Id,N,Isw)
   IMPLICIT NONE
   INTEGER Isw , N
   INTEGER Ia(1) , Id(10)
   INTEGER i , i1 , ib(32) , ic(32) , is1 , j , k , l , n1
   DO i = 1 , N
      ic(i) = Ia(i)
      ib(i) = i
   ENDDO
   n1 = N - 1
   DO i = 1 , n1
      i1 = i + 1
      DO j = i1 , N
         IF ( ic(j)<ic(i) ) THEN
            is1 = ib(j)
            ib(j) = ib(i)
            ib(i) = is1
            is1 = ic(j)
            ic(j) = ic(i)
            ic(i) = is1
         ENDIF
      ENDDO
   ENDDO
   DO i = 1 , N
      IF ( ic(i)>=Isw ) GOTO 100
   ENDDO
   k = 1
   GOTO 200
 100  DO j = i , N
      k = j - i + 1
      Id(k) = ib(j)
   ENDDO
   IF ( k==N ) GOTO 99999
   k = k + 1
 200  DO j = k , N
      l = j - k + 1
      Id(j) = ib(l)
   ENDDO
99999 RETURN
END SUBROUTINE permut