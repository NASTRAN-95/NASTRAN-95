
SUBROUTINE dshxdd(Ii,Iarr,Len)
   IMPLICIT NONE
   INTEGER Iibuff(2048)
   COMMON /dsbuff/ Iibuff
   INTEGER Ii , Len
   INTEGER Iarr(10000)
   INTEGER i , k
   DO k = 1 , Len
      Iibuff(k) = Iarr(k)
   ENDDO
   WRITE (6,99001) Ii , (Iibuff(i),i=1,Len)
99001 FORMAT (I5,200(8(1X,Z8),/))
END SUBROUTINE dshxdd
