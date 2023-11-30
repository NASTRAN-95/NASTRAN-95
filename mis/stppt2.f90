
SUBROUTINE stppt2(Input,W1jk,W2jk)
   IMPLICIT NONE
   INTEGER Ii , Incr , It0 , Iti , Nn
   REAL Tw1jk(7) , Tw2jk(7)
   COMMON /amgp2 / Tw1jk , Tw2jk
   COMMON /packx / Iti , It0 , Ii , Nn , Incr
   INTEGER Input , W1jk , W2jk
   INTEGER i , nj
   COMPLEX one , zero
   one = (1.0,0.0)
   zero = (0.0,0.0)
   CALL fread(Input,nj,1,1)
   DO i = 1 , nj
      Nn = Ii
      CALL pack(one,W1jk,Tw1jk)
      CALL pack(zero,W2jk,Tw2jk)
      Ii = Ii + 1
   ENDDO
END SUBROUTINE stppt2
