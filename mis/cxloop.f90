
SUBROUTINE cxloop(X,Y,N)
   IMPLICIT NONE
   INTEGER M , N
   DOUBLE PRECISION Mpy(2) , X(1) , Xx(2) , Y(1) , Yy(2)
   INTEGER i , mm , nn
   nn = N + N
   DO i = 1 , nn
      X(i) = Y(i)
   ENDDO
   RETURN
   ENTRY cloop(Xx,Yy,Mpy,M)
   mm = M + M
   DO i = 1 , mm , 2
      Xx(i) = Xx(i) - Mpy(1)*Yy(i) + Mpy(2)*Yy(i+1)
      Xx(i+1) = Xx(i+1) - Mpy(2)*Yy(i) - Mpy(1)*Yy(i+1)
   ENDDO
END SUBROUTINE cxloop