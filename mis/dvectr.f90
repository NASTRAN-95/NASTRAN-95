
SUBROUTINE dvectr(Gpt,X,U,Pen)
   IMPLICIT NONE
   INTEGER Ngp
   COMMON /blank / Ngp
   INTEGER Pen
   INTEGER Gpt(1)
   REAL U(2,1) , X(3,1)
   INTEGER i , j
   REAL x1 , x2 , y1 , y2
!
!
   CALL line(0,0,0,0,0,-1)
!
!     DO NOT DRAW A VECTOR AT ANY GRID POINT WHOSE INDEX .LE. 0.
!
   DO i = 1 , Ngp
      j = Gpt(i)
      IF ( j>0 ) THEN
         x1 = X(2,j)
         y1 = X(3,j)
         x2 = U(1,j)
         y2 = U(2,j)
         CALL line(x1,y1,x2,y2,Pen,0)
      ENDIF
   ENDDO
!
   CALL line(0,0,0,0,0,1)
END SUBROUTINE dvectr
