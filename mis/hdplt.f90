
SUBROUTINE hdplt(X1,Y1,Ij,Im)
   IMPLICIT NONE
   REAL Dum(3)
   INTEGER Ibuf , Nout , Ppen
   COMMON /drwdat/ Dum , Ppen
   COMMON /system/ Ibuf , Nout
   INTEGER Ij , Im
   REAL X1(4) , Y1(4)
   LOGICAL debug
   INTEGER i , j
   REAL xold , xvalue , yold , yvalue
!
!     PLOTS POINTS GOVERNED BY THE VALUE OF IM.
!
!     NOTE THAT CALL PLOT(X,Y,2) MEANS MOVE PEN FROM THE CURRENT
!     POSITION TO THE POINT,(X,Y),WITH THE PEN DOWN.
!
!     CALL PLOT(X,Y,3) MEANS MOVE THE PEN FROM THE CURRENT POSITION
!     TO THE POINT,(X,Y), WITH THE PEN UP.
!
   DATA debug/.FALSE./
!
   IF ( debug ) WRITE (Nout,99001) Ij , Im , (X1(i),i=1,4) , (Y1(j),j=1,4)
99001 FORMAT (7H HDPLT ,2I3,8F12.5)
   IF ( Im==1 ) THEN
      Ij = 0
   ELSE
      xvalue = (X1(2))/X1(4)
      yvalue = (Y1(2))/Y1(4)
      IF ( Ij==0 ) THEN
         xold = xvalue
         yold = yvalue
         Ij = 1
      ELSE
         CALL line(xold,yold,xvalue,yvalue,Ppen,0)
         xold = xvalue
         yold = yvalue
      ENDIF
   ENDIF
END SUBROUTINE hdplt