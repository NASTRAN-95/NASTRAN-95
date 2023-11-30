
SUBROUTINE line(X1,Y1,X2,Y2,Penx,Opt)
   IMPLICIT NONE
   INTEGER Model , Npens , Ploter
   REAL Reg(2,2) , Skpa(6) , Skpplt(14)
   COMMON /pltdat/ Model , Ploter , Reg , Skpplt , Skpa , Npens
   INTEGER Opt , Penx
   REAL X1 , X2 , Y1 , Y2
   REAL b , infnty , slp , x , xy(2,2) , y
   INTEGER i , ifl , j , m , pen , tra1 , tra2
!
!     (X1,Y1) = STARTING POINT OF THE LINE
!     (X2,Y2) = TERMINAL POINT OF THE LINE
!     PENX    = PEN NUMBER OR DENSITY (DEPENDING ON PLOTTER)
!     OPT     = -1  TO INITIATE  THE LINE MODE
!             = +1  TO TERMINATE THE LINE MODE
!             =  0  TO DRAW A LINE.
!
   DATA infnty/1.E+10/
!
   IF ( Opt/=0 ) GOTO 1000
   slp = infnty
   b = 0.
   IF ( X1/=X2 ) THEN
      slp = (Y2-Y1)/(X2-X1)
      b = Y1 - slp*X1
   ENDIF
   xy(1,1) = X1
   xy(2,1) = Y1
   xy(1,2) = X2
   xy(2,2) = Y2
!
!     CHECK TO SEE IF AN END OF THE LINE IS OUTSIDE THE PLOT REGION.
!
 100  DO j = 1 , 2
      DO i = 1 , 2
         IF ( xy(i,j)<Reg(i,1) .OR. xy(i,j)>Reg(i,2) ) GOTO 200
      ENDDO
   ENDDO
   GOTO 900
 200  DO i = 1 , 2
      IF ( xy(i,1)<Reg(i,1) .AND. xy(i,2)<Reg(i,1) ) GOTO 1100
      IF ( xy(i,1)>Reg(i,2) .AND. xy(i,2)>Reg(i,2) ) GOTO 1100
   ENDDO
!
!     AN END IS OUTSIDE THE REGION, BUT NOT THE ENTIRE LINE. FIND THE
!     END POINTS OF THE PORTION OF THE LINE WITHIN THE REGION.
!
   j = 1
 300  i = 1
 400  IF ( xy(i,j)>=Reg(i,1) ) GOTO 600
   ASSIGN 500 TO tra2
   IF ( i==2 ) THEN
      ASSIGN 1300 TO tra1
      y = Reg(2,1)
   ELSE
      ASSIGN 1400 TO tra1
      x = Reg(1,1)
   ENDIF
   GOTO 1200
 500  xy(1,j) = x
   xy(2,j) = y
!
 600  IF ( xy(i,j)<=Reg(i,2) ) GOTO 800
   ASSIGN 700 TO tra2
   IF ( i==2 ) THEN
      ASSIGN 1300 TO tra1
      y = Reg(2,2)
   ELSE
      ASSIGN 1400 TO tra1
      x = Reg(1,2)
   ENDIF
   GOTO 1200
 700  xy(1,j) = x
   xy(2,j) = y
 800  i = i + 1
   IF ( i==2 ) GOTO 400
   j = j + 1
   IF ( j==2 ) GOTO 300
!
!     MAKE SURE THE LINE SEGMENT IS WITHIN THE PLOT REGION.
!
   DO j = 1 , 2
      DO i = 1 , 2
         IF ( xy(i,j)+.1<Reg(i,1) .OR. xy(i,j)-.1>Reg(i,2) ) GOTO 99999
      ENDDO
   ENDDO
!
!     FIND THE CORRECT PEN NUMBER FOR THIS PLOTTER.
!
 900  pen = Penx
   pen = pen - Npens*((pen-1)/Npens)
!
!     DRAW THE LINE.
!
 1000 CALL line10(xy(1,1),xy(2,1),xy(1,2),xy(2,2),pen,Opt)
   GOTO 99999
!
 1100 ifl = 0
   DO j = 1 , 2
      DO m = 1 , 2
         IF ( abs(xy(i,j)-Reg(i,m))<=1.0E-8 ) THEN
            ifl = 1
            xy(i,j) = Reg(i,m)
         ENDIF
      ENDDO
   ENDDO
   IF ( ifl>0 ) GOTO 100
   GOTO 99999
!
!
!     CALCULATE THE EQUATION OF THE LINE TO BE DRAWN.
!
 1200 GOTO tra1
!
!     GIVEN Y, CALCULATE X.
!
 1300 IF ( slp==infnty ) THEN
      x = X1
   ELSEIF ( slp==0. ) THEN
      x = infnty
   ELSE
      x = (y-b)/slp
   ENDIF
   GOTO tra2
!
!     GIVEN X, CALCULATE Y.
!
 1400 IF ( slp==infnty ) THEN
      y = infnty
   ELSEIF ( slp==0. ) THEN
      y = Y1
   ELSE
      y = slp*x + b
   ENDIF
   GOTO tra2
!
99999 RETURN
END SUBROUTINE line
