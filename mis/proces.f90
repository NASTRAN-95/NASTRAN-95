
SUBROUTINE proces(X)
   IMPLICIT NONE
   REAL Alpha , Aver(3) , Beta , Beta13 , Beta2 , Consts(5) , Cstm(3,3) , D(3) , G(3,3) , Gamma , Max(3) , Min(3) , Pbufsz ,        &
      & Penpap(30) , Ploter(5) , Rad , Scale(5) , Sign(3) , Skpcom(5) , Vanput(8) , View(4)
   INTEGER Axes(6) , Axis(3) , Ngpset , Prject
   COMMON /blank / Skpcom , Ngpset
   COMMON /condas/ Consts
   COMMON /drwaxs/ G
   COMMON /rstxxx/ Cstm , Min , Max , D , Aver , Axis , Sign
   COMMON /xxparm/ Pbufsz , Ploter , Penpap , Scale , Axes , Alpha , Beta , Gamma , Beta13 , Beta2 , View , Vanput , Prject
   REAL X(3,1)
   REAL cosa , cosb , cosg , sina , sinb , sing , val , xmax(3) , xmin(3)
   INTEGER gp , i , j
   DOUBLE PRECISION sum , v(3)
!
   !>>>>EQUIVALENCE (Consts(3),Rad)
!
!     INITIALIZATION.
!
   DO i = 1 , 3
      Axis(i) = iabs(Axes(i))
      Sign(i) = 1.
      IF ( Axes(i)<0 ) Sign(i) = -1.
      Min(i) = +1.E+20
      Max(i) = -1.E+20
      IF ( Prject==3 ) THEN
         xmin(i) = +1.E+20
         xmax(i) = -1.E+20
      ENDIF
   ENDDO
!
!     CALCULATE THE CO-ORDINATE SYSTEM ROTATION MATRIX.
!
   IF ( Beta<=-1.E+10 ) THEN
      IF ( Prject/=2 ) Beta = Beta13
      IF ( Prject==2 ) Beta = Beta2
   ENDIF
   sina = sin(Alpha/Rad)
   sinb = sin(Beta/Rad)
   sing = sin(Gamma/Rad)
   cosa = cos(Alpha/Rad)
   cosb = cos(Beta/Rad)
   cosg = cos(Gamma/Rad)
!
   Cstm(1,1) = cosb*cosg
   Cstm(2,1) = cosa*sing + sina*sinb*cosg
   Cstm(3,1) = sina*sing - cosa*sinb*cosg
   Cstm(1,2) = -cosb*sing
   Cstm(2,2) = cosa*cosg - sina*sinb*sing
   Cstm(3,2) = sina*cosg + cosa*sinb*sing
   Cstm(1,3) = sinb
   Cstm(2,3) = -sina*cosb
   Cstm(3,3) = cosa*cosb
!
!     SWITCH AXES + ROTATE THE GRID POINT CO-ORDINATES.
!
   DO gp = 1 , Ngpset
      DO i = 1 , 3
         j = Axis(i)
         v(i) = Sign(i)*X(j,gp)
         IF ( Prject==3 ) THEN
            val = v(i)
            xmin(i) = amin1(xmin(i),val)
            xmax(i) = amax1(xmax(i),val)
         ENDIF
      ENDDO
      DO j = 1 , 3
         sum = 0.D0
         DO i = 1 , 3
            sum = sum + Cstm(j,i)*v(i)
         ENDDO
         val = sum
         X(j,gp) = val
         Min(j) = amin1(Min(j),val)
         Max(j) = amax1(Max(j),val)
      ENDDO
   ENDDO
!
!     CALCULATE THE MINIMA-MAXIMA DIFFERENCES + AVERAGES.
!
   DO i = 1 , 3
      IF ( Prject/=3 ) D(i) = Max(i) - Min(i)
      IF ( Prject==3 ) D(i) = xmax(i) - xmin(i)
      Aver(i) = (Max(i)+Min(i))/2.
   ENDDO
!
!     CREATE A X-Y-Z UNIT COORDINATES IN /DRWAXS/ FOR VIEW PLOTTING
!
   DO i = 1 , 9
      G(i,1) = 0.0
   ENDDO
   G(1,1) = 1.0
   G(2,2) = 1.0
   G(3,3) = 1.0
!
   DO gp = 1 , 3
      DO i = 1 , 3
         j = Axis(i)
         v(i) = Sign(i)*G(j,gp)
      ENDDO
      DO j = 1 , 3
         sum = 0.D0
         DO i = 1 , 3
            sum = sum + Cstm(j,i)*v(i)
         ENDDO
         G(j,gp) = sum
      ENDDO
   ENDDO
!
END SUBROUTINE proces