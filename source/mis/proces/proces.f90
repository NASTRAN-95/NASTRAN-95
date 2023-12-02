!*==proces.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE proces(X)
USE C_BLANK
USE C_CONDAS
USE C_DRWAXS
USE C_RSTXXX
USE C_XXPARM
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(3,1) :: X
!
! Local variable declarations rewritten by SPAG
!
   REAL :: cosa , cosb , cosg , rad , sina , sinb , sing , val
   INTEGER :: gp , i , j
   REAL(REAL64) :: sum
   REAL(REAL64) , DIMENSION(3) :: v
   REAL , DIMENSION(3) :: xmax , xmin
!
! End of declarations rewritten by SPAG
!
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
   sina = sin(Alpha/rad)
   sinb = sin(Beta/rad)
   sing = sin(Gamma/rad)
   cosa = cos(Alpha/rad)
   cosb = cos(Beta/rad)
   cosg = cos(Gamma/rad)
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
