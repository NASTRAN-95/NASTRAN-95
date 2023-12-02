!*==proces.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE proces(X)
   USE c_blank
   USE c_condas
   USE c_drwaxs
   USE c_rstxxx
   USE c_xxparm
   USE iso_fortran_env
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
      axis(i) = iabs(axes(i))
      sign(i) = 1.
      IF ( axes(i)<0 ) sign(i) = -1.
      min(i) = +1.E+20
      max(i) = -1.E+20
      IF ( prject==3 ) THEN
         xmin(i) = +1.E+20
         xmax(i) = -1.E+20
      ENDIF
   ENDDO
!
!     CALCULATE THE CO-ORDINATE SYSTEM ROTATION MATRIX.
!
   IF ( beta<=-1.E+10 ) THEN
      IF ( prject/=2 ) beta = beta13
      IF ( prject==2 ) beta = beta2
   ENDIF
   sina = sin(alpha/rad)
   sinb = sin(beta/rad)
   sing = sin(gamma/rad)
   cosa = cos(alpha/rad)
   cosb = cos(beta/rad)
   cosg = cos(gamma/rad)
!
   cstm(1,1) = cosb*cosg
   cstm(2,1) = cosa*sing + sina*sinb*cosg
   cstm(3,1) = sina*sing - cosa*sinb*cosg
   cstm(1,2) = -cosb*sing
   cstm(2,2) = cosa*cosg - sina*sinb*sing
   cstm(3,2) = sina*cosg + cosa*sinb*sing
   cstm(1,3) = sinb
   cstm(2,3) = -sina*cosb
   cstm(3,3) = cosa*cosb
!
!     SWITCH AXES + ROTATE THE GRID POINT CO-ORDINATES.
!
   DO gp = 1 , ngpset
      DO i = 1 , 3
         j = axis(i)
         v(i) = sign(i)*X(j,gp)
         IF ( prject==3 ) THEN
            val = v(i)
            xmin(i) = amin1(xmin(i),val)
            xmax(i) = amax1(xmax(i),val)
         ENDIF
      ENDDO
      DO j = 1 , 3
         sum = 0.D0
         DO i = 1 , 3
            sum = sum + cstm(j,i)*v(i)
         ENDDO
         val = sum
         X(j,gp) = val
         min(j) = amin1(min(j),val)
         max(j) = amax1(max(j),val)
      ENDDO
   ENDDO
!
!     CALCULATE THE MINIMA-MAXIMA DIFFERENCES + AVERAGES.
!
   DO i = 1 , 3
      IF ( prject/=3 ) d(i) = max(i) - min(i)
      IF ( prject==3 ) d(i) = xmax(i) - xmin(i)
      aver(i) = (max(i)+min(i))/2.
   ENDDO
!
!     CREATE A X-Y-Z UNIT COORDINATES IN /DRWAXS/ FOR VIEW PLOTTING
!
   DO i = 1 , 9
      g(i,1) = 0.0
   ENDDO
   g(1,1) = 1.0
   g(2,2) = 1.0
   g(3,3) = 1.0
!
   DO gp = 1 , 3
      DO i = 1 , 3
         j = axis(i)
         v(i) = sign(i)*g(j,gp)
      ENDDO
      DO j = 1 , 3
         sum = 0.D0
         DO i = 1 , 3
            sum = sum + cstm(j,i)*v(i)
         ENDDO
         g(j,gp) = sum
      ENDDO
   ENDDO
!
END SUBROUTINE proces
