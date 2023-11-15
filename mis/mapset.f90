
SUBROUTINE mapset(X1,Y1,X2,Y2,Ki1,Kj1,Ki2,Kj2,L)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Ki , Ki1 , Ki2 , Kj , Kj1 , Kj2 , L
   REAL X , X1 , X2 , Y , Y1 , Y2
!
! Local variable declarations
!
   REAL a , b , c , d , zi , zi1 , zi2 , zj , zj1 , zj2
   INTEGER i , i1 , i2 , j , j1 , j2 , ll
!
! End of declarations
!
!
!     POINT 1 IS LOWER LEFT CORNER OF FRAME
!     POINT 2 IS UPPER RIGHT CORNER OF FRAME
!     I,J ARE IN PLOTTER UNITS
!     X,Y ARE IN PHYSICAL UNITS
!     L IS OUTPUT FLAG, 1=I,J ARE INTEGER, 2=I,J ARE REAL
!
   EQUIVALENCE (i1,zi1) , (j1,zj1) , (i2,zi2) , (j2,zj2)
   EQUIVALENCE (i,zi) , (j,zj)
!
   i1 = Ki1
   j1 = Kj1
   i2 = Ki2
   j2 = Kj2
   ll = L
!
   IF ( L==2 ) THEN
      a = (zi2-zi1)/(X2-X1)
      b = zi1 - a*X1
      c = (zj2-zj1)/(Y2-Y1)
      d = zj1 - c*Y1
      RETURN
   ELSE
      a = float(i2-i1)/(X2-X1)
      b = float(i1) - a*X1
      c = float(j2-j1)/(Y2-Y1)
      d = float(j1) - c*Y1
      RETURN
   ENDIF
!
!
!***********************************************************************
!
   ENTRY map(X,Y,Ki,Kj)
   IF ( ll==2 ) THEN
      zi = a*X + b
      zj = c*Y + d
   ELSE
      i = a*X + b + 0.5
      j = c*Y + d + 0.5
   ENDIF
   Ki = i
   Kj = j
!
END SUBROUTINE mapset
