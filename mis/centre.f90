
SUBROUTINE centre(*,X1,Y1,X2,Y2,X3,Y3,X4,Y4,Center)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL X1 , X2 , X3 , X4 , Y1 , Y2 , Y3 , Y4
   REAL Center(2)
!
! Local variable declarations
!
   REAL xm1 , xm2
!
! End of declarations
!
   IF ( X1==X3 .AND. X2==X4 ) THEN
      Center(1) = X1
      Center(2) = (amax1(Y1,Y2,Y3,Y4)+amin1(Y1,Y2,Y3,Y4))/2.0
      RETURN 1
   ELSEIF ( X1==X3 ) THEN
      Center(1) = X1
      Center(2) = (Y4-Y2)*(Center(1)-X2)/(X4-X2) + Y2
      GOTO 99999
   ELSEIF ( X2/=X4 ) THEN
      xm1 = (Y2-Y4)/(X2-X4)
      xm2 = (Y1-Y3)/(X1-X3)
      IF ( xm1/=xm2 ) THEN
         Center(1) = (Y1-xm2*X1-(Y2-xm1*X2))/(xm1-xm2)
         Center(2) = xm1*(Center(1)-X2) + Y2
         GOTO 99999
      ENDIF
   ELSE
      Center(1) = X2
      Center(2) = (Y3-Y1)*(Center(1)-X1)/(X3-X1) + Y1
      GOTO 99999
   ENDIF
   Center(1) = (X1+X2)/2.0
   Center(2) = (Y1+Y2)/2.0
   RETURN 1
99999 RETURN
END SUBROUTINE centre
