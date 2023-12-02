!*==centre.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE centre(X1,Y1,X2,Y2,X3,Y3,X4,Y4,Center) !HIDESTARS (*,X1,Y1,X2,Y2,X3,Y3,X4,Y4,Center)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: X1
   REAL :: Y1
   REAL :: X2
   REAL :: Y2
   REAL :: X3
   REAL :: Y3
   REAL :: X4
   REAL :: Y4
   REAL , DIMENSION(2) :: Center
!
! Local variable declarations rewritten by SPAG
!
   REAL :: xm1 , xm2
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
   IF ( X1==X3 .AND. X2==X4 ) THEN
      Center(1) = X1
      Center(2) = (amax1(Y1,Y2,Y3,Y4)+amin1(Y1,Y2,Y3,Y4))/2.0
      RETURN 1
   ELSEIF ( X1==X3 ) THEN
      Center(1) = X1
      Center(2) = (Y4-Y2)*(Center(1)-X2)/(X4-X2) + Y2
      RETURN
   ELSEIF ( X2/=X4 ) THEN
      xm1 = (Y2-Y4)/(X2-X4)
      xm2 = (Y1-Y3)/(X1-X3)
      IF ( xm1/=xm2 ) THEN
         Center(1) = (Y1-xm2*X1-(Y2-xm1*X2))/(xm1-xm2)
         Center(2) = xm1*(Center(1)-X2) + Y2
         RETURN
      ENDIF
   ELSE
      Center(1) = X2
      Center(2) = (Y3-Y1)*(Center(1)-X1)/(X3-X1) + Y1
      RETURN
   ENDIF
   Center(1) = (X1+X2)/2.0
   Center(2) = (Y1+Y2)/2.0
   RETURN 1
END SUBROUTINE centre
