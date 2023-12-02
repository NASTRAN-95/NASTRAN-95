!*==mbgeod.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbgeod
   USE c_mboxa
   USE c_mboxc
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: area1 , area2 , areaw , big , tm , xcent , ycent
   INTEGER :: i
   EXTERNAL mbprit
!
! End of declarations rewritten by SPAG
!
!
!     SUBROUTINE TO COMPUTE GEOMETRY AND INDEXES OF REGIONS
!
!
!     MAIN GEOMETRY
!
   big = -1.0E35
   DO i = 1 , 10
      tang(i) = 0.0
      ang(i) = 0.0
   ENDDO
   y(4) = y(1)
   y(6) = y(3)
!
   IF ( .NOT.(crank1) ) THEN
      x(2) = x(3)
      y(2) = y(3)
      tang(2) = 0.0
   ENDIF
!
   IF ( .NOT.(crank2) ) THEN
      x(5) = x(6)
      y(5) = y(6)
      tang(5) = 0.0
   ENDIF
!
   tang(1) = (x(2)-x(1))/(y(2)-y(1))
   ang(1) = 57.2958*atan(tang(1))
   IF ( crank1 ) tang(2) = (x(3)-x(2))/(y(3)-y(2))
   ang(2) = 57.2958*atan(tang(2))
   tang(4) = (x(5)-x(4))/(y(5)-y(4))
   ang(4) = 57.2958*atan(tang(4))
   IF ( crank2 ) tang(5) = (x(6)-x(5))/(y(6)-y(5))
   ang(5) = 57.2958*atan(tang(5))
!
   areaw = 0.5*(x(1)*(y(1)-y(2))+x(2)*(y(1)-y(3))+x(3)*(y(2)-y(3))+x(4)*(y(5)-y(1))+x(5)*(y(3)-y(1))+x(6)*(y(3)-y(5)))
!
!     CONTROL1 SURFACE GEOMETRY
!
   area1 = 0.0
   IF ( cntrl1 ) THEN
      tang(7) = (x(9)-x(8))/(y(9)-y(8))
      ang(7) = 57.2958*atan(tang(7))
!
      IF ( abs(y(7)-y(8))>0.01 ) THEN
!
         tm = (x(7)-x(8))/(y(7)-y(8))
         IF ( y(5)/=y(7) .OR. x(5)/=x(7) ) THEN
            y(7) = (tm*y(8)-tang(4)*y(4)+x(4)-x(8))/(tm-tang(4))
            IF ( y(7)<=y(5) ) THEN
               x(7) = tang(4)*(y(7)-y(4)) + x(4)
            ELSE
               y(7) = (tm*y(8)-tang(5)*y(5)+x(5)-x(8))/(tm-tang(5))
               x(7) = tang(5)*(y(7)-y(5)) + x(5)
            ENDIF
         ENDIF
      ELSE
         y(7) = y(8)
         tm = big
         IF ( y(7)>y(5) ) THEN
            x(7) = tang(5)*(y(7)-y(5)) + x(5)
         ELSE
            x(7) = tang(4)*(y(7)-y(4)) + x(4)
         ENDIF
      ENDIF
      tang(6) = tm
!
      IF ( abs(y(11)-y(9))>0.01 ) THEN
!
         tm = (x(11)-x(9))/(y(11)-y(9))
         IF ( y(5)/=y(11) .OR. x(5)/=x(11) ) THEN
            y(11) = (tm*y(9)-tang(4)*y(4)+x(4)-x(9))/(tm-tang(4))
            IF ( y(11)<=y(5) ) THEN
               x(11) = tang(4)*(y(11)-y(4)) + x(4)
            ELSE
               y(11) = (tm*y(9)-tang(5)*y(5)+x(5)-x(9))/(tm-tang(5))
               x(11) = tang(5)*(y(11)-y(5)) + x(5)
            ENDIF
         ENDIF
      ELSE
         y(11) = y(9)
         tm = big
         IF ( y(11)>y(5) ) THEN
            x(11) = tang(5)*(y(11)-y(5)) + x(5)
         ELSE
            x(11) = tang(4)*(y(11)-y(4)) + x(4)
         ENDIF
      ENDIF
      tang(8) = tm
!
      IF ( y(7)<=y(5) .AND. y(11)>=y(5) ) THEN
!
         area1 = 0.5*(x(5)*(y(11)-y(7))+x(8)*(y(7)-y(9))+x(9)*(y(8)-y(11))+x(7)*(y(5)-y(8))+x(11)*(y(9)-y(5)))
      ELSE
         area1 = 0.5*((x(8)-x(11))*(y(7)-y(9))+(x(9)-x(7))*(y(8)-y(11)))
      ENDIF
   ENDIF
!
!     CONTROL2 SURFACE GEOMETRY
!
   area2 = 0.0
   IF ( cntrl2 ) THEN
      tang(10) = (x(10)-x(9))/(y(10)-y(9))
      ang(10) = 57.2958*atan(tang(10))
      IF ( abs(y(12)-y(10))>0.01 ) THEN
         tm = (x(12)-x(10))/(y(12)-y(10))
         IF ( y(5)/=y(12) .OR. x(5)/=x(12) ) THEN
            y(12) = (tm*y(10)-tang(4)*y(4)+x(4)-x(10))/(tm-tang(4))
            IF ( y(12)<=y(5) ) THEN
               x(12) = tang(4)*(y(12)-y(4)) + x(4)
            ELSE
               y(12) = (tm*y(10)-tang(5)*y(5)+x(5)-x(10))/(tm-tang(5))
               x(12) = tang(5)*(y(12)-y(5)) + x(5)
            ENDIF
         ENDIF
      ELSE
         y(12) = y(10)
         tm = big
         IF ( y(12)>y(5) ) THEN
            x(12) = tang(5)*(y(12)-y(5)) + x(5)
         ELSE
            x(12) = tang(4)*(y(12)-y(4)) + x(4)
         ENDIF
      ENDIF
      tang(9) = tm
!
      IF ( y(11)<=y(5) .AND. y(12)>=y(5) ) THEN
!
         area2 = 0.5*(x(5)*(y(12)-y(11))+x(9)*(y(11)-y(10))+x(10)*(y(9)-y(12))+x(11)*(y(5)-y(9))+x(12)*(y(10)-y(5)))
      ELSE
         area2 = 0.5*((x(9)-x(12))*(y(11)-y(10))+(x(10)-x(11))*(y(9)-y(12)))
      ENDIF
   ENDIF
!
!     PRINT GEOMETRY DATA
!
   cr = x(4) - x(1)
   CALL mbprit(areaw,area1,area2)
   gc = 2.0*cr**2
   xcent = (x(3)+x(4)+x(6))/4.0
   ycent = y(3)*(0.333+0.167*(x(6)-x(3))/x(4))
!
   DO i = 1 , 10
      IF ( tang(i)==0 ) THEN
         cotang(i) = big
      ELSEIF ( tang(i)/=big ) THEN
         cotang(i) = 1./tang(i)
      ELSE
         cotang(i) = 0.
      ENDIF
   ENDDO
END SUBROUTINE mbgeod
