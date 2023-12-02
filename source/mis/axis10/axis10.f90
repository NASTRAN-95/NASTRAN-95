!*==axis10.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE axis10(X1,Y1,X2,Y2,Penden,Opt)
   IMPLICIT NONE
   USE C_PLTDAT
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: X1
   REAL :: Y1
   REAL :: X2
   REAL :: Y2
   INTEGER :: Penden
   INTEGER :: Opt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(6) :: a
   INTEGER , SAVE :: axis , optx
   INTEGER :: i , j
   REAL , DIMENSION(2,2) :: xy
   EXTERNAL wplt10
!
! End of declarations rewritten by SPAG
!
!
!     (X1,Y1) = STARTING POINT OF THE AXIS.
!     (X2,Y2) = TERMINAL POINT OF THE AXIS.
!     PENDEN  = PEN NUMBER OR LINE DENSITY.
!     OPT     = -1 TO INITIATE  THE AXIS MODE.
!     ...     = +1 TO TERMINATE THE AXIS MODE.
!     ...     =  0 TO DRAW AN AXIS.
!
   DATA optx/ - 1/
   DATA axis/6/
!
   IF ( optx>=0 ) optx = Opt
   IF ( Opt<0 ) THEN
   ELSEIF ( Opt==0 ) THEN
      xy(1,1) = X1
      xy(2,1) = Y1
      xy(1,2) = X2
      xy(2,2) = Y2
      DO j = 1 , 2
         DO i = 1 , 2
            IF ( xy(i,j)<Xymin(i) ) xy(i,j) = Xymin(i)
            IF ( xy(i,j)>Xymax(i) ) xy(i,j) = Xymax(i)
         ENDDO
      ENDDO
!
!     DRAW THE AXIS.
!
      a(1) = axis
      a(2) = Penden
      DO j = 1 , 2
         a(2*j+1) = xy(1,j) + .1
         a(2*j+2) = xy(2,j) + .1
      ENDDO
      IF ( optx/=0 ) THEN
!
!     INITIATE THE AXIS MODE.
!
         a(1) = a(1) + 10
         optx = 0
      ENDIF
!
!     DRAW THE LINE.
!
      CALL wplt10(a,0)
   ELSE
!
!
!     TERMINATE THE LINE MODE.
!
      CALL wplt10(a,1)
      optx = -1
   ENDIF
!
END SUBROUTINE axis10
