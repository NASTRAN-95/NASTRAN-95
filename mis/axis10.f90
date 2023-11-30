
SUBROUTINE axis10(X1,Y1,X2,Y2,Penden,Opt)
   IMPLICIT NONE
   REAL Skpplt(2) , Xymax(2) , Xymin(2)
   COMMON /pltdat/ Skpplt , Xymin , Xymax
   INTEGER Opt , Penden
   REAL X1 , X2 , Y1 , Y2
   INTEGER a(6) , axis , i , j , optx
   REAL xy(2,2)
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