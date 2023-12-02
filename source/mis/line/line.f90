!*==line.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE line(X1,Y1,X2,Y2,Penx,Opt)
   USE c_pltdat
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: X1
   REAL :: Y1
   REAL :: X2
   REAL :: Y2
   INTEGER :: Penx
   INTEGER :: Opt
!
! Local variable declarations rewritten by SPAG
!
   REAL :: b , slp , x , y
   INTEGER :: i , ifl , j , m , pen , tra1 , tra2
   REAL , SAVE :: infnty
   REAL , DIMENSION(2,2) :: xy
   EXTERNAL line10
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     (X1,Y1) = STARTING POINT OF THE LINE
!     (X2,Y2) = TERMINAL POINT OF THE LINE
!     PENX    = PEN NUMBER OR DENSITY (DEPENDING ON PLOTTER)
!     OPT     = -1  TO INITIATE  THE LINE MODE
!             = +1  TO TERMINATE THE LINE MODE
!             =  0  TO DRAW A LINE.
!
   DATA infnty/1.E+10/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( Opt/=0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
         spag_nextblock_1 = 2
      CASE (2)
!
!     CHECK TO SEE IF AN END OF THE LINE IS OUTSIDE THE PLOT REGION.
!
         DO j = 1 , 2
            DO i = 1 , 2
               IF ( xy(i,j)<reg(i,1) .OR. xy(i,j)>reg(i,2) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDDO
         spag_nextblock_1 = 8
      CASE (3)
         DO i = 1 , 2
            IF ( xy(i,1)<reg(i,1) .AND. xy(i,2)<reg(i,1) ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( xy(i,1)>reg(i,2) .AND. xy(i,2)>reg(i,2) ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     AN END IS OUTSIDE THE REGION, BUT NOT THE ENTIRE LINE. FIND THE
!     END POINTS OF THE PORTION OF THE LINE WITHIN THE REGION.
!
         j = 1
         spag_nextblock_1 = 4
      CASE (4)
         i = 1
         spag_nextblock_1 = 5
      CASE (5)
         IF ( xy(i,j)>=reg(i,1) ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ASSIGN 20 TO tra2
         IF ( i==2 ) THEN
            ASSIGN 60 TO tra1
            y = reg(2,1)
         ELSE
            ASSIGN 80 TO tra1
            x = reg(1,1)
         ENDIF
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 20      xy(1,j) = x
         xy(2,j) = y
         spag_nextblock_1 = 6
      CASE (6)
!
         IF ( xy(i,j)<=reg(i,2) ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ASSIGN 40 TO tra2
         IF ( i==2 ) THEN
            ASSIGN 60 TO tra1
            y = reg(2,2)
         ELSE
            ASSIGN 80 TO tra1
            x = reg(1,2)
         ENDIF
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 40      xy(1,j) = x
         xy(2,j) = y
         spag_nextblock_1 = 7
      CASE (7)
         i = i + 1
         IF ( i==2 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = j + 1
         IF ( j==2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     MAKE SURE THE LINE SEGMENT IS WITHIN THE PLOT REGION.
!
         DO j = 1 , 2
            DO i = 1 , 2
               IF ( xy(i,j)+.1<reg(i,1) .OR. xy(i,j)-.1>reg(i,2) ) RETURN
            ENDDO
         ENDDO
         spag_nextblock_1 = 8
      CASE (8)
!
!     FIND THE CORRECT PEN NUMBER FOR THIS PLOTTER.
!
         pen = Penx
         pen = pen - npens*((pen-1)/npens)
         spag_nextblock_1 = 9
      CASE (9)
!
!     DRAW THE LINE.
!
         CALL line10(xy(1,1),xy(2,1),xy(1,2),xy(2,2),pen,Opt)
         RETURN
      CASE (10)
!
         ifl = 0
         DO j = 1 , 2
            DO m = 1 , 2
               IF ( abs(xy(i,j)-reg(i,m))<=1.0E-8 ) THEN
                  ifl = 1
                  xy(i,j) = reg(i,m)
               ENDIF
            ENDDO
         ENDDO
         IF ( ifl<=0 ) RETURN
         spag_nextblock_1 = 2
      CASE (11)
!
!
!     CALCULATE THE EQUATION OF THE LINE TO BE DRAWN.
!
         GOTO tra1
!
!     GIVEN Y, CALCULATE X.
!
 60      IF ( slp==infnty ) THEN
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
 80      IF ( slp==infnty ) THEN
            y = infnty
         ELSEIF ( slp==0. ) THEN
            y = Y1
         ELSE
            y = slp*x + b
         ENDIF
         GOTO tra2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE line
