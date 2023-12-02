!*==idplot.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE idplot(Idx)
   IMPLICIT NONE
   USE C_OUTPUT
   USE C_PLTDAT
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Idx
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank , linsiz
   INTEGER :: i , nlines
   REAL , DIMENSION(2,4) :: save
   REAL :: x , y , yy
   EXTERNAL axis , print
!
! End of declarations rewritten by SPAG
!
!
!
   DATA blank , linsiz/1H  , 3/
!
!     DOES A PLOT ID EXIST AT ALL
!
   Idx = 1
   DO i = 1 , 20
      IF ( Id(i)/=blank ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
   Idx = 0
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
      DO i = 1 , 2
         save(i,1) = Xymin(i)
         Xymin(i) = 0.
         save(i,2) = Xymax(i)
         Xymax(i) = Axymax(i) + Edge(i)
         save(i,3) = Axymax(i)
         Axymax(i) = Xymax(i)
         save(i,4) = Edge(i)
         Edge(i) = 0.
      ENDDO
      nlines = (Axymax(2)-7.*Cnty)/float(2*linsiz) + .1
      IF ( iabs(Pltype)/=1 ) THEN
!
!     NOT A CRT PLOTTER. TYPE THE ID ONCE AT THE BOTTOM OF THE PAPER.
!
         CALL print(0,0,0,0,0,-1)
         x = Xymin(1) + amax1(0.,(Axymax(1)-80.*Cntx)/2.)
         y = 0.
         IF ( Pltype<0 ) y = Cnty/2.
         CALL print(x,y,1,Id,20,0)
      ELSE
!
!     FILL TOP HALF OF PLOT WITH X-AXIS LINES ALL THE WAY ACROSS.
!
         CALL axis(0,0,0,0,0,-1)
         DO i = 1 , nlines
            y = Xymax(2) - float((i-1)*linsiz)
            CALL axis(Xymin(1),y,Xymax(1),y,1,0)
         ENDDO
!
!     PRINT THE PLOT ID 2 TIMES IN THE MIDDLE OF THE PLOT.
!
         CALL print(0,0,0,0,0,-1)
         x = Xymin(1) + amax1(0.,(Axymax(1)-80.*Cntx)/2.)
         yy = y - Cnty
         DO i = 1 , 2
            y = yy - Cnty*float(i-1)
            CALL print(x,y,1,Id,20,0)
         ENDDO
!
!     FILL BOTTOM HALF OF PLOT WITH X-AXIS LINES ALL THE WAY ACROSS.
!
         CALL axis(0,0,0,0,0,-1)
         DO i = 1 , nlines
            y = Xymin(2) + float((i-1)*linsiz)
            CALL axis(Xymin(1),y,Xymax(1),y,1,0)
         ENDDO
         CALL axis(0,0,0,0,0,1)
      ENDIF
!
!     END OF ID PLOT. PUT BLANKS IN THE PLOT ID.
!
      CALL print(0,0,0,0,0,1)
      DO i = 1 , 20
         Id(i) = blank
      ENDDO
      DO i = 1 , 2
         Xymin(i) = save(i,1)
         Xymax(i) = save(i,2)
         Axymax(i) = save(i,3)
         Edge(i) = save(i,4)
      ENDDO
   END SUBROUTINE spag_block_1
!
END SUBROUTINE idplot
