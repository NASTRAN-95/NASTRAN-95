!*==idplot.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE idplot(Idx)
   USE c_output
   USE c_pltdat
   IMPLICIT NONE
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
      IF ( id(i)/=blank ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
   Idx = 0
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
      DO I = 1 , 2
         Save(I,1) = xymin(I)
         xymin(I) = 0.
         Save(I,2) = xymax(I)
         xymax(I) = axymax(I) + edge(I)
         Save(I,3) = axymax(I)
         axymax(I) = xymax(I)
         Save(I,4) = edge(I)
         edge(I) = 0.
      ENDDO
      Nlines = (axymax(2)-7.*cnty)/float(2*Linsiz) + .1
      IF ( iabs(pltype)/=1 ) THEN
!
!     NOT A CRT PLOTTER. TYPE THE ID ONCE AT THE BOTTOM OF THE PAPER.
!
         CALL print(0,0,0,0,0,-1)
         X = xymin(1) + amax1(0.,(axymax(1)-80.*cntx)/2.)
         Y = 0.
         IF ( pltype<0 ) Y = cnty/2.
         CALL print(X,Y,1,id,20,0)
      ELSE
!
!     FILL TOP HALF OF PLOT WITH X-AXIS LINES ALL THE WAY ACROSS.
!
         CALL axis(0,0,0,0,0,-1)
         DO I = 1 , Nlines
            Y = xymax(2) - float((I-1)*Linsiz)
            CALL axis(xymin(1),Y,xymax(1),Y,1,0)
         ENDDO
!
!     PRINT THE PLOT ID 2 TIMES IN THE MIDDLE OF THE PLOT.
!
         CALL print(0,0,0,0,0,-1)
         X = xymin(1) + amax1(0.,(axymax(1)-80.*cntx)/2.)
         Yy = Y - cnty
         DO I = 1 , 2
            Y = Yy - cnty*float(I-1)
            CALL print(X,Y,1,id,20,0)
         ENDDO
!
!     FILL BOTTOM HALF OF PLOT WITH X-AXIS LINES ALL THE WAY ACROSS.
!
         CALL axis(0,0,0,0,0,-1)
         DO I = 1 , Nlines
            Y = xymin(2) + float((I-1)*Linsiz)
            CALL axis(xymin(1),Y,xymax(1),Y,1,0)
         ENDDO
         CALL axis(0,0,0,0,0,1)
      ENDIF
!
!     END OF ID PLOT. PUT BLANKS IN THE PLOT ID.
!
      CALL print(0,0,0,0,0,1)
      DO I = 1 , 20
         id(I) = Blank
      ENDDO
      DO I = 1 , 2
         xymin(I) = Save(I,1)
         xymax(I) = Save(I,2)
         axymax(I) = Save(I,3)
         edge(I) = Save(I,4)
      ENDDO
   END SUBROUTINE spag_block_1
!
END SUBROUTINE idplot
