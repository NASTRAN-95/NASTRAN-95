
SUBROUTINE idplot(Idx)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Axymax(2) , Cntx , Cnty , Edge(12) , Skpa(3) , Skpb(4) , Skpout(32,6) , Skpplt(2) , Xymax(2) , Xymin(2)
   INTEGER Id(32) , Pltype
   COMMON /output/ Skpout , Id
   COMMON /pltdat/ Skpplt , Xymin , Xymax , Axymax , Edge , Skpa , Cntx , Cnty , Skpb , Pltype
!
! Dummy argument declarations
!
   INTEGER Idx
!
! Local variable declarations
!
   INTEGER blank , i , linsiz , nlines
   REAL save(2,4) , x , y , yy
!
! End of declarations
!
!
!
   DATA blank , linsiz/1H  , 3/
!
!     DOES A PLOT ID EXIST AT ALL
!
   Idx = 1
   DO i = 1 , 20
      IF ( Id(i)/=blank ) GOTO 100
   ENDDO
   Idx = 0
   GOTO 99999
!
 100  DO i = 1 , 2
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
!
99999 END SUBROUTINE idplot
