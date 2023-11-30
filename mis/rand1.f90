
SUBROUTINE rand1(File,Mid,Type,Id,Comp,Q)
   IMPLICIT NONE
   REAL Head(1)
   COMMON /output/ Head
   INTEGER Comp , File , Id , Mid , Type
   INTEGER Q(2)
   INTEGER i , idr(50) , itype , mid1(2,7)
!
!     PUTS ID RECORD ON RANDOM OUTPUT FILES
!
   DATA mid1/2001 , 4HDISP , 2010 , 4HVELO , 2011 , 4HACCE , 2002 , 4HLOAD , 2003 , 4HSPCF , 2004 , 4HELFO , 2005 , 4HSTRE/
   DATA idr/50*0/
   idr(1) = 50
   idr(3) = Mid
   DO i = 1 , 7
      IF ( Type==mid1(2,i) ) EXIT
   ENDDO
   itype = mid1(1,i)
   idr(2) = itype
   idr(5) = Id*10
   idr(6) = Comp
   idr(8) = Q(1)
   idr(9) = Q(2)
   idr(10) = 2
   CALL write(File,idr(1),50,0)
   CALL write(File,Head(1),96,1)
END SUBROUTINE rand1