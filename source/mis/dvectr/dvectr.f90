!*==dvectr.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dvectr(Gpt,X,U,Pen)
   IMPLICIT NONE
   USE C_BLANK
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gpt
   REAL , DIMENSION(3,1) :: X
   REAL , DIMENSION(2,1) :: U
   INTEGER :: Pen
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j
   REAL :: x1 , x2 , y1 , y2
   EXTERNAL line
!
! End of declarations rewritten by SPAG
!
!
!
   CALL line(0,0,0,0,0,-1)
!
!     DO NOT DRAW A VECTOR AT ANY GRID POINT WHOSE INDEX .LE. 0.
!
   DO i = 1 , Ngp
      j = Gpt(i)
      IF ( j>0 ) THEN
         x1 = X(2,j)
         y1 = X(3,j)
         x2 = U(1,j)
         y2 = U(2,j)
         CALL line(x1,y1,x2,y2,Pen,0)
      ENDIF
   ENDDO
!
   CALL line(0,0,0,0,0,1)
END SUBROUTINE dvectr
