!*==gptsym.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gptsym(Gplst,X,U,Sym,Deform)
   IMPLICIT NONE
   USE C_BLANK
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gplst
   REAL , DIMENSION(3,1) :: X
   REAL , DIMENSION(2,1) :: U
   INTEGER , DIMENSION(2) :: Sym
   INTEGER :: Deform
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j
   REAL :: xx , yy
   EXTERNAL symbol
!
! End of declarations rewritten by SPAG
!
!
!
   CALL symbol(0,0,0,-1)
!
!     IF THE GRID POINT INDEX IS 0 (NOT IN SET) OR NEGATIVE (EXCLUDED),
!     NEVER PUT A SYMBOL AT THAT GRID POINT.
!
   DO i = 1 , Ngp
      j = Gplst(i)
      IF ( j>0 ) THEN
         IF ( Deform/=0 ) THEN
            xx = U(1,j)
            yy = U(2,j)
         ELSE
            xx = X(2,j)
            yy = X(3,j)
         ENDIF
         CALL symbol(xx,yy,Sym,0)
      ENDIF
   ENDDO
!
   CALL symbol(0,0,0,1)
END SUBROUTINE gptsym
