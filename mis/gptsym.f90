
SUBROUTINE gptsym(Gplst,X,U,Sym,Deform)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ngp
   COMMON /blank / Ngp
!
! Dummy argument declarations
!
   INTEGER Deform
   INTEGER Gplst(1) , Sym(2)
   REAL U(2,1) , X(3,1)
!
! Local variable declarations
!
   INTEGER i , j
   REAL xx , yy
!
! End of declarations
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
