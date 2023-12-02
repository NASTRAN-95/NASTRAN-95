!*==fvrs1b.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fvrs1b(Base,W1,Nf)
   USE c_blank
   USE c_condas
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nf
   COMPLEX , DIMENSION(3,Nf) :: Base
   REAL , DIMENSION(Nf) :: W1
!
! Local variable declarations rewritten by SPAG
!
   REAL :: f , phi , rad , xo
   INTEGER :: i , k , lp , lt
   COMPLEX :: p , z1
   EXTERNAL tab
!
! End of declarations rewritten by SPAG
!
!
!     SUBROUTINE TO COMPUTE BASE(FI)(3X1) FOR MODFRL=FALSE
!
!
!
!
   DO k = 1 , Nf
      f = W1(k)/twopi
      lt = 1
      lp = 2
      DO i = 1 , 3
         IF ( it(lt)==-1 ) THEN
            Base(i,k) = (0.0,0.0)
         ELSE
            CALL tab(it(lt),f,xo)
            IF ( it(lp)==-1 ) THEN
               p = (1.0,0.0)
            ELSE
               CALL tab(it(lp),f,phi)
               rad = phi*degra
               z1 = cmplx(0.0,rad)
               p = cexp(z1)
            ENDIF
            Base(i,k) = xo*p
         ENDIF
         lt = lt + 2
         lp = lp + 2
      ENDDO
   ENDDO
END SUBROUTINE fvrs1b
