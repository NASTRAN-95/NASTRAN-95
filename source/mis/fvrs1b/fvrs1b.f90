!*==fvrs1b.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fvrs1b(Base,W1,Nf)
   IMPLICIT NONE
   USE C_BLANK
   USE C_CONDAS
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
      f = W1(k)/Twopi
      lt = 1
      lp = 2
      DO i = 1 , 3
         IF ( It(lt)==-1 ) THEN
            Base(i,k) = (0.0,0.0)
         ELSE
            CALL tab(It(lt),f,xo)
            IF ( It(lp)==-1 ) THEN
               p = (1.0,0.0)
            ELSE
               CALL tab(It(lp),f,phi)
               rad = phi*Degra
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
