!*==fvrs1d.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fvrs1d(Base,Base1,Index,Nfx)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nfx
   COMPLEX , DIMENSION(3,Nfx) :: Base
   COMPLEX , DIMENSION(3,Nfx) :: Base1
   INTEGER , DIMENSION(Nfx) :: Index
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , l , loc
!
! End of declarations rewritten by SPAG
!
!
!
!
   DO i = 1 , Nfx
      loc = Index(i)
      DO l = 1 , 3
         Base1(l,i) = Base(l,loc)
      ENDDO
   ENDDO
!
!-----RETURN BASE1 TO BASE
!
   DO i = 1 , Nfx
      DO l = 1 , 3
         Base(l,i) = Base1(l,i)
      ENDDO
   ENDDO
END SUBROUTINE fvrs1d
