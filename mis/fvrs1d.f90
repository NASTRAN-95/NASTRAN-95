
SUBROUTINE fvrs1d(Base,Base1,Index,Nfx)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Nfx
   COMPLEX Base(3,Nfx) , Base1(3,Nfx)
   INTEGER Index(Nfx)
!
! Local variable declarations
!
   INTEGER i , l , loc
!
! End of declarations
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
