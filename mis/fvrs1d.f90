
SUBROUTINE fvrs1d(Base,Base1,Index,Nfx)
   IMPLICIT NONE
   INTEGER Nfx
   COMPLEX Base(3,Nfx) , Base1(3,Nfx)
   INTEGER Index(Nfx)
   INTEGER i , l , loc
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
