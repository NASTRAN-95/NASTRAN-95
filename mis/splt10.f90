
SUBROUTINE splt10(Icomp,Comps,Nc)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Icomp , Nc
   INTEGER Comps(9)
!
! Local variable declarations
!
   INTEGER i , ic , ix , jx
!
! End of declarations
!
   IF ( Icomp==0 ) Icomp = 1
   ic = Icomp
   Nc = 0
   DO i = 1 , 9
      ix = ic/10
      jx = ic - 10*ix
      ic = ix
      IF ( jx/=0 ) THEN
         Nc = Nc + 1
         Comps(Nc) = jx
      ENDIF
      IF ( ic==0 ) EXIT
   ENDDO
   IF ( Nc==1 ) RETURN
   CALL sort(0,0,1,1,Comps,Nc)
!
!     REMOVE DUPLICATES
   ix = 1
   DO i = 2 , Nc
      IF ( Comps(i)/=Comps(i-1) ) THEN
         ix = ix + 1
         Comps(ix) = Comps(i)
      ENDIF
   ENDDO
   Nc = ix
END SUBROUTINE splt10
