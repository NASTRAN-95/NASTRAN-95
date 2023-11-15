
 
 
SUBROUTINE smcccs(Ctemp,Zil,Ilim,Zol)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Ilim
   COMPLEX Zol
   COMPLEX Ctemp(Ilim) , Zil(Ilim)
!
! Local variable declarations
!
   INTEGER i
!
! End of declarations
!
   DO i = 1 , Ilim
      Ctemp(i) = Ctemp(i) + Zil(i)*Zol
   ENDDO
END SUBROUTINE smcccs
