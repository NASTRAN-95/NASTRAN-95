
 
 
SUBROUTINE smccrd(Dtemp,Zil,Ilim,Zol)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Ilim
   DOUBLE PRECISION Zol
   DOUBLE PRECISION Dtemp(Ilim) , Zil(Ilim)
!
! Local variable declarations
!
   INTEGER i
!
! End of declarations
!
   DO i = 1 , Ilim
      Dtemp(i) = Dtemp(i) + Zil(i)*Zol
   ENDDO
END SUBROUTINE smccrd
