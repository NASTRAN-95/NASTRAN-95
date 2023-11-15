
 
 
SUBROUTINE smccrs(Temp,Zil,Ilim,Zol)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Ilim
   REAL Zol
   REAL Temp(Ilim) , Zil(Ilim)
!
! Local variable declarations
!
   INTEGER i
!
! End of declarations
!
   DO i = 1 , Ilim
      Temp(i) = Temp(i) + Zil(i)*Zol
   ENDDO
END SUBROUTINE smccrs
