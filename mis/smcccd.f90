
 
SUBROUTINE smcccd(Dtemp,Zil,Ilim,Zol)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Ilim
   DOUBLE COMPLEX Zol
   DOUBLE COMPLEX Dtemp(Ilim) , Zil(Ilim)
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
END SUBROUTINE smcccd
