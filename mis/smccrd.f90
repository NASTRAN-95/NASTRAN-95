
 
 
SUBROUTINE smccrd(Dtemp,Zil,Ilim,Zol)
   IMPLICIT NONE
   INTEGER Ilim
   DOUBLE PRECISION Zol
   DOUBLE PRECISION Dtemp(Ilim) , Zil(Ilim)
   INTEGER i
   DO i = 1 , Ilim
      Dtemp(i) = Dtemp(i) + Zil(i)*Zol
   ENDDO
END SUBROUTINE smccrd
