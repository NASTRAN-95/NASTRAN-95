
 
 
SUBROUTINE smccrs(Temp,Zil,Ilim,Zol)
   IMPLICIT NONE
   INTEGER Ilim
   REAL Zol
   REAL Temp(Ilim) , Zil(Ilim)
   INTEGER i
   DO i = 1 , Ilim
      Temp(i) = Temp(i) + Zil(i)*Zol
   ENDDO
END SUBROUTINE smccrs