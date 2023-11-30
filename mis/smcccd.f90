
 
SUBROUTINE smcccd(Dtemp,Zil,Ilim,Zol)
   IMPLICIT NONE
   INTEGER Ilim
   DOUBLE COMPLEX Zol
   DOUBLE COMPLEX Dtemp(Ilim) , Zil(Ilim)
   INTEGER i
   DO i = 1 , Ilim
      Dtemp(i) = Dtemp(i) + Zil(i)*Zol
   ENDDO
END SUBROUTINE smcccd
