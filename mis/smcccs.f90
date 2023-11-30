
 
 
SUBROUTINE smcccs(Ctemp,Zil,Ilim,Zol)
   IMPLICIT NONE
   INTEGER Ilim
   COMPLEX Zol
   COMPLEX Ctemp(Ilim) , Zil(Ilim)
   INTEGER i
   DO i = 1 , Ilim
      Ctemp(i) = Ctemp(i) + Zil(i)*Zol
   ENDDO
END SUBROUTINE smcccs
