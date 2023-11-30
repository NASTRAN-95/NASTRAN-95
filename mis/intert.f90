
SUBROUTINE intert(Nl,Nl1,Nl2,Nm,Ajj,Ta)
   IMPLICIT NONE
   INTEGER Nl , Nl1 , Nl2 , Nm
   REAL Ajj(1) , Ta(1)
   REAL fract , t , t1 , t2
   INTEGER i , n , n1 , n2
!
!
   t1 = Ta(Nl1)
   t2 = Ta(Nl2)
   t = Ta(Nl)
   n1 = Nm*(Nl1-1)
   n2 = Nm*(Nl2-1)
   n = Nm*(Nl-1)
   fract = (t-t1)/(t2-t1)
   DO i = 1 , Nm
      Ajj(i+n) = Ajj(i+n1) + fract*(Ajj(i+n2)-Ajj(i+n1))
   ENDDO
END SUBROUTINE intert
