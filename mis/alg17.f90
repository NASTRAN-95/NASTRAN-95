
SUBROUTINE alg17(Istak,Pltsze,Itrig,Title,Ikdum,Ifplot)
   IMPLICIT NONE
   INTEGER Ifplot , Ikdum , Istak , Itrig
   REAL Pltsze
   REAL Title(18)
   REAL bal , plttit , xback1 , xback2 , xlen1 , xlen2 , yback1 , yback2 , ylen1 , ylen2
!
!
   plttit = Pltsze*.1
   IF ( Istak<2 ) THEN
      IF ( Istak==0 ) THEN
         xlen1 = .15*Pltsze
         xlen2 = .70*Pltsze
         xback1 = -1.9 + .20*Pltsze
         xback2 = -6.2 + .20*Pltsze
         IF ( Ikdum==1 ) GOTO 100
      ELSE
         xlen1 = .70*Pltsze
         xlen2 = .15*Pltsze
         xback1 = -1.9 - .20*Pltsze
         xback2 = -6.2 - .20*Pltsze
         IF ( Ikdum/=1 ) GOTO 100
      ENDIF
      bal = .25*Pltsze
      ylen1 = .50*Pltsze
      ylen2 = -.15*Pltsze
   ELSE
      bal = .35*Pltsze
      xlen1 = .3*Pltsze
      xlen2 = xlen1
      ylen1 = .25*Pltsze
      ylen2 = -1.*ylen1
      xback1 = -1.9
      xback2 = -6.2
   ENDIF
   GOTO 200
 100  bal = .50*Pltsze
   ylen1 = .15*Pltsze
   ylen2 = -.50*Pltsze
 200  yback1 = -(.35+bal)
   yback2 = yback1 - .01*Pltsze - .175
   IF ( Itrig==2 ) xback1 = xback1 + 0.35
END SUBROUTINE alg17